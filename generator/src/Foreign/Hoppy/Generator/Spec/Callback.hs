-- This file is part of Hoppy.
--
-- Copyright 2015-2019 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | Interface for defining foreign language callbacks.
module Foreign.Hoppy.Generator.Spec.Callback (
  -- * Data type
  Callback, callbackT,
  -- * Construction
  makeCallback,
  -- * Properties
  callbackExtName,
  callbackParams,
  callbackReturn,
  callbackReqs,
  callbackAddendum,
  -- ** Exceptions
  callbackThrows,
  callbackSetThrows,
  -- * C++ generator
  cppCallbackToTFn,
  -- ** Names
  callbackClassName,
  callbackImplClassName,
  callbackFnName,
  -- * Haskell generator
  hsCallbackToTFn,
  -- ** Names
  toHsCallbackCtorName, toHsCallbackCtorName',
  toHsCallbackNewFunPtrFnName, toHsCallbackNewFunPtrFnName',
  ) where

import Control.Monad (forM_, when)
import Data.Function (on)
import Data.Maybe (fromMaybe, isJust)
import qualified Foreign.Hoppy.Generator.Language.Cpp as LC
import qualified Foreign.Hoppy.Generator.Language.Haskell as LH
import Foreign.Hoppy.Generator.Spec.Base
import qualified Foreign.Hoppy.Generator.Spec.Function as Function
import Foreign.Hoppy.Generator.Types (boolT, constT, fnT, fnT', intT, manualT, ptrT, voidT)
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (Special, UnQual),
  HsSpecialCon (HsUnitCon),
  HsType (HsTyApp, HsTyCon, HsTyFun),
  )

-- | A non-C++ function that can be invoked via a C++ functor or function
-- pointer.
--
-- Use this data type's 'HasReqs' instance to add extra requirements, however
-- manually adding requirements for parameter and return types is not necessary.
data Callback = Callback
  { callbackExtName :: ExtName
    -- ^ The callback's external name.
  , callbackParams :: [Parameter]
    -- ^ The callback's parameters.
  , callbackReturn :: Type
    -- ^ The callback's return type.
  , callbackThrows :: Maybe Bool
    -- ^ Whether the callback supports throwing C++ exceptions from Haskell into
    -- C++ during its execution.  When absent, the value is inherited from
    -- 'moduleCallbacksThrow' and 'interfaceCallbacksThrow'.
  , callbackReqs :: Reqs
    -- ^ Extra requirements for the callback.
  , callbackAddendum :: Addendum
    -- ^ The callback's addendum.
  }

instance Eq Callback where
  (==) = (==) `on` callbackExtName

instance Show Callback where
  show cb =
    concat ["<Callback ", show (callbackExtName cb), " ", show (callbackParams cb), " ",
            show (callbackReturn cb)]

instance Exportable Callback where
  sayExportCpp = sayCppExport
  sayExportHaskell = sayHsExport

instance HasExtNames Callback where
  getPrimaryExtName = callbackExtName

instance HasReqs Callback where
  getReqs = callbackReqs
  setReqs reqs cb = cb { callbackReqs = reqs }

instance HasAddendum Callback where
  getAddendum = callbackAddendum
  setAddendum addendum cb = cb { callbackAddendum = addendum }

-- | Creates a binding for constructing callbacks into foreign code.
makeCallback :: IsParameter p
             => ExtName
             -> [p]  -- ^ Parameter types.
             -> Type  -- ^ Return type.
             -> Callback
makeCallback extName paramTypes retType =
  Callback extName (toParameters paramTypes) retType Nothing mempty mempty

-- | Sets whether a callback supports handling thrown C++ exceptions and passing
-- them into C++.
callbackSetThrows :: Bool -> Callback -> Callback
callbackSetThrows value cb = cb { callbackThrows = Just value }

makeConversion :: Callback -> ConversionSpec
makeConversion cb =
  (makeConversionSpec (show cb) cpp)
  { conversionSpecHaskell = hs }
  where reqsGen = do
          -- TODO Should this be includeStd?
          cbClassReqs <- reqInclude . includeLocal . moduleHppPath <$>
                         LC.findExportModule (callbackExtName cb)
          -- TODO Is the right 'ReqsType' being used recursively here?
          fnTypeReqs <- LC.typeReqs =<< cppCallbackToTFn cb
          return $ cbClassReqs `mappend` fnTypeReqs

        cpp =
          (makeConversionSpecCpp (callbackClassName cb) reqsGen)
          { conversionSpecCppConversionType = return $ Just $ ptrT callbackImplClassType
          , conversionSpecCppConversionToCppExpr = Just $ \fromVar maybeToVar -> case maybeToVar of
              Just toVar ->
                LC.says [callbackClassName cb, " "] >> toVar >> LC.say "(" >>
                fromVar >> LC.say ");\n"
              Nothing -> LC.says [callbackClassName cb, "("] >> fromVar >> LC.say ")"
            -- No from-C++ conversion; we don't support passing callbacks back out again.
          }

        hs =
          Just $ makeConversionSpecHaskell
          (LH.cppTypeToHsTypeAndUse LH.HsHsSide =<< hsCallbackToTFn LH.HsHsSide cb)
          (Just $ do
             LH.addImports hsImportForRuntime
             HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyFHR.CCallback") <$>
               (LH.cppTypeToHsTypeAndUse LH.HsCSide =<< hsCallbackToTFn LH.HsCSide cb))
          (CustomConversion $ LH.sayLn =<< toHsCallbackCtorName cb)
          ConversionUnsupported  -- Can't receive a callback from C++.

        callbackImplClassType =
          manualT $
          makeConversionSpec implClass $
          makeConversionSpecCpp implClass reqsGen

        implClass = callbackImplClassName cb

-- | Constructs a type value for a callback.
callbackT :: Callback -> Type
callbackT = manualT . makeConversion

-- | Returns the name of the outer, copyable C++ class for a callback.
callbackClassName :: Callback -> String
callbackClassName = fromExtName . callbackExtName

-- | Returns the name of the internal, non-copyable implementation C++ class for
-- a callback.
callbackImplClassName :: Callback -> String
callbackImplClassName = (++ "_impl") . fromExtName . callbackExtName

-- | Returns the name of the C++ binding function that creates a C++ callback
-- wrapper object from a function pointer to foreign code.
callbackFnName :: Callback -> String
callbackFnName = LC.externalNameToCpp . callbackExtName

sayCppExport :: LC.SayExportMode -> Callback -> LC.Generator ()
sayCppExport mode cb = do
  throws <- cppGetEffectiveCallbackThrows cb

  let className = callbackClassName cb
      implClassName = callbackImplClassName cb
      fnName = callbackFnName cb
      params = callbackParams cb
      paramTypes = map parameterType params
      paramCount = length params
      retType = callbackReturn cb
      fnType = fnT' params retType

  -- The function pointer we receive from foreign code will work with C-types,
  -- so determine what that function looks like.
  paramCTypes <- zipWith fromMaybe paramTypes <$> mapM LC.typeToCType paramTypes
  retCType <- fromMaybe retType <$> LC.typeToCType retType

  -- Add requirements specified manually by the callback, and for its parameter
  -- and return types.
  LC.addReqsM . mconcat . (callbackReqs cb:) =<< mapM LC.typeReqs (retType:paramTypes)

  let fnCType = fnT ((if throws then (++ [ptrT intT, ptrT $ ptrT voidT]) else id)
                     paramCTypes)
                    retCType
      fnPtrCType = ptrT fnCType

  case mode of
    LC.SayHeader -> do
      -- Render the class declarations into the header file.
      (sharedPtrReqs, sharedPtrStr) <- interfaceSharedPtr <$> LC.askInterface
      LC.addReqsM sharedPtrReqs

      LC.says ["\nclass ", implClassName, " {\n"]
      LC.say "public:\n"
      LC.says ["    explicit ", implClassName, "("] >> LC.sayType Nothing fnPtrCType >>
        LC.say ", void(*)(void(*)()), bool);\n"
      LC.says ["    ~", implClassName, "();\n"]
      LC.say "    " >> LC.sayVar "operator()" Nothing fnType >> LC.say ";\n"
      LC.say "private:\n"
      LC.says ["    ", implClassName, "(const ", implClassName, "&);\n"]
      LC.says ["    ", implClassName, "& operator=(const ", implClassName, "&);\n"]
      LC.say "\n"
      LC.say "    " >> LC.sayVar "f_" Nothing (constT fnPtrCType) >> LC.say ";\n"
      LC.say "    void (*const release_)(void(*)());\n"
      LC.say "    const bool releaseRelease_;\n"
      LC.say "};\n"

      LC.says ["\nclass ", className, " {\n"]
      LC.say "public:\n"
      LC.says ["    ", className, "() {}\n"]
      LC.says ["    explicit ", className, "(", implClassName, "* impl) : impl_(impl) {}\n"]
      LC.say "    " >> LC.sayVar "operator()" Nothing fnType >> LC.say ";\n"
      LC.say "    operator bool() const;\n"
      LC.say "private:\n"
      LC.says ["    ", sharedPtrStr, "<", implClassName, "> impl_;\n"]
      LC.say "};\n"

    LC.SaySource -> do
      -- Render the classes' methods into the source file.  First render the
      -- impl class's constructor.
      LC.says ["\n", implClassName, "::", implClassName, "("] >> LC.sayVar "f" Nothing fnPtrCType >>
        LC.say ", void (*release)(void(*)()), bool releaseRelease) :\n"
      LC.say "    f_(f), release_(release), releaseRelease_(releaseRelease) {}\n"

      -- Then render the destructor.
      LC.says ["\n", implClassName, "::~", implClassName, "() {\n"]
      LC.say "    if (release_) {\n"
      LC.say "        release_(reinterpret_cast<void(*)()>(f_));\n"
      LC.say "        if (releaseRelease_) {\n"
      LC.say "            release_(reinterpret_cast<void(*)()>(release_));\n"
      LC.say "        }\n"
      LC.say "    }\n"
      LC.say "}\n"

      -- Render the impl operator() method, which does argument decoding and
      -- return value encoding and passes C++ values to underlying function
      -- poiner.
      --
      -- TODO Abstract the duplicated code here and in sayExportFn.
      paramCTypeMaybes <- mapM LC.typeToCType paramTypes
      retCTypeMaybe <- LC.typeToCType retType

      LC.sayFunction (implClassName ++ "::operator()")
                     (zipWith3 (\pt ctm ->
                                  -- TManual needs special handling to determine whether a
                                  -- conversion is necessary.  'typeToCType' doesn't suffice
                                  -- because for TManual this check relies on the direction of
                                  -- the call.  See the special case in 'sayCppArgRead' as
                                  -- well.
                                  let hasConversion = case pt of
                                        Internal_TManual s ->
                                          isJust $ conversionSpecCppConversionToCppExpr $
                                          conversionSpecCpp s
                                        _ -> isJust ctm
                                  in if hasConversion then LC.toArgNameAlt else LC.toArgName)
                               paramTypes
                               paramCTypeMaybes
                               [1..paramCount])
                     fnType $ Just $ do
        -- Convert arguments that aren't passed in directly.
        mapM_ (Function.sayCppArgRead Function.DoEncode) $
          zip3 [1..] paramTypes paramCTypeMaybes

        when throws $ do
          LC.says ["int ", LC.exceptionIdArgName, " = 0;\n"]
          LC.says ["void *", LC.exceptionPtrArgName, " = 0;\n"]

          -- Add an include for the exception support module to be able to call the
          -- C++ rethrow function.
          iface <- LC.askInterface
          currentModule <- LC.askModule
          case interfaceExceptionSupportModule iface of
            Just exceptionSupportModule ->
              when (exceptionSupportModule /= currentModule) $
                -- TODO Should this be includeStd?
                LC.addReqsM $ reqInclude $ includeLocal $ moduleHppPath exceptionSupportModule
            Nothing -> LC.abort $ "sayExportCallback: " ++ show iface ++
                       " uses exceptions, so it needs an exception support " ++
                       "module.  Please use interfaceSetExceptionSupportModule."

        -- Invoke the function pointer into foreign code.
        let -- | Generates the call to the foreign language function pointer.
            sayCall :: LC.Generator ()
            sayCall = do
              LC.say "f_("
              Function.sayCppArgNames paramCount
              when throws $ do
                when (paramCount /= 0) $ LC.say ", "
                LC.says ["&", LC.exceptionIdArgName, ", &", LC.exceptionPtrArgName]
              LC.say ")"

            -- | Generates code to check whether an exception was thrown by the
            -- callback, and rethrows it in C++ if so.
            sayExceptionCheck :: LC.Generator ()
            sayExceptionCheck = when throws $ do
              LC.says ["if (", LC.exceptionIdArgName, " != 0) { ",
                       LC.exceptionRethrowFnName, "(", LC.exceptionIdArgName, ", ",
                       LC.exceptionPtrArgName, "); }\n"]

        case (retType, retCTypeMaybe) of
          (Internal_TVoid, Nothing) -> do
            sayCall >> LC.say ";\n"
            sayExceptionCheck
          (_, Nothing) -> do
            LC.sayVar "result" Nothing retType >> LC.say " = " >> sayCall >> LC.say ";\n"
            sayExceptionCheck
            LC.say "return result;\n"
          (Internal_TObj cls1, Just retCType@(Internal_TPtr (Internal_TConst (Internal_TObj cls2))))
            | cls1 == cls2 -> do
            LC.sayVar "resultPtr" Nothing retCType >> LC.say " = " >> sayCall >> LC.say ";\n"
            sayExceptionCheck
            LC.sayVar "result" Nothing retType >> LC.say " = *resultPtr;\n"
            LC.say "delete resultPtr;\n"
            LC.say "return result;\n"
          (Internal_TRef (Internal_TConst (Internal_TObj cls1)),
           Just (Internal_TPtr (Internal_TConst (Internal_TObj cls2)))) | cls1 == cls2 -> do
            LC.sayVar "resultPtr" Nothing retCType >> LC.say " = " >> sayCall >> LC.say ";\n"
            sayExceptionCheck
            LC.say "return *resultPtr;\n"
          (Internal_TRef (Internal_TObj cls1),
           Just (Internal_TPtr (Internal_TObj cls2))) | cls1 == cls2 -> do
            LC.sayVar "resultPtr" Nothing retCType >> LC.say " = " >> sayCall >> LC.say ";\n"
            sayExceptionCheck
            LC.say "return *resultPtr;\n"
          ts -> LC.abort $ concat
                ["sayExportCallback: Unexpected return types ", show ts, "."]

      -- Render the non-impl operator() method, which simply passes C++ values
      -- along to the impl object.
      LC.sayFunction (className ++ "::operator()")
                     (map LC.toArgName [1..paramCount])
                     fnType $ Just $ do
        case retType of
          Internal_TVoid -> LC.say "(*impl_)("
          _ -> LC.say "return (*impl_)("
        Function.sayCppArgNames paramCount
        LC.say ");\n"

      -- Render "operator bool", which detects whether the callback was not
      -- default-constructed with no actual impl object.
      LC.says [className, "::operator bool() const {\n"]
      LC.say "return static_cast<bool>(impl_);\n"
      LC.say "}\n"

      -- Render the function that creates a new callback object.
      let newCallbackFnType = fnT [ fnPtrCType
                                  , ptrT (fnT [ptrT $ fnT [] voidT] voidT)
                                  , boolT
                                  ] $
                              Internal_TManual $
                              makeConversionSpec ("<Internal " ++ implClassName ++ " pointer>") $
                              makeConversionSpecCpp (implClassName ++ "*") (return mempty)
      LC.sayFunction fnName ["f", "release", "releaseRelease"] newCallbackFnType $ Just $
        LC.says ["return new ", implClassName, "(f, release, releaseRelease);\n"]

-- | Prints \"foreign import\" statements and an internal callback construction
-- function for a given 'Callback' specification.  For example, for a callback
-- of 'HsHsSide' type @Int -> String -> IO Int@, we will generate the following
-- bindings:
--
-- > foreign import ccall "wrapper" name'newFunPtr
-- >   :: (CInt -> Ptr CChar -> IO CInt)
-- >   -> IO (FunPtr (CInt -> Ptr CChar -> IO CInt))
-- >
-- > -- (This is an ad-hoc generated binding for C++ callback impl class constructor.)
-- > foreign import ccall "genpop__name_impl" name'newCallback
-- >   :: FunPtr (CInt -> Ptr CChar -> IO CInt)
-- >   -> FunPtr (FunPtr (IO ()) -> IO ())
-- >   -> Bool
-- >   -> IO (CCallback (CInt -> Ptr CChar -> IO CInt))
-- >
-- > name_newFunPtr :: (Int -> String -> IO Int) -> IO (FunPtr (CInt -> Ptr CChar -> IO CInt))
-- > name_newFunPtr f'hs = name'newFunPtr $ \excIdPtr excPtrPtr arg1 arg2 ->
-- >   internalHandleCallbackExceptions excIdPtr excPtrPtr $
-- >   coerceIntegral arg1 >>= \arg1' ->
-- >   (...decode the C string) >>= \arg2' ->
-- >   fmap coerceIntegral
-- >   (f'hs arg1' arg2')
-- >
-- > name_new :: (Int -> String -> IO Int) -> IO (CCallback (CInt -> Ptr CChar -> IO CInt))
-- > name_new f = do
-- >   f'p <- name_newFunPtr f
-- >   name'newCallback f'p freeHaskellFunPtrFunPtr False
sayHsExport :: LH.SayExportMode -> Callback -> LH.Generator ()
sayHsExport mode cb =
  LH.withErrorContext ("generating callback " ++ show (callbackExtName cb)) $ do
    let name = callbackExtName cb
        params = callbackParams cb
        retType = callbackReturn cb
    hsNewFunPtrFnName <- toHsCallbackNewFunPtrFnName cb
    hsCtorName <- toHsCallbackCtorName cb
    let hsCtorName'newCallback = hsCtorName ++ "'newCallback"
        hsCtorName'newFunPtr = hsCtorName ++ "'newFunPtr"

    hsFnCType <- LH.cppTypeToHsTypeAndUse LH.HsCSide =<< hsCallbackToTFn LH.HsCSide cb
    hsFnHsType <- LH.cppTypeToHsTypeAndUse LH.HsHsSide =<< hsCallbackToTFn LH.HsHsSide cb

    let getWholeNewFunPtrFnType = do
          LH.addImports $ mconcat [hsImportForForeign, hsImportForPrelude]
          return $
            HsTyFun hsFnHsType $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.FunPtr") hsFnCType

        getWholeCtorType = do
          LH.addImports $ mconcat [hsImportForPrelude, hsImportForRuntime]
          return $
            HsTyFun hsFnHsType $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyFHR.CCallback") hsFnCType

    case mode of
      LH.SayExportForeignImports -> do
        LH.addImports $ mconcat [hsImportForForeign, hsImportForPrelude, hsImportForRuntime]
        let hsFunPtrType = HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.FunPtr") hsFnCType
            hsFunPtrImportType =
              HsTyFun hsFnCType $
              HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") hsFunPtrType
            hsCallbackCtorImportType =
              HsTyFun hsFunPtrType $
              HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.FunPtr") $
                       HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.FunPtr") $
                                HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") $
                                HsTyCon $ Special HsUnitCon) $
                       HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") $
                       HsTyCon $ Special HsUnitCon) $
              HsTyFun (HsTyCon $ UnQual $ HsIdent "HoppyP.Bool") $
              HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") $
              HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyFHR.CCallback") hsFnCType

        LH.saysLn ["foreign import ccall \"wrapper\" ", hsCtorName'newFunPtr, " :: ",
                   LH.prettyPrint hsFunPtrImportType]
        LH.saysLn ["foreign import ccall \"", LC.externalNameToCpp name, "\" ",
                   hsCtorName'newCallback, " :: ", LH.prettyPrint hsCallbackCtorImportType]

      LH.SayExportDecls -> do
        LH.addExports [hsNewFunPtrFnName, hsCtorName]

        -- Generate the *_newFunPtr function.
        wholeNewFunPtrFnType <- getWholeNewFunPtrFnType
        let paramCount = length params
            argNames = map LH.toArgName [1..paramCount]
            argNames' = map (++ "'") argNames
        throws <- hsGetEffectiveCallbackThrows cb
        LH.addImports $ mconcat [hsImport1 "Prelude" "($)",
                                 hsImportForRuntime]
        LH.ln
        LH.saysLn [hsNewFunPtrFnName, " :: ", LH.prettyPrint wholeNewFunPtrFnType]
        LH.saysLn $ hsNewFunPtrFnName : " f'hs = " : hsCtorName'newFunPtr : " $" :
          case (if throws then (++ ["excIdPtr", "excPtrPtr"]) else id) argNames of
            [] -> []
            argNames' -> [" \\", unwords argNames', " ->"]
        LH.indent $ do
          when throws $ LH.sayLn "HoppyFHR.internalHandleCallbackExceptions excIdPtr excPtrPtr $"
          forM_ (zip3 params argNames argNames') $ \(p, argName, argName') ->
            Function.sayHsArgProcessing Function.FromCpp (parameterType p) argName argName'
          Function.sayHsCallAndProcessReturn Function.FromCpp retType $
            "f'hs" : map (' ':) argNames'

        -- Generate the *_new function.
        wholeCtorType <- getWholeCtorType
        LH.ln
        LH.saysLn [hsCtorName, " :: ", LH.prettyPrint wholeCtorType]
        LH.saysLn [hsCtorName, " f'hs = do"]
        LH.indent $ do
          LH.saysLn ["f'p <- ", hsNewFunPtrFnName, " f'hs"]
          LH.saysLn [hsCtorName'newCallback, " f'p HoppyFHR.freeHaskellFunPtrFunPtr HoppyP.False"]

      LH.SayExportBoot -> do
        LH.addExports [hsNewFunPtrFnName, hsCtorName]
        wholeNewFunPtrFnType <- getWholeNewFunPtrFnType
        wholeCtorType <- getWholeCtorType
        LH.ln
        LH.saysLn [hsNewFunPtrFnName, " :: ", LH.prettyPrint wholeNewFunPtrFnType]
        LH.ln
        LH.saysLn [hsCtorName, " :: ", LH.prettyPrint wholeCtorType]

-- | The name of the function that takes a Haskell function and wraps it in a
-- callback object.  This is internal to the binding; normal users can pass
-- Haskell functions to be used as callbacks inplicitly.
toHsCallbackCtorName :: Callback -> LH.Generator String
toHsCallbackCtorName callback =
  LH.inFunction "toHsCallbackCtorName" $
  LH.addExtNameModule (callbackExtName callback) $ toHsCallbackCtorName' callback

-- | Pure version of 'toHsCallbackCtorName' that doesn't create a qualified
-- name.
toHsCallbackCtorName' :: Callback -> String
toHsCallbackCtorName' callback =
  LH.toHsFnName' $ toExtName $ fromExtName (callbackExtName callback) ++ "_new"

-- | The name of the function that takes a Haskell function with Haskell-side
-- types and wraps it in a 'Foreign.Ptr.FunPtr' that does appropriate
-- conversions to and from C-side types.
toHsCallbackNewFunPtrFnName :: Callback -> LH.Generator String
toHsCallbackNewFunPtrFnName callback =
  LH.inFunction "toHsCallbackNewFunPtrFnName" $
  LH.addExtNameModule (callbackExtName callback) $ toHsCallbackNewFunPtrFnName' callback

-- | Pure version of 'toHsCallbackNewFunPtrFnName' that doesn't create a qualified
-- name.
toHsCallbackNewFunPtrFnName' :: Callback -> String
toHsCallbackNewFunPtrFnName' callback =
  LH.toHsFnName' $ toExtName $ fromExtName (callbackExtName callback) ++ "_newFunPtr"

cppGetEffectiveCallbackThrows :: Callback -> LC.Generator Bool
cppGetEffectiveCallbackThrows cb = case callbackThrows cb of
  Just b -> return b
  Nothing -> moduleCallbacksThrow <$> LC.askModule >>= \case
    Just b -> return b
    Nothing -> interfaceCallbacksThrow <$> LC.askInterface

hsGetEffectiveCallbackThrows :: Callback -> LH.Generator Bool
hsGetEffectiveCallbackThrows cb = case callbackThrows cb of
  Just b -> return b
  Nothing -> moduleCallbacksThrow <$> LH.askModule >>= \case
    Just b -> return b
    Nothing -> interfaceCallbacksThrow <$> LH.askInterface

-- | Constructs the function type for a callback.  A callback that throws has
-- additional parameters.
--
-- Keep this in sync with 'hsCallbackToTFn'.
cppCallbackToTFn :: Callback -> LC.Generator Type
cppCallbackToTFn cb = do
  throws <- mayThrow
  return $ Internal_TFn ((if throws then addExcParams else id) $ callbackParams cb)
                        (callbackReturn cb)

  where mayThrow = case callbackThrows cb of
          Just t -> return t
          Nothing -> moduleCallbacksThrow <$> LC.askModule >>= \mt -> case mt of
            Just t -> return t
            Nothing -> interfaceCallbacksThrow <$> LC.askInterface

        addExcParams = (++ [toParameter $ ptrT intT, toParameter $ ptrT $ ptrT voidT])

-- | Constructs the function type for a callback.  For Haskell, the type depends
-- on the side; the C++ side has additional parameters.
--
-- Keep this in sync with 'cppCallbackToTFn'.
hsCallbackToTFn :: LH.HsTypeSide -> Callback -> LH.Generator Type
hsCallbackToTFn side cb = do
  needsExcParams <- case side of
    LH.HsCSide -> mayThrow
    LH.HsHsSide -> return False
  return $ Internal_TFn ((if needsExcParams then addExcParams else id) $ callbackParams cb)
                        (callbackReturn cb)

  where mayThrow = case callbackThrows cb of
          Just t -> return t
          Nothing -> moduleCallbacksThrow <$> LH.askModule >>= \mt -> case mt of
            Just t -> return t
            Nothing -> interfaceCallbacksThrow <$> LH.askInterface

        addExcParams = (++ [toParameter $ ptrT intT, toParameter $ ptrT $ ptrT voidT])
