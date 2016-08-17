-- This file is part of Hoppy.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

{-# LANGUAGE CPP #-}

-- | Internal portion of the Haskell code generator.
module Foreign.Hoppy.Generator.Language.Haskell.Internal (
  Generation,
  generate,
  generatedFiles,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif
import Control.Arrow ((&&&), second)
import Control.Monad (forM, unless, when)
#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except (throwError)
#else
import Control.Monad.Error (throwError)
#endif
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriterT, tell)
import Data.Foldable (forM_)
import Data.Graph (SCC (AcyclicSCC, CyclicSCC), stronglyConnComp)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (isJust, mapMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import qualified Data.Set as S
import Foreign.Hoppy.Generator.Common
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Types
import Foreign.Hoppy.Generator.Language.Cpp (
  classCastFnCppName,
  classDeleteFnCppName,
  externalNameToCpp,
  )
import Foreign.Hoppy.Generator.Language.Haskell
import Language.Haskell.Syntax (
  HsAsst,
  HsContext,
  HsName (HsIdent),
  HsQName (Special, UnQual),
  HsQualType (HsQualType),
  HsSpecialCon (HsUnitCon),
  HsType (HsTyApp, HsTyCon, HsTyFun, HsTyVar),
  )
import System.FilePath ((<.>), pathSeparator)

-- | The in-memory result of generating Haskell code for an interface.
data Generation = Generation
  { generatedFiles :: M.Map FilePath String
    -- ^ A map from paths of generated files to the contents of those files.
    -- The file paths are relative paths below the Haskell generation root.
  }

-- | Runs the C++ code generator against an interface.
generate :: Interface -> Either ErrorMsg Generation
generate iface = do
  -- Build the partial generation of each module.
  modPartials <- forM (M.elems $ interfaceModules iface) $ \m ->
    (,) m <$> execGenerator iface m (generateSource m)

  -- Compute the strongly connected components.  If there is a nontrivial SCC,
  -- then there is a module import cycle that we'll have to break with hs-boot
  -- files.
  let partialsByHsName :: M.Map HsModuleName Partial
      partialsByHsName = M.fromList $ map ((partialModuleHsName &&& id) . snd) modPartials

      sccInput :: [((Module, Partial), Partial, [Partial])]
      sccInput = flip map modPartials $ \x@(_, p) ->
        (x, p,
         mapMaybe (flip M.lookup partialsByHsName . hsImportModule) $
         M.keys $ getHsImportSet $ outputImports $ partialOutput p)

      sccs :: [SCC (Module, Partial)]
      sccs = stronglyConnComp sccInput

  fileContents <- execWriterT $ forM_ sccs $ \scc -> case scc of
    AcyclicSCC (_, p) -> tell [finishPartial p "hs"]
    CyclicSCC mps -> do
      let cycleModNames = S.fromList $ map (partialModuleHsName . snd) mps
      forM_ mps $ \(m, p) -> do
        -- Create a boot partial.
        pBoot <- lift $ execGenerator iface m (generateBootSource m)

        -- Change the source and boot partials so that all imports of modules in
        -- this cycle are {-# SOURCE #-} imports.
        let p' = setSourceImports cycleModNames p
            pBoot' = setSourceImports cycleModNames pBoot

        -- Emit the completed partials.
        tell [finishPartial p' "hs", finishPartial pBoot' "hs-boot"]

  return $ Generation $ M.fromList fileContents

  where finishPartial :: Partial -> String -> (FilePath, String)
        finishPartial p fileExt =
          (listSubst '.' pathSeparator (partialModuleHsName p) <.> fileExt,
           prependExtensions $ renderPartial p)

        setSourceImports :: S.Set HsModuleName -> Partial -> Partial
        setSourceImports modulesToSourceImport p =
          let output = partialOutput p
              imports = outputImports output
              imports' = makeHsImportSet $
                         M.mapWithKey (setSourceImportIfIn modulesToSourceImport) $
                         getHsImportSet imports
              output' = output { outputImports = imports' }
          in p { partialOutput = output' }

        setSourceImportIfIn :: S.Set HsModuleName -> HsImportKey -> HsImportSpecs -> HsImportSpecs
        setSourceImportIfIn modulesToSourceImport key specs =
          if hsImportModule key `S.member` modulesToSourceImport
          then specs { hsImportSource = True }
          else specs

prependExtensions :: String -> String
prependExtensions = (prependExtensionsPrefix ++)

prependExtensionsPrefix :: String
prependExtensionsPrefix =
  -- MultiParamTypeClasses is necessary for instances of Decodable and
  -- Encodable.  FlexibleContexts is needed for the type signature of the
  -- function that wraps the actual callback function in callback creation
  -- functions.
  --
  -- FlexibleInstances and TypeSynonymInstances are enabled to allow conversions
  -- to and from String, which is really [Char].
  --
  -- UndecidableInstances is needed for instances of the form "SomeClassConstPtr
  -- a => SomeClassValue a", and overlapping instances are used for the overlap
  -- between these instances and instances of SomeClassValue for the class's
  -- native Haskell type, when it's convertible.  CPP is used for warning-free
  -- compatibility using overlapping instances with both GHC 7.8 and 7.10.
  --
  -- GeneralizedNewtypeDeriving is to enable automatic deriving of
  -- Data.Bits.Bits instances for bitspace newtypes.
  concat
  [ "{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving"
  , ", MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances #-}\n"
  , "#if !MIN_VERSION_base(4,8,0)\n"
  , "{-# LANGUAGE OverlappingInstances #-}\n"
  , "#endif\n\n"
  ]

generateSource :: Module -> Generator ()
generateSource m = do
  forM_ (moduleExports m) $ sayExport SayExportForeignImports
  forM_ (moduleExports m) $ sayExport SayExportDecls

  iface <- askInterface
  when (interfaceExceptionSupportModule iface == Just m) $
    sayExceptionSupport True

generateBootSource :: Module -> Generator ()
generateBootSource m = do
  forM_ (moduleExports m) $ sayExport SayExportBoot

  iface <- askInterface
  when (interfaceExceptionSupportModule iface == Just m) $
    sayExceptionSupport False

data SayExportMode = SayExportForeignImports | SayExportDecls | SayExportBoot
                   deriving (Eq, Show)

sayExport :: SayExportMode -> Export -> Generator ()
sayExport mode export = do
  case export of
    ExportVariable v -> sayExportVar mode v
    ExportEnum enum -> sayExportEnum mode enum
    ExportBitspace bitspace -> sayExportBitspace mode bitspace
    ExportFn fn ->
      (sayExportFn mode <$> fnExtName <*> pure Nothing <*> fnPurity <*>
       fnParams <*> fnReturn <*> fnExceptionHandlers) fn
    ExportClass cls -> sayExportClass mode cls
    ExportCallback cb -> sayExportCallback mode cb

  when (mode == SayExportDecls) $
    addendumHaskell $ exportAddendum export

sayExportVar :: SayExportMode -> Variable -> Generator ()
sayExportVar mode v = do
  withErrorContext ("generating variable " ++ show (varExtName v)) $ do
    let (isConst, deconstType) = case varType v of
          Internal_TConst t -> (True, t)
          t -> (False, t)
    sayExportFn mode (varGetterExtName v) Nothing Nonpure [] deconstType mempty
    unless isConst $
      sayExportFn mode (varSetterExtName v) Nothing Nonpure [deconstType] voidT mempty

sayExportEnum :: SayExportMode -> CppEnum -> Generator ()
sayExportEnum mode enum =
  withErrorContext ("generating enum " ++ show (enumExtName enum)) $
  case mode of
    -- Nothing to import from the C++ side of an enum.
    SayExportForeignImports -> return ()

    SayExportDecls -> do
      let hsTypeName = toHsEnumTypeName enum
          values :: [(Int, String)]
          values = map (second $ toHsEnumCtorName enum) $ enumValueNames enum
      addImports $ mconcat [hsImports "Prelude" ["($)", "(++)"], hsImportForPrelude]

      -- Print out the data declaration.
      ln
      addExport' hsTypeName
      saysLn ["data ", hsTypeName, " ="]
      indent $ do
        forM_ (zip (False:repeat True) values) $ \(cont, (_, hsCtorName)) ->
          saysLn [if cont then "| " else "", hsCtorName]
        sayLn "deriving (HoppyP.Bounded, HoppyP.Eq, HoppyP.Ord, HoppyP.Show)"

      -- Print out the Enum instance.
      ln
      saysLn ["instance HoppyP.Enum ", hsTypeName, " where"]
      indent $ do
        forM_ values $ \(num, hsCtorName) ->
          saysLn ["fromEnum ", hsCtorName, " = ", show num]
        ln
        forM_ values $ \(num, hsCtorName) ->
          saysLn ["toEnum (", show num, ") = ", hsCtorName]
        saysLn ["toEnum n' = HoppyP.error $ ",
                show (concat ["Unknown ", hsTypeName, " numeric value: "]),
                " ++ HoppyP.show n'"]

    SayExportBoot -> do
      let hsTypeName = toHsEnumTypeName enum
      addImports hsImportForPrelude
      addExport hsTypeName
      ln
      saysLn ["data ", hsTypeName]
      saysLn ["instance HoppyP.Bounded ", hsTypeName]
      saysLn ["instance HoppyP.Enum ", hsTypeName]
      saysLn ["instance HoppyP.Eq ", hsTypeName]
      saysLn ["instance HoppyP.Ord ", hsTypeName]
      saysLn ["instance HoppyP.Show ", hsTypeName]

sayExportBitspace :: SayExportMode -> Bitspace -> Generator ()
sayExportBitspace mode bitspace =
  withErrorContext ("generating bitspace " ++ show (bitspaceExtName bitspace)) $
  let hsTypeName = toHsBitspaceTypeName bitspace
      fromFnName = toHsBitspaceToNumName bitspace
      className = toHsBitspaceClassName bitspace
      toFnName = toHsBitspaceFromValueName bitspace
      hsType = HsTyCon $ UnQual $ HsIdent hsTypeName
  in case mode of
    -- Nothing to import from the C++ side of a bitspace.
    SayExportForeignImports -> return ()

    SayExportDecls -> do
      let values :: [(Int, String)]
          values = map (second $ toHsBitspaceValueName bitspace) $ bitspaceValueNames bitspace

      hsCNumType <- cppTypeToHsTypeAndUse HsCSide $ bitspaceType bitspace
      hsHsNumType <- cppTypeToHsTypeAndUse HsHsSide $ bitspaceType bitspace

      -- Print out the data declaration and conversion functions.
      addImports $ mconcat [hsImportForBits, hsImportForPrelude, hsImportForRuntime]
      addExport' hsTypeName
      addExport' className
      ln
      saysLn ["newtype ", hsTypeName, " = ", hsTypeName, " { ",
              fromFnName, " :: ", prettyPrint hsCNumType, " }"]
      indent $ sayLn "deriving (HoppyDB.Bits, HoppyP.Bounded, HoppyP.Eq, HoppyP.Ord, HoppyP.Show)"
      ln
      saysLn ["class ", className, " a where"]
      indent $ do
        let tyVar = HsTyVar $ HsIdent "a"
        saysLn [toFnName, " :: ", prettyPrint $ HsTyFun tyVar hsType]
      ln
      saysLn ["instance ", className, " (", prettyPrint hsCNumType, ") where"]
      indent $ saysLn [toFnName, " = ", hsTypeName]
      saysLn ["instance ", className, " (", prettyPrint hsHsNumType, ") where"]
      indent $ saysLn [toFnName, " = ", hsTypeName, " . HoppyFHR.coerceIntegral"]
      saysLn ["instance ", className, " ", hsTypeName, " where"]
      indent $ saysLn [toFnName, " = HoppyP.id"]

      -- If the bitspace has an associated enum, then print out a conversion
      -- instance for it as well.
      forM_ (bitspaceEnum bitspace) $ \enum -> do
        let enumTypeName = toHsEnumTypeName enum
        importHsModuleForExtName $ enumExtName enum
        addImports $ mconcat [hsImport1 "Prelude" "(.)", hsImportForPrelude, hsImportForRuntime]
        ln
        saysLn ["instance ", className, " ", enumTypeName, " where"]
        indent $
          saysLn [toFnName, " = ", hsTypeName, " . HoppyFHR.coerceIntegral . HoppyP.fromEnum"]

      -- Print out the constants.
      ln
      forM_ values $ \(num, valueName) -> do
        addExport valueName
        saysLn [valueName, " = ", hsTypeName, " ", show num]

    SayExportBoot -> do
      hsCNumType <- cppTypeToHsTypeAndUse HsCSide $ bitspaceType bitspace
      hsHsNumType <- cppTypeToHsTypeAndUse HsHsSide $ bitspaceType bitspace

      addImports $ mconcat [hsImportForBits, hsImportForPrelude]
      addExport' hsTypeName
      addExport' className
      ln
      saysLn ["newtype ", hsTypeName, " = ", hsTypeName, " { ",
              fromFnName, " :: ", prettyPrint hsCNumType, " }"]
      ln
      saysLn ["instance HoppyDB.Bits ", hsTypeName]
      saysLn ["instance HoppyP.Bounded ", hsTypeName]
      saysLn ["instance HoppyP.Eq ", hsTypeName]
      saysLn ["instance HoppyP.Ord ", hsTypeName]
      saysLn ["instance HoppyP.Show ", hsTypeName]
      ln
      saysLn ["class ", className, " a where"]
      indent $ do
        let tyVar = HsTyVar $ HsIdent "a"
        saysLn [toFnName, " :: ", prettyPrint $ HsTyFun tyVar hsType]
      ln
      saysLn ["instance ", className, " (", prettyPrint hsCNumType, ")"]
      saysLn ["instance ", className, " (", prettyPrint hsHsNumType, ")"]
      saysLn ["instance ", className, " ", hsTypeName]
      forM_ (bitspaceEnum bitspace) $ \enum -> do
        let enumTypeName = toHsEnumTypeName enum
        importHsModuleForExtName $ enumExtName enum
        saysLn ["instance ", className, " ", enumTypeName]

sayExportFn :: SayExportMode
            -> ExtName
            -> Maybe (Constness, Class)
            -> Purity
            -> [Type]
            -> Type
            -> ExceptionHandlers
            -> Generator ()
sayExportFn mode name methodInfo purity paramTypes retType exceptionHandlers = do
  effectiveHandlers <- getEffectiveExceptionHandlers exceptionHandlers
  let handlerList = exceptionHandlersList effectiveHandlers
      catches = not $ null handlerList

      hsFnName = toHsFnName name
      hsFnImportedName = hsFnName ++ "'"
  case mode of
    SayExportForeignImports ->
      withErrorContext ("generating imports for function " ++ show name) $ do
        -- Print a "foreign import" statement.
        hsCType <- fnToHsTypeAndUse HsCSide methodInfo purity paramTypes retType effectiveHandlers
        saysLn ["foreign import ccall \"", externalNameToCpp name, "\" ", hsFnImportedName, " :: ",
                prettyPrint hsCType]

    SayExportDecls -> withErrorContext ("generating function " ++ show name) $ do
      -- Print the type signature.
      ln
      addExport hsFnName
      hsHsType <- fnToHsTypeAndUse HsHsSide methodInfo purity paramTypes retType effectiveHandlers
      saysLn [hsFnName, " :: ", prettyPrint hsHsType]

      case purity of
        Nonpure -> return ()
        Pure -> saysLn ["{-# NOINLINE ", hsFnName, " #-}"]

      -- Print the function body.
      let argNames = map toArgName [1..length paramTypes]
          argNamesWithThis = (if isJust methodInfo then ("this":) else id) argNames
          convertedArgNames = map (++ "'") argNames
      -- Operators on this line must bind more weakly than operators used below,
      -- namely ($) and (>>=).  (So finish the line with ($).)
      lineEnd <- case purity of
        Nonpure -> return [" ="]
        Pure -> do addImports $ mconcat [hsImport1 "Prelude" "($)", hsImportForUnsafeIO]
                   return [" = HoppySIU.unsafePerformIO $"]
      saysLn $ hsFnName : map (' ':) argNamesWithThis ++ lineEnd
      indent $ do
        forM_ (zip3 paramTypes argNames convertedArgNames) $ \(t, argName, argName') ->
          sayArgProcessing ToCpp t argName argName'

        when catches $ do
          iface <- askInterface
          currentModule <- askModule
          let exceptionSupportModule = interfaceExceptionSupportModule iface
          when (exceptionSupportModule /= Just currentModule) $
            addImports . hsWholeModuleImport . getModuleName iface =<<
            fromMaybeM (throwError "Internal error, an exception support module is not available")
            exceptionSupportModule
          addImports $ mconcat [hsImport1 "Prelude" "($)", hsImportForRuntime]
          sayLn "HoppyFHR.internalHandleExceptions exceptionDb' $"

        sayCallAndProcessReturn ToCpp retType $
          hsFnImportedName :
          (case methodInfo of
             Just (cst, cls) -> " (" ++ toHsCastMethodName cst cls ++ " this)"
             Nothing -> "") :
          map (' ':) convertedArgNames

    SayExportBoot ->
      -- Functions (methods included) cannot be referenced from other exports,
      -- so we don't need to emit anything.
      return ()

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
-- > name :: (CInt -> String -> IO CInt) -> IO (CCallback (CInt -> Ptr CChar -> IO CInt))
-- > name f = do
-- >   let cf arg1' arg2' = do
-- >         arg1 <- return arg1'
-- >         arg2 <- ...decode the string...
-- >         f arg1 arg2 >>= return
-- >   cfp <- name'newFunPtr cf
-- >   name'newCallback cfp freeHaskellFunPtrFunPtr False
--
-- Only the implementation of bindings that take a callback of this type will
-- make use of this @name@ binding; @name@ is not useful to users of bindings.
sayExportCallback :: SayExportMode -> Callback -> Generator ()
sayExportCallback mode cb =
  withErrorContext ("generating callback " ++ show (callbackExtName cb)) $ do
    let name = callbackExtName cb
        paramTypes = callbackParams cb
        retType = callbackReturn cb
        fnType = callbackToTFn cb
        hsFnName = toHsCallbackCtorName cb
        hsFnName'newCallback = hsFnName ++ "'newCallback"
        hsFnName'newFunPtr = hsFnName ++ "'newFunPtr"

    hsFnCType <- cppTypeToHsTypeAndUse HsCSide fnType
    hsFnHsType <- cppTypeToHsTypeAndUse HsHsSide fnType

    let getWholeFnType = do
          addImports $ mconcat [hsImportForPrelude, hsImportForRuntime]
          return $
            HsTyFun hsFnHsType $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyFHR.CCallback") hsFnCType

    case mode of
      SayExportForeignImports -> do
        addImports $ mconcat [hsImportForForeign, hsImportForPrelude, hsImportForRuntime]
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

        saysLn ["foreign import ccall \"wrapper\" ", hsFnName'newFunPtr, " :: ",
                prettyPrint hsFunPtrImportType]
        saysLn ["foreign import ccall \"", externalNameToCpp name, "\" ",
                hsFnName'newCallback, " :: ", prettyPrint hsCallbackCtorImportType]

      SayExportDecls -> do
        addExport hsFnName
        wholeFnType <- getWholeFnType
        let paramCount = length paramTypes
            argNames = map toArgName [1..paramCount]
            argNames' = map (++ "'") argNames
        ln
        saysLn [hsFnName, " :: ", prettyPrint wholeFnType]
        saysLn [hsFnName, " f'hs = do"]
        indent $ do
          sayLet
            [do saysLn ["f'c ", unwords argNames, " ="]
                indent $ do
                  forM_ (zip3 paramTypes argNames argNames') $ \(t, argName, argName') ->
                    sayArgProcessing FromCpp t argName argName'
                  sayCallAndProcessReturn FromCpp retType $
                    "f'hs" : map (' ':) argNames']
            Nothing
          saysLn ["f'p <- ", hsFnName'newFunPtr, " f'c"]
          saysLn [hsFnName'newCallback, " f'p HoppyFHR.freeHaskellFunPtrFunPtr HoppyP.False"]

      SayExportBoot -> do
        addExport hsFnName
        wholeFnType <- getWholeFnType
        ln
        saysLn [hsFnName, " :: ", prettyPrint wholeFnType]

data CallDirection =
  ToCpp  -- ^ Haskell code is calling out to C++.
  | FromCpp  -- ^ C++ is invoking a callback.

sayArgProcessing :: CallDirection -> Type -> String -> String -> Generator ()
sayArgProcessing dir t fromVar toVar =
  withErrorContext ("processing argument of type " ++ show t) $
  case t of
    Internal_TVoid -> throwError $ "TVoid is not a valid argument type"
    Internal_TBool -> case dir of
      ToCpp -> saysLn ["let ", toVar, " = if ", fromVar, " then 1 else 0 in"]
      FromCpp -> do addImports $ hsImport1 "Prelude" "(/=)"
                    saysLn ["let ", toVar, " = ", fromVar, " /= 0 in"]
    Internal_TChar -> noConversion
    Internal_TUChar -> noConversion
    Internal_TShort -> noConversion
    Internal_TUShort -> noConversion
    Internal_TInt -> sayCoerceIntegral
    Internal_TUInt -> noConversion
    Internal_TLong -> noConversion
    Internal_TULong -> noConversion
    Internal_TLLong -> noConversion
    Internal_TULLong -> noConversion
    Internal_TFloat -> sayCoerceFloating
    Internal_TDouble -> sayCoerceFloating
    Internal_TInt8 -> noConversion
    Internal_TInt16 -> noConversion
    Internal_TInt32 -> noConversion
    Internal_TInt64 -> noConversion
    Internal_TWord8 -> noConversion
    Internal_TWord16 -> noConversion
    Internal_TWord32 -> noConversion
    Internal_TWord64 -> noConversion
    Internal_TPtrdiff -> noConversion
    Internal_TSize -> noConversion
    Internal_TSSize -> noConversion
    Internal_TEnum _ -> do
      addImports $ mconcat [hsImport1 "Prelude" "($)", hsImportForPrelude, hsImportForRuntime]
      saysLn ["let ", toVar,
              -- TODO The coersion here is unnecssary if we replace the C numeric
              -- types with their Haskell ones across the board (e.g. CInt ->
              -- Int).
              case dir of
                ToCpp -> " = HoppyFHR.coerceIntegral $ HoppyP.fromEnum "
                FromCpp -> " = HoppyP.toEnum $ HoppyFHR.coerceIntegral ",
              fromVar, " in"]
    Internal_TBitspace b -> do
      importHsModuleForExtName $ bitspaceExtName b
      saysLn $ concat [ ["let ", toVar, " = "]
                      , case dir of
                          ToCpp -> [toHsBitspaceToNumName b, " $ ", toHsBitspaceFromValueName b]
                          FromCpp -> [toHsBitspaceTypeName b],
                        [" ", fromVar, " in"]
                      ]
    -- References and pointers are handled equivalently.
    Internal_TPtr (Internal_TObj cls) -> do
      addImportForClass cls
      case dir of
        ToCpp -> do
          addImports $ mconcat [hsImport1 "Prelude" "($)",
                                hsImportForRuntime]
          saysLn ["HoppyFHR.withCppPtr (", toHsCastMethodName Nonconst cls, " ", fromVar,
                  ") $ \\", toVar, " ->"]
        FromCpp ->
          saysLn ["let ", toVar, " = ", toHsDataCtorName Unmanaged Nonconst cls,
                  " ", fromVar, " in"]
    Internal_TPtr (Internal_TConst (Internal_TObj cls)) -> do
      addImportForClass cls
      case dir of
        ToCpp -> do
          -- Same as the (TObj _), ToCpp case.
          addImports $ mconcat [hsImport1 "Prelude" "($)",
                                hsImportForPrelude,
                                hsImportForRuntime]
          saysLn [toHsWithValuePtrName cls, " ", fromVar,
                  " $ HoppyP.flip HoppyFHR.withCppPtr $ \\", toVar, " ->"]
        FromCpp ->
          saysLn ["let ", toVar, " = ", toHsDataCtorName Unmanaged Const cls,
                  " ", fromVar, " in"]
    Internal_TPtr _ -> noConversion
    Internal_TRef t' -> sayArgProcessing dir (ptrT t') fromVar toVar
    Internal_TFn {} -> throwError "TFn unimplemented"
    Internal_TCallback cb -> case dir of
      ToCpp -> do
        addImports $ hsImport1 "Prelude" "(>>=)"
        importHsModuleForExtName $ callbackExtName cb
        saysLn [toHsCallbackCtorName cb, " ", fromVar, " >>= \\", toVar, " ->"]
      FromCpp -> throwError "Can't receive a callback from C++"
    Internal_TObj cls -> case dir of
      ToCpp -> do
        -- Same as the (TPtr (TConst (TObj _))), ToPtr case.
        addImportForClass cls
        addImports $ mconcat [hsImport1 "Prelude" "($)",
                              hsImportForPrelude,
                              hsImportForRuntime]
        saysLn [toHsWithValuePtrName cls, " ", fromVar,
                " $ HoppyP.flip HoppyFHR.withCppPtr $ \\", toVar, " ->"]
      FromCpp -> case classHaskellConversion $ classConversion cls of
        ClassConversionNone ->
          throwError $ concat
          ["Can't pass a TObj of ", show cls,
           " from C++ to Haskell because no class conversion is defined"]
        ClassConversionManual _ -> do
          addImportForClass cls
          addImports $ mconcat [hsImport1 "Prelude" "(>>=)",
                                hsImportForRuntime]
          saysLn ["HoppyFHR.decode (", toHsDataCtorName Unmanaged Const cls, " ",
                  fromVar, ") >>= \\", toVar, " ->"]
        ClassConversionToHeap -> sayArgProcessing dir (objToHeapT cls) fromVar toVar
        ClassConversionToGc -> sayArgProcessing dir (toGcT t) fromVar toVar
    Internal_TObjToHeap cls -> case dir of
      ToCpp -> throwError $ objToHeapTWrongDirectionErrorMsg Nothing cls
      FromCpp -> sayArgProcessing dir (ptrT $ objT cls) fromVar toVar
    Internal_TToGc t' -> case dir of
      ToCpp -> throwError $ toGcTWrongDirectionErrorMsg Nothing t'
      FromCpp -> do
        addImports $ mconcat [hsImport1 "Prelude" "(>>=)",
                              hsImportForRuntime]
        saysLn ["HoppyFHR.toGc ", fromVar, " >>= \\", toVar, " ->"]
    Internal_TConst t' -> sayArgProcessing dir t' fromVar toVar
  where noConversion = saysLn ["let ", toVar, " = ", fromVar, " in"]
        sayCoerceIntegral = do
          addImports hsImportForRuntime
          saysLn ["let ", toVar, " = HoppyFHR.coerceIntegral ", fromVar, " in"]
        sayCoerceFloating = do
          addImports hsImportForPrelude
          saysLn ["let ", toVar, " = HoppyP.realToFrac ", fromVar, " in"]

-- | Note that the 'CallDirection' is the direction of the call, not the
-- direction of the return.  'ToCpp' means we're returning to the foreign
-- language, 'FromCpp' means we're returning from it.
sayCallAndProcessReturn :: CallDirection -> Type -> [String] -> Generator ()
sayCallAndProcessReturn dir t callWords =
  withErrorContext ("processing return value of type " ++ show t) $
  case t of
    Internal_TVoid -> sayCall
    Internal_TBool -> do
      case dir of
        ToCpp -> do addImports $ mconcat [hsImport1 "Prelude" "(/=)", hsImportForPrelude]
                    sayLn "HoppyP.fmap (/= 0)"
        FromCpp -> sayLn "HoppyP.fmap (\\x -> if x then 1 else 0)"
      sayCall
    Internal_TChar -> sayCall
    Internal_TUChar -> sayCall
    Internal_TShort -> sayCall
    Internal_TUShort -> sayCall
    Internal_TInt -> sayCoerceIntegral >> sayCall
    Internal_TUInt -> sayCall
    Internal_TLong -> sayCall
    Internal_TULong -> sayCall
    Internal_TLLong -> sayCall
    Internal_TULLong -> sayCall
    Internal_TFloat -> sayCoerceFloating >> sayCall
    Internal_TDouble -> sayCoerceFloating >> sayCall
    Internal_TInt8 -> sayCall
    Internal_TInt16 -> sayCall
    Internal_TInt32 -> sayCall
    Internal_TInt64 -> sayCall
    Internal_TWord8 -> sayCall
    Internal_TWord16 -> sayCall
    Internal_TWord32 -> sayCall
    Internal_TWord64 -> sayCall
    Internal_TPtrdiff -> sayCall
    Internal_TSize -> sayCall
    Internal_TSSize -> sayCall
    Internal_TEnum _ -> do
      addImports $ mconcat [hsImport1 "Prelude" "(.)", hsImportForPrelude, hsImportForRuntime]
      case dir of
        -- TODO The coersion here is unnecssary if we replace the C numeric types
        -- with their Haskell ones across the board (e.g. CInt -> Int).
        ToCpp -> saysLn ["HoppyP.fmap (HoppyP.toEnum . HoppyFHR.coerceIntegral)"]
        FromCpp -> saysLn ["HoppyP.fmap (HoppyFHR.coerceIntegral . HoppyP.fromEnum)"]
      sayCall
    Internal_TBitspace b -> do
      addImports hsImportForPrelude
      importHsModuleForExtName $ bitspaceExtName b
      saysLn ["HoppyP.fmap ", bitspaceConvFn dir b]
      sayCall
    -- The same as TPtr (TConst (TObj _)), but nonconst.
    Internal_TPtr (Internal_TObj cls) -> do
      addImportForClass cls
      case dir of
        ToCpp -> do
          addImports hsImportForPrelude
          saysLn ["HoppyP.fmap ", toHsDataCtorName Unmanaged Nonconst cls]
          sayCall
        FromCpp -> do
          addImports $ mconcat [hsImportForPrelude, hsImportForRuntime]
          sayLn "HoppyP.fmap HoppyFHR.toPtr"
          sayCall
    -- The same as TPtr (TConst (TObj _)), but nonconst.
    Internal_TPtr (Internal_TConst (Internal_TObj cls)) -> do
      addImportForClass cls
      case dir of
        ToCpp -> do
          addImports hsImportForPrelude
          saysLn ["HoppyP.fmap ", toHsDataCtorName Unmanaged Const cls]
          sayCall
        FromCpp -> do
          addImports $ mconcat [hsImportForPrelude, hsImportForRuntime]
          sayLn "HoppyP.fmap HoppyFHR.toPtr"
          sayCall
    Internal_TPtr _ -> sayCall
    Internal_TRef t' -> sayCallAndProcessReturn dir (ptrT t') callWords
    Internal_TFn {} -> throwError "TFn unimplemented"
    Internal_TCallback cb -> case dir of
      ToCpp -> throwError "Can't receive a callback from C++"
      FromCpp -> do
        addImports $ hsImport1 "Prelude" "(=<<)"
        importHsModuleForExtName $ callbackExtName cb
        saysLn [toHsCallbackCtorName cb, "=<<"]
        sayCall
    Internal_TObj cls -> case dir of
      ToCpp -> case classHaskellConversion $ classConversion cls of
        ClassConversionNone ->
          throwError $ concat
          ["Can't return a TObj of ", show cls,
           " from C++ to Haskell because no class conversion is defined"]
        ClassConversionManual _ -> do
          addImportForClass cls
          addImports $ mconcat [hsImports "Prelude" ["(.)", "(=<<)"],
                                hsImportForRuntime]
          saysLn ["(HoppyFHR.decodeAndDelete . ", toHsDataCtorName Unmanaged Const cls, ") =<<"]
          sayCall
        ClassConversionToHeap -> sayCallAndProcessReturn dir (objToHeapT cls) callWords
        ClassConversionToGc -> sayCallAndProcessReturn dir (toGcT t) callWords
      FromCpp -> do
        addImportForClass cls
        addImports $ mconcat [hsImports "Prelude" ["(.)", "(=<<)"],
                              hsImportForPrelude,
                              hsImportForRuntime]
        sayLn "(HoppyP.fmap (HoppyFHR.toPtr) . HoppyFHR.encode) =<<"
        sayCall
    Internal_TObjToHeap cls -> case dir of
      ToCpp -> sayCallAndProcessReturn dir (ptrT $ objT cls) callWords
      FromCpp -> throwError $ objToHeapTWrongDirectionErrorMsg Nothing cls
    Internal_TToGc t' -> case dir of
      ToCpp -> do
        addImports $ mconcat [hsImport1 "Prelude" "(=<<)",
                              hsImportForRuntime]
        sayLn "HoppyFHR.toGc =<<"
        -- TToGc (TObj _) should create a pointer rather than decoding, so we
        -- change the TObj _ into a TPtr (TObj _).
        case t' of
          Internal_TObj _ -> sayCallAndProcessReturn dir (ptrT t') callWords
          _ -> sayCallAndProcessReturn dir t' callWords
      FromCpp -> throwError $ toGcTWrongDirectionErrorMsg Nothing t'
    Internal_TConst t' -> sayCallAndProcessReturn dir t' callWords
  where sayCall = saysLn $ "(" : callWords ++ [")"]
        sayCoerceIntegral = do addImports $ mconcat [hsImportForPrelude, hsImportForRuntime]
                               sayLn "HoppyP.fmap HoppyFHR.coerceIntegral"
        sayCoerceFloating = do addImports hsImportForPrelude
                               sayLn "HoppyP.fmap HoppyP.realToFrac"
        bitspaceConvFn dir = case dir of
          ToCpp -> toHsBitspaceTypeName
          FromCpp -> toHsBitspaceToNumName

sayExportClass :: SayExportMode -> Class -> Generator ()
sayExportClass mode cls = do
  case mode of
    SayExportForeignImports -> do
      sayExportClassHsCtors mode cls

      forM_ (classMethods cls) $ \method ->
        (sayExportFn mode <$> getClassyExtName cls <*> pure Nothing <*> methodPurity <*>
         pure (getMethodEffectiveParams cls method) <*> methodReturn <*> methodExceptionHandlers)
        method

    SayExportDecls -> do
      sayExportClassHsClass True cls Const
      sayExportClassHsClass True cls Nonconst

      sayExportClassHsStaticMethods cls

      -- Create a newtype for referencing foreign objects with pointers.  The
      -- newtype is not used with encodings of value objects.
      sayExportClassHsType True cls Const
      sayExportClassHsType True cls Nonconst

      sayExportClassExceptionSupport True cls

      sayExportClassHsCtors mode cls

    SayExportBoot -> do
      sayExportClassHsClass False cls Const
      sayExportClassHsClass False cls Nonconst

      sayExportClassHsType False cls Const
      sayExportClassHsType False cls Nonconst

      sayExportClassExceptionSupport False cls

  sayExportClassCastPrimitives mode cls
  sayExportClassHsSpecialFns mode cls

sayExportClassHsClass :: Bool -> Class -> Constness -> Generator ()
sayExportClassHsClass doDecls cls cst = do
  let hsTypeName = toHsDataTypeName cst cls
      hsValueClassName = toHsValueClassName cls
      hsWithValuePtrName = toHsWithValuePtrName cls
      hsPtrClassName = toHsPtrClassName cst cls
      hsCastMethodName = toHsCastMethodName cst cls
      supers = classSuperclasses cls

  forM_ supers $ importHsModuleForExtName . classExtName
  hsSupers <-
    (\x -> if null x
           then do addImports hsImportForRuntime
                   return ["HoppyFHR.CppPtr"]
           else return x) $
    case cst of
      Const -> map (toHsPtrClassName Const) supers
      Nonconst -> toHsPtrClassName Const cls : map (toHsPtrClassName Nonconst) supers

  -- Print the value class definition.  There is only one of these, and it is
  -- spiritually closer to the const version of the pointers for this class, so
  -- we emit for the const case only.
  when (cst == Const) $ do
    addImports hsImportForPrelude
    addExport' hsValueClassName
    ln
    saysLn ["class ", hsValueClassName, " a where"]
    indent $
      saysLn [hsWithValuePtrName, " :: a -> (", hsTypeName, " -> HoppyP.IO b) -> HoppyP.IO b"]

    -- Generate instances for all pointer subtypes.
    ln
    saysLn ["#if MIN_VERSION_base(4,8,0)"]
    saysLn ["instance {-# OVERLAPPABLE #-} ", hsPtrClassName, " a => ", hsValueClassName, " a",
            if doDecls then " where" else ""]
    saysLn ["#else"]
    saysLn ["instance ", hsPtrClassName, " a => ", hsValueClassName, " a",
            if doDecls then " where" else ""]
    saysLn ["#endif"]
    when doDecls $ do
      addImports $ mconcat [hsImports "Prelude" ["($)", "(.)"],
                            hsImportForPrelude]
      indent $ saysLn [hsWithValuePtrName, " = HoppyP.flip ($) . ", hsCastMethodName]

    -- When the class has a native Haskell type, also print an instance for it.
    forM_ (getClassHaskellConversion cls) $ \conv -> do
      hsType <- classHaskellConversionType conv
      ln
      saysLn ["#if MIN_VERSION_base(4,8,0)"]
      saysLn ["instance {-# OVERLAPPING #-} ", hsValueClassName, " (", prettyPrint hsType, ")",
              if doDecls then " where" else ""]
      saysLn ["#else"]
      saysLn ["instance ", hsValueClassName, " (", prettyPrint hsType, ")",
              if doDecls then " where" else ""]
      saysLn ["#endif"]
      when doDecls $ do
        addImports hsImportForRuntime
        indent $ saysLn [hsWithValuePtrName, " = HoppyFHR.withCppObj"]

  -- Print the pointer class definition.
  addExport' hsPtrClassName
  ln
  saysLn $
    "class (" :
    intersperse ", " (map (++ " this") hsSupers) ++
    [") => ", hsPtrClassName, " this where"]
  indent $ saysLn [hsCastMethodName, " :: this -> ", hsTypeName]

  -- Print the non-static methods.
  when doDecls $ do
    let methods = filter ((cst ==) . methodConst) $ classMethods cls
    forM_ methods $ \method ->
      when (methodStatic method == Nonstatic) $
      (sayExportFn SayExportDecls <$> getClassyExtName cls <*> pure Nothing <*>
       methodPurity <*> pure (getMethodEffectiveParams cls method) <*>
       methodReturn <*> methodExceptionHandlers) method

sayExportClassHsStaticMethods :: Class -> Generator ()
sayExportClassHsStaticMethods cls =
  forM_ (classMethods cls) $ \method ->
    when (methodStatic method == Static) $
    (sayExportFn SayExportDecls <$> getClassyExtName cls <*> pure Nothing <*> methodPurity <*>
     methodParams <*> methodReturn <*> methodExceptionHandlers) method

sayExportClassHsType :: Bool -> Class -> Constness -> Generator ()
sayExportClassHsType doDecls cls cst = do
  addImports $ mconcat [hsImportForForeign, hsImportForPrelude, hsImportForRuntime]
  -- Unfortunately, we must export the data constructor, so that GHC can marshal
  -- it in foreign calls in other modules.
  addExport' hsTypeName
  ln
  saysLn ["data ", hsTypeName, " ="]
  indent $ do
    saysLn ["  ", hsCtor, " (HoppyF.Ptr ", hsTypeName, ")"]
    saysLn ["| ", hsCtorGc, " (HoppyF.ForeignPtr ()) (HoppyF.Ptr ", hsTypeName, ")"]
  when doDecls $ do
    addImports $ hsImport1 "Prelude" "(==)"
    indent $ sayLn "deriving (HoppyP.Show)"
    ln
    saysLn ["instance HoppyP.Eq ", hsTypeName, " where"]
    indent $ saysLn ["x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y"]
    ln
    saysLn ["instance HoppyP.Ord ", hsTypeName, " where"]
    indent $ saysLn ["compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)"]

  -- Generate const_cast functions:
  --   castFooToConst :: Foo -> FooConst
  --   castFooToNonconst :: FooConst -> Foo
  ln
  let constCastFnName = toHsConstCastFnName cst cls
  addExport constCastFnName
  saysLn [constCastFnName, " :: ", toHsDataTypeName (constNegate cst) cls, " -> ", hsTypeName]
  when doDecls $ do
    addImports $ hsImport1 "Prelude" "($)"
    saysLn [constCastFnName, " (", toHsDataCtorName Unmanaged (constNegate cst) cls,
            " ptr') = ", hsCtor, " $ HoppyF.castPtr ptr'"]
    saysLn [constCastFnName, " (", toHsDataCtorName Managed (constNegate cst) cls,
            " fptr' ptr') = ", hsCtorGc, " fptr' $ HoppyF.castPtr ptr'"]

  -- Generate an instance of CppPtr.
  ln
  if doDecls
    then do addImports $ hsImport1 "Prelude" "($)"
            saysLn ["instance HoppyFHR.CppPtr ", hsTypeName, " where"]
            indent $ do
              saysLn ["nullptr = ", toHsDataCtorName Unmanaged cst cls, " HoppyF.nullPtr"]
              ln
              saysLn ["withCppPtr (", hsCtor, " ptr') f' = f' ptr'"]
              saysLn ["withCppPtr (", hsCtorGc,
                      " fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \\_ -> f' ptr'"]
              ln
              saysLn ["toPtr (", hsCtor, " ptr') = ptr'"]
              saysLn ["toPtr (", hsCtorGc, " _ ptr') = ptr'"]
              ln
              saysLn ["touchCppPtr (", hsCtor, " _) = HoppyP.return ()"]
              saysLn ["touchCppPtr (", hsCtorGc, " fptr' _) = HoppyF.touchForeignPtr fptr'"]
            when (classDtorIsPublic cls) $ do
              addImports $ hsImport1 "Prelude" "(==)"
              ln
              saysLn ["instance HoppyFHR.Deletable ", hsTypeName, " where"]
              indent $ do
                -- Note, similar "delete" and "toGc" functions are generated for exception
                -- classes' ExceptionClassInfo structures.
                saysLn $
                  "delete (" : toHsDataCtorName Unmanaged cst cls : " ptr') = " :
                  toHsClassDeleteFnName cls :
                  case cst of
                    Const -> [" ptr'"]
                    Nonconst -> [" $ (HoppyF.castPtr ptr' :: HoppyF.Ptr ",
                                 toHsDataTypeName Const cls, ")"]
                saysLn ["delete (", toHsDataCtorName Managed cst cls,
                        " _ _) = HoppyP.fail $ HoppyP.concat ",
                        "[\"Deletable.delete: Asked to delete a GC-managed \", ",
                        show hsTypeName, ", \" object.\"]"]
                ln
                saysLn ["toGc this'@(", hsCtor, " ptr') = ",
                        -- No sense in creating a ForeignPtr for a null pointer.
                        "if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap ",
                        "(HoppyP.flip ", hsCtorGc, " ptr') $ ",
                        "HoppyF.newForeignPtr ",
                        -- The foreign delete function takes a const pointer; we cast it to
                        -- take a Ptr () to match up with the ForeignPtr () we're creating,
                        -- assuming that data pointers have the same representation.
                        "(HoppyF.castFunPtr ", toHsClassDeleteFnPtrName cls,
                        " :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) ",
                        "(HoppyF.castPtr ptr' :: HoppyF.Ptr ())"]
                saysLn ["toGc this'@(", hsCtorGc, " {}) = HoppyP.return this'"]
    else do saysLn ["instance HoppyFHR.CppPtr ", hsTypeName]
            when (classDtorIsPublic cls) $
              saysLn ["instance HoppyFHR.Deletable ", hsTypeName]

  -- Generate instances for all superclasses' typeclasses.
  genInstances [] cls

  where hsTypeName :: String
        hsTypeName = toHsDataTypeName cst cls

        hsCtor :: String
        hsCtor = toHsDataCtorName Unmanaged cst cls

        hsCtorGc :: String
        hsCtorGc = toHsDataCtorName Managed cst cls

        genInstances :: [Class] -> Class -> Generator ()
        genInstances path ancestorCls = do
          -- In this example Bar inherits from Foo.  We are generating instances
          -- either for BarConst or Bar, depending on 'cst'.
          --
          -- BarConst's instances:
          --   instance FooConstPtr BarConst where
          --     toFooConst (BarConst ptr') = FooConst $ castBarToFoo ptr'
          --     toFooConst (BarConstGc fptr' ptr') = FooConstGc fptr' $ castBarToFoo ptr'
          --
          --   instance BarConstPtr BarConst where
          --     toFooConst = id
          --
          -- Bar's instances:
          --   instance FooConstPtr Bar
          --     toFooConst (Bar ptr') =
          --       FooConst $ castBarToFoo $ castBarToConst ptr'
          --     toFooConst (BarGc fptr' ptr') =
          --       FooConstGc fptr' $ castBarToFoo $ castBarToConst ptr'
          --
          --   instance FooPtr Bar
          --     toFoo (Bar ptr') =
          --       Foo $ castFooToNonconst $ castBarToFoo $ castBarToConst ptr'
          --     toFoo (BarGc fptr' ptr') =
          --       FooGc fptr' $ castFooToNonconst $ castBarToFoo $ castBarToConst ptr'
          --
          --   instance BarConstPtr Bar
          --     toBarConst (Bar ptr') = Bar $ castBarToConst ptr'
          --     toBarConst (BarGc fptr' ptr') = BarGc fptr' $ castBarToConst ptr'
          --
          --   instance BarPtr Bar
          --     toBar = id
          --
          -- In all cases, we unwrap the pointer, maybe add const, maybe do an
          -- upcast, maybe remove const, then rewrap the pointer.  The identity
          -- cases are where we just unwrap and wrap again.

          addImportForClass ancestorCls
          forM_ (case cst of
                   Const -> [Const]
                   Nonconst -> [Const, Nonconst]) $ \ancestorCst -> do
            ln
            saysLn ["instance ", toHsPtrClassName ancestorCst ancestorCls, " ", hsTypeName,
                    if doDecls then " where" else ""]
            when doDecls $ indent $ do
              let castMethodName = toHsCastMethodName ancestorCst ancestorCls
              if null path && cst == ancestorCst
                then do addImports hsImportForPrelude
                        saysLn [castMethodName, " = HoppyP.id"]
                else do let addConst = cst == Nonconst
                            removeConst = ancestorCst == Nonconst
                        when (addConst || removeConst) $
                          addImports hsImportForForeign
                        forM_ ([minBound..] :: [Managed]) $ \managed -> do
                          let ancestorCtor = case managed of
                                Unmanaged -> [toHsDataCtorName Unmanaged ancestorCst ancestorCls]
                                Managed -> [toHsDataCtorName Managed ancestorCst ancestorCls,
                                            " fptr'"]
                              ptrPattern = case managed of
                                Unmanaged -> [toHsDataCtorName Unmanaged cst cls, " ptr'"]
                                Managed -> [toHsDataCtorName Managed cst cls, " fptr' ptr'"]
                          saysLn $ concat
                            [ [castMethodName, " ("], ptrPattern, [") = "], ancestorCtor
                            , if removeConst
                              then [" $ (HoppyF.castPtr :: HoppyF.Ptr ",
                                    toHsDataTypeName Const ancestorCls, " -> HoppyF.Ptr ",
                                    toHsDataTypeName Nonconst ancestorCls, ")"]
                              else []
                            , if not $ null path
                              then [" $ ", toHsCastPrimitiveName cls ancestorCls]
                              else []
                            , if addConst
                              then [" $ (HoppyF.castPtr :: HoppyF.Ptr ",
                                    toHsDataTypeName Nonconst cls, " -> HoppyF.Ptr ",
                                    toHsDataTypeName Const cls, ")"]
                              else []
                            , [" ptr'"]
                            ]

          forM_ (classSuperclasses ancestorCls) $ genInstances $ ancestorCls : path

sayExportClassHsCtors :: SayExportMode -> Class -> Generator ()
sayExportClassHsCtors mode cls =
  forM_ (classCtors cls) $ \ctor ->
  (sayExportFn mode <$> getClassyExtName cls <*> pure Nothing <*>
   pure Nonpure <*> ctorParams <*> pure (ptrT $ objT cls) <*> ctorExceptionHandlers)
  ctor

sayExportClassHsSpecialFns :: SayExportMode -> Class -> Generator ()
sayExportClassHsSpecialFns mode cls = do
  let typeName = toHsDataTypeName Nonconst cls
      typeNameConst = toHsDataTypeName Const cls

  -- Say the delete function.
  case mode of
    SayExportForeignImports -> when (classDtorIsPublic cls) $ do
      addImports $ mconcat [hsImportForForeign, hsImportForPrelude]
      saysLn ["foreign import ccall \"", classDeleteFnCppName cls, "\" ",
              toHsClassDeleteFnName cls, " :: HoppyF.Ptr ",
              toHsDataTypeName Const cls, " -> HoppyP.IO ()"]
      saysLn ["foreign import ccall \"&", classDeleteFnCppName cls, "\" ",
              toHsClassDeleteFnPtrName cls, " :: HoppyF.FunPtr (HoppyF.Ptr ",
              toHsDataTypeName Const cls, " -> HoppyP.IO ())"]
    -- The user interface to this is the generic 'delete' function, rendered
    -- elsewhere.
    SayExportDecls -> return ()
    SayExportBoot -> return ()

  case mode of
    SayExportForeignImports -> return ()
    SayExportDecls -> do
      addImports $ mconcat [hsImport1 "Prelude" "($)",
                            hsImportForForeign,
                            hsImportForRuntime]
      ln
      saysLn ["instance HoppyFHR.Assignable (HoppyF.Ptr (HoppyF.Ptr ", typeName, ")) ",
              typeName, " where"]
      indent $ sayLn "assign ptr' value' = HoppyF.poke ptr' $ HoppyFHR.toPtr value'"
    SayExportBoot -> return ()

  -- If the class has an assignment operator that takes its own type, then
  -- generate an instance of Assignable.
  let assignmentMethods = flip filter (classMethods cls) $ \m ->
        methodApplicability m == MNormal &&
        (methodParams m == [objT cls] || methodParams m == [refT $ constT $ objT cls]) &&
        (case methodImpl m of
          RealMethod name -> name == FnOp OpAssign
          FnMethod name -> name == FnOp OpAssign)
      withAssignmentMethod f = case assignmentMethods of
        [] -> return ()
        [m] -> f m
        _ ->
          throwError $ concat
          ["Can't determine an Assignable instance to generator for ", show cls,
          " because it has multiple assignment operators ", show assignmentMethods]
  when (mode == SayExportDecls) $ withAssignmentMethod $ \m -> do
    addImports $ mconcat [hsImport1 "Prelude" "(>>)", hsImportForPrelude]
    ln
    saysLn ["instance ", toHsValueClassName cls, " a => HoppyFHR.Assignable ", typeName,
            " a where"]
    indent $
      saysLn ["assign x' y' = ", toHsFnName $ getClassyExtName cls m,
                " x' y' >> HoppyP.return ()"]

  -- A pointer to an object pointer is decodable to an object pointer by peeking
  -- at the value, so generate a Decodable instance.  You are now a two-star
  -- programmer.  There is a generic @Ptr (Ptr a)@ to @Ptr a@ instance which
  -- handles deeper levels.
  case mode of
    SayExportForeignImports -> return ()

    SayExportDecls -> do
      addImports $ mconcat [hsImport1 "Prelude" "(.)",
                            hsImportForForeign,
                            hsImportForPrelude,
                            hsImportForRuntime]
      ln
      saysLn ["instance HoppyFHR.Decodable (HoppyF.Ptr (HoppyF.Ptr ",
              typeName, ")) ", typeName, " where"]
      indent $
        saysLn ["decode = HoppyP.fmap ",
                toHsDataCtorName Unmanaged Nonconst cls, " . HoppyF.peek"]

    SayExportBoot -> do
      addImports $ mconcat [hsImportForForeign, hsImportForRuntime]
      ln
      -- TODO Encodable.
      saysLn ["instance HoppyFHR.Decodable (HoppyF.Ptr (HoppyF.Ptr ", typeName, ")) ", typeName]

  -- Say Encodable and Decodable instances, if the class is encodable and
  -- decodable.
  forM_ (getClassHaskellConversion cls) $ \conv -> do
    hsType <- classHaskellConversionType conv
    let hsTypeStr = concat ["(", prettyPrint hsType, ")"]
    case mode of
      SayExportForeignImports -> return ()

      SayExportDecls -> do
        addImports $ mconcat [hsImportForPrelude, hsImportForRuntime]

        -- Say the Encodable instances.
        ln
        saysLn ["instance HoppyFHR.Encodable ", typeName, " ", hsTypeStr, " where"]
        indent $ do
          sayLn "encode ="
          indent $ classHaskellConversionToCppFn conv
        ln
        saysLn ["instance HoppyFHR.Encodable ", typeNameConst, " ", hsTypeStr, " where"]
        indent $
          saysLn ["encode = HoppyP.fmap (", toHsCastMethodName Const cls,
                  ") . HoppyFHR.encodeAs (HoppyP.undefined :: ", typeName, ")"]

        -- Say the Decodable instances.
        ln
        saysLn ["instance HoppyFHR.Decodable ", typeName, " ", hsTypeStr, " where"]
        indent $
          saysLn ["decode = HoppyFHR.decode . ", toHsCastMethodName Const cls]
        ln
        saysLn ["instance HoppyFHR.Decodable ", typeNameConst, " ", hsTypeStr, " where"]
        indent $ do
          sayLn "decode ="
          indent $ classHaskellConversionFromCppFn conv

      SayExportBoot -> do
        addImports hsImportForRuntime
        ln
        saysLn ["instance HoppyFHR.Encodable ", typeName, " (", hsTypeStr, ")"]
        saysLn ["instance HoppyFHR.Encodable ", typeNameConst, " (", hsTypeStr, ")"]
        saysLn ["instance HoppyFHR.Decodable ", typeName, " (", hsTypeStr, ")"]
        saysLn ["instance HoppyFHR.Decodable ", typeNameConst, " (", hsTypeStr, ")"]

-- | Generates a non-const @CppException@ instance if the class is an exception
-- class.
sayExportClassExceptionSupport :: Bool -> Class -> Generator ()
sayExportClassExceptionSupport doDecls cls = when (classIsException cls) $ do
  let typeName = toHsDataTypeName Nonconst cls
      typeNameConst = toHsDataTypeName Const cls

  -- Generate a non-const CppException instance.
  exceptionId <- getClassExceptionId cls
  addImports hsImportForRuntime
  saysLn ["instance HoppyFHR.CppException ", typeName,
          if doDecls then " where" else ""]
  when doDecls $ indent $ do
    let ctorName = toHsDataCtorName Unmanaged Nonconst cls
    addImports $ mconcat [hsImport1 "Prelude" "($)",
                          hsImportForForeign,
                          hsImportForMap,
                          hsImportForPrelude]
    sayLn "cppExceptionInfo _ ="
    indent $ do
      saysLn ["HoppyFHR.ExceptionClassInfo (HoppyFHR.ExceptionId ",
              show $ getExceptionId exceptionId, ") ", show typeName,
              " upcasts' delete' toGc'"]

      -- Note, similar "delete" and "toGc" functions are generated for the class's
      -- Deletable instance.
      saysLn ["where delete' ptr' = ", toHsClassDeleteFnName cls,
              " (HoppyF.castPtr ptr' :: HoppyF.Ptr ", toHsDataTypeName Const cls, ")"]

      indentSpaces 6 $ do
        saysLn ["toGc' ptr' = HoppyF.newForeignPtr ",
                -- The foreign delete function takes a const pointer; we cast it to
                -- take a Ptr () to match up with the ForeignPtr () we're creating,
                -- assuming that data pointers have the same representation.
                "(HoppyF.castFunPtr ", toHsClassDeleteFnPtrName cls,
                " :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) ",
                "ptr'"]

        sayLn "upcasts' = HoppyDM.fromList"
        indent $ case classSuperclasses cls of
          [] -> sayLn "[]"
          _ -> do
            let genCast :: Bool -> [Class] -> Class -> Generator ()
                genCast first path ancestorCls =
                  when (classIsException ancestorCls) $ do
                    let path' = ancestorCls : path
                    ancestorId <- getClassExceptionId ancestorCls
                    saysLn $ concat [ [if first then "[" else ",",
                                       " ( HoppyFHR.ExceptionId ",
                                       show $ getExceptionId ancestorId,
                                       ", \\(e' :: HoppyF.Ptr ()) -> "]
                                    , intersperse " $ " $
                                        "HoppyF.castPtr" :
                                        map (uncurry $ flip toHsCastPrimitiveName)
                                            (zip path' $ drop 1 path') ++
                                        ["HoppyF.castPtr e' :: HoppyF.Ptr ()"]
                                    , [")"]
                                    ]
                    forM_ (classSuperclasses ancestorCls) $ genCast False path'

            forM_ (zip (classSuperclasses cls) (True : repeat False)) $
              \(ancestorCls, first) -> genCast first [cls] ancestorCls
            sayLn "]"

    ln
    saysLn ["cppExceptionBuild ptr' = ", ctorName,
            " (HoppyF.castPtr ptr' :: HoppyF.Ptr ", typeName, ")"]
    ln
    saysLn ["cppExceptionBuildGc fptr' ptr' = ", toHsDataCtorName Managed Nonconst cls,
            " fptr' (HoppyF.castPtr ptr' :: HoppyF.Ptr ", typeName, ")"]

  -- Generate a const CppException instance that piggybacks off of the
  -- non-const implementation.
  saysLn ["instance HoppyFHR.CppException ", typeNameConst,
          if doDecls then " where" else ""]
  when doDecls $ indent $ do
    addImports $ mconcat [hsImport1 "Prelude" "(.)",
                          hsImportForPrelude]
    saysLn ["cppExceptionInfo _ = HoppyFHR.cppExceptionInfo (HoppyP.undefined :: ",
            typeName, ")"]
    saysLn ["cppExceptionBuild = ", toHsConstCastFnName Const cls,
            " . HoppyFHR.cppExceptionBuild"]
    saysLn ["cppExceptionBuildGc = (", toHsConstCastFnName Const cls,
            " .) . HoppyFHR.cppExceptionBuildGc"]

sayExportClassCastPrimitives :: SayExportMode -> Class -> Generator ()
sayExportClassCastPrimitives mode cls = do
  let clsType = toHsDataTypeName Const cls
  case mode of
    SayExportForeignImports ->
      forAncestors cls $ \super -> do
        let hsCastFnName = toHsCastPrimitiveName cls super
            hsDownCastFnName = toHsCastPrimitiveName super cls
            superType = toHsDataTypeName Const super
        addImports hsImportForForeign
        addExport hsCastFnName
        saysLn [ "foreign import ccall \"", classCastFnCppName cls super
               , "\" ", hsCastFnName, " :: HoppyF.Ptr ", clsType, " -> HoppyF.Ptr ", superType
               ]
        unless (classIsSubclassOfMonomorphic cls || classIsMonomorphicSuperclass super) $ do
          addExport hsDownCastFnName
          saysLn [ "foreign import ccall \"", classCastFnCppName super cls
                 , "\" ", hsDownCastFnName, " :: HoppyF.Ptr ", superType, " -> HoppyF.Ptr ", clsType
                 ]
        return True

    SayExportDecls ->
      -- Generate a downcast typeclass and instances for all ancestor classes
      -- for the current constness.  These don't need to be in the boot file,
      -- since they're not used by other generated bindings.
      unless (classIsSubclassOfMonomorphic cls) $
      forM_ [minBound..] $ \cst -> do
        let downCastClassName = toHsDownCastClassName cst cls
            downCastMethodName = toHsDownCastMethodName cst cls
        addExport' downCastClassName
        ln
        saysLn ["class ", downCastClassName, " a where"]
        indent $ saysLn [downCastMethodName, " :: ",
                         prettyPrint $ HsTyFun (HsTyVar $ HsIdent "a") $
                         HsTyCon $ UnQual $ HsIdent $ toHsDataTypeName cst cls]
        ln
        forAncestors cls $ \super -> case classIsMonomorphicSuperclass super of
          True -> return False
          False -> do
            let superTypeName = toHsDataTypeName cst super
                primitiveCastFn = toHsCastPrimitiveName super cls
            addImportForClass super
            saysLn ["instance ", downCastClassName, " ", superTypeName, " where"]

            -- If Foo is a superclass of Bar:
            --
            -- instance BarSuper Foo where
            --   downToBar castFooToNonconst . downcast' . castFooToConst
            --     where downcast' (FooConst ptr') = BarConst $ castFooToBar ptr'
            --           downcast' (FooConstGc fptr' ptr') = BarConstGc fptr' $ castFooToBar ptr'
            --
            -- instance BarSuperConst FooConst where
            --   downToBarConst = downcast'
            --     where downcast' (FooConst ptr') = BarConst $ castFooToBar ptr'
            --           downcast' (FooConstGc fptr' ptr') = BarConstGc fptr' $ castFooToBar ptr'

            indent $ do
              saysLn $
                downCastMethodName : " = " :
                case cst of
                  Const -> ["cast'"]
                  Nonconst -> [toHsConstCastFnName Nonconst cls,
                               " . cast' . ",
                               toHsConstCastFnName Const super]
              indent $ do
                sayLn "where"
                indent $ do
                  saysLn ["cast' (", toHsDataCtorName Unmanaged Const super, " ptr') = ",
                          toHsDataCtorName Unmanaged Const cls, " $ ",
                          primitiveCastFn, " ptr'"]
                  saysLn ["cast' (", toHsDataCtorName Managed Const super, " fptr' ptr') = ",
                          toHsDataCtorName Managed Const cls, " fptr' $ ",
                          primitiveCastFn, " ptr'"]
            return True

    SayExportBoot -> do
      forAncestors cls $ \super -> do
        let hsCastFnName = toHsCastPrimitiveName cls super
            superType = toHsDataTypeName Const super
        addImports $ hsImportForForeign
        addExport hsCastFnName
        saysLn [hsCastFnName, " :: HoppyF.Ptr ", clsType, " -> HoppyF.Ptr ", superType]
        return True

  where forAncestors :: Class -> (Class -> Generator Bool) -> Generator ()
        forAncestors cls' f = forM_ (classSuperclasses cls') $ \super -> do
          recur <- f super
          when recur $ forAncestors super f

-- | Outputs the @ExceptionDb@ needed by all Haskell gateway functions that deal
-- with exceptions.
sayExceptionSupport :: Bool -> Generator ()
sayExceptionSupport doDecls = do
  iface <- askInterface
  addExport "exceptionDb'"
  addImports hsImportForRuntime
  ln
  sayLn "exceptionDb' :: HoppyFHR.ExceptionDb"
  when doDecls $ do
    addImports $ mconcat [hsImport1 "Prelude" "($)",
                          hsImportForMap]
    sayLn "exceptionDb' = HoppyFHR.ExceptionDb $ HoppyDM.fromList"
    indent $ do
      let classes = interfaceAllExceptionClasses iface
      case classes of
        [] -> sayLn "[]"
        _ -> do
          addImports hsImportForPrelude
          forM_ (zip classes (True : repeat False)) $ \(cls, first) -> do
            exceptionId <-
              fromMaybeM (throwError $ "sayExceptionSupport: Internal error, " ++ show cls ++
                          " has no exception ID.") $
              interfaceExceptionClassId iface cls
            addImportForClass cls
            saysLn [if first then "[ (" else ", (",
                    "HoppyFHR.ExceptionId ", show $ getExceptionId exceptionId,
                    ", HoppyFHR.cppExceptionInfo (HoppyP.undefined :: ",
                    toHsDataTypeName Nonconst cls, "))"]
          sayLn "]"

-- | Implements special logic on top of 'cppTypeToHsTypeAndUse', that computes
-- the Haskell __qualified__ type for a function, including typeclass
-- constraints.
fnToHsTypeAndUse :: HsTypeSide
                 -> Maybe (Constness, Class)
                 -> Purity
                 -> [Type]
                 -> Type
                 -> ExceptionHandlers
                 -> Generator HsQualType
fnToHsTypeAndUse side methodInfo purity paramTypes returnType exceptionHandlers = do
  let catches = not $ null $ exceptionHandlersList exceptionHandlers

  params <- mapM contextForParam $
            (if catches && side == HsCSide
             then (++ [("excId", ptrT intT), ("excPtr", ptrT $ ptrT voidT)])
             else id) $
            (case methodInfo of
                Just (cst, cls) -> [("this", case cst of
                                        Nonconst -> ptrT $ objT cls
                                        Const -> ptrT $ constT $ objT cls)]
                Nothing -> []) ++
            zip (map toArgName [1..]) paramTypes
  let context = mapMaybe fst params :: HsContext
      hsParams = map snd params

  -- Determine the 'HsHsSide' return type for the function.  If the function is
  -- returning a 'TObj' of a class that uses 'ClassConversionToHeap' or
  -- 'ClassConversionToGc', then first we wrap the return type.  Then we do the
  -- conversion to a Haskell type, and wrap the result in 'IO' if the function
  -- is impure.  (HsCSide types always get wrapped in IO.)
  returnForGc <- case returnType of
    Internal_TObj cls -> case classHaskellConversion $ classConversion cls of
      ClassConversionNone ->
        throwError $ concat ["Expected ", show cls, " to be returnable from a C++ function"]
      ClassConversionManual _ -> return returnType
      ClassConversionToHeap -> return $ objToHeapT cls
      ClassConversionToGc -> return $ toGcT returnType
    _ -> return returnType
  hsReturnForGc <- cppTypeToHsTypeAndUse side returnForGc
  hsReturnForPurity <- case (purity, side) of
    (Pure, HsHsSide) -> return hsReturnForGc
    _ -> do
      addImports hsImportForPrelude
      return $ HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") hsReturnForGc

  return $ HsQualType context $ foldr HsTyFun hsReturnForPurity hsParams

  where contextForParam :: (String, Type) -> Generator (Maybe HsAsst, HsType)
        contextForParam (s, t) = case t of
          Internal_TBitspace b -> receiveBitspace s t b
          Internal_TPtr (Internal_TObj cls) -> receivePtr s cls Nonconst
          Internal_TPtr (Internal_TConst (Internal_TObj cls)) -> receiveValue s t cls
          Internal_TRef (Internal_TObj cls) -> receivePtr s cls Nonconst
          Internal_TRef (Internal_TConst (Internal_TObj cls)) -> receiveValue s t cls
          Internal_TObj cls -> receiveValue s t cls
          Internal_TConst t' -> contextForParam (s, t')
          _ -> handoff side t

        -- Use whatever type 'cppTypeToHsTypeAndUse' suggests, with no typeclass
        -- constraints.
        handoff :: HsTypeSide -> Type -> Generator (Maybe HsAsst, HsType)
        handoff side t = (,) Nothing <$> cppTypeToHsTypeAndUse side t

        -- Receives a @IsFooBitspace a => a@.
        receiveBitspace s t b = case side of
          HsCSide -> handoff side t
          HsHsSide -> do
            importHsModuleForExtName $ bitspaceExtName b
            let t' = HsTyVar $ HsIdent s
            return (Just (UnQual $ HsIdent $ toHsBitspaceClassName b, [t']),
                    t')

        -- Receives a @FooPtr this => this@.
        receivePtr :: String -> Class -> Constness -> Generator (Maybe HsAsst, HsType)
        receivePtr s cls cst = do
          addImportForClass cls
          case side of
            HsHsSide -> do
              let t' = HsTyVar $ HsIdent s
              return (Just (UnQual $ HsIdent $ toHsPtrClassName cst cls, [t']),
                      t')
            HsCSide -> do
              addImports $ hsImportForForeign
              return (Nothing, HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.Ptr") $
                               HsTyVar $ HsIdent $ toHsDataTypeName cst cls)

        -- Receives a @FooValue a => a@.
        receiveValue :: String -> Type -> Class -> Generator (Maybe HsAsst, HsType)
        receiveValue s t cls = case side of
          HsCSide -> handoff side t
          HsHsSide -> do
            addImports hsImportForRuntime
            addImportForClass cls
            let t' = HsTyVar $ HsIdent s
            return (Just (UnQual $ HsIdent $ toHsValueClassName cls, [t']),
                    t')

getMethodEffectiveParams :: Class -> Method -> [Type]
getMethodEffectiveParams cls method =
  (case methodImpl method of
     RealMethod {} -> case methodApplicability method of
       MNormal -> (ptrT (objT cls):)
       MConst -> (ptrT (constT $ objT cls):)
       MStatic -> id
     FnMethod {} -> id) $
  methodParams method

getEffectiveExceptionHandlers :: ExceptionHandlers -> Generator ExceptionHandlers
getEffectiveExceptionHandlers handlers = do
  ifaceHandlers <- interfaceExceptionHandlers <$> askInterface
  moduleHandlers <- getExceptionHandlers <$> askModule
  -- Exception handlers declared lower in the hierarchy take precedence over
  -- those in the hierarchy; ExceptionHandlers is a left-biased monoid.
  return $ mconcat [handlers, moduleHandlers, ifaceHandlers]

getClassExceptionId :: Class -> Generator ExceptionId
getClassExceptionId cls = do
  iface <- askInterface
  fromMaybeM (throwError $ concat
              ["Internal error, exception class ", show cls, " doesn't have an exception ID"]) $
    interfaceExceptionClassId iface cls

-- | Imports bindings for the given class into the Haskell module.
addImportForClass :: Class -> Generator ()
addImportForClass = importHsModuleForExtName . classExtName
