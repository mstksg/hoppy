-- This file is part of Hoppy.
--
-- Copyright 2015-2023 Bryan Gardiner <bog@khumba.net>
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

{-# LANGUAGE ViewPatterns #-}

-- | Interface for defining bindings to C++ functions.
module Foreign.Hoppy.Generator.Spec.Function (
  -- * Data type
  Function, fnT, fnT',
  -- * Construction
  makeFn,
  -- * Properties
  fnExtName,
  fnCName,
  fnPurity,
  fnParams,
  fnReturn,
  fnReqs,
  fnAddendum,
  fnExceptionHandlers,
  -- * Code generators
  CallDirection (..),
  -- ** C++ generator
  CppCallType (..),
  sayCppArgRead,
  sayCppArgNames,
  -- * Internal
  -- ** C++ generator
  sayCppExportFn,
  -- ** Haskell generator
  sayHsExportFn,
  sayHsArgProcessing,
  sayHsCallAndProcessReturn,
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (throwError)
import Data.Function (on)
import Data.List (intersperse)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Foreign.Hoppy.Generator.Common (fromMaybeM)
import qualified Foreign.Hoppy.Generator.Language.Cpp as LC
import qualified Foreign.Hoppy.Generator.Language.Haskell as LH
import {-# SOURCE #-} qualified Foreign.Hoppy.Generator.Spec.Class as Class
import Foreign.Hoppy.Generator.Spec.Base
import Foreign.Hoppy.Generator.Types (constT, intT, objT, objToHeapT, ptrT, refT, voidT)
import Language.Haskell.Syntax (
  HsContext,
  HsName (HsIdent),
  HsQName (UnQual),
  HsQualType (HsQualType),
  HsType (HsTyApp, HsTyCon, HsTyFun, HsTyVar),
  )

-- | A C++ function declaration.
--
-- Use this data type's 'HasReqs' instance to make the function accessible.  You
-- do not need to add requirements for parameter or return types.
data Function = Function
  { fnCName :: FnName Identifier
    -- ^ The identifier used to call the function.
  , fnExtName :: ExtName
    -- ^ The function's external name.
  , fnPurity :: Purity
    -- ^ Whether the function is pure.
  , fnParams :: [Parameter]
    -- ^ The function's parameters.
  , fnReturn :: Type
    -- ^ The function's return type.
  , fnReqs :: Reqs
    -- ^ Requirements for bindings to access this function.
  , fnExceptionHandlers :: ExceptionHandlers
    -- ^ Exceptions that the function might throw.
  , fnAddendum :: Addendum
    -- ^ The function's addendum.
  }

instance Eq Function where
  (==) = (==) `on` fnExtName

instance Show Function where
  show fn =
    concat ["<Function ", show (fnExtName fn), " ", show (fnCName fn),
            show (fnParams fn), " ", show (fnReturn fn), ">"]

instance Exportable Function where
  sayExportCpp = sayCppExport
  sayExportHaskell = sayHsExport

instance HasExtNames Function where
  getPrimaryExtName = fnExtName

instance HasReqs Function where
  getReqs = fnReqs
  setReqs reqs fn = fn { fnReqs = reqs }

instance HasAddendum Function where
  getAddendum = fnAddendum
  setAddendum addendum fn = fn { fnAddendum = addendum }

instance HandlesExceptions Function where
  getExceptionHandlers = fnExceptionHandlers
  modifyExceptionHandlers f fn = fn { fnExceptionHandlers = f $ fnExceptionHandlers fn }

-- | Creates a binding for a C++ function.
makeFn :: (IsFnName Identifier name, IsParameter p)
       => name
       -> Maybe ExtName
       -- ^ An optional external name; will be automatically derived from
       -- the identifier if absent.
       -> Purity
       -> [p]  -- ^ Parameter types.
       -> Type  -- ^ Return type.
       -> Function
makeFn cName maybeExtName purity paramTypes retType =
  let fnName = toFnName cName
  in Function fnName
              (extNameOrFnIdentifier fnName maybeExtName)
              purity (toParameters paramTypes) retType mempty mempty mempty

-- | A function taking parameters and returning a value (or 'voidT').  Function
-- pointers must wrap a 'fnT' in a 'ptrT'.
--
-- See also 'fnT'' which accepts parameter information.
fnT :: [Type] -> Type -> Type
-- (Keep docs in sync with hs-boot.)
fnT = Internal_TFn . map toParameter

-- | A version of 'fnT' that accepts additional information about parameters.
fnT' :: [Parameter] -> Type -> Type
-- (Keep docs in sync with hs-boot.)
fnT' = Internal_TFn

sayCppExport :: LC.SayExportMode -> Function -> LC.Generator ()
sayCppExport mode fn = case mode of
  LC.SayHeader -> return ()
  LC.SaySource -> do
    LC.addReqsM $ fnReqs fn
    sayCppExportFn (fnExtName fn)
                   (case fnCName fn of
                      FnName identifier -> CallFn $ LC.sayIdentifier identifier
                      FnOp op -> CallOp op)
                   Nothing
                   (fnParams fn)
                   (fnReturn fn)
                   (fnExceptionHandlers fn)
                   True  -- Render the body.

-- | The direction between languages in which a value is being passed.
data CallDirection =
  ToCpp  -- ^ Haskell code is calling out to C++.
  | FromCpp  -- ^ C++ is invoking a callback.
  deriving (Show)

-- | The name of a function to call.
data CppCallType =
    CallOp Operator
    -- ^ A call to the given operator, for example @x++@, @x * y@, @a[i]@.
  | CallFn (LC.Generator ())
    -- ^ A call to the function whose name is emitted by the given action.
  | VarRead (LC.Generator ())
    -- ^ Not a function call, but a read from a variable whose name is emitted
    -- by the given action.
  | VarWrite (LC.Generator ())
    -- ^ Not a function call, but a write to a variable whose name is emitted by
    -- the given action.

-- | Generates a C++ wrapper function for calling a C++ function (or method, or
-- reading from or writing to a variable).  The generated function handles
-- C++-side marshalling of values and propagating exceptions as requested.
--
-- See also 'sayHsExportFn'.
sayCppExportFn ::
     ExtName  -- ^ The external name of the function.
  -> CppCallType  -- ^ The C++ name at which the function can be invoked.
  -> Maybe Type
     -- ^ If present, then we are wrapping a method within some class, and the
     -- type is that of the class.
  -> [Parameter]  -- ^ Info about the function's parameters.
  -> Type  -- ^ The function's return type.
  -> ExceptionHandlers
     -- ^ Exception handlers configured on the function itself.  No need to call
     -- 'LC.getEffectiveExceptionHandlers' to combine the function's handlers
     -- with those from the module and interface; this function does that already.
  -> Bool
     -- ^ Whether to generate the function definition.  If false, only the
     -- declaration is generated (no function body).
  -> LC.Generator ()
sayCppExportFn extName callType maybeThisType params retType exceptionHandlers sayBody = do
  handlerList <- exceptionHandlersList <$> LC.getEffectiveExceptionHandlers exceptionHandlers
  let paramTypes = map parameterType params
      catches = not $ null handlerList
      addExceptionParamNames =
        if catches then (++ [LC.exceptionIdArgName, LC.exceptionPtrArgName]) else id
      addExceptionParamTypes = if catches then (++ [ptrT intT, ptrT $ ptrT voidT]) else id

      paramCount = length paramTypes
  paramCTypeMaybes <- mapM LC.typeToCType paramTypes
  let paramCTypes = zipWith fromMaybe paramTypes paramCTypeMaybes
  retCTypeMaybe <- LC.typeToCType retType
  let retCType = fromMaybe retType retCTypeMaybe

  LC.addReqsM . mconcat =<< mapM LC.typeReqs (retType:paramTypes)

  LC.sayFunction (LC.externalNameToCpp extName)
                 (maybe id (const ("self":)) maybeThisType $
                  addExceptionParamNames $
                  zipWith3 (\pt ctm ->
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
                 (fnT (addExceptionParamTypes $ maybe id (:) maybeThisType paramCTypes)
                      retCType) $
    if not sayBody
    then Nothing
    else Just $ do
      when catches $ do
        LC.say "try {\n"
        LC.says ["*", LC.exceptionIdArgName, " = 0;\n"]

      -- Convert arguments that aren't passed in directly.
      mapM_ (sayCppArgRead ToCpp) $ zip3 [1..] paramTypes paramCTypeMaybes

      let -- Determines how to call the exported function or method.
          sayCall = case callType of
            CallOp op -> do
              LC.say "("
              let effectiveParamCount = paramCount + if isJust maybeThisType then 1 else 0
                  paramNames@(p1:p2:_) = (if isJust maybeThisType then ("(*self)":) else id) $
                                         map LC.toArgName [1..]
                  assertParamCount n =
                    when (effectiveParamCount /= n) $ LC.abort $ concat
                    ["sayCppExportFn: Operator ", show op, " for export ", show extName,
                     " requires ", show n, " parameter(s), but has ", show effectiveParamCount,
                     "."]
              case operatorType op of
                UnaryPrefixOperator symbol -> assertParamCount 1 >> LC.says [symbol, p1]
                UnaryPostfixOperator symbol -> assertParamCount 1 >> LC.says [p1, symbol]
                BinaryOperator symbol -> assertParamCount 2 >> LC.says [p1, symbol, p2]
                CallOperator ->
                  LC.says $ p1 : "(" : take (effectiveParamCount - 1) (drop 1 paramNames) ++ [")"]
                ArrayOperator -> assertParamCount 2 >> LC.says [p1, "[", p2, "]"]
              LC.say ")"
            CallFn sayCppName -> do
              when (isJust maybeThisType) $ LC.say "self->"
              sayCppName
              LC.say "("
              sayCppArgNames paramCount
              LC.say ")"
            VarRead sayVarName -> do
              when (isJust maybeThisType) $ LC.say "self->"
              sayVarName
            VarWrite sayVarName -> do
              when (isJust maybeThisType) $ LC.say "self->"
              sayVarName
              LC.says [" = ", LC.toArgName 1]

          -- Writes the call, transforming the return value if necessary.
          -- These translations should be kept in sync with typeToCType.
          sayCallAndReturn retType' retCTypeMaybe' = case (retType', retCTypeMaybe') of
            -- Void needs special handling because we don't want a return statement.
            (Internal_TVoid, Nothing) -> sayCall >> LC.say ";\n"

            -- Custom conversions.
            (Internal_TManual s, _) -> do
              -- The ConversionSpec s may or may not specify an intermediate
              -- type to pass over the FFI boundary: the second value in the
              -- pair (we check this before the (_, Nothing) case below).  We
              -- don't actually care what it is though, because s already
              -- specifies how to convert.
              case conversionSpecCppConversionToCppExpr $ conversionSpecCpp s of
                -- If there is a custom conversion expression defined, use it.
                Just convFn -> LC.say "return " >> convFn sayCall Nothing >> LC.say ";\n"
                -- Otherwise, assume we can just return the value directly.
                Nothing -> sayCallAndReturnDirect

            -- The case of a value for which no conversion is necessary.
            (_, Nothing) -> sayCallAndReturnDirect

            -- Object cases.
            (Internal_TRef cls, Just (Internal_TPtr cls')) | cls == cls' ->
              LC.say "return &(" >> sayCall >> LC.say ");\n"
            (Internal_TObj cls,
             Just (Internal_TPtr (Internal_TConst (Internal_TObj cls')))) | cls == cls' ->
              sayReturnNew cls sayCall
            (Internal_TObjToHeap cls, Just (Internal_TPtr (Internal_TObj cls'))) | cls == cls' ->
              sayReturnNew cls sayCall
            (Internal_TToGc (Internal_TObj cls),
             Just (Internal_TPtr (Internal_TObj cls'))) | cls == cls' ->
              sayReturnNew cls sayCall
            (Internal_TToGc retType'', _) -> sayCallAndReturn retType'' retCTypeMaybe'

            ts -> LC.abort $ concat ["sayCppExportFn: Unexpected return types ", show ts,
                                     " while generating binding for ", show extName, "."]

          sayCallAndReturnDirect = LC.say "return " >> sayCall >> LC.say ";\n"

      sayCallAndReturn retType retCTypeMaybe

      when catches $ do
        iface <- LC.askInterface

        forM_ handlerList $ \handler -> do
          LC.say "} catch ("
          case handler of
            CatchClass cls -> LC.sayVar LC.exceptionVarName Nothing $ refT $ constT $ objT cls
            CatchAll -> LC.say "..."
          LC.say ") {\n"

          exceptionId <- case handler of
            CatchClass cls -> case interfaceExceptionClassId iface cls of
              Just exceptionId -> return exceptionId
              Nothing -> LC.abort $ concat
                         ["sayCppExportFn: Trying to catch non-exception class ", show cls,
                          " while generating binding for ", show extName, "."]
            CatchAll -> return exceptionCatchAllId
          LC.says ["*", LC.exceptionIdArgName, " = ", show $ getExceptionId exceptionId, ";\n"]

          case handler of
            CatchAll -> LC.says ["*", LC.exceptionPtrArgName, " = 0;\n"]
            CatchClass cls -> do
              -- Object pointers don't convert automatically to void*.
              LC.says ["*", LC.exceptionPtrArgName, " = reinterpret_cast<void*>(new "]
              LC.sayType Nothing $ objT cls
              LC.says ["(", LC.exceptionVarName, "));\n"]

          -- For all of the types our gateway functions actually return, "return
          -- 0" is a valid statement.
          when (retType /= Internal_TVoid) $ LC.say "return 0;\n"

        LC.say "}\n"

  where sayReturnNew cls sayCall =
          LC.say "return new" >> LC.sayIdentifier (Class.classIdentifier cls) >> LC.say "(" >>
          sayCall >> LC.say ");\n"

-- | Generates code to marshal a value between a C++ type and the intermediate
-- type to be used over the FFI.  If @dir@ is 'ToCpp', then we are a C++
-- function reading an argument from foreign code.  If @dir@ is 'FromCpp', then
-- we are invoking a foreign callback.
sayCppArgRead :: CallDirection -> (Int, Type, Maybe Type) -> LC.Generator ()
sayCppArgRead dir (n, stripConst . normalizeType -> cppType, maybeCType) = case cppType of
  t@(Internal_TPtr (Internal_TFn params retType)) -> do
    -- Assert that all types referred to in a function pointer type are all
    -- representable as C types.
    let paramTypes = map parameterType params
        check label t' = ((label ++ " " ++ show t') <$) <$> LC.typeToCType t'
    mismatches <-
      fmap catMaybes $
      (:) <$> check "return type" retType
          <*> mapM (\paramType -> check "parameter" paramType) paramTypes
    unless (null mismatches) $
      LC.abort $ concat $
      "sayCppArgRead: Some types within a function pointer type use non-C types, " :
      "but only C types may be used.  The unsupported types are: " :
      intersperse "; " mismatches ++ [".  The whole function type is ", show t, "."]

    convertDefault

  Internal_TRef t -> convertObj t

  Internal_TObj _ -> convertObj $ constT cppType

  Internal_TObjToHeap cls -> case dir of
    ToCpp -> error $ objToHeapTWrongDirectionErrorMsg (Just "sayCppArgRead") cls
    FromCpp -> do
      LC.sayIdentifier $ Class.classIdentifier cls
      LC.says ["* ", LC.toArgName n, " = new "]
      LC.sayIdentifier $ Class.classIdentifier cls
      LC.says ["(", LC.toArgNameAlt n, ");\n"]

  Internal_TToGc t' -> case dir of
    ToCpp -> error $ toGcTWrongDirectionErrorMsg (Just "sayCppArgRead") t'
    FromCpp -> do
      let newCppType = case t' of
            -- In the case of (TToGc (TObj _)), we copy the temporary object to
            -- the heap and let the foreign language manage that value.
            Internal_TObj cls -> objToHeapT cls
            _ -> t'
      cType <- LC.typeToCType newCppType
      sayCppArgRead dir (n, newCppType, cType)

  -- In case of a manual type, apply the custom conversion, if there is one.
  Internal_TManual s -> do
    let maybeConvExpr =
          (case dir of
             ToCpp -> conversionSpecCppConversionToCppExpr
             FromCpp -> conversionSpecCppConversionFromCppExpr) $
          conversionSpecCpp s
    forM_ maybeConvExpr $ \gen ->
      gen (LC.say $ LC.toArgNameAlt n) (Just $ LC.say $ LC.toArgName n)

  _ -> convertDefault

  where -- Primitive types don't need to be encoded/decoded.  But if maybeCType is a
        -- Just, then we're expected to do some encoding/decoding, so something is
        -- wrong.
        --
        -- TODO Do we need to handle TConst?
        convertDefault = forM_ maybeCType $ \cType ->
          LC.abort $ concat
          ["sayCppArgRead: Don't know how to convert ", show dir, " between C-type ", show cType,
           " and C++-type ", show cppType, "."]

        convertObj cppType' = case dir of
          ToCpp -> do
            LC.sayVar (LC.toArgName n) Nothing $ refT cppType'
            LC.says [" = *", LC.toArgNameAlt n, ";\n"]
          FromCpp -> do
            LC.sayVar (LC.toArgName n) Nothing $ ptrT cppType'
            LC.says [" = &", LC.toArgNameAlt n, ";\n"]

-- | Prints a comma-separated list of the argument names used for C++ gateway
-- functions.  The number specifies how many names to print.
sayCppArgNames :: Int -> LC.Generator ()
sayCppArgNames count =
  LC.says $ intersperse ", " $ map LC.toArgName [1..count]

sayHsExport :: LH.SayExportMode -> Function -> LH.Generator ()
sayHsExport mode fn =
  (sayHsExportFn mode <$> fnExtName <*> fnExtName <*> fnPurity <*>
   fnParams <*> fnReturn <*> fnExceptionHandlers) fn

-- | Generates a Haskell wrapper function for calling a C++ function (or method,
-- or reading from or writing to a variable, as with 'sayCppExportFn').  The
-- generated function handles Haskell-side marshalling of values and propagating
-- exceptions as requested.
sayHsExportFn ::
     LH.SayExportMode  -- ^ The phase of code generation.
  -> ExtName
     -- ^ The external name for the entity we're generating.  For class
     -- entities, this will include the class's external name as a prefix.
  -> ExtName
     -- ^ An alternate external name to use to generate Haskell function names.
     -- For non-class entities, this can be just the regular external name.  For
     -- class entities, in order to strip off the class name that was added so
     -- that the entity's external name is unique, this can just be the name of
     -- the function, variable, etc.
  -> Purity  -- ^ Whether or not the function is pure (free of side effects).
  -> [Parameter]  -- ^ Parameter info.
  -> Type  -- ^ The return type.
  -> ExceptionHandlers
     -- ^ Any exception handlers to apply to the binding, in addition to what
     -- its module and interface provide.
  -> LH.Generator ()
sayHsExportFn mode extName foreignName purity params retType exceptionHandlers = do
  effectiveHandlers <- LH.getEffectiveExceptionHandlers exceptionHandlers
  let handlerList = exceptionHandlersList effectiveHandlers
      catches = not $ null handlerList

      paramTypes = map parameterType params

  -- We use the pure version of toHsFnName here; because foreignName isn't an
  -- ExtName present in the interface's lookup table, toHsFnName would bail on
  -- it.  Since functions don't reference each other (e.g. we don't put anything
  -- in .hs-boot files for them in circular modules cases), this isn't a problem.
  let hsFnName = LH.toHsFnName' foreignName
      hsFnImportedName = hsFnName ++ "'"

  case mode of
    LH.SayExportForeignImports ->
      LH.withErrorContext ("generating imports for function " ++ show extName) $ do
        -- Print a "foreign import" statement.
        hsCType <- fnToHsTypeAndUse LH.HsCSide purity params retType effectiveHandlers
        LH.saysLn ["foreign import ccall \"", LC.externalNameToCpp extName, "\" ", hsFnImportedName,
                    " :: ", renderFnHsType hsCType]

    LH.SayExportDecls -> LH.withErrorContext ("generating function " ++ show extName) $ do
      -- Print the type signature.
      LH.ln
      LH.addExport hsFnName
      hsHsType <- fnToHsTypeAndUse LH.HsHsSide purity params retType effectiveHandlers
      LH.saysLn [hsFnName, " :: ", renderFnHsTypeWithNames hsHsType]

      case purity of
        Nonpure -> return ()
        Pure -> LH.saysLn ["{-# NOINLINE ", hsFnName, " #-}"]

      -- Print the function body.
      let argNames = map LH.toArgName [1..length paramTypes]
          convertedArgNames = map (++ "'") argNames
      -- Operators on this line must bind more weakly than operators used below,
      -- namely ($) and (>>=).  (So finish the line with ($).)
      lineEnd <- case purity of
        Nonpure -> return [" ="]
        Pure -> do LH.addImports $ mconcat [hsImport1 "Prelude" "($)", hsImportForUnsafeIO]
                   return [" = HoppySIU.unsafePerformIO $"]
      LH.saysLn $ hsFnName : map (' ':) argNames ++ lineEnd
      LH.indent $ do
        forM_ (zip3 paramTypes argNames convertedArgNames) $ \(t, argName, argName') ->
          sayHsArgProcessing ToCpp t argName argName'

        exceptionHandling <-
          if catches
          then do iface <- LH.askInterface
                  currentModule <- LH.askModule
                  let exceptionSupportModule = interfaceExceptionSupportModule iface
                  when (exceptionSupportModule /= Just currentModule) $
                    LH.addImports . hsWholeModuleImport . LH.getModuleName iface =<<
                    fromMaybeM (throwError
                                "Internal error, an exception support module is not available")
                    exceptionSupportModule
                  LH.addImports $ mconcat [hsImport1 "Prelude" "($)", hsImportForRuntime]
                  return "HoppyFHR.internalHandleExceptions exceptionDb' $"
          else return ""

        let callWords = exceptionHandling : hsFnImportedName : map (' ':) convertedArgNames
        sayHsCallAndProcessReturn ToCpp retType callWords

    LH.SayExportBoot ->
      -- Functions (methods included) cannot be referenced from other exports,
      -- so we don't need to emit anything.
      --
      -- If this changes, revisit the comment on hsFnName above.
      return ()

-- | Generates Haskell code to perform marshalling of a function's argument in a
-- specified direction.
--
-- This function either generates a line or lines such that subsequent lines can
-- refer to the output binding.  The final line is either terminated with
--
-- > ... $ \value ->
--
-- or
--
-- > let ... in
--
-- so that precedence is not an issue.
sayHsArgProcessing ::
     CallDirection  -- ^ The direction of the FFI call.
  -> Type  -- ^ The type of the value to be marshalled.
  -> String  -- ^ The name of the binding holding the input value.
  -> String  -- ^ The name of the binding to create for the output value.
  -> LH.Generator ()
sayHsArgProcessing dir t fromVar toVar =
  LH.withErrorContext ("processing argument of type " ++ show t) $
  case t of
    Internal_TVoid -> throwError $ "TVoid is not a valid argument type"
    -- References and pointers are handled equivalently.
    Internal_TPtr (Internal_TObj cls) -> case dir of
      ToCpp -> do
        LH.addImports $ mconcat [hsImport1 "Prelude" "($)",
                                 hsImportForRuntime]
        castMethodName <- Class.toHsCastMethodName Nonconst cls
        LH.saysLn ["HoppyFHR.withCppPtr (", castMethodName, " ", fromVar,
                   ") $ \\", toVar, " ->"]
      FromCpp -> do
        ctorName <- Class.toHsDataCtorName LH.Unmanaged Nonconst cls
        LH.saysLn ["let ", toVar, " = ", ctorName, " ", fromVar, " in"]
    Internal_TPtr (Internal_TConst (Internal_TObj cls)) -> case dir of
      ToCpp -> do
        -- Same as the (TObj _), ToCpp case.
        LH.addImports $ mconcat [hsImport1 "Prelude" "($)",
                                 hsImportForPrelude,
                                 hsImportForRuntime]
        withValuePtrName <- Class.toHsWithValuePtrName cls
        LH.saysLn [withValuePtrName, " ", fromVar,
                   " $ HoppyP.flip HoppyFHR.withCppPtr $ \\", toVar, " ->"]
      FromCpp -> do
        ctorName <- Class.toHsDataCtorName LH.Unmanaged Const cls
        LH.saysLn ["let ", toVar, " = ", ctorName, " ", fromVar, " in"]
    Internal_TPtr _ -> noConversion
    Internal_TRef t' -> sayHsArgProcessing dir (ptrT t') fromVar toVar
    Internal_TFn {} -> throwError "TFn unimplemented"
    Internal_TObj cls -> case dir of
      ToCpp -> do
        -- Same as the (TPtr (TConst (TObj _))), ToPtr case.
        LH.addImports $ mconcat [hsImport1 "Prelude" "($)",
                                 hsImportForPrelude,
                                 hsImportForRuntime]
        withValuePtrName <- Class.toHsWithValuePtrName cls
        LH.saysLn [withValuePtrName, " ", fromVar,
                " $ HoppyP.flip HoppyFHR.withCppPtr $ \\", toVar, " ->"]
      FromCpp -> case Class.classHaskellConversionFromCppFn $ LH.getClassHaskellConversion cls of
        Just _ -> do
          LH.addImports $ mconcat [hsImport1 "Prelude" "(>>=)",
                                   hsImportForRuntime]
          ctorName <- Class.toHsDataCtorName LH.Unmanaged Const cls
          LH.saysLn ["HoppyFHR.decode (", ctorName, " ", fromVar, ") >>= \\", toVar, " ->"]
        Nothing ->
          throwError $ concat
          ["Can't pass a TObj of ", show cls,
           " from C++ to Haskell because no class decode conversion is defined"]
    Internal_TObjToHeap cls -> case dir of
      ToCpp -> throwError $ objToHeapTWrongDirectionErrorMsg Nothing cls
      FromCpp -> sayHsArgProcessing dir (ptrT $ objT cls) fromVar toVar
    Internal_TToGc t' -> case dir of
      ToCpp -> throwError $ toGcTWrongDirectionErrorMsg Nothing t'
      FromCpp -> do
        LH.addImports $ mconcat [hsImport1 "Prelude" "(>>=)",
                                 hsImportForRuntime]
        ctorName <-
          maybe (throwError $ tToGcInvalidFormErrorMessage Nothing t')
                (Class.toHsDataCtorName LH.Unmanaged Nonconst) $
          case stripConst t' of
            Internal_TObj cls -> Just cls
            Internal_TRef (Internal_TConst (Internal_TObj cls)) -> Just cls
            Internal_TRef (Internal_TObj cls) -> Just cls
            Internal_TPtr (Internal_TConst (Internal_TObj cls)) -> Just cls
            Internal_TPtr (Internal_TObj cls) -> Just cls
            _ -> Nothing
        LH.saysLn ["HoppyFHR.toGc (", ctorName, " ", fromVar, ") >>= \\", toVar, " ->"]
    Internal_TConst t' -> sayHsArgProcessing dir t' fromVar toVar

    Internal_TManual s -> do
      let maybeGen =
            fmap (case dir of
                    ToCpp -> conversionSpecHaskellToCppFn
                    FromCpp -> conversionSpecHaskellFromCppFn) $
            conversionSpecHaskell s
          throwForNoConversion =
            throwError $ concat
            ["No conversion defined for ", show s,
             case dir of
               ToCpp -> " to C++ from Haskell"
               FromCpp -> " from C++ to Haskell"]
      case maybeGen of
        Just (CustomConversion gen) -> do
          LH.addImports $ hsImport1 "Prelude" "(>>=)"
          LH.sayLn "("
          LH.indent gen
          LH.saysLn [") ", fromVar, " >>= \\", toVar, " ->"]
        Just BinaryCompatible -> LH.saysLn ["let ", toVar, " = ", fromVar, " in"]
        Just ConversionUnsupported -> throwForNoConversion
        Nothing -> throwForNoConversion

  where noConversion = LH.saysLn ["let ", toVar, " = ", fromVar, " in"]

-- | Note that the 'CallDirection' is the direction of the call, not the
-- direction of the return.  'ToCpp' means we're returning to the foreign
-- language, 'FromCpp' means we're returning from it.
sayHsCallAndProcessReturn :: CallDirection -> Type -> [String] -> LH.Generator ()
sayHsCallAndProcessReturn dir t callWords =
  LH.withErrorContext ("processing return value of type " ++ show t) $
  case t of
    Internal_TVoid -> sayCall
    -- The same as TPtr (TConst (TObj _)), but nonconst.
    Internal_TPtr (Internal_TObj cls) -> do
      case dir of
        ToCpp -> do
          LH.addImports hsImportForPrelude
          ctorName <- Class.toHsDataCtorName LH.Unmanaged Nonconst cls
          LH.saysLn ["HoppyP.fmap ", ctorName]
          sayCall
        FromCpp -> do
          LH.addImports $ mconcat [hsImportForPrelude, hsImportForRuntime]
          LH.sayLn "HoppyP.fmap HoppyFHR.toPtr"
          sayCall
    -- The same as TPtr (TConst (TObj _)), but nonconst.
    Internal_TPtr (Internal_TConst (Internal_TObj cls)) -> case dir of
      ToCpp -> do
        LH.addImports hsImportForPrelude
        ctorName <- Class.toHsDataCtorName LH.Unmanaged Const cls
        LH.saysLn ["HoppyP.fmap ", ctorName]
        sayCall
      FromCpp -> do
        LH.addImports $ mconcat [hsImportForPrelude, hsImportForRuntime]
        LH.sayLn "HoppyP.fmap HoppyFHR.toPtr"
        sayCall
    Internal_TPtr _ -> sayCall
    Internal_TRef t' -> sayHsCallAndProcessReturn dir (ptrT t') callWords
    Internal_TFn {} -> throwError "TFn unimplemented"
    Internal_TObj cls -> case dir of
      ToCpp -> case Class.classHaskellConversionFromCppFn $ LH.getClassHaskellConversion cls of
        Just _ -> do
          LH.addImports $ mconcat [hsImports "Prelude" ["(.)", "(=<<)"],
                                   hsImportForRuntime]
          ctorName <- Class.toHsDataCtorName LH.Unmanaged Const cls
          LH.saysLn ["(HoppyFHR.decodeAndDelete . ", ctorName, ") =<<"]
          sayCall
        Nothing ->
          throwError $ concat
          ["Can't return a TObj of ", show cls,
           " from C++ to Haskell because no class decode conversion is defined"]
      FromCpp -> do
        LH.addImports $ mconcat [hsImports "Prelude" ["(.)", "(=<<)"],
                                 hsImportForPrelude,
                                 hsImportForRuntime]
        LH.sayLn "(HoppyP.fmap (HoppyFHR.toPtr) . HoppyFHR.encode) =<<"
        sayCall
    Internal_TObjToHeap cls -> case dir of
      ToCpp -> sayHsCallAndProcessReturn dir (ptrT $ objT cls) callWords
      FromCpp -> throwError $ objToHeapTWrongDirectionErrorMsg Nothing cls
    Internal_TToGc t' -> case dir of
      ToCpp -> do
        LH.addImports $ mconcat [hsImport1 "Prelude" "(=<<)",
                                 hsImportForRuntime]
        LH.sayLn "HoppyFHR.toGc =<<"
        -- TToGc (TObj _) should create a pointer rather than decoding, so we
        -- change the TObj _ into a TPtr (TObj _).
        case t' of
          Internal_TObj _ -> sayHsCallAndProcessReturn dir (ptrT t') callWords
          _ -> sayHsCallAndProcessReturn dir t' callWords
      FromCpp -> throwError $ toGcTWrongDirectionErrorMsg Nothing t'
    Internal_TConst t' -> sayHsCallAndProcessReturn dir t' callWords

    Internal_TManual s -> do
      -- Remember 'dir' is backward here, because we're dealing with a return
      -- value, so these functions look backward.
      let maybeGen =
            fmap (case dir of
                    ToCpp -> conversionSpecHaskellFromCppFn
                    FromCpp -> conversionSpecHaskellToCppFn) $
            conversionSpecHaskell s
          throwForNoConversion =
            throwError $ concat
            ["No conversion defined for ", show s,
             case dir of
               ToCpp -> " from C++ to Haskell"
               FromCpp -> " to C++ from Haskell"]
      case maybeGen of
        Just (CustomConversion gen) -> do
          LH.addImports $ hsImport1 "Prelude" "(=<<)"
          LH.sayLn "("
          LH.indent gen
          LH.sayLn ") =<<"
        Just BinaryCompatible -> return ()
        Just ConversionUnsupported -> throwForNoConversion
        Nothing -> throwForNoConversion
      sayCall

  where sayCall = LH.saysLn $ "(" : callWords ++ [")"]

-- | The Haskell type of a 'Function', as computed by 'fnToHsTypeAndUse'.  This
-- combines a 'HsQualType' with a list of parameter names.
data FnHsType = FnHsType
  { fnHsQualType :: HsQualType
  , fnHsParamNameMaybes :: [Maybe String]
  }

-- | Implements special logic on top of 'LH.cppTypeToHsTypeAndUse', that
-- computes the Haskell __qualified__ type for a function, including typeclass
-- constraints, and bundles it with parameter names.
fnToHsTypeAndUse ::
     LH.HsTypeSide
  -> Purity
  -> [Parameter]
  -> Type
  -> ExceptionHandlers
     -- ^ These should be the effective exception handlers for the function, as
     -- returned by
     -- @'LH.getEffectiveExceptionHandlers' . 'fnExceptionHandlers'@,
     -- not just the function's exception handlers directly from
     -- @fnExceptionHandlers@.
  -> LH.Generator FnHsType
fnToHsTypeAndUse side purity params returnType exceptionHandlers = do
  let catches = not $ null $ exceptionHandlersList exceptionHandlers
      getsExcParams = catches && side == LH.HsCSide

      paramTypes =
        (if getsExcParams then (++ [ptrT intT, ptrT $ ptrT voidT]) else id) $
        map parameterType params

      paramNameMaybes =
        (if getsExcParams then (++ [Just "excId", Just "excPtr"]) else id) $
        map parameterName params

      defaultParamNames = map LH.toArgName [1..]

      defaultedParamNames = zipWith fromMaybe defaultParamNames paramNameMaybes

  paramQualTypes <- mapM contextForParam $ zip defaultedParamNames paramTypes
  let context = concatMap (\(HsQualType ctx _) -> ctx) paramQualTypes :: HsContext
      hsParams = map (\(HsQualType _ t) -> t) paramQualTypes

  -- Determine the 'HsHsSide' return type for the function.  Do the conversion
  -- to a Haskell type, and wrap the result in 'IO' if the function is impure.
  -- (HsCSide types always get wrapped in IO.)
  hsReturnInitial <- LH.cppTypeToHsTypeAndUse side returnType
  hsReturnForPurity <- case (purity, side) of
    (Pure, LH.HsHsSide) -> return hsReturnInitial
    _ -> do
      LH.addImports hsImportForPrelude
      return $ HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") hsReturnInitial

  return FnHsType
    { fnHsQualType = HsQualType context $ foldr HsTyFun hsReturnForPurity hsParams
    , fnHsParamNameMaybes = paramNameMaybes
    }

  where contextForParam :: (String, Type) -> LH.Generator HsQualType
        contextForParam (s, t) = case t of
          Internal_TPtr (Internal_TObj cls) -> receivePtr s cls Nonconst
          Internal_TPtr (Internal_TConst (Internal_TObj cls)) -> receiveValue s t cls
          Internal_TRef (Internal_TObj cls) -> receivePtr s cls Nonconst
          Internal_TRef (Internal_TConst (Internal_TObj cls)) -> receiveValue s t cls
          Internal_TObj cls -> receiveValue s t cls
          Internal_TManual spec ->
            -- We add a typeclass constraint iff we're generating an exposed
            -- Haskell function (HsHsSide) and there is a constraint declared.
            -- If we're generating the underlying C FFI function, or if there is
            -- no constraint declared, then don't add one.
            case (side, conversionSpecHaskell spec >>= conversionSpecHaskellHsArgType) of
              (LH.HsHsSide, Just f) -> f $ HsIdent s
              _ -> handoff side t
          Internal_TConst t' -> contextForParam (s, t')
          _ -> handoff side t

        -- Use whatever type 'cppTypeToHsTypeAndUse' suggests, with no typeclass
        -- constraints.
        handoff :: LH.HsTypeSide -> Type -> LH.Generator HsQualType
        handoff side' t = HsQualType [] <$> LH.cppTypeToHsTypeAndUse side' t

        -- Receives a @FooPtr this => this@.
        receivePtr :: String -> Class.Class -> Constness -> LH.Generator HsQualType
        receivePtr s cls cst = case side of
          LH.HsHsSide -> do
            ptrClassName <- Class.toHsPtrClassName cst cls
            let t' = HsTyVar $ HsIdent s
            return $ HsQualType [(UnQual $ HsIdent ptrClassName, [t'])] t'
          LH.HsCSide -> do
            LH.addImports hsImportForForeign
            typeName <- Class.toHsDataTypeName cst cls
            return $
              HsQualType [] $
              HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.Ptr")
                      (HsTyVar $ HsIdent typeName)

        -- Receives a @FooValue a => a@.
        receiveValue :: String -> Type -> Class.Class -> LH.Generator HsQualType
        receiveValue s t cls = case side of
          LH.HsCSide -> handoff side t
          LH.HsHsSide -> do
            LH.addImports hsImportForRuntime
            valueClassName <- Class.toHsValueClassName cls
            let t' = HsTyVar $ HsIdent s
            return $ HsQualType [(UnQual $ HsIdent valueClassName, [t'])] t'

-- | Renders a 'FnHsType' as a Haskell type, ignoring parameter names.  This
-- implementation uses haskell-src.
renderFnHsType :: FnHsType -> String
renderFnHsType = LH.prettyPrint . fnHsQualType

-- | Renders a 'FnHsType' as a Haskell type, including Haddock for parameter
-- names.
--
-- Unfortunately, we have to implement this ourselves, because haskell-src
-- doesn't support comments, and haskell-src-exts's comments implementation
-- relies on using specific source spans, and we don't want all that complexity
-- here.  So instead we render it ourselves, inserting "{- ^ ... -}" tags where
-- appropriate.
renderFnHsTypeWithNames :: FnHsType -> String
renderFnHsTypeWithNames fnHsType =
  concat $ renderedContextStrs ++ renderedParamStrs

  where HsQualType assts unqualType = fnHsQualType fnHsType
        paramNameMaybes = fnHsParamNameMaybes fnHsType

        renderedContextStrs :: [String]
        renderedContextStrs =
          if null assts
          then []
          else "(" : intersperse ", " (map renderAsst assts) ++ [") => "]

        renderAsst :: (HsQName, [HsType]) -> String
        renderAsst asst = case asst of
          (UnQual (HsIdent typeclass), [HsTyVar (HsIdent typeVar)]) ->
            concat [typeclass, " ", typeVar]
          _ -> error $ "renderAsst: Unexpected argument: " ++ show asst

        renderedParamStrs :: [String]
        renderedParamStrs = renderParams unqualType paramNameMaybes

        renderParams :: HsType -> [Maybe String] -> [String]
        renderParams fnType' paramNameMaybes' = case (fnType', paramNameMaybes') of
          -- If there's a parameter name, then generate a Haddock comment
          -- showing the name.
          (HsTyFun a b, (Just name):restNames) ->
            "(" : LH.prettyPrint a : ") {- ^ " : name : " -} -> " : renderParams b restNames

          -- If there's no parameter name, then don't generate any documentation
          -- for it, but continue to recur in case there are other parameters
          -- with names.
          (HsTyFun a b, Nothing:restNames) ->
            "(" : LH.prettyPrint a : ") -> " : renderParams b restNames

          -- If we've reached the end of the TyFun chain, then we don't need to
          -- recur further.  We can use 'prettyPrint' to render the rest.
          _ -> "(" : LH.prettyPrint fnType' : [")"]
