-- This file is part of Hoppy.
--
-- Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
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

{-# LANGUAGE CPP, ViewPatterns #-}

-- | Internal portion of the C++ code generator.
module Foreign.Hoppy.Generator.Language.Cpp.Internal (
  Generation,
  generate,
  generatedFiles,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (liftM, unless, when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Writer (WriterT, execWriterT, runWriterT, tell)
import Control.Monad.Trans (lift)
import Data.Foldable (forM_)
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$))
#endif
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mappend, mconcat, mempty)
#endif
import qualified Data.Set as S
import Foreign.Hoppy.Generator.Common
import Foreign.Hoppy.Generator.Language.Cpp
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Types

data CoderDirection = DoDecode | DoEncode
                    deriving (Eq, Show)

type Generator = ReaderT Env (WriterT [Chunk] (WriterT (S.Set Include) (Either ErrorMsg)))

data Env = Env
  { envInterface :: Interface
  , envModule :: Module
  }

addIncludes :: [Include] -> Generator ()
addIncludes = lift . lift . tell . S.fromList

addInclude :: Include -> Generator ()
addInclude = addIncludes . (:[])

-- Have to call this addReqsM, addReqs is taken by HasReqs.
addReqsM :: Reqs -> Generator ()
addReqsM = lift . lift . tell . reqsIncludes

askInterface :: MonadReader Env m => m Interface
askInterface = liftM envInterface ask

askModule :: MonadReader Env m => m Module
askModule = liftM envModule ask

-- | Halts generation and returns the given error message.
abort :: ErrorMsg -> Generator a
abort = lift . lift . lift . Left

execGenerator :: Interface -> Module -> Maybe String -> Generator a -> Either ErrorMsg String
execGenerator interface m maybeHeaderGuardName action = do
  (contents, includes) <-
    (runWriterT $
     -- WriterT (S.Set Include) (Either String) String:
     execChunkWriterT $
     -- WriterT [Chunk] (WriterT (S.Set Include) (Either String)) a:
     runReaderT action $ Env interface m)
    :: Either String (String, S.Set Include)
  return $ execChunkWriter $ do
    say "////////// GENERATED FILE, EDITS WILL BE LOST //////////\n"
    forM_ maybeHeaderGuardName $ \x -> do
      says ["\n#ifndef ", x, "\n"]
      says ["#define ", x, "\n"]
    unless (S.null includes) $ do
      say "\n"
      forM_ includes $ say . includeToString
    say "\nextern \"C\" {\n"
    say contents
    say "\n}  // extern \"C\"\n"
    forM_ maybeHeaderGuardName $ \x ->
      says ["\n#endif  // ifndef ", x, "\n"]

sayFunction :: String -> [String] -> Type -> Maybe (Generator ()) -> Generator ()
sayFunction name paramNames t maybeBody = do
  case t of
    Internal_TFn {} -> return ()
    _ -> abort $ concat ["sayFunction: A function type is required, given ", show t, "."]
  say "\n"  -- New top-level structure, leave a blank line.
  sayVar name (Just paramNames) t
  case maybeBody of
    Nothing -> say ";\n"
    Just body -> do
      say " {\n"
      body  -- TODO Indent.
      say "}\n"

-- | The in-memory result of generating C++ code for an interface.
data Generation = Generation
  { generatedFiles :: M.Map FilePath String
    -- ^ A map from paths of generated files to the contents of those files.
    -- The file paths are relative paths below the C++ generation root.
  }

-- | Runs the C++ code generator against an interface.
generate :: Interface -> Either ErrorMsg Generation
generate interface =
  fmap (Generation . M.fromList) $
  execWriterT $
  forM_ (M.elems $ interfaceModules interface) $ \m -> do
    let headerGuard = concat ["HOPPY_MODULE_", interfaceName interface, "_", moduleName m]
    header <- lift $ execGenerator interface m (Just headerGuard) sayModuleHeader
    tell [(moduleHppPath m, header)]
    source <- lift $ execGenerator interface m Nothing sayModuleSource
    tell [(moduleCppPath m, source)]

sayModuleHeader :: Generator ()
sayModuleHeader = do
  m <- askModule
  addReqsM $ moduleReqs m
  mapM_ (sayExport False) $ M.elems $ moduleExports m

  iface <- askInterface
  when (interfaceExceptionSupportModule iface == Just m) $
    sayExceptionSupport False

sayModuleSource :: Generator ()
sayModuleSource = do
  m <- askModule
  addInclude $ includeLocal $ moduleHppPath m
  mapM_ (sayExport True) $ M.elems $ moduleExports m

  iface <- askInterface
  when (interfaceExceptionSupportModule iface == Just m) $
    sayExceptionSupport True

sayExport :: Bool -> Export -> Generator ()
sayExport sayBody export = case export of
  ExportVariable v -> when sayBody $ sayExportVariable v

  -- Nothing to do C++ side for an enum or bitspace.
  ExportEnum _ -> return ()
  ExportBitspace _ -> return ()

  ExportFn fn ->
    -- Export a single function.
    when sayBody $ do
      addReqsM $ fnReqs fn
      sayExportFn (fnExtName fn)
                  (case fnCName fn of
                     FnName identifier -> CallFn $ sayIdentifier identifier
                     FnOp op -> CallOp op)
                  Nothing
                  (fnParams fn)
                  (fnReturn fn)
                  (fnExceptionHandlers fn)
                  sayBody

  ExportClass cls -> when sayBody $ do
    let clsPtr = ptrT $ objT cls
        constClsPtr = ptrT $ constT $ objT cls
    -- TODO Is this redundant for a completely empty class?  (No ctors or methods, private dtor.)
    addReqsM $ classReqs cls  -- This is needed at least for the delete function.

    -- Export each of the class's constructors.
    forM_ (classCtors cls) $ \ctor ->
      sayExportFn (classEntityExtName cls ctor)
                  (CallFn $ say "new" >> sayIdentifier (classIdentifier cls))
                  Nothing
                  (ctorParams ctor)
                  clsPtr
                  (ctorExceptionHandlers ctor)
                  sayBody

    -- Export a delete function for the class.
    when (classDtorIsPublic cls) $
      sayFunction (classDeleteFnCppName cls)
                  ["self"]
                  (fnT [constClsPtr] voidT) $
        Just $ say "delete self;\n"

    -- Export each of the class's variables.
    forM_ (classVariables cls) $ sayExportClassVariable cls

    -- Export each of the class's methods.
    forM_ (classMethods cls) $ \method -> do
      let static = case methodStatic method of
            Static -> True
            Nonstatic -> False
          thisType = case methodConst method of
            Const -> constClsPtr
            Nonconst -> clsPtr
          nonMemberCall = static || case methodImpl method of
            RealMethod {} -> False
            FnMethod {} -> True
      sayExportFn (classEntityExtName cls method)
                  (case methodImpl method of
                     RealMethod name -> case name of
                       FnName cName -> CallFn $ do
                         when static $ do
                           sayIdentifier (classIdentifier cls)
                           say "::"
                         say cName
                       FnOp op -> CallOp op
                     FnMethod name -> case name of
                       FnName cName -> CallFn $ sayIdentifier cName
                       FnOp op -> CallOp op)
                  (if nonMemberCall then Nothing else Just thisType)
                  (methodParams method)
                  (methodReturn method)
                  (methodExceptionHandlers method)
                  sayBody

    -- Export upcast functions for the class to its direct superclasses.
    forM_ (classSuperclasses cls) $ genUpcastFns cls
    -- Export downcast functions from the class's direct and indirect
    -- superclasses to it.
    unless (classIsSubclassOfMonomorphic cls) $
      forM_ (classSuperclasses cls) $ genDowncastFns cls

  ExportCallback cb -> sayExportCallback sayBody cb

  where genUpcastFns :: Class -> Class -> Generator ()
        genUpcastFns cls ancestorCls = do
          sayFunction (classCastFnCppName cls ancestorCls)
                      ["self"]
                      (fnT [ptrT $ constT $ objT cls] $ ptrT $ constT $ objT ancestorCls)
                      (Just $ say "return self;\n")
          forM_ (classSuperclasses ancestorCls) $ genUpcastFns cls

        genDowncastFns :: Class -> Class -> Generator ()
        genDowncastFns cls ancestorCls = unless (classIsMonomorphicSuperclass ancestorCls) $ do
          let clsPtr = ptrT $ constT $ objT cls
              ancestorPtr = ptrT $ constT $ objT ancestorCls
          sayFunction (classCastFnCppName ancestorCls cls)
                      ["self"]
                      (fnT [ancestorPtr] clsPtr) $ Just $ do
            say "return dynamic_cast<"
            sayType Nothing clsPtr
            say ">(self);\n"
          forM_ (classSuperclasses ancestorCls) $ genDowncastFns cls

sayExportVariable :: Variable -> Generator ()
sayExportVariable v =
  sayExportVariable' (varType v)
                     Nothing
                     True
                     (varGetterExtName v)
                     (varSetterExtName v)
                     (sayIdentifier $ varIdentifier v)

sayExportClassVariable :: Class -> ClassVariable -> Generator ()
sayExportClassVariable cls v =
  sayExportVariable' (classVarType v)
                     (case classVarStatic v of
                        Nonstatic -> Just (ptrT $ constT $ objT cls, ptrT $ objT cls)
                        Static -> Nothing)
                     (classVarGettable v)
                     (classVarGetterExtName cls v)
                     (classVarSetterExtName cls v)
                     (case classVarStatic v of
                        Nonstatic -> say $ classVarCName v
                        Static -> do sayIdentifier $ classIdentifier cls
                                     says ["::", classVarCName v])

sayExportVariable' :: Type
                   -> Maybe (Type, Type)
                   -> Bool
                   -> ExtName
                   -> ExtName
                   -> Generator ()
                   -> Generator ()
sayExportVariable' t maybeThisTypes gettable getterName setterName sayVarName = do
  let (isConst, deconstType) = case t of
        Internal_TConst t -> (True, t)
        t -> (False, t)

  -- Say a getter function.
  when gettable $
    sayExportFn getterName
                (VarRead sayVarName)
                (fmap fst maybeThisTypes)
                []
                deconstType
                mempty
                True

  -- Say a setter function.
  unless isConst $
    sayExportFn setterName
                (VarWrite sayVarName)
                (fmap snd maybeThisTypes)
                [deconstType]
                voidT
                mempty
                True

data CallType =
    CallOp Operator
  | CallFn (Generator ())
  | VarRead (Generator ())
  | VarWrite (Generator ())

sayExportFn :: ExtName
            -> CallType
            -> Maybe Type
            -> [Type]
            -> Type
            -> ExceptionHandlers
            -> Bool
            -> Generator ()
sayExportFn extName callType maybeThisType paramTypes retType exceptionHandlers sayBody = do
  handlerList <- exceptionHandlersList <$> getEffectiveExceptionHandlers exceptionHandlers
  let catches = not $ null handlerList
      addExceptionParamNames =
        if catches then (++ [exceptionIdArgName, exceptionPtrArgName]) else id
      addExceptionParamTypes = if catches then (++ [ptrT intT, ptrT $ ptrT voidT]) else id

      paramCount = length paramTypes
      paramCTypeMaybes = map typeToCType paramTypes
      paramCTypes = zipWith fromMaybe paramTypes paramCTypeMaybes
      retCTypeMaybe = typeToCType retType
      retCType = fromMaybe retType retCTypeMaybe

  addReqsM . mconcat =<< mapM typeReqs (retType:paramTypes)

  sayFunction (externalNameToCpp extName)
              (maybe id (const ("self":)) maybeThisType $
               addExceptionParamNames $
               zipWith3 (\t ctm -> case t of
                           Internal_TCallback {} -> toArgNameAlt
                           _ -> if isJust ctm then toArgNameAlt else toArgName)
               paramTypes paramCTypeMaybes [1..paramCount])
              (fnT (addExceptionParamTypes $ maybe id (:) maybeThisType paramCTypes)
                   retCType) $
    if not sayBody
    then Nothing
    else Just $ do
      when catches $ do
        say "try {\n"
        says ["*", exceptionIdArgName, " = 0;\n"]

      -- Convert arguments that aren't passed in directly.
      mapM_ (sayArgRead DoDecode) $ zip3 [1..] paramTypes paramCTypeMaybes

      let -- Determines how to call the exported function or method.
          sayCall = case callType of
            CallOp op -> do
              say "("
              let effectiveParamCount = paramCount + if isJust maybeThisType then 1 else 0
                  paramNames@(p1:p2:_) = (if isJust maybeThisType then ("(*self)":) else id) $
                                         map toArgName [1..]
                  assertParamCount n =
                    when (effectiveParamCount /= n) $ abort $ concat
                    ["sayExportFn: Operator ", show op, " for export ", show extName,
                     " requires ", show n, " parameter(s), but has ", show effectiveParamCount,
                     "."]
              case operatorType op of
                UnaryPrefixOperator symbol -> assertParamCount 1 >> says [symbol, p1]
                UnaryPostfixOperator symbol -> assertParamCount 1 >> says [p1, symbol]
                BinaryOperator symbol -> assertParamCount 2 >> says [p1, symbol, p2]
                CallOperator ->
                  says $ p1 : "(" : take (effectiveParamCount - 1) (drop 1 paramNames) ++ [")"]
                ArrayOperator -> assertParamCount 2 >> says [p1, "[", p2, "]"]
              say ")"
            CallFn sayCppName -> do
              when (isJust maybeThisType) $ say "self->"
              sayCppName
              say "("
              sayArgNames paramCount
              say ")"
            VarRead sayVarName -> do
              when (isJust maybeThisType) $ say "self->"
              sayVarName
            VarWrite sayVarName -> do
              when (isJust maybeThisType) $ say "self->"
              sayVarName
              says [" = ", toArgName 1]

          -- Writes the call, transforming the return value if necessary.
          -- These translations should be kept in sync with typeToCType.
          sayCallAndReturn retType' retCTypeMaybe' = case (retType', retCTypeMaybe') of
            (Internal_TVoid, Nothing) -> sayCall >> say ";\n"
            (_, Nothing) -> say "return " >> sayCall >> say ";\n"
            (Internal_TBitspace b, Just _) -> do
              addReqsM $ bitspaceReqs b
              let convFn = bitspaceFromCppValueFn b
              say "return "
              forM_ convFn $ \f -> says [f, "("]
              sayCall
              when (isJust convFn) $ say ")"
              say ";\n";
            (Internal_TRef cls, Just (Internal_TPtr cls')) | cls == cls' ->
              say "return &(" >> sayCall >> say ");\n"
            (Internal_TObj cls,
             Just (Internal_TPtr (Internal_TConst (Internal_TObj cls')))) | cls == cls' ->
              sayReturnNew cls sayCall
            (Internal_TObjToHeap cls, Just (Internal_TPtr (Internal_TObj cls'))) | cls == cls' ->
              sayReturnNew cls sayCall
            (Internal_TToGc (Internal_TObj cls),
             Just (Internal_TPtr (Internal_TObj cls'))) | cls == cls' ->
              sayReturnNew cls sayCall
            (Internal_TToGc retType'', _) -> sayCallAndReturn retType'' retCTypeMaybe'
            ts -> abort $ concat ["sayExportFn: Unexpected return types ", show ts,
                                  "while generating binding for ", show extName, "."]

      sayCallAndReturn retType retCTypeMaybe

      when catches $ do
        iface <- askInterface

        forM_ handlerList $ \handler -> do
          say "} catch ("
          case handler of
            CatchClass cls -> sayVar exceptionVarName Nothing $ refT $ constT $ objT cls
            CatchAll -> say "..."
          say ") {\n"

          exceptionId <- case handler of
            CatchClass cls -> case interfaceExceptionClassId iface cls of
              Just exceptionId -> return exceptionId
              Nothing -> abort $ concat
                         ["sayExportFn: Trying to catch non-exception class ", show cls,
                          " while generating binding for ", show extName, "."]
            CatchAll -> return exceptionCatchAllId
          says ["*", exceptionIdArgName, " = ", show $ getExceptionId exceptionId, ";\n"]

          case handler of
            CatchAll -> says ["*", exceptionPtrArgName, " = 0;\n"]
            CatchClass cls -> do
              -- Object pointers don't convert automatically to void*.
              says ["*", exceptionPtrArgName, " = reinterpret_cast<void*>(new "]
              sayType Nothing $ objT cls
              says ["(", exceptionVarName, "));\n"]

          -- For all of the types our gateway functions actually return, "return
          -- 0" is a valid statement.
          when (retType /= Internal_TVoid) $ say "return 0;\n"

        say "}\n"

  where sayReturnNew cls sayCall =
          say "return new" >> sayIdentifier (classIdentifier cls) >> say "(" >>
          sayCall >> say ");\n"

-- | If @dir@ is 'DoDecode', then we are a C++ function reading an argument from
-- foreign code.  If @dir@ is 'DoEncode', then we are invoking a foreign
-- callback.
sayArgRead :: CoderDirection -> (Int, Type, Maybe Type) -> Generator ()
sayArgRead dir (n, stripConst . normalizeType -> cppType, maybeCType) = case cppType of
  Internal_TBitspace b -> case maybeCType of
    Just cType -> do
      let cppTypeId = fromMaybe (error $ concat
                                 ["sayArgRead: Expected ", show b,
                                  " to have a C++ type, but it doesn't."]) $
                      bitspaceCppTypeIdentifier b
      addReqsM $ bitspaceReqs b
      case dir of
        -- Convert from cType to cppType.
        DoDecode -> do
          sayIdentifier cppTypeId
          says [" ", toArgName n, " = ", fromMaybe "" $ bitspaceToCppValueFn b,
                "(", toArgNameAlt n, ");\n"]
        -- Convert from cppType to cType.
        DoEncode -> do
          sayVar (toArgName n) Nothing cType
          says [" = ", fromMaybe "" $ bitspaceFromCppValueFn b,
                "(", toArgNameAlt n, ");\n"]
    Nothing ->
      return ()

  Internal_TCallback cb -> do
    case dir of
      DoDecode -> return ()
      DoEncode -> abort $ concat
                  ["sayArgRead: Encoding of callbacks is not supported.  Given ",
                   show cb, "."]
    says [callbackClassName cb, " ", toArgName n, "(", toArgNameAlt n, ");\n"]

  t@(Internal_TPtr (Internal_TFn paramTypes retType)) -> do
    -- Assert that all types referred to in a function pointer type are all
    -- representable as C types.
    let check label t' = (label ++ " " ++ show t') <$ typeToCType t'
        mismatches = catMaybes $
                     check "return type" retType :
                     map (\paramType -> check "parameter" paramType)
                         paramTypes
    unless (null mismatches) $
      abort $ concat $
      "sayArgRead: Some types within a function pointer type use non-C types, " :
      "but only C types may be used.  The unsupported types are: " :
      intersperse "; " mismatches ++ [".  The whole function type is ", show t, "."]

    convertDefault

  Internal_TRef t -> convertObj t

  Internal_TObj _ -> convertObj $ constT cppType

  Internal_TObjToHeap cls -> case dir of
    DoDecode -> error $ objToHeapTWrongDirectionErrorMsg (Just "sayArgRead") cls
    DoEncode -> do
      sayIdentifier $ classIdentifier cls
      says ["* ", toArgName n, " = new "]
      sayIdentifier $ classIdentifier cls
      says ["(", toArgNameAlt n, ");\n"]

  Internal_TToGc t' -> case dir of
    DoDecode -> error $ toGcTWrongDirectionErrorMsg (Just "sayArgRead") t'
    DoEncode -> do
      let newCppType = case t' of
            -- In the case of (TToGc (TObj _)), we copy the temporary object to
            -- the heap and let the foreign language manage that value.
            Internal_TObj cls -> objToHeapT cls
            _ -> t'
      sayArgRead dir (n, newCppType, typeToCType newCppType)

  _ -> convertDefault

  where -- Primitive types don't need to be encoded/decoded.  But if maybeCType is a
        -- Just, then we're expected to do some encoding/decoding, so something is
        -- wrong.
        --
        -- TODO Do we need to handle TConst?
        convertDefault = forM_ maybeCType $ \cType ->
          abort $ concat
          ["sayArgRead: Don't know how to ", show dir, " between C-type ", show cType,
           " and C++-type ", show cppType, "."]

        convertObj cppType' = case dir of
          DoDecode -> do
            sayVar (toArgName n) Nothing $ refT cppType'
            says [" = *", toArgNameAlt n, ";\n"]
          DoEncode -> do
            sayVar (toArgName n) Nothing $ ptrT cppType'
            says [" = &", toArgNameAlt n, ";\n"]

sayArgNames :: Int -> Generator ()
sayArgNames count =
  says $ intersperse ", " $ map toArgName [1..count]

sayExportCallback :: Bool -> Callback -> Generator ()
sayExportCallback sayBody cb = do
  throws <- getEffectiveCallbackThrows cb

  let className = callbackClassName cb
      implClassName = callbackImplClassName cb
      fnName = callbackFnName cb
      paramTypes = callbackParams cb
      paramCount = length paramTypes
      retType = callbackReturn cb
      cbType = callbackT cb
      fnType = fnT paramTypes retType

  -- The function pointer we receive from foreign code will work with C-types,
  -- so determine what that function looks like.
  let paramCTypes = zipWith fromMaybe paramTypes $ map typeToCType paramTypes
      retCType = fromMaybe retType $ typeToCType retType

  -- Add requirements specified manually by the callback, and for its parameter
  -- and return types.
  addReqsM . mconcat . (callbackReqs cb:) =<< mapM typeReqs (retType:paramTypes)

  let fnCType = fnT ((if throws then (++ [ptrT intT, ptrT $ ptrT voidT]) else id)
                     paramCTypes)
                    retCType
      fnPtrCType = ptrT fnCType

  if not sayBody
    then do
      -- Render the class declarations into the header file.
      (sharedPtrReqs, sharedPtrStr) <- interfaceSharedPtr <$> askInterface
      addReqsM sharedPtrReqs

      says ["\nclass ", implClassName, " {\n"]
      say "public:\n"
      says ["    explicit ", implClassName, "("] >> sayType Nothing fnPtrCType >>
        say ", void(*)(void(*)()), bool);\n"
      says ["    ~", implClassName, "();\n"]
      say "    " >> sayVar "operator()" Nothing fnType >> say ";\n"
      say "private:\n"
      says ["    ", implClassName, "(const ", implClassName, "&);\n"]
      says ["    ", implClassName, "& operator=(const ", implClassName, "&);\n"]
      say "\n"
      say "    " >> sayVar "f_" Nothing (constT fnPtrCType) >> say ";\n"
      say "    void (*const release_)(void(*)());\n"
      say "    const bool releaseRelease_;\n"
      say "};\n"

      says ["\nclass ", className, " {\n"]
      say "public:\n"
      says ["    ", className, "() {}\n"]
      says ["    explicit ", className, "(", implClassName, "* impl) : impl_(impl) {}\n"]
      say "    " >> sayVar "operator()" Nothing fnType >> say ";\n"
      say "    operator bool() const;\n"
      say "private:\n"
      says ["    ", sharedPtrStr, "<", implClassName, "> impl_;\n"]
      say "};\n"

    else do
      -- Render the classes' methods into the source file.  First render the
      -- impl class's constructor.
      says ["\n", implClassName, "::", implClassName, "("] >> sayVar "f" Nothing fnPtrCType >>
        say ", void (*release)(void(*)()), bool releaseRelease) :\n"
      say "    f_(f), release_(release), releaseRelease_(releaseRelease) {}\n"

      -- Then render the destructor.
      says ["\n", implClassName, "::~", implClassName, "() {\n"]
      say "    if (release_) {\n"
      say "        release_(reinterpret_cast<void(*)()>(f_));\n"
      say "        if (releaseRelease_) {\n"
      say "            release_(reinterpret_cast<void(*)()>(release_));\n"
      say "        }\n"
      say "    }\n"
      say "}\n"

      -- Render the impl operator() method, which does argument decoding and
      -- return value encoding and passes C++ values to underlying function
      -- poiner.
      --
      -- TODO Abstract the duplicated code here and in sayExportFn.
      let paramCTypeMaybes = map typeToCType paramTypes
          retCTypeMaybe = typeToCType retType

      sayFunction (implClassName ++ "::operator()")
                  (zipWith (\ctm -> if isJust ctm then toArgNameAlt else toArgName)
                   paramCTypeMaybes [1..paramCount])
                  fnType $ Just $ do
        -- Convert arguments that aren't passed in directly.
        mapM_ (sayArgRead DoEncode) $ zip3 [1..] paramTypes paramCTypeMaybes

        when throws $ do
          says ["int ", exceptionIdArgName, " = 0;\n"]
          says ["void *", exceptionPtrArgName, " = 0;\n"]

          -- Add an include for the exception support module to be able to call the
          -- C++ rethrow function.
          iface <- askInterface
          currentModule <- askModule
          case interfaceExceptionSupportModule iface of
            Just exceptionSupportModule ->
              when (exceptionSupportModule /= currentModule) $
                -- TODO Should this be includeStd?
                addReqsM $ reqInclude $ includeLocal $ moduleHppPath exceptionSupportModule
            Nothing -> abort $ "sayExportCallback: " ++ show iface ++
                       " uses exceptions, so it needs an exception support " ++
                       "module.  Please use interfaceSetExceptionSupportModule."

        -- Invoke the function pointer into foreign code.
        let -- | Generates the call to the foreign language function pointer.
            sayCall :: Generator ()
            sayCall = do
              say "f_("
              sayArgNames paramCount
              when throws $ do
                when (paramCount /= 0) $ say ", "
                says ["&", exceptionIdArgName, ", &", exceptionPtrArgName]
              say ")"

            -- | Generates code to check whether an exception was thrown by the
            -- callback, and rethrows it in C++ if so.
            sayExceptionCheck :: Generator ()
            sayExceptionCheck = when throws $ do
              says ["if (", exceptionIdArgName, " != 0) { ",
                    exceptionRethrowFnName, "(", exceptionIdArgName, ", ",
                    exceptionPtrArgName, "); }\n"]

        case (retType, retCTypeMaybe) of
          (Internal_TVoid, Nothing) -> do
            sayCall >> say ";\n"
            sayExceptionCheck
          (_, Nothing) -> do
            sayVar "result" Nothing retType >> say " = " >> sayCall >> say ";\n"
            sayExceptionCheck
            say "return result;\n"
          (Internal_TBitspace b, Just _) -> do
            addReqsM $ bitspaceReqs b
            let convFn = bitspaceToCppValueFn b
            sayVar "result" Nothing retType
            say " = "
            forM_ convFn $ \f -> says [f, "("]
            sayCall
            when (isJust convFn) $ say ")"
            say ";\n";
            sayExceptionCheck
            say "return result;\n"
          (Internal_TObj cls1, Just retCType@(Internal_TPtr (Internal_TConst (Internal_TObj cls2))))
            | cls1 == cls2 -> do
            sayVar "resultPtr" Nothing retCType >> say " = " >> sayCall >> say ";\n"
            sayExceptionCheck
            sayVar "result" Nothing retType >> say " = *resultPtr;\n"
            say "delete resultPtr;\n"
            say "return result;\n"
          (Internal_TRef (Internal_TConst (Internal_TObj cls1)),
           Just (Internal_TPtr (Internal_TConst (Internal_TObj cls2)))) | cls1 == cls2 -> do
            sayVar "resultPtr" Nothing retCType >> say " = " >> sayCall >> say ";\n"
            sayExceptionCheck
            say "return *resultPtr;\n"
          (Internal_TRef (Internal_TObj cls1),
           Just (Internal_TPtr (Internal_TObj cls2))) | cls1 == cls2 -> do
            sayVar "resultPtr" Nothing retCType >> say " = " >> sayCall >> say ";\n"
            sayExceptionCheck
            say "return *resultPtr;\n"
          ts -> abort $ concat
                ["sayExportCallback: Unexpected return types ", show ts, "."]

      -- Render the non-impl operator() method, which simply passes C++ values
      -- along to the impl object.
      sayFunction (className ++ "::operator()")
                  (map toArgName [1..paramCount])
                  fnType $ Just $ do
        case retType of
          Internal_TVoid -> say "(*impl_)("
          _ -> say "return (*impl_)("
        sayArgNames paramCount
        say ");\n"

      -- Render "operator bool", which detects whether the callback was not
      -- default-constructed with no actual impl object.
      says [className, "::operator bool() const {\n"]
      say "return static_cast<bool>(impl_);\n"
      say "}\n"

      -- Render the function that creates a new callback object.
      let newCallbackFnType = fnT [ fnPtrCType
                                  , ptrT (fnT [ptrT $ fnT [] voidT] voidT)
                                  , boolT
                                  ]
                              cbType
      sayFunction fnName ["f", "release", "releaseRelease"] newCallbackFnType $ Just $
        says ["return new ", implClassName, "(f, release, releaseRelease);\n"]

-- | Outputs interface-wide code needed to support exceptions.  Currently, this
-- comprises the function for rethrowing in C++ an exception transferred from
-- a foreign language.
sayExceptionSupport :: Bool -> Generator ()
sayExceptionSupport sayBody =
  sayFunction exceptionRethrowFnName
              ["excId", "voidPtr"]
              (fnT [intT, ptrT voidT] voidT) $
  if not sayBody
  then Nothing
  else Just $ do
    iface <- askInterface
    let excClasses = interfaceAllExceptionClasses iface

    says ["switch (excId) {\n"]

    forM_ excClasses $ \cls -> do
      excId <- fmap getExceptionId $
               fromMaybeM (abort $ "sayExceptionSupport: Internal error, " ++ show cls ++
                           "should have an exception ID, but doesn't.") $
               interfaceExceptionClassId iface cls
      says ["case ", show excId, ": {\n"]
      sayVar "excPtr" Nothing (ptrT $ objT cls) >> say " = reinterpret_cast<" >>
        sayType Nothing (ptrT $ objT cls) >> says [">(voidPtr);\n"]
      sayVar "exc" Nothing (objT cls) >> say " = *excPtr;\n"
      say "delete excPtr;\n"
      say "throw exc;\n"
      say "}\n"

    say "}\n"
    says ["throw \"Internal Hoppy error, ", exceptionRethrowFnName,
          " got an unknown exception ID.\";\n"]

-- | Returns a 'Type' iff there is a C type distinct from the given C++ type
-- that should be used for conversion.
typeToCType :: Type -> Maybe Type
typeToCType t = case t of
  -- Because we don't know (although we could...) the direction in which we're
  -- converting the bitspace value, when the bitspace has a C++ type we have to
  -- assume that it needs to be converted.  The caller will sort out whether a
  -- conversion is actually requested.
  Internal_TBitspace b -> case bitspaceCppTypeIdentifier b of
    Just _ -> Just $ bitspaceType b
    Nothing -> Nothing
  Internal_TRef t' -> Just $ ptrT t'
  Internal_TObj _ -> Just $ ptrT $ constT t
  Internal_TObjToHeap cls -> Just $ ptrT $ objT cls
  Internal_TToGc t'@(Internal_TObj _) -> Just $ ptrT t'
  Internal_TToGc t' -> typeToCType t'
  Internal_TConst t' -> typeToCType t'
  _ -> Nothing

typeReqs :: Type -> Generator Reqs
typeReqs t = case t of
  Internal_TVoid -> return mempty
  Internal_TBool -> return mempty
  Internal_TChar -> return mempty
  Internal_TUChar -> return mempty
  Internal_TShort -> return mempty
  Internal_TUShort -> return mempty
  Internal_TInt -> return mempty
  Internal_TUInt -> return mempty
  Internal_TLong -> return mempty
  Internal_TULong -> return mempty
  Internal_TLLong -> return mempty
  Internal_TULLong -> return mempty
  Internal_TFloat -> return mempty
  Internal_TDouble -> return mempty
  Internal_TInt8 -> return cstdintReqs
  Internal_TInt16 -> return cstdintReqs
  Internal_TInt32 -> return cstdintReqs
  Internal_TInt64 -> return cstdintReqs
  Internal_TWord8 -> return cstdintReqs
  Internal_TWord16 -> return cstdintReqs
  Internal_TWord32 -> return cstdintReqs
  Internal_TWord64 -> return cstdintReqs
  Internal_TPtrdiff -> return cstddefReqs
  Internal_TSize -> return cstddefReqs
  Internal_TSSize -> return cstddefReqs
  Internal_TEnum e -> return $ enumReqs e
  Internal_TBitspace b -> typeReqs $ bitspaceType b
  Internal_TPtr t' -> typeReqs t'
  Internal_TRef t' -> typeReqs t'
  Internal_TFn paramTypes retType ->
    -- TODO Is the right 'ReqsType' being used recursively here?
    mconcat <$> mapM typeReqs (retType:paramTypes)
  Internal_TCallback cb -> do
    -- TODO Should this be includeStd?
    cbClassReqs <- reqInclude . includeLocal . moduleHppPath <$>
                   findExportModule (callbackExtName cb)
    -- TODO Is the right 'ReqsType' being used recursively here?
    fnTypeReqs <- typeReqs =<< callbackToTFn cb
    return $ cbClassReqs `mappend` fnTypeReqs
  Internal_TObj cls -> return $ classReqs cls
  Internal_TObjToHeap cls -> return $ classReqs cls
  Internal_TToGc t' -> typeReqs t'
  Internal_TConst t' -> typeReqs t'

cstddefReqs :: Reqs
cstddefReqs = reqInclude $ includeStd "cstddef"

cstdintReqs :: Reqs
cstdintReqs = reqInclude $ includeStd "cstdint"

findExportModule :: ExtName -> Generator Module
findExportModule extName =
  fromMaybeM (abort $ concat
              ["findExportModule: Can't find module exporting ", fromExtName extName, "."]) =<<
  fmap (M.lookup extName . interfaceNamesToModules) askInterface

getEffectiveExceptionHandlers :: ExceptionHandlers -> Generator ExceptionHandlers
getEffectiveExceptionHandlers handlers = do
  ifaceHandlers <- interfaceExceptionHandlers <$> askInterface
  moduleHandlers <- getExceptionHandlers <$> askModule
  -- Exception handlers declared lower in the hierarchy take precedence over
  -- those higher in the hierarchy; ExceptionHandlers is a left-biased monoid.
  return $ mconcat [handlers, moduleHandlers, ifaceHandlers]

getEffectiveCallbackThrows :: Callback -> Generator Bool
getEffectiveCallbackThrows cb = case callbackThrows cb of
  Just b -> return b
  Nothing -> moduleCallbacksThrow <$> askModule >>= \case
    Just b -> return b
    Nothing -> interfaceCallbacksThrow <$> askInterface

-- | Constructs the function type for a callback.  A callback that throws has
-- additional parameters.
--
-- Keep this in sync with the Haskell generator's version.
callbackToTFn :: Callback -> Generator Type
callbackToTFn cb = do
  throws <- mayThrow
  return $ Internal_TFn ((if throws then addExcParams else id) $ callbackParams cb)
                        (callbackReturn cb)

  where mayThrow = case callbackThrows cb of
          Just t -> return t
          Nothing -> moduleCallbacksThrow <$> askModule >>= \mt -> case mt of
            Just t -> return t
            Nothing -> interfaceCallbacksThrow <$> askInterface

        addExcParams = (++ [ptrT intT, ptrT $ ptrT voidT])
