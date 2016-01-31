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
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mappend, mconcat, mempty)
#endif
import qualified Data.Set as S
import Foreign.Hoppy.Generator.Common
import Foreign.Hoppy.Generator.Language.Cpp
import Foreign.Hoppy.Generator.Spec

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
    TFn {} -> return ()
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

sayModuleSource :: Generator ()
sayModuleSource = do
  m <- askModule
  addInclude $ includeLocal $ moduleHppPath m
  mapM_ (sayExport True) $ M.elems $ moduleExports m

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
                  sayBody

  ExportClass cls -> when sayBody $ do
    let clsPtr = TPtr $ TObj cls
        justClsPtr = Just clsPtr
    -- TODO Is this redundant for a completely empty class?  (No ctors or methods, private dtor.)
    addReqsM $ classReqs cls  -- This is needed at least for the delete function.

    -- Export each of the class's constructors.
    forM_ (classCtors cls) $ \ctor ->
      sayExportFn (getClassyExtName cls ctor)
                  (CallFn $ say "new" >> sayIdentifier (classIdentifier cls))
                  Nothing
                  (ctorParams ctor)
                  clsPtr
                  sayBody

    -- Export a delete function for the class.
    when (classDtorIsPublic cls) $
      sayFunction (classDeleteFnCppName cls)
                  ["self"]
                  (TFn [TPtr $ TConst $ TObj cls] TVoid) $
        Just $ say "delete self;\n"

    -- Export each of the class's methods.
    forM_ (classMethods cls) $ \method -> do
      let nonMemberCall =
            methodStatic method == Static ||
            case methodImpl method of
              RealMethod {} -> False
              FnMethod {} -> True
      let static = methodStatic method == Static
      sayExportFn (getClassyExtName cls method)
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
                  (if nonMemberCall then Nothing else justClsPtr)
                  (methodParams method)
                  (methodReturn method)
                  sayBody

    -- Export upcast functions for the class to its direct superclasses.
    forM_ (classSuperclasses cls) $ genUpcastFns cls
    -- Export downcast functions from the class's direct and indirect
    -- superclasses to it.
    unless (classIsSubclassOfMonomorphic cls) $
      forM_ (classSuperclasses cls) $ genDowncastFns cls

  ExportCallback cb -> do
    -- Need <memory> for std::shared_ptr.
    addReqsM $ callbackReqs cb `mappend` reqInclude (includeStd "memory")
    sayExportCallback sayBody cb

  where genUpcastFns :: Class -> Class -> Generator ()
        genUpcastFns cls ancestorCls = do
          sayFunction (classCastFnCppName cls ancestorCls)
                      ["self"]
                      (TFn [TPtr $ TConst $ TObj cls] $ TPtr $ TConst $ TObj ancestorCls)
                      (Just $ say "return self;\n")
          forM_ (classSuperclasses ancestorCls) $ genUpcastFns cls

        genDowncastFns :: Class -> Class -> Generator ()
        genDowncastFns cls ancestorCls = unless (classIsMonomorphicSuperclass ancestorCls) $ do
          let clsPtr = TPtr $ TConst $ TObj cls
              ancestorPtr = TPtr $ TConst $ TObj ancestorCls
          sayFunction (classCastFnCppName ancestorCls cls)
                      ["self"]
                      (TFn [ancestorPtr] clsPtr) $ Just $ do
            say "return dynamic_cast<"
            sayType Nothing clsPtr
            say ">(self);\n"
          forM_ (classSuperclasses ancestorCls) $ genDowncastFns cls

sayExportVariable :: Variable -> Generator ()
sayExportVariable v = do
  let (isConst, deconstType) = case varType v of
        TConst t -> (True, t)
        t -> (False, t)

  -- Say a getter function.
  sayExportFn (varGetterExtName v)
              (VarRead $ varIdentifier v)
              Nothing
              []
              deconstType
              True

  -- Say a setter function.
  unless isConst $
    sayExportFn (varSetterExtName v)
                (VarWrite $ varIdentifier v)
                Nothing
                [deconstType]
                TVoid
                True

data CallType =
    CallOp Operator
  | CallFn (Generator ())
  | VarRead Identifier
  | VarWrite Identifier

sayExportFn :: ExtName
            -> CallType
            -> Maybe Type
            -> [Type]
            -> Type
            -> Bool
            -> Generator ()
sayExportFn extName callType maybeThisType paramTypes retType sayBody = do
  let paramCount = length paramTypes
      paramCTypeMaybes = map typeToCType paramTypes
      paramCTypes = zipWith fromMaybe paramTypes paramCTypeMaybes
      retCTypeMaybe = typeToCType retType
      retCType = fromMaybe retType retCTypeMaybe

  addReqsM . mconcat =<< mapM typeReqs (retType:paramTypes)

  sayFunction (externalNameToCpp extName)
              (maybe id (const ("self":)) maybeThisType $
               zipWith3 (\t ctm -> case t of
                           TCallback {} -> toArgNameAlt
                           _ -> if isJust ctm then toArgNameAlt else toArgName)
               paramTypes paramCTypeMaybes [1..paramCount])
              (TFn (maybe id (:) maybeThisType paramCTypes) retCType) $
    if not sayBody
    then Nothing
    else Just $ do
      -- Convert arguments that aren't passed in directly.
      mapM_ (sayArgRead DoDecode) $ zip3 [1..] paramTypes paramCTypeMaybes

      -- Determine how to call the exported function or method.
      let sayCall = case callType of
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
            VarRead identifier -> sayIdentifier identifier
            VarWrite identifier -> sayIdentifier identifier >> says [" = ", toArgName 1]

      -- Write the call, transforming the return value if necessary.
      case (retType, retCTypeMaybe) of
        (TVoid, Nothing) -> sayCall >> say ";\n"
        (_, Nothing) -> say "return " >> sayCall >> say ";\n"
        (TBitspace b, Just _) -> do
          addReqsM $ bitspaceReqs b
          let convFn = bitspaceFromCppValueFn b
          say "return "
          forM_ convFn $ \f -> says [f, "("]
          sayCall
          when (isJust convFn) $ say ")"
          say ";\n";
        (TRef cls, Just (TPtr cls')) | cls == cls' -> say "return &(" >> sayCall >> say ");\n"
        (TObj cls, Just (TPtr (TConst (TObj cls')))) | cls == cls' -> sayReturnNewCopy cls sayCall
        (TObjToHeap cls, Just (TPtr (TObj cls'))) | cls == cls' -> sayReturnNewCopy cls sayCall
        ts -> abort $ concat ["sayExportFn: Unexpected return types ", show ts,
                              "while generating binding for ", show extName, "."]

  where sayReturnNewCopy cls sayCall =
          say "return new" >> sayIdentifier (classIdentifier cls) >> say "(" >>
          sayCall >> say ");\n"

-- | If @dir@ is 'DoDecode', then we are a C++ function reading an argument from
-- foreign code.  If @dir@ is 'DoEncode', then we are invoking a foreign
-- callback.
sayArgRead :: CoderDirection -> (Int, Type, Maybe Type) -> Generator ()
sayArgRead dir (n, stripConst . normalizeType -> cppType, maybeCType) = case cppType of
  TBitspace b -> case maybeCType of
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

  TCallback cb -> do
    case dir of
      DoDecode -> return ()
      DoEncode -> abort $ concat
                  ["sayArgRead: Encoding of callbacks is not supported.  Given ",
                   show cb, "."]
    says [callbackClassName cb, " ", toArgName n, "(", toArgNameAlt n, ");\n"]

  TRef t -> convertObj t

  TObj _ -> convertObj $ TConst cppType
  TObjToHeap cls -> case dir of
    DoDecode -> error $ tObjToHeapWrongDirectionErrorMsg (Just "sayArgRead") cls
    DoEncode -> do
      sayIdentifier $ classIdentifier cls
      says ["* ", toArgName n, " = new "]
      sayIdentifier $ classIdentifier cls
      says ["(", toArgNameAlt n, ");\n"]

  -- Primitive types don't need to be encoded/decoded.  But if maybeCType is a
  -- Just, then we're expected to do some encoding/decoding, so something is
  -- wrong.
  --
  -- TODO Do we need to handle TConst?
  _ -> forM_ maybeCType $ \cType ->
    abort $ concat
    ["sayArgRead: Don't know how to ", show dir, " between C-type ", show cType,
     " and C++-type ", show cppType, "."]

  where convertObj cppType' = case dir of
          DoDecode -> do
            sayVar (toArgName n) Nothing $ TRef cppType'
            says [" = *", toArgNameAlt n, ";\n"]
          DoEncode -> do
            sayVar (toArgName n) Nothing $ TPtr cppType'
            says [" = &", toArgNameAlt n, ";\n"]

sayArgNames :: Int -> Generator ()
sayArgNames count =
  says $ intersperse ", " $ map toArgName [1..count]

sayExportCallback :: Bool -> Callback -> Generator ()
sayExportCallback sayBody cb = do
  let className = callbackClassName cb
      implClassName = callbackImplClassName cb
      fnName = callbackFnName cb
      paramTypes = callbackParams cb
      paramCount = length paramTypes
      retType = callbackReturn cb
      cbType = TCallback cb
      fnType = TFn paramTypes retType

  -- The function pointer we receive from foreign code will work with C-types,
  -- so determine what that function looks like.
  let paramCTypes = zipWith fromMaybe paramTypes $ map typeToCType paramTypes
      retCType = fromMaybe retType $ typeToCType retType

  addReqsM . mconcat =<< mapM typeReqs (retType:paramTypes)

  let fnCType = TFn paramCTypes retCType
      fnPtrCType = TPtr fnCType

  if not sayBody
    then do
      -- Render the class declarations into the header file.
      addInclude $ includeStd "memory"  -- Needed for std::shared_ptr.

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
      say "    " >> sayVar "f_" Nothing (TConst fnPtrCType) >> say ";\n"
      say "    void (*const release_)(void(*)());\n"
      say "    const bool releaseRelease_;\n"
      say "};\n"

      says ["\nclass ", className, " {\n"]
      say "public:\n"
      says ["    explicit ", className, "(", implClassName, "* impl) : impl_(impl) {}\n"]
      say "    " >> sayVar "operator()" Nothing fnType >> say ";\n"
      say "private:\n"
      says ["    std::shared_ptr<", implClassName, "> impl_;\n"]
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
        -- Invoke the function pointer into foreign code.
        let sayCall = say "f_(" >> sayArgNames paramCount >> say ")"
        case (retType, retCTypeMaybe) of
          (TVoid, Nothing) -> sayCall >> say ";\n"
          (_, Nothing) -> say "return " >> sayCall >> say ";\n"
          (TBitspace b, Just _) -> do
            addReqsM $ bitspaceReqs b
            let convFn = bitspaceToCppValueFn b
            say "return "
            forM_ convFn $ \f -> says [f, "("]
            sayCall
            when (isJust convFn) $ say ")"
            say ";\n";
          (TObj cls1, Just retCType@(TPtr (TConst (TObj cls2)))) | cls1 == cls2 -> do
            sayVar "resultPtr" Nothing retCType >> say " = " >> sayCall >> say ";\n"
            sayVar "result" Nothing retType >> say " = *resultPtr;\n"
            say "delete resultPtr;\n"
            say "return result;\n"
          (TRef (TConst (TObj cls1)), Just (TPtr (TConst (TObj cls2)))) | cls1 == cls2 ->
            say "return *(" >> sayCall >> say ");\n"
          (TRef (TObj cls1), Just (TPtr (TObj cls2))) | cls1 == cls2 ->
            say "return *(" >> sayCall >> say ");\n"
          ts -> abort $ concat
                ["sayExportCallback: Unexpected return types ", show ts, "."]

      -- Render the non-impl operator() method, which simply passes C++ values
      -- along to the impl object.
      sayFunction (className ++ "::operator()")
                  (map toArgName [1..paramCount])
                  fnType $ Just $ do
        case retType of
          TVoid -> say "(*impl_)("
          _ -> say "return (*impl_)("
        sayArgNames paramCount
        say ");\n"

      -- Render the function that creates a new callback object.
      let newCallbackFnType = TFn [ fnPtrCType
                                  , TPtr (TFn [TPtr $ TFn [] TVoid] TVoid)
                                  , TBool
                                  ]
                              cbType
      sayFunction fnName ["f", "release", "releaseRelease"] newCallbackFnType $ Just $
        says ["return new ", implClassName, "(f, release, releaseRelease);\n"]

-- | Returns a 'Type' iff there is a C type distinct from the given C++ type
-- that should be used for conversion.
typeToCType :: Type -> Maybe Type
typeToCType t = case t of
  -- Because we don't know (although we could...) the direction in which we're
  -- converting the bitspace value, when the bitspace has a C++ type we have to
  -- assume that it needs to be converted.  The caller will sort out whether a
  -- conversion is actually requested.
  TBitspace b -> case bitspaceCppTypeIdentifier b of
    Just _ -> Just $ bitspaceType b
    Nothing -> Nothing
  TRef t' -> Just $ TPtr t'
  TObj _ -> Just $ TPtr $ TConst t
  TObjToHeap cls -> Just $ TPtr $ TObj cls
  TConst t' -> typeToCType t'
  _ -> Nothing

typeReqs :: Type -> Generator Reqs
typeReqs t = case t of
  TVoid -> return mempty
  TBool -> return mempty
  TChar -> return mempty
  TUChar -> return mempty
  TShort -> return mempty
  TUShort -> return mempty
  TInt -> return mempty
  TUInt -> return mempty
  TLong -> return mempty
  TULong -> return mempty
  TLLong -> return mempty
  TULLong -> return mempty
  TFloat -> return mempty
  TDouble -> return mempty
  TInt8 -> return cstdintReqs
  TInt16 -> return cstdintReqs
  TInt32 -> return cstdintReqs
  TInt64 -> return cstdintReqs
  TWord8 -> return cstdintReqs
  TWord16 -> return cstdintReqs
  TWord32 -> return cstdintReqs
  TWord64 -> return cstdintReqs
  TPtrdiff -> return cstddefReqs
  TSize -> return cstddefReqs
  TSSize -> return cstddefReqs
  TEnum e -> return $ enumReqs e
  TBitspace b -> typeReqs $ bitspaceType b
  TPtr t' -> typeReqs t'
  TRef t' -> typeReqs t'
  TFn paramTypes retType ->
    -- TODO Is the right 'ReqsType' being used recursively here?
    mconcat <$> mapM typeReqs (retType:paramTypes)
  TCallback cb -> do
    cbClassReqs <- reqInclude . includeLocal . moduleHppPath <$>
                   findExportModule (callbackExtName cb)
    -- TODO Is the right 'ReqsType' being used recursively here?
    fnTypeReqs <- typeReqs $ callbackToTFn cb
    return $ cbClassReqs `mappend` fnTypeReqs
  TObj cls -> return $ classReqs cls
  TObjToHeap cls -> return $ classReqs cls
  TConst t' -> typeReqs t'

cstddefReqs :: Reqs
cstddefReqs = reqInclude $ includeStd "cstddef"

cstdintReqs :: Reqs
cstdintReqs = reqInclude $ includeStd "cstdint"

findExportModule :: ExtName -> Generator Module
findExportModule extName =
  fromMaybeM (abort $ concat
              ["findExportModule: Can't find module exporting ", fromExtName extName, "."]) =<<
  fmap (M.lookup extName . interfaceNamesToModules) askInterface
