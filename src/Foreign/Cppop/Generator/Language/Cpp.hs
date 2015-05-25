module Foreign.Cppop.Generator.Language.Cpp (
  Generation,
  generate,
  generatedFiles,
  -- * Exported only for other generators, do not use.
  externalNameToCpp,
  classDeleteFnCppName,
  ) where

-- How callbacks work:
--
-- data Type = ... | TCallback Callback
--
-- data Callback = Callback ExtName [Type] Type  -- Parameter and return types.
--
-- We want to call some foreign code from C++.  What C++ type do we associate
-- with such an entry point?  (Both the C++ and foreign sides of the callback
-- will need to perform en-/decoding.)
--
-- Function pointer: Create a function pointer to a foreign wrapper which does
-- en-/decoding on the foreign side.  But then we need to wrap this in a C++
-- function (pointer) which does the C++-side conversions.  Function pointers
-- can't close over variables, so this doesn't work.
--
-- C++ functor: Create a class G that takes a foreign function pointer and
-- implements operator(), performing the necessary conversions around invoking
-- the pointer.  In the event that the function pointer is dynamically allocated
-- (as in Haskell), then this class also ties the lifetime of the function
-- pointer to the lifetime of the class.  But this would cause problems for
-- passing this object around by value, so instead we make G non-copyable and
-- non-assignable, allocate our G instance on the heap, and create a second
-- class F that holds a shared_ptr<G> and whose operator() calls through to G.
--
-- This way, the existance of the F and G objects are invisible to the foreign
-- language, and (for now) passing these callbacks back to the foreign language
-- is not supported.
--
-- When a binding is declared to take a callback type, the generated foreign
-- side of the binding will take a foreign function (the callback) with
-- foreign-side types, and use a function (Haskell: callbackName) generated for
-- the callback type to wrap the callback in a foreign function that does
-- argument decoding and return value encoding: this wrapped function will have
-- C-side types.  The binding will then create a G object (above) for this
-- wrapped function (Haskell: using callbackName'), and pass a G pointer into
-- the C side of the binding.  The binding will decode this C pointer by
-- wrapping it in a temporary F object, and passing that to the C++ function.
-- The C++ code is free to copy this F object as much as it likes.  If it
-- doesn't store a copy somewhere before returning, then the when the temporary
-- F object is destructed, the G object will get deleted.

import Control.Applicative ((<$>))
import Control.Monad (liftM, unless, when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Writer (WriterT, execWriterT, runWriterT, tell)
import Control.Monad.Trans (lift)
import Data.Foldable (forM_)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mappend, mconcat, mempty)
import qualified Data.Set as S
import Foreign.Cppop.Common
import Foreign.Cppop.Generator.Language.Cpp.General
import Foreign.Cppop.Generator.Spec

data CoderDirection = DoDecode | DoEncode
                    deriving (Eq, Show)

type Generator = ReaderT Env (WriterT [Chunk] (WriterT (S.Set Include) (Either String)))

data Env = Env
  { envInterface :: Interface
  , envModule :: Module
  }

addIncludes :: [Include] -> Generator ()
addIncludes = lift . lift . tell . S.fromList

addInclude :: Include -> Generator ()
addInclude = addIncludes . (:[])

addReqs :: Reqs -> Generator ()
addReqs = lift . lift . tell . reqsIncludes

askInterface :: MonadReader Env m => m Interface
askInterface = liftM envInterface ask

askModule :: MonadReader Env m => m Module
askModule = liftM envModule ask

-- | Halts generation and returns the given error message.
abort :: String -> Generator a
abort = lift . lift . lift . Left

execGenerator :: Interface -> Module -> Maybe String -> Generator a -> Either String String
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

class HasExternalName a where
  getExternalName :: a -> ExtName

instance HasExternalName Function where
  getExternalName = fnExtName

data Generation = Generation
  { generatedFiles :: M.Map FilePath String
    -- ^ A map from paths of generated files to the contents of those files.
    -- The file paths are relative paths below the C++ generation root.
  }

generate :: Interface -> Either String Generation
generate interface =
  fmap (Generation . M.fromList) $
  execWriterT $
  forM_ (M.elems $ interfaceModules interface) $ \m -> do
    let headerGuard = concat ["CPPOP_MODULE_", interfaceName interface, "_", moduleName m]
    header <- lift $ execGenerator interface m (Just headerGuard) sayModuleHeader
    tell [(moduleHppPath m, header)]
    source <- lift $ execGenerator interface m Nothing sayModuleSource
    tell [(moduleCppPath m, source)]

sayModuleHeader :: Generator ()
sayModuleHeader = do
  m <- askModule
  addReqs $ moduleReqs m
  mapM_ (sayExport False) $ M.elems $ moduleExports m

sayModuleSource :: Generator ()
sayModuleSource = do
  m <- askModule
  addInclude $ includeLocal $ moduleHppPath m
  mapM_ (sayExport True) $ M.elems $ moduleExports m

sayExport :: Bool -> Export -> Generator ()
sayExport sayBody export = case export of
  -- Nothing to do C++ side for an enum.
  ExportEnum _ -> return ()

  ExportFn fn ->
    -- Export a single function.
    when sayBody $ do
      addReqs $ fnUseReqs fn
      sayExportFn (fnExtName fn)
                  (sayIdentifier $ fnIdentifier fn)
                  Nothing
                  (fnParams fn)
                  (fnReturn fn)
                  sayBody

  ExportClass cls -> when sayBody $ do
    let clsPtr = TPtr $ TObj cls
        justClsPtr = Just clsPtr
    addReqs $ classUseReqs cls  -- This is needed at least for the delete function.
    -- Export each of the class's constructors.
    forM_ (classCtors cls) $ \ctor ->
      sayExportFn (ctorExtName ctor)
                  (say "new" >> sayIdentifier (classIdentifier cls))
                  Nothing
                  (ctorParams ctor)
                  clsPtr
                  sayBody
    -- Export a delete function for the class.
    sayFunction (classDeleteFnCppName cls)
                ["self"]
                (TFn [TPtr $ TConst $ TObj cls] TVoid) $
      Just $ say "delete self;\n"
    -- Export each of the class's methods.
    forM_ (classMethods cls) $ \method -> do
      let static = methodStatic method == Static
      sayExportFn (methodExtName method)
                  (do when static $ do
                        sayIdentifier (classIdentifier cls)
                        say "::"
                      say $ methodCName method)
                  (if static then Nothing else justClsPtr)
                  (methodParams method)
                  (methodReturn method)
                  sayBody

  ExportCallback cb -> sayExportCallback sayBody cb

sayExportFn :: ExtName -> Generator () -> Maybe Type -> [Type] -> Type -> Bool -> Generator ()
sayExportFn extName sayCppName maybeThisType paramTypes retType sayBody = do
  let paramCount = length paramTypes
  paramCTypeMaybes <- mapM typeToCType paramTypes
  let paramCTypes = zipWith fromMaybe paramTypes paramCTypeMaybes
  retCTypeMaybe <- typeToCType retType
  let retCType = fromMaybe retType retCTypeMaybe

  addReqs . mconcat =<< mapM typeUseReqs (retType:paramTypes)

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
      -- Call the exported function or method.
      let sayCall = do when (isJust maybeThisType) $ say "self->"
                       sayCppName
                       say "("
                       sayArgNames paramCount
                       say ")"
      case (retType, retCTypeMaybe) of
        (TVoid, Nothing) -> sayCall >> say ";\n"
        (_, Nothing) -> say "return " >> sayCall >> say ";\n"
        (TRef _, Just (TPtr _)) -> say "return &(" >> sayCall >> say ");\n"
        (TObj cls, Just (TPtr (TObj _))) ->
          say "return new" >> sayIdentifier (classIdentifier cls) >> say "(" >>
          sayCall >> say ");\n"
        ts -> abort $ concat ["sayExportFn: Unexpected return types ", show ts,
                              "while generating binding for ", show extName, "."]

-- | If @dir@ is 'DoDecode', then we are a C++ function reading an argument from
-- foreign code.  If @dir@ is 'DoEncode', then we are invoking a foreign
-- callback.
sayArgRead :: CoderDirection -> (Int, Type, Maybe Type) -> Generator ()
sayArgRead dir (n, cppType, maybeCType) = case cppType of
  TCallback cb -> do
    case dir of
      DoDecode -> return ()
      DoEncode -> abort $ concat
                  ["sayArgRead: Encoding of callbacks is not supported.  Given ",
                   show cb, "."]
    says [callbackClassName cb, " ", toArgName n, "(", toArgNameAlt n, ");\n"]

  TRef (TObj _) -> convertObj
  TRef (TConst (TObj _)) -> convertObj
  TObj _ -> convertObj

  -- Primitive types don't need to be encoded/decoded.  But if maybeCType is a
  -- Just, then we're expected to do some encoding/decoding, so something is
  -- wrong.
  --
  -- TODO Do we need to handle TConst?
  _ -> forM_ maybeCType $ \cType ->
    abort $ concat
    ["sayArgRead: Don't know how to ", show dir, " to type ", show cType,
     " from type ", show cppType, "."]

  where convertObj = case dir of
          DoDecode -> do
            sayVar (toArgName n) Nothing $ TRef cppType
            says [" = *", toArgNameAlt n, ";\n"]
          DoEncode -> do
            sayVar (toArgName n) Nothing $ TPtr cppType
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
  paramCTypes <- zipWith fromMaybe paramTypes <$> mapM typeToCType paramTypes
  retCType <- fromMaybe retType <$> typeToCType retType

  addReqs . mconcat =<< mapM typeUseReqs (retType:paramTypes)

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
      paramCTypeMaybes <- mapM typeToCType paramTypes
      retCTypeMaybe <- typeToCType retType

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
          (TObj _, Just retCType@(TPtr (TObj _))) -> do
            sayVar "resultPtr" Nothing retCType >> say " = " >> sayCall >> say ";\n"
            sayVar "result" Nothing retType >> say " = *resultPtr;\n"
            say "delete resultPtr;\n"
            say "return result;\n"
          (TRef _, Just (TPtr (TObj _))) ->
            say "return *(" >> sayCall >> say ");\n"
          ts -> abort $ concat
                ["sayExportCallback: Unexpected return types ", show ts, "."]

      -- Render the non-impl operator() method, which simply passes C++ values
      -- along to the impl object.
      sayFunction (className ++ "::operator()")
                  (map toArgName [1..paramCount])
                  fnType $ Just $
        say "(*impl_)(" >> sayArgNames paramCount >> say ");\n"

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
typeToCType :: Type -> Generator (Maybe Type)
typeToCType t = case t of
  TRef t' -> return $ Just $ TPtr t'
  TObj _ -> return $ Just $ TPtr t
  TConst t' -> typeToCType t'
  _ -> return Nothing

typeUseReqs :: Type -> Generator Reqs
typeUseReqs t = case t of
  TVar _ -> abort $ freeVarErrorMsg "typeUseReqs" t
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
  TSize -> return sizeTReqs
  TSSize -> return sizeTReqs
  TEnum e -> return $ enumUseReqs e
  TPtr t' -> typeUseReqs t'
  TRef t' -> typeUseReqs t'
  TFn paramTypes retType ->
    -- TODO Is the right 'ReqsType' being used recursively here?
    mconcat <$> mapM typeUseReqs (retType:paramTypes)
  TCallback cb -> do
    cbClassReqs <- reqInclude . includeLocal . moduleHppPath <$>
                   findExportModule (callbackExtName cb)
    -- TODO Is the right 'ReqsType' being used recursively here?
    fnTypeReqs <- typeUseReqs $ callbackToTFn cb
    return $ cbClassReqs `mappend` fnTypeReqs
  TObj cls -> return $ classUseReqs cls
  TConst t' -> typeUseReqs t'

sizeTReqs :: Reqs
sizeTReqs = reqInclude $ includeStd "cstddef"

findExportModule :: ExtName -> Generator Module
findExportModule extName =
  fromMaybeM (abort $ concat
              ["findExportModule: Can't find module exporting ", fromExtName extName, "."]) =<<
  fmap (M.lookup extName . interfaceNamesToModules) askInterface
