module Foreign.Cppop.Generator.Language.Cpp (
  Generation,
  generate,
  generatedBindingsHeader,
  generatedBindingsSource,
  generatedCallbacksHeader,
  generatedCallbacksSource,
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

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Trans (lift)
import Data.Foldable (forM_)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Foreign.Cppop.Common
import Foreign.Cppop.Generator.Language.Cpp.General
import Foreign.Cppop.Generator.Spec

data CoderDirection = DoDecode | DoEncode
                    deriving (Eq, Show)

getCoder :: CoderDirection -> ClassEncoding -> Maybe CppCoder
getCoder DoDecode = classCppDecoder
getCoder DoEncode = classCppEncoder

type Generator = ReaderT Env (WriterT [Chunk] (Either String))

data Env = Env
  { envInterface :: Interface
  }

askInterface :: Generator Interface
askInterface = fmap envInterface ask

askExports :: Generator [Export]
askExports = fmap (interfaceExports . envInterface) ask

-- | Halts generation and returns the given error message.
abort :: String -> Generator a
abort = lift . lift . Left

execGenerator :: Interface -> Generator a -> Either String String
execGenerator interface = execChunkWriterT . flip runReaderT (Env interface)

sayBindingsHeader :: Generator ()
sayBindingsHeader = do
  say "////////// GENERATED FILE, EDITS WILL BE LOST //////////\n\n"
  mapM_ sayInclude . interfaceBindingsIncludes =<< askInterface
  say "\nextern \"C\" {\n"
  mapM_ (sayExport False) =<< askExports
  say "\n}\n"

sayBindingsSource :: Generator ()
sayBindingsSource = do
  say "////////// GENERATED FILE, EDITS WILL BE LOST //////////\n\n"
  iface <- askInterface
  -- cstdlib is required for the free() call that 'classCppDecodeThenFree'
  -- provides.
  say $ includeToString $ includeStd "cstdlib"
  say "\n"
  say $ includeToString $ includeLocal $ interfaceBindingsHppPath iface
  forM_ (interfaceCallbacksHppPath iface) $ say . includeToString . includeLocal
  say "\nextern \"C\" {\n"
  mapM_ (sayExport True) =<< askExports
  say "\n}\n"

sayCallbacksHeader :: Generator ()
sayCallbacksHeader = do
  say "////////// GENERATED FILE, EDITS WILL BE LOST //////////\n\n"
  iface <- askInterface
  let guardName = ("CPPOP_GEN_CB_" ++) $ interfaceName iface
  says ["#ifndef ", guardName, "\n"]
  says ["#define ", guardName, "\n"]
  says ["\n", includeToString $ includeStd "memory", "\n"]  -- Needed for shared_ptr.
  mapM_ sayInclude $ interfaceCallbacksIncludes iface
  callbacks <- mapMaybe (\export -> case export of
                           ExportCallback cb -> Just cb
                           _ -> Nothing) <$>
               askExports
  forM_ callbacks $ sayExportCallback SayCallbackImpl False
  say "\n#endif\n"

sayCallbacksSource :: Generator ()
sayCallbacksSource = do
  say "////////// GENERATED FILE, EDITS WILL BE LOST //////////\n\n"
  iface <- askInterface
  maybe (abort $ "Interface " ++ show (interfaceName iface) ++
         " requires a .hpp file path for generating callbacks.")
        (say . includeToString . includeLocal) $
    interfaceCallbacksHppPath iface
  callbacks <- mapMaybe (\export -> case export of
                           ExportCallback cb -> Just cb
                           _ -> Nothing) <$>
               askExports
  forM_ callbacks $ sayExportCallback SayCallbackImpl True

sayInclude :: Include -> Generator ()
sayInclude = say . includeToString

sayFunction :: String -> [String] -> Type -> Maybe (Generator ()) -> Generator ()
sayFunction name paramNames t maybeBody = do
  case t of
    TFn {} -> return ()
    _ -> abort $ "sayFunction requires a function type, given: " ++ show t
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
  { generatedBindingsHeader :: String
  , generatedBindingsSource :: String
  , generatedCallbacksHeader :: String
  , generatedCallbacksSource :: String
  }

generate :: Interface -> Either String Generation
generate interface =
  Generation <$>
  execGenerator interface sayBindingsHeader <*>
  execGenerator interface sayBindingsSource <*>
  execGenerator interface sayCallbacksHeader <*>
  execGenerator interface sayCallbacksSource

sayExport :: Bool -> Export -> Generator ()
sayExport sayBody export = case export of
  -- Nothing to do C++ side for an enum.
  ExportEnum _ -> return ()

  ExportFn fn ->
    -- Export a single function.
    sayExportFn (fnExtName fn)
                (sayIdentifier $ fnIdentifier fn)
                Nothing
                (fnParams fn)
                (fnReturn fn)
                sayBody
  ExportClass cls -> do
    let clsPtr = TPtr $ TObj cls
        justClsPtr = Just clsPtr
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
      if sayBody
      then Just $ say "delete self;\n"
      else Nothing
    -- Export encode and decode functions for the class.
    sayClassEncodeFn sayBody cls
    sayClassDecodeFn sayBody cls
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
  ExportCallback cb -> sayExportCallback SayCallbackBinding sayBody cb

sayExportFn :: ExtName -> Generator () -> Maybe Type -> [Type] -> Type -> Bool -> Generator ()
sayExportFn extName sayCppName maybeThisType paramTypes retType sayBody = do
  let paramCount = length paramTypes
  paramCTypeMaybes <- mapM typeToCType paramTypes
  let paramCTypes = zipWith fromMaybe paramTypes paramCTypeMaybes
  retCTypeMaybe <- typeToCType retType
  let retCType = fromMaybe retType retCTypeMaybe
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
        (TObj cls, Just _) -> do
          encoder <- fromMaybeM (abort $ "sayExportFn: Class lacks an encoder: " ++ show cls) $
                     classCppEncoder $ classEncoding cls
          case encoder of
            CppCoderFn fn ->
              say "return " >> sayIdentifier fn >> say "(" >> sayCall >> say ");\n"
            CppCoderExpr terms -> do
              sayVar "result" Nothing retType >> say " = " >> sayCall >> say ";\n"
              say "return " >> sayExpr "result" terms >> say ";\n"
        ts -> abort $ "sayExportFn: Unexpected return types: " ++ show ts

sayArgRead :: CoderDirection -> (Int, Type, Maybe Type) -> Generator ()
sayArgRead dir (n, cppType, maybeCType) = case cppType of
  TCallback cb -> do
    case dir of
      DoDecode -> return ()
      DoEncode -> abort $ "sayArgRead: Encoding of callbacks is not supported.  Given " ++
                  show cb ++ "."
    says [callbackClassName cb, " ", toArgName n, "(", toArgNameAlt n, ");\n"]

  TObj cls -> do
    let encoding = classEncoding cls
    coder <- fromMaybeM (abort $ "sayArgRead: Class lacks a coder for " ++ show dir ++ ": " ++
                         show (classExtName cls)) $
             getCoder dir encoding
    sayVar (toArgName n) Nothing =<< case dir of
      DoDecode -> return cppType
      DoEncode -> fromMaybeM (abort $
                              "sayArgRead: Internal error, don't have a C type for class: " ++
                              show (classExtName cls))
                  maybeCType
    say " = "
    let inputVar = toArgNameAlt n
    case coder of
      CppCoderFn fn -> sayIdentifier fn >> says ["(", inputVar, ");\n"]
      CppCoderExpr terms -> sayExpr inputVar terms >> say ";\n"
    when (dir == DoDecode && classCppDecodeThenFree encoding) $
      says ["free(", inputVar, ");\n"]

  -- Primitive types don't need to be encoded/decoded.  But it we maybeCType is
  -- a Just, then we're expected to do some encoding/decoding, so something is
  -- wrong.
  --
  -- TODO Do we need to handle TConst?
  _ -> forM_ maybeCType $ \cType ->
    abort $ "sayArgRead: Don't know how to " ++ show dir ++ " to type " ++ show cType ++
    " from type " ++ show cppType ++ "."

sayExpr :: String -> [Maybe String] -> Generator ()
sayExpr arg terms = do
  say "("
  forM_ terms $ maybe (says ["(", arg, ")"]) say
  say ")"

sayArgNames :: Int -> Generator ()
sayArgNames count =
  says $ intersperse ", " $ map toArgName [1..count]

-- | When a class is encodable from a foreign value (as described in
-- 'ClassEncoding'), then generate an encode function.
sayClassEncodeFn :: Bool -> Class -> Generator ()
sayClassEncodeFn sayBody cls =
  when (isJust $ classCppDecoder $ classEncoding cls) $ do
    cType <-
      fromMaybeM (abort $ "sayClassEncodeFn: Should have a C type for an object of class " ++
                  show (fromExtName $ classExtName cls) ++ ".") =<<
      typeToCType (TObj cls)
    sayFunction (classEncodeFnCppName cls)
                [toArgNameAlt 1]
                (TFn [cType] $ TPtr $ TObj cls) $
      if sayBody
      then Just $ do
        -- TODO This may result in an redundant temporary on the stack for some
        -- classes; optimize.
        sayArgRead DoDecode (1, TObj cls, Just cType)
        say "return new" >> sayIdentifier (classIdentifier cls) >> says ["(", toArgName 1, ");\n"]
      else Nothing

-- | When a class is decodable to a foreign value (as described in
-- 'ClassEncoding'), then generate an decode function.
sayClassDecodeFn :: Bool -> Class -> Generator ()
sayClassDecodeFn sayBody cls =
  forM_ (classCppEncoder $ classEncoding cls) $ \encoder -> do
    cType <-
      fromMaybeM (abort $ "sayClassDecodeFn: Should have a C type for an object of class " ++
                  show (fromExtName $ classExtName cls) ++ ".") =<<
      typeToCType (TObj cls)
    sayFunction (classDecodeFnCppName cls)
                ["result"]
                (TFn [TPtr $ TObj cls] cType) $
      if sayBody
      then Just $ case encoder of
        CppCoderFn fn -> say "return " >> sayIdentifier fn >> say "(*result);\n"
        CppCoderExpr terms -> say "return " >> sayExpr "(*result)" terms >> say ";\n"
      else Nothing

data SayCallbackMode = SayCallbackImpl | SayCallbackBinding

sayExportCallback :: SayCallbackMode -> Bool -> Callback -> Generator ()
sayExportCallback mode sayBody cb = do
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
  let fnCType = TFn paramCTypes retCType
      fnPtrCType = TPtr fnCType

  case mode of
    SayCallbackBinding -> do
      -- Render the function that creates a new callback object.
      let newCallbackFnType = TFn [ fnPtrCType
                                  , TPtr (TFn [TPtr $ TFn [] TVoid] TVoid)
                                  , TBool
                                  ]
                              cbType
      sayFunction fnName ["f", "release", "releaseRelease"] newCallbackFnType $
        if sayBody
        then Just $ says ["return new ", implClassName, "(f, release, releaseRelease);\n"]
        else Nothing

    SayCallbackImpl ->
      if sayBody
      then do
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
            (TObj cls, Just _) -> do
              decoder <- fromMaybeM (abort $ "sayExportCallback: Class lacks a decoder: " ++
                                     show cls) $
                         classCppDecoder $ classEncoding cls
              case decoder of
                CppCoderFn fn ->
                  say "return " >> sayIdentifier fn >> say "(" >> sayCall >> say ");\n"
                CppCoderExpr terms -> do
                  sayVar "result" Nothing retType >> say " = " >> sayCall >> say ";\n"
                  say "return " >> sayExpr "result" terms >> say ";\n"
            ts -> abort $ "sayExportCallback: Unexpected return types: " ++ show ts

        -- Render the non-impl operator() method, which simply passes C++ values
        -- along to the impl object.
        sayFunction (className ++ "::operator()")
                    (map toArgName [1..paramCount])
                    fnType $ Just $
          say "(*impl_)(" >> sayArgNames paramCount >> say ");\n"

      else do
        -- Render the class declarations into the header file.
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

-- | Returns a 'Type' iff there is a C type distinct from the given C++ type
-- that should be used for conversion.
typeToCType :: Type -> Generator (Maybe Type)
typeToCType t = case t of
  TObj cls -> do
    t' <- fromMaybeM (abort $ "typeToCType: Don't have a C type for class: " ++ show cls) $
          classCppCType $ classEncoding cls
    Just <$> ensureNonObj t'
  TConst t' -> typeToCType t'
  _ -> return Nothing
  where ensureNonObj t' = case t' of
          TObj cls' -> abort $ "typeToCType: Class's C type cannot be an object: " ++ show cls'
          TConst t'' -> ensureNonObj t''
          _ -> return t'
