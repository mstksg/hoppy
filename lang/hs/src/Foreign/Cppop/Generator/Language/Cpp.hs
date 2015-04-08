module Foreign.Cppop.Generator.Language.Cpp (
  Generation,
  generate,
  generatedHeader,
  generatedSource,
  -- * Exported only for other generators, do not use.
  externalNameToCpp,
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans (lift)
import Data.Foldable (forM_)
import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe, isJust)
import Data.Tuple (swap)
import Foreign.Cppop.Common
import Foreign.Cppop.Generator.Spec

cppNameSeparator :: String
cppNameSeparator = "__"

makeCppName :: [String] -> String
makeCppName = intercalate cppNameSeparator

-- | "genpop" is used for individually exported functions.
externalNamePrefix :: String
externalNamePrefix = "genpop"

externalNameToCpp :: ExtName -> String
externalNameToCpp extName =
  makeCppName [externalNamePrefix, fromExtName extName]

toArgName :: Int -> String
toArgName = ("arg" ++) . show

toArgNameAlt :: Int -> String
toArgNameAlt n = "arg" ++ show n ++ "_"

-- | A chunk is a string that contains an arbitrary portion of C++ code.  The
-- only requirement is that chunk boundaries are also C++ token boundaries,
-- because the generator monad automates the process of inserting whitespace
-- between chunk boundaries where necessary.
data Chunk = Chunk
  { chunkContents :: String
  }

type Generator = ReaderT Env (WriterT [Chunk] (Either String))

data Env = Env
  { envInterface :: Interface
  }

askIncludes :: Generator [Include]
askIncludes = fmap (interfaceIncludes . envInterface) ask

askExports :: Generator [Export]
askExports = fmap (interfaceExports . envInterface) ask

-- | Halts generation and returns the given error message.
abort :: String -> Generator a
abort = lift . lift . Left

runGenerator :: Interface -> Generator a -> Either String (String, a)
runGenerator interface =
  fmap (first combineChunks . swap) . runWriterT . flip runReaderT (Env interface)

-- | Flattens a list of chunks down into a single string.  Inserts spaces
-- between chunks where the ends of adjacent chunks would otherwise merge into a
-- single C++ token.
combineChunks :: [Chunk] -> String
combineChunks chunks =
  let strs = map chunkContents chunks
  in concat $ flip map (zip ("":strs) strs) $ \(prev, cur) ->
       let needsSpace =
             not (null prev) && not (null cur) &&
             (let a = last prev
                  b = head cur
              in -- "intconstx" should become "int const x"
                 isIdentifierChar a && isIdentifierChar b ||
                 -- Adjacent template parameter '>'s need spacing in old C++.
                 a == '>' && b == '>')
       in if needsSpace then ' ':cur else cur

-- | Emits a single 'Chunk'.
say :: String -> Generator ()
say = tell . (:[]) . Chunk

-- | Emits a 'Chunk' for each string in a list.
says :: [String] -> Generator ()
says = tell . map Chunk

sayIdentifier :: Identifier -> Generator ()
sayIdentifier = say . idToString

sayInterfaceHeader :: Generator ()
sayInterfaceHeader = do
  mapM_ (say . includeToString) =<< askIncludes
  say "\nextern \"C\" {\n"
  mapM_ (sayExport False) =<< askExports
  say "\n}\n"

sayInterfaceSource :: Generator ()
sayInterfaceSource = do
  mapM_ (say . includeToString) =<< askIncludes
  say "\nextern \"C\" {\n"
  mapM_ (sayExport True) =<< askExports
  say "\n}\n"

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

-- TODO Fixme, this is most likely backwards, it should be a finite set of
-- non-identifier chars.  Also (maybe) share some logic with the toExtName
-- requirements?
isIdentifierChar :: Char -> Bool
isIdentifierChar = (`elem` identifierChars)

identifierChars :: String
identifierChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"

data Generation = Generation
  { generatedHeader :: String
  , generatedSource :: String
  }

generate :: Interface -> Either String Generation
generate interface = do
  header <- fst <$> runGenerator interface sayInterfaceHeader
  source <- fst <$> runGenerator interface sayInterfaceSource
  return Generation
    { generatedHeader = header
    , generatedSource = source
    }

sayExport :: Bool -> Export -> Generator ()
sayExport sayBody export = case export of
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

sayExportFn :: ExtName -> Generator () -> Maybe Type -> [Type] -> Type -> Bool -> Generator ()
sayExportFn extName sayCppName maybeThisType paramTypes retType sayBody = do
  let paramCount = length paramTypes
  paramCTypeMaybes <- mapM typeToCType paramTypes
  let paramCTypes = zipWith (fromMaybe) paramTypes paramCTypeMaybes
  retCTypeMaybe <- typeToCType retType
  let retCType = fromMaybe retType retCTypeMaybe
  sayFunction (externalNameToCpp extName)
              (maybe id (const ("self":)) maybeThisType $
               zipWith (\ctm -> if isJust ctm then toArgNameAlt else toArgName)
               paramCTypeMaybes [1..paramCount])
              (TFn (maybe id (:) maybeThisType paramCTypes) retCType) $
    if not sayBody
    then Nothing
    else Just $ do
      -- Convert arguments that aren't passed in directly.
      mapM_ sayArgRead $ zip3 [1..] paramTypes paramCTypeMaybes
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

sayArgRead :: (Int, Type, Maybe Type) -> Generator ()
sayArgRead (n, cppType, maybeCType) = when (isJust maybeCType) $ do
  cls <- case cppType of
    TObj cls -> return cls
    _ -> abort $ "sayArgRead: Don't know how to decode to type " ++ show cppType ++ "."
  decoder <- fromMaybeM (abort $ "sayArgRead: Class lacks a decoder: " ++ show cls) $
             classCppDecoder $ classEncoding cls
  sayVar (toArgName n) Nothing cppType
  say " = "
  let inputVar = toArgNameAlt n
  case decoder of
    CppCoderFn fn -> do
      sayIdentifier fn
      says ["(", inputVar, ");\n"]
    CppCoderExpr terms -> sayExpr inputVar terms

sayExpr :: String -> [Maybe String] -> Generator ()
sayExpr arg terms = do
  say "("
  forM_ terms $ maybe (says ["(", arg, ")"]) say
  say ")"

sayArgNames :: Int -> Generator ()
sayArgNames count =
  says $ intersperse ", " $ map toArgName [1..count]

sayVar :: String -> Maybe [String] -> Type -> Generator ()
sayVar name maybeParamNames t = sayType' t maybeParamNames topPrecedence $ say name

sayType' :: Type -> Maybe [String] -> Int -> Generator () -> Generator ()
sayType' t maybeParamNames outerPrec unwrappedOuter =
  let prec = typePrecedence t
      outer = if prec <= outerPrec
              then unwrappedOuter
              else say "(" >> unwrappedOuter >> say ")"
  in case t of
    TVoid -> say "void" >> outer
    TBool -> say "bool" >> outer
    TChar -> say "char" >> outer
    TUChar -> say "unsigned char" >> outer
    TShort -> say "short" >> outer
    TUShort -> say "unsigned short" >> outer
    TInt -> say "int" >> outer
    TUInt -> say "unsigned int" >> outer
    TLong -> say "long" >> outer
    TULong -> say "unsigned long" >> outer
    TLLong -> say "long long" >> outer
    TULLong -> say "unsigned long long" >> outer
    TFloat -> say "float" >> outer
    TDouble -> say "double" >> outer
    TSize -> say "size_t" >> outer
    TSSize -> say "ssize_t" >> outer
    TArray maybeSize t' -> sayType' t' Nothing prec $ do
      outer
      say $ maybe "[]" (\n -> '[' : show n ++ "]") maybeSize
      -- int[]             Array of int.
      -- int*[]            Array of pointers to ints.
      -- int*(*var[x])[y]  Array(x) of pointers to arrays(y) of pointers to ints.  (Ptr to array(y) must be sized.)
      -- int(*[])()        Array of pointers to functions returning ints.
      -- int(**[])()       Array of pointers to pointers to functions returning ints.
    TPtr t' -> sayType' t' Nothing prec $ say "*" >> outer
      -- int*              Pointer to an int.
      -- int(*)[]          Pointer to an array of ints.  (C requires the array be sized.)
      -- int(*)()          Pointer to a function returning an int.
    TRef t' -> sayType' t' Nothing prec $ say "&" >> outer
      -- int&              Reference to an int.
      -- int(&)[]          Reference to an array of ints.  (C requires the array be sized.)
      -- int(&)()          Reference to a function returning an int.
    TFn paramTypes retType -> sayType' retType Nothing prec $ do
      outer
      say "("
      --sequence_ $ intersperse (tell [", "]) $ map sayType paramTypes
      sequence_ $ intersperse (say ", ") $
        flip map (zip paramTypes $ maybe (repeat Nothing) (map Just) maybeParamNames) $ \(ptype, pname) ->
        sayType' ptype Nothing topPrecedence $ forM_ pname $ say
      say ")"
      -- int(*)()          Pointer to a function returning an int.
      -- int(*)()[]        Pointer to a function returning an array of ints.  (Must be sized...)
      -- int*(*)()         Pointer to a function returning a pointer to an int.
      -- int(*(*var)())[]  Pointer to a function returning a pointer to an array of ints.  (Must be sized...)
      -- A function can't return an array.
    TObj cls -> sayIdentifier (classIdentifier cls) >> outer
    TOpaque s -> say s >> outer
    TBlob -> say "void*" >> outer
    TConst t' -> sayType' t' maybeParamNames outerPrec $ say "const" >> unwrappedOuter  -- TODO Is using the outer stuff correctly here?

topPrecedence :: Int
topPrecedence = 11

typePrecedence :: Type -> Int
typePrecedence t = case t of
  TFn {} -> 10
  TArray {} -> 9
  TPtr {} -> 8
  TRef {} -> 8
  _ -> 7

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
