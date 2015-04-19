module Foreign.Cppop.Generator.Language.Cpp.General (
  makeCppName,
  externalNameToCpp,
  classDeleteFnCppName,
  classEncodeFnCppName,
  classDecodeFnCppName,
  callbackClassName,
  callbackImplClassName,
  callbackFnName,
  toArgName,
  toArgNameAlt,
  Chunk (..),
  runChunkWriter,
  evalChunkWriter,
  execChunkWriter,
  runChunkWriterT,
  evalChunkWriterT,
  execChunkWriterT,
  say,
  says,
  sayIdentifier,
  sayVar,
  sayType,
  sayType',
  ) where

import Control.Monad (liftM)
import Control.Monad.Writer (MonadWriter, Writer, WriterT, runWriter, runWriterT, tell)
import Data.Foldable (forM_)
import Data.List (intercalate, intersperse)
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

makeClassCppName :: String -> Class -> String
makeClassCppName prefix cls = makeCppName [prefix, fromExtName $ classExtName cls]

classDeleteFnPrefix :: String
classDeleteFnPrefix = "gendel"

classDeleteFnCppName :: Class -> String
classDeleteFnCppName = makeClassCppName classDeleteFnPrefix

classEncodeFnPrefix :: String
classEncodeFnPrefix = "genenc"

classEncodeFnCppName :: Class -> String
classEncodeFnCppName = makeClassCppName classEncodeFnPrefix

classDecodeFnPrefix :: String
classDecodeFnPrefix = "gendec"

classDecodeFnCppName :: Class -> String
classDecodeFnCppName = makeClassCppName classDecodeFnPrefix

callbackClassName :: Callback -> String
callbackClassName = fromExtName . callbackExtName

callbackImplClassName :: Callback -> String
callbackImplClassName = (++ "_impl") . fromExtName . callbackExtName

callbackFnName :: Callback -> String
callbackFnName = externalNameToCpp . callbackExtName

toArgName :: Int -> String
toArgName = ("arg" ++) . show

toArgNameAlt :: Int -> String
toArgNameAlt n = "arg" ++ show n ++ "_"

-- TODO Fixme, this is most likely backwards, it should be a finite set of
-- non-identifier chars.  Also (maybe) share some logic with the toExtName
-- requirements?
isIdentifierChar :: Char -> Bool
isIdentifierChar = (`elem` identifierChars)

identifierChars :: String
identifierChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"

-- | A chunk is a string that contains an arbitrary portion of C++ code.  The
-- only requirement is that chunk boundaries are also C++ token boundaries,
-- because the generator monad automates the process of inserting whitespace
-- between chunk boundaries where necessary.
data Chunk = Chunk
  { chunkContents :: String
  }

runChunkWriter :: Writer [Chunk] a -> (a, String)
runChunkWriter = fmap combineChunks . runWriter

evalChunkWriter :: Writer [Chunk] a -> a
evalChunkWriter = fst . runChunkWriter

execChunkWriter :: Writer [Chunk] a -> String
execChunkWriter = snd . runChunkWriter

runChunkWriterT :: Monad m => WriterT [Chunk] m a -> m (a, String)
runChunkWriterT = liftM (fmap combineChunks) . runWriterT

evalChunkWriterT :: Monad m => WriterT [Chunk] m a -> m a
evalChunkWriterT = liftM fst . runChunkWriterT

execChunkWriterT :: Monad m => WriterT [Chunk] m a -> m String
execChunkWriterT = liftM snd . runChunkWriterT

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
say :: MonadWriter [Chunk] m => String -> m ()
say = tell . (:[]) . Chunk

-- | Emits a 'Chunk' for each string in a list.
says :: MonadWriter [Chunk] m => [String] -> m ()
says = tell . map Chunk

sayIdentifier :: MonadWriter [Chunk] m => Identifier -> m ()
sayIdentifier = say . idToString

sayVar :: MonadWriter [Chunk] m => String -> Maybe [String] -> Type -> m ()
sayVar name maybeParamNames t = sayType' t maybeParamNames topPrecedence $ say name

sayType :: MonadWriter [Chunk] m => Maybe [String] -> Type -> m ()
sayType maybeParamNames t = sayType' t maybeParamNames topPrecedence $ return ()

sayType' :: MonadWriter [Chunk] m => Type -> Maybe [String] -> Int -> m () -> m ()
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
    TEnum e -> sayIdentifier (enumIdentifier e) >> outer
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
        sayType' ptype Nothing topPrecedence $ forM_ pname say
      say ")"
      -- int(*)()          Pointer to a function returning an int.
      -- int(*)()[]        Pointer to a function returning an array of ints.  (Must be sized...)
      -- int*(*)()         Pointer to a function returning a pointer to an int.
      -- int(*(*var)())[]  Pointer to a function returning a pointer to an array of ints.  (Must be sized...)
      -- A function can't return an array.
    TCallback cb -> says [callbackImplClassName cb, "*"] >> outer
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
