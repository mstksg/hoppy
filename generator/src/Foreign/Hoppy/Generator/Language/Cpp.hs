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

{-# LANGUAGE ViewPatterns #-}

-- | Shared portion of the C++ code generator.  Usable by binding definitions.
module Foreign.Hoppy.Generator.Language.Cpp (
  externalNameToCpp,
  classDeleteFnCppName,
  classCastFnCppName,
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
  ) where

import Control.Monad (liftM)
import Control.Monad.Writer (MonadWriter, Writer, WriterT, runWriter, runWriterT, tell)
import Data.Foldable (forM_)
import Data.List (intercalate, intersperse)
import Foreign.Hoppy.Generator.Spec

cppNameSeparator :: String
cppNameSeparator = "__"

makeCppName :: [String] -> String
makeCppName = intercalate cppNameSeparator

-- | \"genpop\" is the prefix used for individually exported functions.
externalNamePrefix :: String
externalNamePrefix = "genpop"

-- | Returns the C++ binding function name for an external name.
externalNameToCpp :: ExtName -> String
externalNameToCpp extName =
  makeCppName [externalNamePrefix, fromExtName extName]

makeClassCppName :: String -> Class -> String
makeClassCppName prefix cls = makeCppName [prefix, fromExtName $ classExtName cls]

-- | \"gendel\" is the prefix used for wrappers for @delete@ calls.
classDeleteFnPrefix :: String
classDeleteFnPrefix = "gendel"

-- | Returns the C++ binding function name of the wrapper for the delete method
-- for a class.
classDeleteFnCppName :: Class -> String
classDeleteFnCppName = makeClassCppName classDeleteFnPrefix

-- | @classCastFnCppName fromCls toCls@ returns the name of the generated C++
-- function that casts a pointer from @fromCls@ to @toCls@.
classCastFnCppName :: Class -> Class -> String
classCastFnCppName from to =
  concat [ "gencast__"
         , fromExtName $ classExtName from
         , "__"
         , fromExtName $ classExtName to
         ]

-- | Returns the name of the outer, copyable class for a callback.
callbackClassName :: Callback -> String
callbackClassName = fromExtName . callbackExtName

-- | Returns the name of the internal, non-copyable implementation class for a
-- callback.
callbackImplClassName :: Callback -> String
callbackImplClassName = (++ "_impl") . fromExtName . callbackExtName

-- | Returns the name of the C++ binding function that creates a C++ callback
-- wrapper object from a function pointer to foreign code.
callbackFnName :: Callback -> String
callbackFnName = externalNameToCpp . callbackExtName

-- | Returns a distinct argument variable name for each nonnegative number.
toArgName :: Int -> String
toArgName = ("arg" ++) . show

-- | Same as 'toArgName', but with distinct names, with with similarity between
-- @toArgName n@ and @toArgNameAlt n@.
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
newtype Chunk = Chunk { chunkContents :: String }

-- | Runs a 'Chunk' writer, combining them with 'combineChunks' to form a single
-- string.
runChunkWriter :: Writer [Chunk] a -> (a, String)
runChunkWriter = fmap combineChunks . runWriter

-- | Runs a 'Chunk' writer and returns the monad's value.
evalChunkWriter :: Writer [Chunk] a -> a
evalChunkWriter = fst . runChunkWriter

-- | Runs a 'Chunk' writer and returns the written log.
execChunkWriter :: Writer [Chunk] a -> String
execChunkWriter = snd . runChunkWriter

-- | Runs a 'Chunk' writer transformer, combining them with 'combineChunks' to
-- form a single string.
runChunkWriterT :: Monad m => WriterT [Chunk] m a -> m (a, String)
runChunkWriterT = liftM (fmap combineChunks) . runWriterT

-- | Runs a 'Chunk' writer transformer and returns the monad's value.
evalChunkWriterT :: Monad m => WriterT [Chunk] m a -> m a
evalChunkWriterT = liftM fst . runChunkWriterT

-- | Runs a 'Chunk' writer transformer and returns the written log.
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

-- | Emits an 'Identifier'.
sayIdentifier :: MonadWriter [Chunk] m => Identifier -> m ()
sayIdentifier =
  sequence_ . intersperse (say "::") . map renderPart . identifierParts
  where renderPart part = do
          say $ idPartBase part
          case idPartArgs part of
            Nothing -> return ()
            Just args -> do
              say "<"
              sequence_ $ intersperse (say ", ") $ map (sayType Nothing) args
              say ">"

-- | @sayVar name maybeParamNames t@ speaks a variable declaration of the form
-- @\<type\> \<name\>@, where @\<name\>@ is the given name, and @\<type\>@ is
-- rendered by giving @maybeParamNames@ and @t@ to 'sayType'.
--
-- This function is useful for generating variable declarations, declarations
-- with assignments, and function prototypes and definitions.
sayVar :: MonadWriter [Chunk] m => String -> Maybe [String] -> Type -> m ()
sayVar name maybeParamNames t = sayType' t maybeParamNames topPrecedence $ say name

-- | @sayType maybeParamNames t@ renders @t@ in C++ syntax.  If @t@ is a 'TFn',
-- then @maybeParamNames@ will provide variable names for parameters, if
-- present.
sayType :: MonadWriter [Chunk] m => Maybe [String] -> Type -> m ()
sayType maybeParamNames t = sayType' t maybeParamNames topPrecedence $ return ()

-- | Implementation of 'sayType', deals with recursion, precedence, and the
-- inside-out style of C++ type syntax.
sayType' :: MonadWriter [Chunk] m => Type -> Maybe [String] -> Int -> m () -> m ()
sayType' (normalizeType -> t) maybeParamNames outerPrec unwrappedOuter =
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
    TInt8 -> say "int8_t" >> outer
    TInt16 -> say "int16_t" >> outer
    TInt32 -> say "int32_t" >> outer
    TInt64 -> say "int64_t" >> outer
    TWord8 -> say "uint8_t" >> outer
    TWord16 -> say "uint16_t" >> outer
    TWord32 -> say "uint32_t" >> outer
    TWord64 -> say "uint64_t" >> outer
    TPtrdiff -> say "ptrdiff_t" >> outer
    TSize -> say "size_t" >> outer
    TSSize -> say "ssize_t" >> outer
    TEnum e -> sayIdentifier (enumIdentifier e) >> outer
    TBitspace b -> case bitspaceCppTypeIdentifier b of
      Just identifier -> sayIdentifier identifier >> outer
      Nothing -> sayType' (bitspaceType b) maybeParamNames outerPrec unwrappedOuter
    TPtr t' -> sayType' t' Nothing prec $ say "*" >> outer
    TRef t' -> sayType' t' Nothing prec $ say "&" >> outer
    TFn paramTypes retType -> sayType' retType Nothing prec $ do
      outer
      say "("
      sequence_ $ intersperse (say ", ") $
        flip map (zip paramTypes $ maybe (repeat Nothing) (map Just) maybeParamNames) $
        \(ptype, pname) ->
        sayType' ptype Nothing topPrecedence $ forM_ pname say
      say ")"
    TCallback cb -> says [callbackImplClassName cb, "*"] >> outer
    TObj cls -> sayIdentifier (classIdentifier cls) >> outer
    TObjToHeap cls -> sayType' (TRef $ TConst $ TObj cls) maybeParamNames outerPrec unwrappedOuter
    TToGc t' -> sayType' t' maybeParamNames outerPrec unwrappedOuter
    TConst t' -> sayType' t' maybeParamNames outerPrec $ say "const" >> unwrappedOuter
                 -- TODO ^ Is using the outer stuff correctly here?

topPrecedence :: Int
topPrecedence = 11

typePrecedence :: Type -> Int
typePrecedence t = case t of
  TFn {} -> 10
  TPtr {} -> 9
  TRef {} -> 9
  _ -> 8
