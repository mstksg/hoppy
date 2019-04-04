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

{-# LANGUAGE ViewPatterns #-}

-- | Shared portion of the C++ code generator.  Usable by binding definitions.
module Foreign.Hoppy.Generator.Language.Cpp (
  -- * Code generation monad
  Generator,
  Env,
  execGenerator,
  addIncludes, addInclude, addReqsM,
  askInterface, askModule, abort,
  -- * Names
  makeCppName,
  externalNameToCpp,
  toArgName,
  toArgNameAlt,
  exceptionIdArgName,
  exceptionPtrArgName,
  exceptionVarName,
  exceptionRethrowFnName,
  -- * Token rendering
  Chunk (..),
  codeChunk,
  includesChunk,
  runChunkWriter,
  evalChunkWriter,
  execChunkWriter,
  runChunkWriterT,
  evalChunkWriterT,
  execChunkWriterT,
  -- * High-level code generation
  SayExportMode (..),
  say,
  says,
  sayIdentifier,
  renderIdentifier,
  sayVar,
  sayType,
  sayFunction,
  -- * Auxiliary functions
  typeToCType,
  typeReqs,
  findExportModule,
  getEffectiveExceptionHandlers,
  ) where

import Control.Monad (unless)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Writer (MonadWriter, Writer, WriterT, runWriter, runWriterT, tell)
import Control.Monad.Trans (lift)
import Data.Foldable (forM_)
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import qualified Data.Set as S
import Foreign.Hoppy.Generator.Common
import Foreign.Hoppy.Generator.Spec.Base
import {-# SOURCE #-} Foreign.Hoppy.Generator.Spec.Class (classIdentifier, classReqs)
import Foreign.Hoppy.Generator.Types

-- TODO Wow let's make this not a type synonym.
type Generator = ReaderT Env (WriterT [Chunk] (Either ErrorMsg))

data Env = Env
  { envInterface :: Interface
  , envModule :: Module
  }

execGenerator :: Interface -> Module -> Maybe String -> Generator a -> Either ErrorMsg String
execGenerator interface m maybeHeaderGuardName action = do
  chunk <- execChunkWriterT $ runReaderT action $ Env interface m
  let contents = chunkContents chunk
      includes = chunkIncludes chunk
  return $ chunkContents $ execChunkWriter $ do
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

addIncludes :: MonadWriter [Chunk] m => [Include] -> m ()
addIncludes = tell . (:[]) . includesChunk . S.fromList

addInclude :: MonadWriter [Chunk] m => Include -> m ()
addInclude = addIncludes . (:[])

-- Have to call this addReqsM, addReqs is taken by HasReqs.
addReqsM :: MonadWriter [Chunk] m => Reqs -> m ()
addReqsM = tell . (:[]) . includesChunk . reqsIncludes

askInterface :: MonadReader Env m => m Interface
askInterface = fmap envInterface ask

askModule :: MonadReader Env m => m Module
askModule = fmap envModule ask

-- | Halts generation and returns the given error message.
abort :: ErrorMsg -> Generator a
abort = lift . lift . Left

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

-- | Returns a distinct argument variable name for each nonnegative number.
toArgName :: Int -> String
toArgName = ("arg" ++) . show

-- | Same as 'toArgName', but with distinct names, with with similarity between
-- @toArgName n@ and @toArgNameAlt n@.
toArgNameAlt :: Int -> String
toArgNameAlt n = "arg" ++ show n ++ "_"

-- | The C++ variable name to use for the exception ID argument in a gateway
-- function.
exceptionIdArgName :: String
exceptionIdArgName = "excId"

-- | The C++ variable name to use for the exception pointer argument in a
-- gateway function.
exceptionPtrArgName :: String
exceptionPtrArgName = "excPtr"

-- | The C++ variable name to use in a @catch@ statement in a gateway function.
exceptionVarName :: String
exceptionVarName = "exc_"

-- | The name of the C++ function that receives an exception from a foreign
-- language and throws it in C++.
exceptionRethrowFnName :: String
exceptionRethrowFnName = "genthrow"

-- TODO Fixme, this is most likely backwards, it should be a finite set of
-- non-identifier chars.  Also (maybe) share some logic with the toExtName
-- requirements?
isIdentifierChar :: Char -> Bool
isIdentifierChar = (`elem` identifierChars)

identifierChars :: String
identifierChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"

-- | A chunk is a string that contains an arbitrary portion of C++ code,
-- together with a set of includes.  The only requirement is that chunk's code
-- boundaries are also C++ token boundaries, because the generator monad
-- automates the process of inserting whitespace between chunk boundaries where
-- necessary.
data Chunk = Chunk
  { chunkContents :: !String
  , chunkIncludes :: !(S.Set Include)
  }

-- | Builds a 'Chunk' that contains the given code string.
codeChunk :: String -> Chunk
codeChunk code =
  Chunk
  { chunkContents = code
  , chunkIncludes = S.empty
  }

-- | Builds a 'Chunk' that contains the given includes.
includesChunk :: S.Set Include -> Chunk
includesChunk includes =
  Chunk
  { chunkContents = ""
  , chunkIncludes = includes
  }

-- | Runs a 'Chunk' writer, combining them with 'combineChunks' to form a single
-- string.
runChunkWriter :: Writer [Chunk] a -> (a, Chunk)
runChunkWriter = fmap combineChunks . runWriter

-- | Runs a 'Chunk' writer and returns the monad's value.
evalChunkWriter :: Writer [Chunk] a -> a
evalChunkWriter = fst . runChunkWriter

-- | Runs a 'Chunk' writer and returns the written log.
execChunkWriter :: Writer [Chunk] a -> Chunk
execChunkWriter = snd . runChunkWriter

-- | Runs a 'Chunk' writer transformer, combining them with 'combineChunks' to
-- form a single string.
runChunkWriterT :: Monad m => WriterT [Chunk] m a -> m (a, Chunk)
runChunkWriterT = fmap (fmap combineChunks) . runWriterT

-- | Runs a 'Chunk' writer transformer and returns the monad's value.
evalChunkWriterT :: Monad m => WriterT [Chunk] m a -> m a
evalChunkWriterT = fmap fst . runChunkWriterT

-- | Runs a 'Chunk' writer transformer and returns the written log.
execChunkWriterT :: Monad m => WriterT [Chunk] m a -> m Chunk
execChunkWriterT = fmap snd . runChunkWriterT

-- | Flattens a list of chunks down into a single chunk.  Inserts spaces
-- between chunks where the ends of adjacent chunks would otherwise merge into a
-- single C++ token.  Combines include sets into a single include set.
combineChunks :: [Chunk] -> Chunk
combineChunks chunks =
  let strs = map chunkContents chunks
  in Chunk
     { chunkContents =
         concat $ for (zip ("":strs) strs) $ \(prev, cur) ->
           let needsSpace =
                 not (null prev) && not (null cur) &&
                 (let a = last prev
                      b = head cur
                  in -- "intconstx" should become "int const x"
                     isIdentifierChar a && isIdentifierChar b ||
                     -- Adjacent template parameter '>'s need spacing in old C++.
                     a == '>' && b == '>')
           in if needsSpace then ' ':cur else cur

     , chunkIncludes = S.unions $ map chunkIncludes chunks
     }

-- | The section of code that Hoppy is generating, for an export.
data SayExportMode =
    SaySource
    -- ^ Hoppy is generating the C++ source file for a module.  The generator
    -- should emit C++ definitions that will be imported over foreign language's
    -- FFIs.  This is the main place for code generation in C++ bindings.
  | SayHeader
    -- ^ Hoppy is generating the C++ header file for a module.  The generator
    -- should emit C++ declarations that can be @#include@d during the source
    -- file generation of other exportable entities, in order to refer to the
    -- current entity.  If it is not possible for other entities to refer to
    -- this one, then nothing needs to be generated.

-- | Emits a single 'Chunk'.
say :: MonadWriter [Chunk] m => String -> m ()
say = tell . (:[]) . codeChunk

-- | Emits a 'Chunk' for each string in a list.
says :: MonadWriter [Chunk] m => [String] -> m ()
says = tell . map codeChunk

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

-- | Renders an 'Identifier' to a string.
renderIdentifier :: Identifier -> String
renderIdentifier = chunkContents . execChunkWriter . sayIdentifier

-- | @sayVar name maybeParamNames t@ speaks a variable declaration of the form
-- @\<type\> \<name\>@, where @\<name\>@ is the given name, and @\<type\>@ is
-- rendered by giving @maybeParamNames@ and @t@ to 'sayType'.
--
-- This function is useful for generating variable declarations, declarations
-- with assignments, and function prototypes and definitions.
sayVar :: MonadWriter [Chunk] m => String -> Maybe [String] -> Type -> m ()
sayVar name maybeParamNames t = sayType' t maybeParamNames topPrecedence $ say name

-- | @sayType maybeParamNames t@ renders @t@ in C++ syntax.  If @t@ is a
-- 'fnT', then @maybeParamNames@ will provide variable names for parameters, if
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
    Internal_TVoid -> say "void" >> outer
    Internal_TPtr t' -> sayType' t' Nothing prec $ say "*" >> outer
    Internal_TRef t' -> sayType' t' Nothing prec $ say "&" >> outer
    Internal_TFn params retType -> sayType' retType Nothing prec $ do
      outer
      say "("
      sequence_ $ intersperse (say ", ") $
        for (zip params $ maybe (repeat Nothing) (map Just) $ maybeParamNames) $
        \(param, pname) ->
        sayType' (parameterType param) Nothing topPrecedence $ forM_ pname say
      say ")"
    Internal_TObj cls -> sayIdentifier (classIdentifier cls) >> outer
    Internal_TObjToHeap cls ->
      sayType' (refT $ constT $ objT cls) maybeParamNames outerPrec unwrappedOuter
    Internal_TToGc t' -> sayType' t' maybeParamNames outerPrec unwrappedOuter
    Internal_TManual s -> say (conversionSpecCppName $ conversionSpecCpp s) >> outer
    Internal_TConst t' -> sayType' t' maybeParamNames outerPrec $ say "const" >> unwrappedOuter
                 -- TODO ^ Is using the outer stuff correctly here?

topPrecedence :: Int
topPrecedence = 11

typePrecedence :: Type -> Int
typePrecedence t = case t of
  Internal_TFn {} -> 10
  Internal_TPtr {} -> 9
  Internal_TRef {} -> 9
  _ -> 8

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

-- | Returns a 'Type' iff there is a C type distinct from the given C++ type
-- that should be used for conversion.
--
-- This returns @Nothing@ for 'Internal_TManual'.  TManual needs special
-- handling.
typeToCType :: Type -> Generator (Maybe Type)
typeToCType t = case t of
  Internal_TRef t' -> return $ Just $ ptrT t'
  Internal_TObj _ -> return $ Just $ ptrT $ constT t
  Internal_TObjToHeap cls -> return $ Just $ ptrT $ objT cls
  Internal_TToGc t'@(Internal_TObj _) -> return $ Just $ ptrT t'
  Internal_TToGc t' -> typeToCType t'
  Internal_TConst t' -> typeToCType t'
  Internal_TManual s -> conversionSpecCppConversionType $ conversionSpecCpp s
  _ -> return Nothing

typeReqs :: Type -> Generator Reqs
typeReqs t = case t of
  Internal_TVoid -> return mempty
  Internal_TPtr t' -> typeReqs t'
  Internal_TRef t' -> typeReqs t'
  Internal_TFn params retType ->
    -- TODO Is the right 'ReqsType' being used recursively here?
    mconcat <$> mapM typeReqs (retType : map parameterType params)
  Internal_TObj cls -> return $ classReqs cls
  Internal_TObjToHeap cls -> return $ classReqs cls
  Internal_TToGc t' -> typeReqs t'
  Internal_TConst t' -> typeReqs t'
  Internal_TManual s -> conversionSpecCppReqs $ conversionSpecCpp s

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
