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

-- | Hooks for controlling various parts of generators.
module Foreign.Hoppy.Generator.Hook (
  Hooks (..),
  defaultHooks,
  -- * Enum evaluation
  EnumEvaluator,
  EnumEvaluatorArgs (..),
  EnumEvaluatorEntry (..),
  EnumEvaluatorResult (..),
  evaluateEnumsWithCompiler,
  evaluateEnumsWithDefaultCompiler,
  makeCppSourceToEvaluateEnums,
  interpretOutputToEvaluateEnums,
  -- * Internal
  internalEvaluateEnumsForInterface,
  ) where

import Control.Arrow ((&&&))
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState, execStateT, modify')
import Control.Monad.Writer (execWriter, tell)
import Data.ByteString.Lazy (ByteString, hPut)
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.List (splitAt)
import qualified Data.Map as M
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import qualified Data.Set as S
import Foreign.C (CInt, CLong, CLLong, CUInt, CULong, CULLong)
import Foreign.Hoppy.Generator.Common (doubleQuote, for, fromMaybeM, pluralize)
import Foreign.Hoppy.Generator.Common.Consume (MonadConsume, evalConsume, next)
import Foreign.Hoppy.Generator.Compiler (Compiler, SomeCompiler (SomeCompiler), compileProgram)
import Foreign.Hoppy.Generator.Language.Cpp (renderIdentifier)
import Foreign.Hoppy.Generator.Spec.Base
import Foreign.Hoppy.Generator.Types (intT, llongT, longT, uintT, ullongT, ulongT)
import Foreign.Hoppy.Generator.Util (withTempFile)
import Foreign.Hoppy.Generator.Version (CppVersion (Cpp2011), activeCppVersion)
import Foreign.Storable (Storable, sizeOf)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.IO (hClose, hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | These hooks can be used to customize the behaviour of a Hoppy generator.
data Hooks = Hooks
  { hookEvaluateEnums :: EnumEvaluator
    -- ^ This hook is invoked once for an interface when the generator needs
    -- information about some enums beyond what's been baked into the interface
    -- (for example, to compute the enum's numeric type or entry values, see
    -- 'EvaluatedEnumData').  This will be called at most once per interface per
    -- invocation of the generator.
  }

-- | The default set of hooks associated with an interface.  This sets
-- 'hookEvaluateEnums' to 'evaluateEnumsWithDefaultCompiler'.
defaultHooks :: Hooks
defaultHooks =
  Hooks
  { hookEvaluateEnums = evaluateEnumsWithDefaultCompiler
  }

-- | A function that answers with representation information about an enum (e.g.
-- entries' numeric values) for a given request.  On success, it returns the
-- requested data.  On failure, it prints a message to standard error and
-- returns @Nothing@.
type EnumEvaluator = EnumEvaluatorArgs -> IO (Maybe EnumEvaluatorResult)

-- | Inputs to the process of automatically evaluting enums.
data EnumEvaluatorArgs = EnumEvaluatorArgs
  { enumEvaluatorArgsInterface :: Interface
    -- ^ The interface that enum values are being calculated for.
  , enumEvaluatorArgsReqs :: Reqs
    -- ^ Requirements (includes, etc.) needed to reference the enum identifiers
    -- being evaluated.
  , enumEvaluatorArgsSizeofIdentifiers :: [Identifier]
    -- ^ The list of identifiers that we need to compute sizeof() for.
  , enumEvaluatorArgsEntries :: [EnumEvaluatorEntry]
    -- ^ The list of entries to calculate values for.
  , enumEvaluatorArgsKeepOutputsOnFailure :: Bool
    -- ^ Whether to leave temporary build inputs and outputs on disk in case the
    -- calculation fails.  If failure does occur and this is true, then the
    -- calculation should print to standard error the location of these files
    -- (this is taken care of by the @calculateEnumValues*@ functions here.)
  }

-- | An entry in an enumeration.  This also tracks whether the entry came from a
-- scoped enum, for assertion reasons.
data EnumEvaluatorEntry = EnumEvaluatorEntry
  { enumEvaluatorEntryScoped :: Scoped
    -- ^ Whether the entry comes from a scoped enum.
  , enumEvaluatorEntryIdentifier :: Identifier
    -- ^ The identifier referring to the entry.
  }
  deriving (Eq)

instance Ord EnumEvaluatorEntry where
  compare (EnumEvaluatorEntry _ i1) (EnumEvaluatorEntry _ i2) =
    compare (OrdIdentifier i1) (OrdIdentifier i2)

-- | Raw outputs parsed from the output of an enum evaluator.
data EnumEvaluatorResult = EnumEvaluatorResult
  { enumEvaluatorResultSizes :: ![Int]
    -- ^ The sizeof() for each identifier in 'enumEvaluatorArgsSizeofIdentifiers'.
    -- The lengths of these two lists must match.
  , enumEvaluatorResultValues :: ![Integer]
    -- ^ The numeric value for each identifier in 'enumEvaluatorArgsEntries'.
    -- The lengths of these two lists must match.
  } deriving (Show)

-- | An 'EnumEvaluatorResult' without any data in it.
emptyEnumEvaluatorResult :: EnumEvaluatorResult
emptyEnumEvaluatorResult = EnumEvaluatorResult
  { enumEvaluatorResultSizes = []
  , enumEvaluatorResultValues = []
  }

-- | Calculates enum values using an interface's compiler.
evaluateEnumsWithDefaultCompiler :: EnumEvaluator
evaluateEnumsWithDefaultCompiler args = do
  let iface = enumEvaluatorArgsInterface args
  case interfaceCompiler iface of
    Just (SomeCompiler compiler) -> evaluateEnumsWithCompiler compiler args
    Nothing -> do
      hPutStrLn stderr $
        "evaluateEnumsWithDefaultCompiler: Don't have a compiler to evaluate enums with in " ++
        show iface ++ "."
      return Nothing

-- | Evaluate enums using a specified compiler.
evaluateEnumsWithCompiler :: Compiler a => a -> EnumEvaluator
evaluateEnumsWithCompiler compiler args =
  withTempFile "hoppy-enum.cpp" removeBuildFailures $ \cppPath cppHandle ->
  withTempFile "hoppy-enum" removeBuildFailures $ \binPath binHandle -> do
  hPut cppHandle program
  hClose cppHandle
  hClose binHandle
  success <- compileProgram compiler cppPath binPath
  result <- case success of
    False -> do
      hPutStrLn stderr $
        "evaluateEnumsWithCompiler: Failed to build program " ++ show cppPath ++
        " to evaluate enums with " ++ show compiler ++ "." ++ removeBuildFailuresNote
      return Nothing
    True -> runAndGetOutput binPath
  let remove = isJust result || removeBuildFailures
  return (remove, (remove, result))

  where removeBuildFailures = not $ enumEvaluatorArgsKeepOutputsOnFailure args

        removeBuildFailuresNote =
          if removeBuildFailures
          then "  Pass --keep-temp-outputs-on-failure to keep build outputs around for debugging."
          else "  --keep-temp-outputs-on-failure was given, leaving files on disk."

        program = makeCppSourceToEvaluateEnums args

        runAndGetOutput :: FilePath -> IO (Maybe EnumEvaluatorResult)
        runAndGetOutput binPath = do
          result <- runExceptT $ do
            (exitCode, out, err) <- liftIO $ readProcessWithExitCode binPath [] ""
            case exitCode of
              ExitFailure code ->
                throwError $
                "evaluateEnumsWithCompiler: Failed to run binary " ++ show binPath ++
                ", code = " ++ show code ++ ", stdout = <<<" ++ out ++ ">>>, stderr = <<<" ++
                err ++ ">>>." ++ removeBuildFailuresNote
              ExitSuccess ->
                ExceptT $ return $ interpretOutputToEvaluateEnums args out

          case result of
            Right value -> return $ Just value
            Left err -> do
              hPutStrLn stderr err
              return Nothing

-- | Constructs the C++ source program to evaluate enums.
makeCppSourceToEvaluateEnums :: EnumEvaluatorArgs -> ByteString
makeCppSourceToEvaluateEnums args =
  toLazyByteString $ stringUtf8 $ unlines $
  [ "#include <iostream>"
  ] ++
  (if any isEntryScoped $ enumEvaluatorArgsEntries args
    then [ "#include <type_traits>" ]  -- We've asserted that we have C++11 in this case.
    else []) ++
  [ ""
  ] ++ [concatMap includeToString $
        S.elems $ reqsIncludes $ enumEvaluatorArgsReqs args] ++
  [ ""
  , "int main() {"
  , "  std::cout << \"#sizes\\n\";"
  ] ++ for (enumEvaluatorArgsSizeofIdentifiers args)
       (\identifier ->
         let rendered = renderIdentifier identifier
         in "  std::cout << sizeof(" ++ rendered ++ ") << ' ' << " ++
            doubleQuote rendered ++ " << '\\n';") ++
  [ "  std::cout << \"#values\\n\";"
  ] ++ for (enumEvaluatorArgsEntries args)
       (\(EnumEvaluatorEntry scoped identifier) ->
         let rendered = renderIdentifier identifier
             numericExpr = case scoped of
               Unscoped -> rendered
               Scoped ->
                 "static_cast<std::underlying_type<decltype(" ++ rendered ++ ")>::type>(" ++
                 rendered ++ ")"
         in "  std::cout << (" ++ numericExpr ++ ") << ' ' << " ++
            doubleQuote rendered ++ " << '\\n';") ++
  [ ""
  , "  return 0;"
  , "}"
  ]

-- | Interprets the output of a program generated by
-- 'makeCppSourceToEvaluateEnums', returning parsed values if successful, and an
-- error string otherwise.
interpretOutputToEvaluateEnums ::
  EnumEvaluatorArgs
  -> String
  -> Either String EnumEvaluatorResult
interpretOutputToEvaluateEnums args out =
  evalConsume (lines out) $ runExceptT $ flip execStateT emptyEnumEvaluatorResult $ do
  expectLine "#sizes"
  readSizes $ enumEvaluatorArgsSizeofIdentifiers args
  expectLine "#values"
  readValues $ map (\(EnumEvaluatorEntry _ i) -> i) $ enumEvaluatorArgsEntries args
  expectEof
  modify' $ \EnumEvaluatorResult
             { enumEvaluatorResultSizes = sizes
             , enumEvaluatorResultValues = values
             } ->
    EnumEvaluatorResult
    { enumEvaluatorResultSizes = reverse sizes
    , enumEvaluatorResultValues = reverse values
    }
  where expectEof :: (MonadConsume String m, MonadError String m) => m ()
        expectEof = next >>= \case
          Nothing -> return ()
          Just line -> throwError $ "Expected EOF, got " ++ show line ++ "."

        expectLine :: (MonadConsume String m, MonadError String m) => String -> m ()
        expectLine expected = do
          line <- next
          when (line /= Just expected) $
            throwError $ "Expected " ++ show expected ++ ", got " ++ show line ++ "."

        expectIdentifier :: (MonadError String m, Read a) => Identifier -> String -> m a
        expectIdentifier identifier line = case reads line of
          [(value, ' ':identStr)] -> do
            let expectedStr = renderIdentifier identifier
            unless (identStr == expectedStr) $
              throwError $ "Expected identifier " ++ show expectedStr ++ ", but saw identifier " ++
              show identStr ++ "."
            return value
          _ ->
            throwError $ "Expected a line for " ++ show identifier ++ ", but got line " ++
            show line ++ "."

        readSizes :: (MonadConsume String m, MonadError String m, MonadState EnumEvaluatorResult m)
                  => [Identifier]
                  -> m ()
        readSizes expectedIdentifiers = case expectedIdentifiers of
          [] -> return ()
          expectedIdentifier:restIdentifiers -> next >>= \case
            Just line -> do
              size <- expectIdentifier expectedIdentifier line
              modify' $ \r@EnumEvaluatorResult { enumEvaluatorResultSizes = sizes } ->
                r { enumEvaluatorResultSizes = size:sizes }
              readSizes restIdentifiers
            Nothing -> throwError "Unexpected end of input while reading enum sizes."

        readValues :: (MonadConsume String m, MonadError String m, MonadState EnumEvaluatorResult m)
                   => [Identifier]
                   -> m ()
        readValues expectedIdentifiers = case expectedIdentifiers of
          [] -> return ()
          expectedIdentifier:restIdentifiers -> next >>= \case
            Just line -> do
              value <- expectIdentifier expectedIdentifier line
              modify' $ \r@EnumEvaluatorResult { enumEvaluatorResultValues = values } ->
                r { enumEvaluatorResultValues = value:values }
              readValues restIdentifiers
            Nothing -> throwError "Unexpected end of input while reading enum sizes."

-- | Collects all of the enum values that need calculating in an interface, runs
-- the hook to evaluate them, and stores the result in the interface.  This
-- won't recalculate enum data if it's already been calculated.
internalEvaluateEnumsForInterface :: Interface -> Bool -> IO Interface
internalEvaluateEnumsForInterface iface keepBuildFailures =
  case interfaceEvaluatedEnumData iface of
    Just _ -> return iface
    Nothing -> internalEvaluateEnumsForInterface' iface keepBuildFailures

internalEvaluateEnumsForInterface' :: Interface -> Bool -> IO Interface
internalEvaluateEnumsForInterface' iface keepBuildFailures = do
  let validateEnumTypes = interfaceValidateEnumTypes iface

      -- Collect all exports in the interface.
      allExports :: M.Map ExtName Export
      allExports = M.unions $ map moduleExports $ M.elems $ interfaceModules iface

      -- Collect pertinent information about all enum exports that we need to
      -- evaluate.
      --
      -- ExtName: The name of the enum.
      -- Maybe Type: The enum's numeric type, if explicitly set.
      -- Maybe Identifier: The enum's identifier, if we need to evaluate it.
      -- Reqs: Requirements to reference the enum.
      -- EnumValueMap: Entries, so that we can evaluate the auto ones.
      enumExports :: [(ExtName, Maybe Type, Scoped, Maybe Identifier, Reqs, EnumValueMap)]
      enumExports = flip mapMaybe (M.elems allExports) $ \export ->
        flip fmap (getExportEnumInfo export) $ \(info :: EnumInfo) ->
          (enumInfoExtName info,
           enumInfoNumericType info,
           enumInfoScoped info,
           case (enumInfoNumericType info, validateEnumTypes) of
             (Just _, False) -> Nothing  -- Don't need to evaluate sizeof().
             _ -> Just $ enumInfoIdentifier info,  -- Need to evaluate sizeof().
           enumInfoReqs info,
           enumInfoValues info)

      -- Determine a list of all values to evaluate, and the Reqs required to do
      -- so.
      sumReqs :: Reqs
      sizeofIdentifiersToEvaluate :: [OrdIdentifier]
      entriesToEvaluate :: [EnumEvaluatorEntry]
      (sumReqs, sizeofIdentifiersToEvaluate, entriesToEvaluate) =
        -- Deduplicate entries by passing them through a set.
        (\(a, b, c) -> (a, b, S.toList $ S.fromList c)) $
        execWriter $ forM_ enumExports $ \(_, _, scoped, maybeIdent, reqs, entries) -> do
          tell (reqs,
                maybe [] (\i -> [OrdIdentifier i]) maybeIdent,
                [])
          forM_ (M.toList $ enumValueMapValues entries) $ \(_, value) -> case value of
            EnumValueManual _ -> return ()
            EnumValueAuto identifier -> tell (mempty, [], [EnumEvaluatorEntry scoped identifier])

  -- We currently only support evaluation of scoped enum entries in C++11
  -- and later, because we use std::underlying_type to perform the
  -- conversion of those entries to integral types, rather than doing
  -- e.g. two compilations, first determining their size.
  when (activeCppVersion < Cpp2011) $ do
    let scopedEnumsWithAutoEntries :: [ExtName] = flip mapMaybe enumExports $
          \(extName, _, _, _, _, entries) ->
            if any isAuto $ M.elems $ enumValueMapValues entries
            then Just extName
            else Nothing
        (namesToShow, namesToSkip) = splitAt 10 scopedEnumsWithAutoEntries
    unless (null scopedEnumsWithAutoEntries) $ do
      hPutStrLn stderr $
        "internalEvaluateEnumsForInterface': Automatic evaluation of enum values is not " ++
        "requires at least " ++ show Cpp2011 ++ ", but we are compiling for " ++
        show activeCppVersion ++ ", aborting.  Enums requesting evaluation are " ++
        show namesToShow ++
        (if not $ null namesToSkip then " (and more)" else "") ++ "."
      exitFailure

  -- Evaluate the identifiers we are curious about, using the hook provided by
  -- the interface.
  evaluatorResult :: EnumEvaluatorResult <-
    case (sizeofIdentifiersToEvaluate, entriesToEvaluate) of
      ([], []) -> return emptyEnumEvaluatorResult
      _ -> do
        let hooks = interfaceHooks iface
            args = EnumEvaluatorArgs
                   { enumEvaluatorArgsInterface = iface
                   , enumEvaluatorArgsReqs = sumReqs
                   , enumEvaluatorArgsSizeofIdentifiers =
                       map ordIdentifier sizeofIdentifiersToEvaluate
                   , enumEvaluatorArgsEntries = entriesToEvaluate
                   , enumEvaluatorArgsKeepOutputsOnFailure = keepBuildFailures
                   }
        hookEvaluateEnums hooks args >>=
          fromMaybeM
          (do hPutStrLn stderr $
                "internalEvaluateEnumsForInterface': Failed to build and run program.  Aborting."
              exitFailure)

  let entryIdentifiersToEvaluate :: [OrdIdentifier]
      entryIdentifiersToEvaluate =
        map (OrdIdentifier . enumEvaluatorEntryIdentifier) entriesToEvaluate

      evaluatedIdentifierSizes :: M.Map OrdIdentifier Int
      evaluatedIdentifierSizes =
        M.fromList $ zip sizeofIdentifiersToEvaluate $ enumEvaluatorResultSizes evaluatorResult

      evaluatedIdentifierValues :: M.Map OrdIdentifier Integer
      evaluatedIdentifierValues =
        M.fromList $ zip entryIdentifiersToEvaluate $ enumEvaluatorResultValues evaluatorResult

      getIdentifierSize :: Identifier -> IO Int
      getIdentifierSize identifier =
        fromMaybeM
          (do hPutStrLn stderr $
                "internalEvaluateEnumsForInterface': Internal error, " ++
                "failed to find evaluated size for " ++ show identifier ++ "."
              exitFailure) $
          M.lookup (OrdIdentifier identifier) evaluatedIdentifierSizes

      getIdentifierValue :: Identifier -> IO Integer
      getIdentifierValue identifier =
        fromMaybeM
          (do hPutStrLn stderr $
                "internalEvaluateEnumsForInterface': Internal error, " ++
                "failed to find evaluated value for " ++ show identifier ++ "."
              exitFailure) $
          M.lookup (OrdIdentifier identifier) evaluatedIdentifierValues

      getNumericTypeInfo :: ExtName -> Type -> IO NumericTypeInfo
      getNumericTypeInfo extName t =
        fromMaybeM
          (do hPutStrLn stderr $
                "internalEvaluateEnumsForInterface': Explicit type " ++ show t ++
                " for enum " ++ show extName ++ " is not a usable numeric type."
              exitFailure) $
        findNumericTypeInfo t

  -- Build a map containing the evaluated type and numeric values for all of
  -- the enums in the interface.
  evaluatedDataMap :: M.Map ExtName EvaluatedEnumData <-
    fmap M.fromList $ forM enumExports $
    \(extName, maybeNumericType, _, maybeIdent, _, values) -> do
      -- Build a map containing all of the numeric values in the enum.
      numMap :: M.Map [String] Integer <-
        fmap M.fromList $ forM (M.toList $ enumValueMapValues values) $ \(label, value) -> do
          num <- case value of
            EnumValueManual n -> return n
            EnumValueAuto entryIdent -> getIdentifierValue entryIdent
          return (label, num)

      -- Determine the bounds for those values, and use those to select a
      -- numeric type that we should use outside of C++ to represent the enum's
      -- values.  C++ doesn't give us a way to ask for the numeric type it uses
      -- directly, so we manually pick a numeric type of the right size that can
      -- handle everything.
      bytes <- case (maybeNumericType, maybeIdent) of
        (Just numericType, Just identifier) -> do
          providedBytes <- numBytes <$> getNumericTypeInfo extName numericType
          evaluatedBytes <- getIdentifierSize identifier

          -- Verify that the explicit numeric type set on the enum is correct to
          -- use.
          when (providedBytes /= evaluatedBytes) $ do
            hPutStrLn stderr $
              "internalEvaluateEnumsForInterface': The explicit type " ++ show numericType ++
              " for enum " ++ show extName ++ " takes " ++ pluralize providedBytes "byte" "bytes" ++
              ", but sizeof(" ++ renderIdentifier identifier ++ ") evaluates to " ++
              pluralize evaluatedBytes "byte" "bytes" ++ "."
            exitFailure

          return providedBytes

        (Just numericType, Nothing) -> numBytes <$> getNumericTypeInfo extName numericType

        (Nothing, Just identifier) -> getIdentifierSize identifier

        (Nothing, Nothing) ->
          error $ "internalEvaluateEnumsForInterface': Internal error, don't have a size for " ++
          "enum " ++ show extName ++ ", shouldn't happen."

      let (low, high) = minimum &&& maximum $ M.elems numMap
      numericType <-
        fromMaybeM
          (do hPutStrLn stderr $
                "internalEvaluateEnumsForInterface': Couldn't find a numeric type " ++
                "to use to represent the C++ enumeration " ++ show extName ++ "."
              exitFailure) $
        pickNumericType bytes low high

      let result = EvaluatedEnumData
            { evaluatedEnumType = numericType
            , evaluatedEnumValueMap = numMap
            }
      return (extName, result)

  return iface { interfaceEvaluatedEnumData = Just evaluatedDataMap }

newtype OrdIdentifier = OrdIdentifier { ordIdentifier :: Identifier }
  deriving (Eq, Show)

instance Ord OrdIdentifier where
  compare (OrdIdentifier i1) (OrdIdentifier i2) =
    compare (renderIdentifier i1) (renderIdentifier i2)

-- | Bound information about numeric types.
data NumericTypeInfo = NumericTypeInfo
  { numType :: Type
  , numBytes :: Int
  , numMinBound :: Integer
  , numMaxBound :: Integer
  }

-- | Numeric types usable to hold enum values.  These are ordered by decreasing
-- precedence (increasing word size).
numericTypeInfo :: [NumericTypeInfo]
numericTypeInfo =
  [ mk intT (undefined :: CInt)
  , mk uintT (undefined :: CUInt)
  , mk longT (undefined :: CLong)
  , mk ulongT (undefined :: CULong)
  , mk llongT (undefined :: CLLong)
  , mk ullongT (undefined :: CULLong)
  ]
  where mk :: forall a. (Bounded a, Integral a, Storable a) => Type -> a -> NumericTypeInfo
        mk t _ = NumericTypeInfo
                 { numType = t
                 , numBytes = sizeOf (undefined :: a)
                 , numMinBound = toInteger (minBound :: a)
                 , numMaxBound = toInteger (maxBound :: a)
                 }

findNumericTypeInfo :: Type -> Maybe NumericTypeInfo
findNumericTypeInfo t = listToMaybe $ filter (\i -> numType i == t) numericTypeInfo

-- | Selects the preferred numeric type for holding numeric values in the given
-- range.
pickNumericType :: Int -> Integer -> Integer -> Maybe Type
pickNumericType bytes low high =
  fmap numType $ listToMaybe $ flip filter numericTypeInfo $ \info ->
  numBytes info == bytes &&
  numMinBound info <= low &&
  numMaxBound info >= high

isAuto :: EnumValue -> Bool
isAuto (EnumValueAuto _) = True
isAuto (EnumValueManual _) = False

isEntryScoped :: EnumEvaluatorEntry -> Bool
isEntryScoped (EnumEvaluatorEntry scoped _) = isScoped scoped
