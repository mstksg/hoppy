-- This file is part of Hoppy.
--
-- Copyright 2015-2022 Bryan Gardiner <bog@khumba.net>
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

-- | Interface for defining bindings to C++ enumerations.
--
-- In generated Haskell code:
--
-- An enum gets a single algebraic data type with data constructors for each of
-- the values defined in the interface.  If the enum has an unknown value name
-- defined, then an additional data constructor is generated that holds a
-- numeric value, and this constructor is used whenever numeric values for which
-- no name is explicitly defined are encountered (otherwise, 'error' is called).
--
-- From the runtime module, a @CppEnum@ instance is generated for the type, and
-- if the enum is declared to permit bit operations, then a 'Data.Bits.Bits'
-- instance is also generated.  'Eq' and 'Ord' instances are generated that
-- compare numeric values.
module Foreign.Hoppy.Generator.Spec.Enum (
  -- * Data type
  CppEnum, enumT,
  -- * Construction
  makeEnum, makeAutoEnum, IsAutoEnumValue (..),
  -- * Properties
  enumExtName,
  enumIdentifier,
  enumNumericType, enumSetNumericType,
  enumValues,
  enumReqs,
  enumAddendum,
  enumValuePrefix, enumSetValuePrefix,
  enumAddEntryNameOverrides,
  enumGetOverriddenEntryName,
  IsEnumUnknownValueEntry (..),
  enumUnknownValueEntry, enumSetUnknownValueEntry, enumSetNoUnknownValueEntry,
  enumUnknownValueEntryDefault,
  enumHasBitOperations, enumSetHasBitOperations,
  -- * C++ generator
  cppGetEvaluatedEnumData,
  -- * Haskell generator
  hsGetEvaluatedEnumData,
  -- ** Names
  toHsEnumTypeName, toHsEnumTypeName',
  toHsEnumCtorName, toHsEnumCtorName',
  ) where

import Control.Arrow ((&&&), (***))
import Control.Monad (forM, forM_, when)
import Control.Monad.Except (throwError)
import Data.Function (on)
import qualified Data.Map as M
import Foreign.Hoppy.Generator.Common (butLast, capitalize, for)
import Foreign.Hoppy.Generator.Spec.Base
import Foreign.Hoppy.Generator.Spec.Computed (
  EvaluatedEnumData,
  evaluatedEnumNumericType,
  evaluatedEnumValueMap,
  getEvaluatedEnumData,
  numType,
  )
import qualified Foreign.Hoppy.Generator.Language.Cpp as LC
import qualified Foreign.Hoppy.Generator.Language.Haskell as LH
import Foreign.Hoppy.Generator.Override (addOverrideMap, overriddenMapLookup, plainMap)
import Foreign.Hoppy.Generator.Types (manualT)
import Foreign.Hoppy.Generator.Util (splitIntoWords)
import GHC.Stack (HasCallStack)
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

-- | A C++ enum declaration.
--
-- See 'Foreign.Hoppy.Generator.Spec.EnumInfo'.
data CppEnum = CppEnum
  { enumExtName :: ExtName
    -- ^ The enum's external name.
  , enumIdentifier :: Identifier
    -- ^ The identifier used to refer to the enum.
  , enumNumericType :: Maybe Type
    -- ^ An optional, explicit numeric type provided for the enum's values, that
    -- matches what the C++ compiler uses.  Hoppy will use
    -- 'Foreign.Hoppy.Generator.Hook.Hooks' to compute this automatically, if
    -- not given manually.  This does not need to be provided.  If absent
    -- (default), then Hoppy will calculate the enum's numeric type on its own,
    -- using a C++ compiler.  If this is present however, Hoppy will use it, and
    -- additionally validate it against what the C++ compiler thinks, if
    -- validation is enabled (see 'interfaceValidateEnumTypes').
  , enumScoped :: Scoped
    -- ^ Whether the enum is scoped or unscoped.
  , enumValues :: EnumValueMap
    -- ^ The numeric values and names of the enum entires.
  , enumReqs :: Reqs
    -- ^ Requirements for bindings to access this enum.  Currently unused, but
    -- will be in the future.
  , enumAddendum :: Addendum
    -- ^ The enum's addendum.
  , enumValuePrefix :: String
    -- ^ The prefix applied to value names ('enumValues') when determining the
    -- names of values in foreign languages.  This defaults to the external name
    -- of the enum, plus an underscore.
    --
    -- See 'enumSetValuePrefix'.
  , enumUnknownValueEntry :: Maybe EnumEntryWords
    -- ^ A name (a list of words, a la the fields in 'EnumValueMap') for an
    -- optional fallback enum "entry" in generated bindings for holding unknown
    -- values.  See 'enumUnknownValueEntryDefault'.
    --
    -- When this is a @Just@, then the generated foreign binding gets an extra
    -- entry that takes an argument holding an arbitrary numeric value (an extra
    -- data constructor in Haskell), and this value is used whenever an unknown
    -- value is seen.
    --
    -- When this is @Nothing@, the enum will not support unknown values.
    -- @toCppEnum@ in the @Foreign.Hoppy.Runtime.CppEnum@ typeclass, as well as
    -- calls or returns from C++ that pass a value not defined in the interface,
    -- will raise an 'error'.
    --
    -- Enums that have this set to @Nothing@ should also have
    -- 'enumHasBitOperations' set to false, to avoid potential errors at
    -- runtime; see that function's documentation.
    --
    -- The 'enumValuePrefix' applies to this name, just as it does to other enum
    -- entries.
  , enumHasBitOperations :: Bool
    -- ^ Whether generated bindings should support bitwise operations on the
    -- enum.  This defaults to true.
    --
    -- It is not recommended to disable the unknown value entry
    -- ('enumUnknownValueEntry') while having this be true, because any
    -- computation involving enum values not explicitly defined will cause a
    -- runtime error.  This includes undefined combinations of defined values.
  }

instance Eq CppEnum where
  (==) = (==) `on` enumExtName

instance Show CppEnum where
  show e = concat ["<Enum ", show (enumExtName e), " ", show (enumIdentifier e), ">"]

instance Exportable CppEnum where
  sayExportCpp _ _ = return ()  -- Nothing to do for the C++ side of an enum.

  sayExportHaskell = sayHsExport

  getExportEnumInfo e =
    Just EnumInfo
    { enumInfoExtName = enumExtName e
    , enumInfoIdentifier = enumIdentifier e
    , enumInfoNumericType = enumNumericType e
    , enumInfoReqs = enumReqs e
    , enumInfoScoped = enumScoped e
    , enumInfoValues = enumValues e
    }

instance HasExtNames CppEnum where
  getPrimaryExtName = enumExtName

instance HasReqs CppEnum where
  getReqs = enumReqs
  setReqs reqs e = e { enumReqs = reqs }

instance HasAddendum CppEnum where
  getAddendum = enumAddendum
  setAddendum addendum e = e { enumAddendum = addendum }

-- | Sets an explicit numeric type for the enum.  See 'enumNumericType'.
enumSetNumericType :: Maybe Type -> CppEnum -> CppEnum
enumSetNumericType maybeType enum = enum { enumNumericType = maybeType }

-- | The default value for 'enumUnknownValueEntry'.  This is @[\"Unknown\"]@.
enumUnknownValueEntryDefault :: EnumEntryWords
enumUnknownValueEntryDefault = ["Unknown"]

-- | Creates a binding for a C++ enum.
--
-- The numeric values of each of the enum's entries must be specified manually
-- using this function.  To have these determined automatically, instead use
-- 'makeAutoEnum'.
makeEnum ::
  Identifier  -- ^ 'enumIdentifier'
  -> Maybe ExtName
  -- ^ An optional external name; will be automatically derived from
  -- the identifier if absent.
  -> [(Integer, EnumEntryWords)]
  -- ^ A list of (numeric value, symbolic name) pairs describing enum entries to
  -- generate bindings for.  Each symbolic name is a list of words, which will
  -- be combined into a single identifier of appropriate naming style for the
  -- target language (title case, for Haskell) with 'enumValuePrefix' prepended.
  -> CppEnum
makeEnum identifier maybeExtName entries =
  let extName = extNameOrIdentifier identifier maybeExtName
  in CppEnum
     extName
     identifier
     Nothing
     Unscoped  -- Assume this is an unscoped enum.
     (let entries' = for entries $ \(num, words') -> (words', EnumValueManual num)
          entryNames = map fst entries'
      in EnumValueMap
         { enumValueMapNames = entryNames
         , enumValueMapForeignNames = plainMap $ M.fromList $ map (id &&& id) entryNames
         , enumValueMapValues = M.fromList entries'
         })
     mempty
     mempty
     (fromExtName extName ++ "_")
     (Just enumUnknownValueEntryDefault)
     True

-- | Creates a binding for a C++ enum.
--
-- An enum created using this function will determine its entries' numeric
-- values automatically when the generator is run, by compiling a temporary,
-- autogenerated C++ helper program.
--
-- This helper program needs to be able to access the C++ declaration of the
-- enum.  In addition to any 'includeStd' or 'includeLocal' requirements added
-- to the enum for the generated C++ bindings to use, the /interface's compiler/
-- ('interfaceCompiler') will need to be able to use these includes to access
-- the enum from C++ file built in a temporary directory.  To add @-I@ arguments
-- or otherwise change the compiler, you can reconfigure the interface:
--
-- @
-- myInterface =
--   'interfaceSetCompiler' (prependArguments [\"-I\" ++ pathToIncludes] defaultCompiler) $
--   'interface' ...
-- @
--
-- See "Foreign.Hoppy.Generator.Compiler".
makeAutoEnum ::
  IsAutoEnumValue v
  => Identifier  -- ^ 'enumIdentifier'
  -> Maybe ExtName
  -- ^ An optional external name; will be automatically derived from the
  -- identifier if absent.
  -> Scoped
  -- ^ Is the enum scoped (@enum class@ or @enum struct@)?  That is, are its
  -- entries scoped underneath its name, rather than being at the same level as
  -- its name (as with just @enum@).
  -> [v]
  -- ^ A list of enum entries to calculate and generate bindings for.  See
  -- 'IsAutoEnumValue'.
  -> CppEnum
makeAutoEnum identifier maybeExtName scoped entries =
  let extName = extNameOrIdentifier identifier maybeExtName
  in CppEnum
     extName
     identifier
     Nothing
     scoped
     (let namespaceForValues = case scoped of
            Scoped -> identifier
            Unscoped -> makeIdentifier $ butLast $ identifierParts identifier
          entries' =
            map (fmap (\name -> namespaceForValues `mappend` ident name) .
                 toAutoEnumValue)
            entries
          entryNames = map fst entries'
       in EnumValueMap
          { enumValueMapNames = entryNames
          , enumValueMapForeignNames = plainMap $ M.fromList $ map (id &&& id) entryNames
          , enumValueMapValues = M.map EnumValueAuto $ M.fromList entries'
          })
     mempty
     mempty
     (fromExtName extName ++ "_")
     (Just enumUnknownValueEntryDefault)
     True

-- | Represents a mapping to an automatically evaluated C++ enum entry.
--
-- The @('EnumEntryWords', String)@ instance is the canonical one, with
-- 'toAutoEnumValue' defined as @id@.  The string on the right is the C++ name
-- of the entry, and the list of strings on the left are the words from which to
-- generate foreign bindings' entry names.
--
-- The @String@ instance takes the C++ name of the entry, and splits it into
-- words via 'splitIntoWords'.
class IsAutoEnumValue a where
  toAutoEnumValue :: a -> (EnumEntryWords, String)

instance IsAutoEnumValue (EnumEntryWords, String) where
  toAutoEnumValue = id

instance IsAutoEnumValue String where
  toAutoEnumValue = splitIntoWords &&& id

-- | Adds overrides for some of an enum's entry names, in a specific language.
enumAddEntryNameOverrides :: IsAutoEnumValue v => ForeignLanguage -> [(v, v)] -> CppEnum -> CppEnum
enumAddEntryNameOverrides lang nameOverrides enum = enum { enumValues = enumValues' }
  where enumValues' =
          (enumValues enum)
          { enumValueMapForeignNames =
            addOverrideMap lang overrideMap $ enumValueMapForeignNames $ enumValues enum }
        overrideMap = M.fromList $ map (toEntryName *** toEntryName) nameOverrides
        toEntryName = fst . toAutoEnumValue

-- | Retrieves the name for an enum entry in a specific foreign language.
enumGetOverriddenEntryName :: ForeignLanguage -> CppEnum -> EnumEntryWords -> EnumEntryWords
enumGetOverriddenEntryName lang enum words' =
  case overriddenMapLookup lang words' $ enumValueMapForeignNames $ enumValues enum of
    Just words'' -> words''
    Nothing ->
      error $ "enumGetOverriddenEntryName: Entry with name " ++ show words' ++
      " not found in " ++ show enum ++ "."

-- | Sets the prefix applied to the names of enum values' identifiers in foreign
-- languages.
--
-- See 'enumValuePrefix'.
enumSetValuePrefix :: String -> CppEnum -> CppEnum
enumSetValuePrefix prefix enum = enum { enumValuePrefix = prefix }

-- | Sets the entry name (a list of words, a la the fields in 'EnumValueMap')
-- for the fallback enum entry that holds unknown values.
--
-- Set 'enumUnknownValueEntry', 'enumSetNoUnknownValueEntry'.
enumSetUnknownValueEntry :: IsEnumUnknownValueEntry a => a -> CppEnum -> CppEnum
enumSetUnknownValueEntry name enum =
  enum { enumUnknownValueEntry = Just $ toEnumUnknownValueEntry name }

-- | Sets an enum to have no unknown value entry.
--
-- Set 'enumUnknownValueEntry', 'enumSetUnknownValueEntry'.
enumSetNoUnknownValueEntry :: CppEnum -> CppEnum
enumSetNoUnknownValueEntry enum =
  enum { enumUnknownValueEntry = Nothing }

-- | Values that can be used as a name for an enum's unknown value entry.  See
-- 'enumUnknownValueEntry'.
class IsEnumUnknownValueEntry a where
  -- | Converts a value to a list of words to use for an enum's unknown entry
  -- name.
  toEnumUnknownValueEntry :: a -> EnumEntryWords

instance IsEnumUnknownValueEntry EnumEntryWords where
  toEnumUnknownValueEntry = id

instance IsEnumUnknownValueEntry String where
  toEnumUnknownValueEntry = splitIntoWords

-- | Sets whether generated bindings will support bitwise operations on the
-- enum.
--
-- See 'enumHasBitOperations'.
enumSetHasBitOperations :: Bool -> CppEnum -> CppEnum
enumSetHasBitOperations b enum = enum { enumHasBitOperations = b }

makeConversion :: CppEnum -> ConversionSpec
makeConversion e =
  (makeConversionSpec (show e) cpp)
  { conversionSpecHaskell = Just hs }
  where cpp =
          makeConversionSpecCpp (LC.renderIdentifier $ enumIdentifier e)
                                (return $ enumReqs e)

        hs =
          makeConversionSpecHaskell
            (HsTyCon . UnQual . HsIdent <$> toHsEnumTypeName e)
            (Just $ do evaluatedData <- hsGetEvaluatedEnumData $ enumExtName e
                       LH.cppTypeToHsTypeAndUse LH.HsCSide $
                         numType $ evaluatedEnumNumericType evaluatedData)
            (CustomConversion $ do
               LH.addImports $ mconcat [hsImport1 "Prelude" "(.)",
                                        hsImportForPrelude,
                                        hsImportForRuntime]
               LH.sayLn "HoppyP.return . HoppyFHR.fromCppEnum")
            (CustomConversion $ do
               LH.addImports $ mconcat [hsImport1 "Prelude" "(.)",
                                        hsImportForPrelude,
                                        hsImportForRuntime]
               LH.sayLn "HoppyP.return . HoppyFHR.toCppEnum")

-- | Constructs a type value for an enum.
enumT :: CppEnum -> Type
-- (Keep docs in sync with hs-boot.)
enumT = manualT . makeConversion

sayHsExport :: LH.SayExportMode -> CppEnum -> LH.Generator ()
sayHsExport mode enum =
  LH.withErrorContext ("generating enum " ++ show (enumExtName enum)) $
  case mode of
    -- Nothing to import from the C++ side of an enum.
    LH.SayExportForeignImports -> return ()

    LH.SayExportDecls -> do
      hsTypeName <- toHsEnumTypeName enum
      evaluatedData <- hsGetEvaluatedEnumData $ enumExtName enum
      numericType <- LH.cppTypeToHsTypeAndUse LH.HsCSide $
        numType $ evaluatedEnumNumericType evaluatedData
      let evaluatedValueMap = evaluatedEnumValueMap evaluatedData
      evaluatedValues <- forM (enumValueMapNames $ enumValues enum) $ \name ->
        case M.lookup name evaluatedValueMap of
          Just value -> return (name, value)
          Nothing -> throwError $ "Couldn't find evaluated value for " ++ show name
      values :: [(Integer, String)] <- forM evaluatedValues $ \(entryName, value) -> do
        let entryName' = enumGetOverriddenEntryName Haskell enum entryName
        ctorName <- toHsEnumCtorName enum entryName'
        return (value, ctorName)
      maybeUnknownValueCtorName <- forM (enumUnknownValueEntry enum) $ toHsEnumCtorName enum
      LH.addImports $ mconcat [hsImport1 "Prelude" "(==)",
                               hsImportForPrelude,
                               hsImportForRuntime]

      -- Print out the data declaration.
      LH.ln
      LH.addExport' hsTypeName
      LH.saysLn ["data ", hsTypeName, " ="]
      LH.indent $ do
        forM_ (zip (False:repeat True) values) $ \(cont, (_, hsCtorName)) ->
          LH.saysLn [if cont then "| " else "", hsCtorName]
        -- Only print an unknown value ctor if one has been requested.
        forM_ maybeUnknownValueCtorName $ \unknownValueCtorName ->
          LH.saysLn ["| ", unknownValueCtorName, " (", LH.prettyPrint numericType, ")"]
        LH.sayLn "deriving (HoppyP.Show)"

      -- Print out the (runtime) CppEnum instance.
      LH.ln
      LH.saysLn ["instance HoppyFHR.CppEnum (", LH.prettyPrint numericType, ") ", hsTypeName,
                 " where"]
      LH.indent $ do
        forM_ values $ \(num, hsCtorName) ->
          LH.saysLn ["fromCppEnum ", hsCtorName, " = ", show num]
        forM_ maybeUnknownValueCtorName $ \unknownValueCtorName ->
          LH.saysLn ["fromCppEnum (", unknownValueCtorName, " n) = n"]
        LH.ln
        -- We pass the values list through a map here to only keep the first
        -- constructor mapped to each numeric value, otherwise we'd write
        -- duplicate cases.
        forM_ (M.toList $ M.fromListWith const values) $ \(num, hsCtorName) ->
          LH.saysLn ["toCppEnum (", show num, ") = ", hsCtorName]
        case maybeUnknownValueCtorName of
          Just unknownValueCtorName -> LH.saysLn ["toCppEnum n = ", unknownValueCtorName, " n"]
          Nothing -> do
            LH.addImports $ hsImports "Prelude" ["($)", "(++)"]
            LH.saysLn ["toCppEnum n' = HoppyP.error $ ",
                       show (concat ["Unknown ", hsTypeName, " numeric value: "]),
                       " ++ HoppyP.show n'"]

      -- Print out Eq and Ord instances.
      LH.ln
      LH.saysLn ["instance HoppyP.Eq ", hsTypeName, " where"]
      LH.indent $
        LH.sayLn "x == y = HoppyFHR.fromCppEnum x == HoppyFHR.fromCppEnum y"
      LH.ln
      LH.saysLn ["instance HoppyP.Ord ", hsTypeName, " where"]
      LH.indent $
        LH.sayLn "compare x y = HoppyP.compare (HoppyFHR.fromCppEnum x) (HoppyFHR.fromCppEnum y)"

      when (enumHasBitOperations enum) $ do
        LH.addImports $ mconcat [hsImports "Prelude" ["($)", "(.)"],
                                 hsImports "Data.Bits" ["(.&.)", "(.|.)"],
                                 hsImportForBits]
        LH.saysLn ["instance HoppyDB.Bits ", hsTypeName, " where"]
        LH.indent $ do
          let fun1 f =
                LH.saysLn [f, " x = HoppyFHR.toCppEnum $ HoppyDB.",
                           f, " $ HoppyFHR.fromCppEnum x"]
              fun1Int f =
                LH.saysLn [f, " x i = HoppyFHR.toCppEnum $ HoppyDB.",
                           f, " (HoppyFHR.fromCppEnum x) i"]
              fun2 f =
                LH.saysLn [f, " x y = HoppyFHR.toCppEnum $ HoppyDB.",
                           f, " (HoppyFHR.fromCppEnum x) (HoppyFHR.fromCppEnum y)"]
              op2 op =
                LH.saysLn ["x ", op, " y = HoppyFHR.toCppEnum ",
                           "(HoppyFHR.fromCppEnum x ", op, " HoppyFHR.fromCppEnum y)"]
          op2 ".&."
          op2 ".|."
          fun2 "xor"
          fun1 "complement"
          fun1Int "shift"
          fun1Int "rotate"
          LH.sayLn "bitSize x = case HoppyDB.bitSizeMaybe x of"
          LH.indent $ do
            LH.sayLn "  HoppyP.Just n -> n"
            -- Same error message as the prelude here:
            LH.sayLn "  HoppyP.Nothing -> HoppyP.error \"bitSize is undefined\""
          LH.sayLn "bitSizeMaybe = HoppyDB.bitSizeMaybe . HoppyFHR.fromCppEnum"
          LH.sayLn "isSigned = HoppyDB.isSigned . HoppyFHR.fromCppEnum"
          LH.sayLn "testBit x i = HoppyDB.testBit (HoppyFHR.fromCppEnum x) i"
          LH.sayLn "bit = HoppyFHR.toCppEnum . HoppyDB.bit"
          LH.sayLn "popCount = HoppyDB.popCount . HoppyFHR.fromCppEnum"

    LH.SayExportBoot -> do
      hsTypeName <- toHsEnumTypeName enum
      evaluatedData <- hsGetEvaluatedEnumData $ enumExtName enum
      numericType <- LH.cppTypeToHsTypeAndUse LH.HsCSide $
        numType $ evaluatedEnumNumericType evaluatedData
      LH.addImports $ mconcat [hsImportForPrelude, hsImportForRuntime]
      LH.addExport hsTypeName
      LH.ln
      LH.saysLn ["data ", hsTypeName]
      LH.saysLn ["instance HoppyFHR.CppEnum (", LH.prettyPrint numericType, ") ", hsTypeName]
      LH.saysLn ["instance HoppyP.Eq ", hsTypeName]
      LH.saysLn ["instance HoppyP.Ord ", hsTypeName]
      LH.saysLn ["instance HoppyP.Show ", hsTypeName]
      when (enumHasBitOperations enum) $ do
        LH.addImports hsImportForBits
        LH.saysLn ["instance HoppyDB.Bits ", hsTypeName]

-- | Reads evaluated data for the named enum from the C++ generator environment.
cppGetEvaluatedEnumData :: HasCallStack => ExtName -> LC.Generator EvaluatedEnumData
cppGetEvaluatedEnumData extName = do
  computed <- LC.askComputedInterfaceData
  return $ getEvaluatedEnumData computed extName

-- | Reads evaluated data for the named enum from the Haskell generator
-- environment.
hsGetEvaluatedEnumData :: HasCallStack => ExtName -> LH.Generator EvaluatedEnumData
hsGetEvaluatedEnumData extName = do
  computed <- LH.askComputedInterfaceData
  return $ getEvaluatedEnumData computed extName

-- | Returns the Haskell name for an enum.
--
-- TODO Clarify, and split into type and data ctor names.
toHsEnumTypeName :: CppEnum -> LH.Generator String
toHsEnumTypeName enum =
  LH.inFunction "toHsEnumTypeName" $
  LH.addExtNameModule (enumExtName enum) $ toHsEnumTypeName' enum

-- | Pure version of 'toHsEnumTypeName' that doesn't create a qualified name.
toHsEnumTypeName' :: CppEnum -> String
toHsEnumTypeName' = LH.toHsTypeName' Nonconst . enumExtName

-- | Constructs the data constructor name for a value in an enum.  Like C++ and
-- unlike say Java, Haskell enum values aren't in a separate enum-specific
-- namespace, so we prepend the enum name to the value name to get the data
-- constructor name.  The value name is a list of words.
toHsEnumCtorName :: CppEnum -> EnumEntryWords -> LH.Generator String
toHsEnumCtorName enum words' =
  LH.inFunction "toHsEnumCtorName" $
  LH.addExtNameModule (enumExtName enum) $ toHsEnumCtorName' enum words'

-- | Pure version of 'toHsEnumCtorName' that doesn't create a qualified name.
toHsEnumCtorName' :: CppEnum -> EnumEntryWords -> String
toHsEnumCtorName' enum words' =
  concat $ enumValuePrefix enum : map capitalize words'
