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

{-# LANGUAGE CPP #-}

-- | Shared portion of the Haskell code generator.  Usable by binding
-- definitions.
module Foreign.Hoppy.Generator.Language.Haskell (
  Managed (..),
  getModuleName,
  toModuleName,
  -- * Code generators
  Partial (..),
  Output (..),
  Generator,
  runGenerator,
  evalGenerator,
  execGenerator,
  renderPartial,
  withErrorContext,
  inFunction,
  -- * Exports
  HsExport,
  addExport,
  addExport',
  addExports,
  -- * Imports
  addImports,
  importHsModuleForExtName,
  -- * Code generation
  sayLn,
  saysLn,
  ln,
  indent,
  sayLet,
  toHsEnumTypeName,
  toHsEnumCtorName,
  toHsBitspaceTypeName,
  toHsBitspaceValueName,
  toHsBitspaceToNumName,
  toHsBitspaceClassName,
  toHsBitspaceFromValueName,
  toHsValueClassName,
  toHsWithValuePtrName,
  toHsPtrClassName,
  toHsCastMethodName,
  toHsDownCastClassName,
  toHsDownCastMethodName,
  toHsCastPrimitiveName,
  toHsConstCastFnName,
  toHsDataTypeName,
  toHsDataCtorName,
  toHsClassDeleteFnName,
  toHsClassDeleteFnPtrName,
  toHsMethodName,
  toHsMethodName',
  toHsCallbackCtorName,
  toHsFnName,
  toArgName,
  HsTypeSide (..),
  cppTypeToHsTypeAndUse,
  prettyPrint,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Arrow (first)
import Control.Monad (when)
#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except (Except, catchError, runExcept, throwError)
#else
import Control.Monad.Error (catchError, throwError)
#endif
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer (WriterT, censor, runWriterT, tell)
import Data.Char (toUpper)
import Data.Foldable (forM_)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid, mappend, mconcat, mempty)
#endif
import Data.Tuple (swap)
import Foreign.Hoppy.Generator.Common (capitalize, lowerFirst)
import Foreign.Hoppy.Generator.Spec
import qualified Language.Haskell.Pretty as P
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (Special, UnQual),
  HsSpecialCon (HsUnitCon),
  HsType (HsTyApp, HsTyCon, HsTyFun),
  )

-- | Indicates who is managing the lifetime of an object via an object pointer.
data Managed =
    Unmanaged
    -- ^ The object's lifetime is being managed manually.
  | Managed
    -- ^ The object's lifetime is being managed by the Haskell garbage
    -- collector.
  deriving (Bounded, Enum, Eq, Ord)

-- | Returns the complete Haskell module name for a 'Module' in an 'Interface',
-- taking into account the 'interfaceHaskellModuleBase' and the
-- 'moduleHaskellName'.
getModuleName :: Interface -> Module -> String
getModuleName interface m =
  intercalate "." $
  interfaceHaskellModuleBase interface ++
  fromMaybe [toModuleName $ moduleName m] (moduleHaskellName m)

-- | Performs case conversions on the given string to ensure that it is a valid
-- component of a Haskell module name.
toModuleName :: String -> String
toModuleName (x:xs) = toUpper x : xs
toModuleName "" = ""

-- | Renders a set of imports in Haskell syntax on multiple lines.
renderImports :: HsImportSet -> [String]
renderImports = map renderModuleImport . M.assocs . getHsImportSet
  where -- | Renders an import as a string that contains one or more lines.
        renderModuleImport :: (HsImportKey, HsImportSpecs) -> String
        renderModuleImport (key, specs) =
          let moduleName = hsImportModule key
              maybeQualifiedName = hsImportQualifiedName key
              isQual = isJust maybeQualifiedName
              importPrefix = if hsImportSource specs
                             then "import {-# SOURCE #-} "
                             else "import "
              importQualifiedPrefix =
                if hsImportSource specs
                then "import {-# SOURCE #-} qualified "
                else "import qualified "
          in case getHsImportSpecs specs of
            Nothing -> case maybeQualifiedName of
              Nothing -> importPrefix ++ moduleName
              Just qualifiedName ->
                concat [importQualifiedPrefix, moduleName, " as ", qualifiedName]
            Just specMap ->
              let specWords :: [String]
                  specWords = concatWithCommas $ map renderSpecAsWords $ M.assocs specMap
                  singleLineImport :: String
                  singleLineImport =
                    concat $
                    (if isQual then importQualifiedPrefix else importPrefix) :
                    moduleName : " (" : intersperse " " specWords ++
                    case maybeQualifiedName of
                      Nothing -> [")"]
                      Just qualifiedName -> [") as ", qualifiedName]
              in if null $ drop maxLineLength singleLineImport
                 then singleLineImport
                 else intercalate "\n" $
                      (importPrefix ++ moduleName ++ " (") :
                      groupWordsIntoLines specWords ++
                      case maybeQualifiedName of
                        Nothing -> ["  )"]
                        Just qualifiedName -> ["  ) as " ++ qualifiedName]

        -- | Takes an import spec, and returns a list of words that comprise
        -- that spec.  Line breaking may be performed by the caller only between
        -- these words.
        renderSpecAsWords :: (HsImportName, HsImportVal) -> [String]
        renderSpecAsWords (name, val) = case val of
          HsImportVal -> [name]
          -- When we export specific subnames under a name, then we put a
          -- non-breaking space between the outer name and the first inner name,
          -- just for a little readability.
          HsImportValSome parts -> case parts of
            [] -> [name ++ " ()"]
            [part] -> [concat [name, " (", part, ")"]]
            part0:parts -> let (parts', [partN]) = splitAt (length parts - 1) parts
                           in concat [name, " (", part0, ","] :
                              map (++ ",") parts' ++
                              [partN ++ ")"]
          HsImportValAll -> [name ++ " (..)"]

        -- | Takes a list of list of words.  Concatenates to get a list of
        -- words, appending a comma to the final word in each list of words.
        concatWithCommas :: [[String]] -> [String]
        concatWithCommas [] = []
        concatWithCommas ss =
          let (ss', ssLast@[_]) = splitAt (length ss - 1) ss
          in concat $ map (onLast (++ ",")) ss' ++ ssLast

        -- | Applies a function to the final element of a list, if the list is
        -- nonempty.
        onLast :: (a -> a) -> [a] -> [a]
        onLast _ [] = []
        onLast f xs = let (xs', [x]) = splitAt (length xs - 1) xs
                      in xs' ++ [f x]

        -- | Takes a list of words, and returns a list of lines with the words
        -- flowed.
        groupWordsIntoLines :: [String] -> [String]
        groupWordsIntoLines [] = []
        groupWordsIntoLines words =
          let (wordCount, line, _) =
                last $
                takeWhile (\(wordCount, _, len) -> wordCount <= 1 || len <= maxLineLength) $
                scanl (\(wordCount, acc, len) word ->
                        (wordCount + 1,
                         concat [acc, " ", word],
                         len + 1 + length word))
                      (0, "", 0)
                      words
          in line : groupWordsIntoLines (drop wordCount words)

        maxLineLength :: Int
        maxLineLength = 100

-- | A generator monad for Haskell code.
--
-- Errors thrown in this monad are of the form:
--
-- > "$problem; $context; $moreContext; $evenMoreContext."
--
-- For example, "Class Foo is not convertible (use classModifyConversion);
-- generating function bar; in module baz.".
--
-- The main error message given to 'throwError' should be capitalized and should
-- not end with punctuation.  If there is a suggestion, include it in
-- parentheses at the end of the message.  'withErrorContext' and 'inFunction'
-- add context information, and should be given clauses, without punctuation.
#if MIN_VERSION_mtl(2,2,1)
type Generator = ReaderT Env (WriterT Output (Except ErrorMsg))
#else
type Generator = ReaderT Env (WriterT Output (Either ErrorMsg))
#endif

-- | Context information for generating Haskell code.
data Env = Env
  { envInterface :: Interface
  , envModuleName :: String
  }

askInterface :: Generator Interface
askInterface = envInterface <$> ask

askModuleName :: Generator String
askModuleName = envModuleName <$> ask

-- | A partially-rendered 'Module'.  Contains all of the module's bindings, but
-- may be subject to further processing.
data Partial = Partial
  { partialModuleHsName :: String
  , partialOutput :: Output
  }

instance Eq Partial where
  (==) = (==) `on` partialModuleHsName

instance Ord Partial where
  compare = compare `on` partialModuleHsName

-- | A chunk of generated Haskell code, including information about imports and
-- exports.
data Output = Output
  { outputExports :: [HsExport]
    -- ^ Haskell module exports.  Each 'HsExport' should include one item to go
    -- in the export list of the generated module.  Should only contain objects
    -- imported or defined in the same 'Output'.
  , outputImports :: HsImportSet
    -- ^ Haskell module imports.  Should include all imports needed for the
    -- 'outputBody'.
  , outputBody :: [String]
    -- ^ Lines of Haskell code (possibly empty).  These lines may not contain
    -- the newline character in them.  There is an implicit newline between each
    -- string, as given by @intercalate \"\\n\" . outputBody@.
  }

instance Monoid Output where
  mempty = Output mempty mempty mempty

  (Output e i b) `mappend` (Output e' i' b') =
    Output (e `mappend` e') (i `mappend` i') (b `mappend` b')

  mconcat os =
    Output (mconcat $ map outputExports os)
           (mconcat $ map outputImports os)
           (mconcat $ map outputBody os)

-- | Runs a generator action for the given interface and module name string.
-- Returns an error message if an error occurred, otherwise the action's output
-- together with its value.
runGenerator :: Interface -> String -> Generator a -> Either ErrorMsg (Partial, a)
runGenerator iface modName generator =
  fmap (first (Partial modName) . swap) $
#if MIN_VERSION_mtl(2,2,1)
  runExcept $
#endif
  flip catchError (\msg -> throwError $ msg ++ ".") $
  runWriterT $ runReaderT generator $ Env iface modName

-- | Runs a generator action and returns the its value.
evalGenerator :: Interface -> String -> Generator a -> Either ErrorMsg a
evalGenerator iface modName =
  fmap snd . runGenerator iface modName

-- | Runs a generator action and returns its output.
execGenerator :: Interface -> String -> Generator a -> Either ErrorMsg Partial
execGenerator iface modName =
  fmap fst . runGenerator iface modName

-- | Converts a 'Partial' into a complete Haskell module.
renderPartial :: Partial -> String
renderPartial partial =
  let modName = partialModuleHsName partial
      output = partialOutput partial
      imports = outputImports output
      body =
        intercalate "\n" $ concat
        [ [ "---------- GENERATED FILE, EDITS WILL BE LOST ----------"
          , ""
          ]
        , case outputExports output of
            [] -> [concat ["module ", modName, " where"]]
            exports ->
              concat ["module ", modName, " ("] :
              map (\export -> concat ["  ", export, ","]) exports ++
              ["  ) where"]
        , if M.null $ getHsImportSet imports
          then []
          else "" : renderImports imports
        , [""]
        , outputBody output
        ]
  in body

-- | Adds context information to the end of any error message thrown by the
-- action.  See 'Generator'.
withErrorContext :: String -> Generator a -> Generator a
withErrorContext msg' action = catchError action $ \msg -> throwError $ concat [msg, "; ", msg']

-- | Adds the given function name to any error message thrown by the action, for
-- context.
inFunction :: String -> Generator a -> Generator a
inFunction fnName = withErrorContext $ "in " ++ fnName

-- | Indicates strings that represent an item in a Haskell module export list.
type HsExport = String

-- | Adds an export to the current module.
addExport :: HsExport -> Generator ()
addExport = addExports . (:[])

-- | @addExport' \"x\"@ adds an export of the form @x (..)@ to the current
-- module.
addExport' :: HsExport -> Generator ()
addExport' x = addExports [x ++ " (..)"]

-- | Adds multiple exports to the current module.
addExports :: [HsExport] -> Generator ()
addExports exports = tell $ mempty { outputExports = exports }

-- | Adds imports to the current module.
addImports :: HsImportSet -> Generator ()
addImports imports = tell mempty { outputImports = imports }

-- | Imports all of the objects for the given external name into the current
-- module.  This is a no-op of the external name is defined in the current
-- module.
importHsModuleForExtName :: ExtName -> Generator ()
importHsModuleForExtName extName = inFunction "importHsModuleForExtName" $ do
  iface <- askInterface
  case M.lookup extName $ interfaceNamesToModules iface of
    Just ownerModule -> do
      let ownerModuleName = getModuleName iface ownerModule
      currentModuleName <- askModuleName
      when (currentModuleName /= ownerModuleName) $
        -- Yes, this currently imports the whole dang module to keep things
        -- simple.
        addImports $ hsWholeModuleImport ownerModuleName
    Nothing ->
      throwError $ concat
      ["Couldn't find module for ", show extName,
       " (maybe you forgot to include it in an exports list?)"]

-- | Outputs a line of Haskell code.  A newline will be added on the end of the
-- input.  Newline characters must not be given to this function.
sayLn :: String -> Generator ()
sayLn x =
  if '\n' `elem` x
  then inFunction "sayLn" $ throwError $ concat
       ["Refusing to speak '\n', received ", show x, " (use (mapM_ sayLn . lines) instead)"]
  else tell $ mempty { outputBody = [x] }

-- | Outputs multiple words to form a line of Haskell code (effectively @saysLn
-- = sayLn . concat@).
saysLn :: [String] -> Generator ()
saysLn = sayLn . concat

-- | Outputs an empty line of Haskell code.  This is reportedly valid Perl code
-- as well.
ln :: Generator ()
ln = sayLn ""

-- | Runs the given action, indenting all code output by the action one level.
indent :: Generator a -> Generator a
indent = censor $ \o -> o { outputBody = map (\x -> ' ':' ':x) $ outputBody o }

-- | Takes a list of binding actions and a body action, and outputs a @let@
-- expression.  By passing in 'Nothing' for the body, it will be omitted, so
-- @let@ statements in @do@ blocks can be created as well.  Output is of the
-- form:
--
-- > let
-- >   <binding1>
-- >   ...
-- >   <bindingN>
-- >   in
-- >     <body>
--
-- To stretch a binding over multiple lines, lines past the first should use
-- 'indent' manually.
sayLet :: [Generator ()] -> Maybe (Generator ()) -> Generator ()
sayLet bindings maybeBody = do
  sayLn "let"
  indent $ sequence_ bindings
  forM_ maybeBody $ \body ->
    -- Indent here in case we have a "let ... in ..." within a do block.
    indent $ do
      sayLn "in"
      indent body

-- | Internal helper function for constructing Haskell names from external
-- names.  Returns a name that is a suitable Haskell type name for the external
-- name, and if given 'Const', then with @\"Const\"@ appended.
toHsTypeName :: Constness -> ExtName -> String
toHsTypeName cst extName =
  (case cst of
      Const -> (++ "Const")
      Nonconst -> id) $
  case fromExtName extName of
    x:xs -> toUpper x:xs
    [] -> []

-- | Returns the Haskell name for an enum.
toHsEnumTypeName :: CppEnum -> String
toHsEnumTypeName = toHsTypeName Nonconst . enumExtName

-- | Constructs the data constructor name for a value in an enum.  Like C++ and
-- unlike say Java, Haskell enum values aren't in a separate enum-specific
-- namespace, so we prepend the enum name to the value name to get the data
-- constructor name.  The value name is a list of words; see 'enumValueNames'.
toHsEnumCtorName :: CppEnum -> [String] -> String
toHsEnumCtorName enum words =
  concat $ toHsEnumTypeName enum : "_" : map capitalize words

-- | Returns the Haskell name for a bitspace.  See 'toHsEnumTypeName'.
toHsBitspaceTypeName :: Bitspace -> String
toHsBitspaceTypeName = toHsTypeName Nonconst . bitspaceExtName

-- | Constructs the data constructor name for a value in a bitspace.  See
-- 'toHsEnumCtorName'.
toHsBitspaceValueName :: Bitspace -> [String] -> String
toHsBitspaceValueName bitspace words =
  lowerFirst $ concat $ toHsBitspaceTypeName bitspace : "_" : map capitalize words

-- | Returns the name of the function that will convert a bitspace value into a
-- raw numeric value.
toHsBitspaceToNumName :: Bitspace -> String
toHsBitspaceToNumName = ("from" ++) . toHsBitspaceTypeName

-- | The name of the Haskell typeclass that contains a method for converting to
-- a bitspace value.
toHsBitspaceClassName :: Bitspace -> String
toHsBitspaceClassName bitspace = 'I':'s':toHsBitspaceTypeName bitspace

-- | The name of the method in the 'toHsBitspaceClassName' typeclass that
-- constructs bitspace values.
toHsBitspaceFromValueName :: Bitspace -> String
toHsBitspaceFromValueName = ("to" ++) . toHsBitspaceTypeName

-- | The name for the typeclass of types that can be represented as values of
-- the given C++ class.
toHsValueClassName :: Class -> String
toHsValueClassName cls = toHsDataTypeName Nonconst cls ++ "Value"

-- | The name of the method within the 'toHsValueClassName' typeclass for
-- accessing an object of the type as a pointer.
toHsWithValuePtrName :: Class -> String
toHsWithValuePtrName cls = concat ["with", toHsDataTypeName Nonconst cls, "Ptr"]

-- | The name for the typeclass of types that are (possibly const) pointers to
-- objects of the given C++ class, or subclasses.
toHsPtrClassName :: Constness -> Class -> String
toHsPtrClassName cst cls = toHsDataTypeName cst cls ++ "Ptr"

-- | The name of the function that upcasts pointers to the specific class type
-- and constness.
toHsCastMethodName :: Constness -> Class -> String
toHsCastMethodName cst cls = "to" ++ toHsDataTypeName cst cls

-- | The name of the typeclass that provides a method to downcast to a specific
-- class type.  See 'toHsDownCastMethodName'.
toHsDownCastClassName :: Constness -> Class -> String
toHsDownCastClassName cst cls =
  concat [toHsDataTypeName Nonconst cls,
          "Super",
          case cst of
            Const -> "Const"
            Nonconst -> ""]

-- | The name of the function that downcasts pointers to the specific class type
-- and constness.
toHsDownCastMethodName :: Constness -> Class -> String
toHsDownCastMethodName cst cls = "downTo" ++ toHsDataTypeName cst cls

-- | The import name for the foreign function that casts between two specific
-- pointer types.  Used for upcasting and downcasting.
toHsCastPrimitiveName :: Class -> Class -> String
toHsCastPrimitiveName from to =
  concat ["cast", toHsDataTypeName Nonconst from, "To", toHsDataTypeName Nonconst to]

-- | The name of one of the functions that add/remove const to/from a class's
-- pointer type.  Given 'Const', it will return the function that adds const,
-- and given 'Nonconst', it will return the function that removes const.
toHsConstCastFnName :: Constness -> Class -> String
toHsConstCastFnName cst cls =
  concat ["cast", toHsDataTypeName Nonconst cls,
          case cst of
            Const -> "ToConst"
            Nonconst -> "ToNonconst"]

-- | The name of the data type that represents a pointer to an object of the
-- given class and constness.
toHsDataTypeName :: Constness -> Class -> String
toHsDataTypeName cst cls = toHsTypeName cst $ classExtName cls

-- | The name of a data constructor for one of the object pointer types.
toHsDataCtorName :: Managed -> Constness -> Class -> String
toHsDataCtorName m cst cls = case m of
  Unmanaged -> base
  Managed -> base ++ "Gc"
  where base = toHsDataTypeName cst cls

-- | The name of the foreign function import wrapping @delete@ for the given
-- class type.  This is in internal to the binding; normal users should use
-- 'Foreign.Hoppy.Runtime.delete'.
toHsClassDeleteFnName :: Class -> String
toHsClassDeleteFnName cls = 'd':'e':'l':'e':'t':'e':'\'':toHsDataTypeName Nonconst cls

-- | The name of the foreign import that imports the same function as
-- 'toHsClassDeleteFnName', but as a 'Foreign.Ptr.FunPtr' rather than an actual
-- function.
toHsClassDeleteFnPtrName :: Class -> String
toHsClassDeleteFnPtrName cls =
  'd':'e':'l':'e':'t':'e':'P':'t':'r':'\'':toHsDataTypeName Nonconst cls

-- | Returns the name of the Haskell function that invokes the given method.
--
-- See also 'getClassyExtName'.
toHsMethodName :: Class -> Method -> String
toHsMethodName cls method = toHsMethodName' cls $ fromExtName $ methodExtName method

-- | Returns the name of the Haskell function that invokes a method with a
-- specific name in a class.
toHsMethodName' :: IsFnName String name => Class -> name -> String
toHsMethodName' cls methodName =
  lowerFirst $
  concat [fromExtName $ classExtName cls, "_",
          case toFnName methodName of
            FnName name -> name
            FnOp op -> fromExtName $ operatorPreferredExtName op]

-- | The name of the function that takes a Haskell function and wraps it in a
-- callback object.  This is internal to the binding; normal users can pass
-- Haskell functions to be used as callbacks inplicitly.
toHsCallbackCtorName :: Callback -> String
toHsCallbackCtorName = toHsFnName . callbackExtName

-- | Converts an external name into a name suitable for a Haskell function or
-- variable.
toHsFnName :: ExtName -> String
toHsFnName = lowerFirst . fromExtName

-- | Returns a distinct argument variable name for each nonnegative number.
toArgName :: Int -> String
toArgName = ("arg'" ++) . show

-- | The Haskell side of bindings performs conversions between C FFI types and
-- Haskell types.  This denotes which side's type is being used.
data HsTypeSide =
  HsCSide  -- ^ The C type sent from C++.
  | HsHsSide  -- ^ The Haskell-native type.
  deriving (Eq, Show)

-- | Returns the 'HsType' corresponding to a 'Type', and also adds imports to
-- the 'Generator' as necessary for Haskell types that the 'Type' references.
-- On failure, an error is thrown.
cppTypeToHsTypeAndUse :: HsTypeSide -> Type -> Generator HsType
cppTypeToHsTypeAndUse side t =
  withErrorContext (concat ["converting ", show t, " to ", show side, " type"]) $
  case t of
    TVoid -> return $ HsTyCon $ Special HsUnitCon
    -- C++ has sizeof(bool) == 1, whereas Haskell can > 1, so we have to convert.
    TBool -> case side of
      HsCSide -> addImports hsImportForRuntime $> HsTyCon (UnQual $ HsIdent "HoppyFHR.CBool")
      HsHsSide -> addImports hsImportForPrelude $> HsTyCon (UnQual $ HsIdent "HoppyP.Bool")
    TChar -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CChar")
    TUChar -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CUChar")
    TShort -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CShort")
    TUShort -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CUShort")
    TInt -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CInt")
    TUInt -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CUInt")
    TLong -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CLong")
    TULong -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CULong")
    TLLong -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CLLong")
    TULLong -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CULLong")
    TFloat -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CFloat")
    TDouble -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CDouble")
    TInt8 -> addImports hsImportForInt $> HsTyCon (UnQual $ HsIdent "HoppyDI.Int8")
    TInt16 -> addImports hsImportForInt $> HsTyCon (UnQual $ HsIdent "HoppyDI.Int16")
    TInt32 -> addImports hsImportForInt $> HsTyCon (UnQual $ HsIdent "HoppyDI.Int32")
    TInt64 -> addImports hsImportForInt $> HsTyCon (UnQual $ HsIdent "HoppyDI.Int64")
    TWord8 -> addImports hsImportForWord $> HsTyCon (UnQual $ HsIdent "HoppyDW.Word8")
    TWord16 -> addImports hsImportForWord $> HsTyCon (UnQual $ HsIdent "HoppyDW.Word16")
    TWord32 -> addImports hsImportForWord $> HsTyCon (UnQual $ HsIdent "HoppyDW.Word32")
    TWord64 -> addImports hsImportForWord $> HsTyCon (UnQual $ HsIdent "HoppyDW.Word64")
    TPtrdiff -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CPtrdiff")
    TSize -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CSize")
    TSSize -> addImports hsImportForSystemPosixTypes $> HsTyCon (UnQual $ HsIdent "HoppySPT.CSsize")
    TEnum e -> HsTyCon . UnQual . HsIdent <$> case side of
      HsCSide -> addImports hsImportForForeignC $> "HoppyFC.CInt"
      HsHsSide -> importHsModuleForExtName (enumExtName e) $> toHsEnumTypeName e
    TBitspace b -> case side of
      HsCSide -> cppTypeToHsTypeAndUse side $ bitspaceType b
      HsHsSide -> importHsModuleForExtName (bitspaceExtName b) $>
                  HsTyCon (UnQual $ HsIdent $ toHsBitspaceTypeName b)
    TPtr (TObj cls) -> do
      -- Same as TPtr (TConst (TObj cls)), but nonconst.
      importHsModuleForExtName (classExtName cls)
      let dataType = HsTyCon $ UnQual $ HsIdent $ toHsTypeName Nonconst $ classExtName cls
      case side of
        HsCSide -> do
          addImports hsImportForForeign
          return $ HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.Ptr") dataType
        HsHsSide -> return dataType
    TPtr (TConst (TObj cls)) -> do
      -- Same as TPtr (TObj cls), but const.
      importHsModuleForExtName (classExtName cls)
      let dataType = HsTyCon $ UnQual $ HsIdent $ toHsTypeName Const $ classExtName cls
      case side of
        HsCSide -> do
          addImports hsImportForForeign
          return $ HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.Ptr") dataType
        HsHsSide -> return dataType
    TPtr (TFn paramTypes retType) -> do
      paramHsTypes <- mapM (cppTypeToHsTypeAndUse side) paramTypes
      retHsType <- cppTypeToHsTypeAndUse side retType
      sideFn <- case side of
        HsCSide -> do addImports hsImportForForeign
                      return $ HsTyApp $ HsTyCon $ UnQual $ HsIdent "HoppyF.FunPtr"
        HsHsSide -> return id
      addImports hsImportForPrelude
      return $ sideFn $
        foldr HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") retHsType) paramHsTypes
    TPtr t' -> do
      addImports hsImportForForeign
      -- Pointers to types not covered above point to raw C++ values, so we need
      -- to use the C-side type of the pointer target here.
      HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.Ptr") <$> cppTypeToHsTypeAndUse HsCSide t'
    TRef t' -> cppTypeToHsTypeAndUse side $ TPtr t'
    TFn paramTypes retType -> do
      paramHsTypes <- mapM (cppTypeToHsTypeAndUse side) paramTypes
      retHsType <- cppTypeToHsTypeAndUse side retType
      addImports hsImportForPrelude
      return $
        foldr HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") retHsType) paramHsTypes
    TCallback cb -> do
      hsType <- cppTypeToHsTypeAndUse side $ callbackToTFn cb
      case side of
        HsHsSide -> return hsType
        HsCSide -> do
          addImports hsImportForRuntime
          return $ HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyFHR.CCallback") hsType
    TObj cls -> case side of
      HsCSide -> cppTypeToHsTypeAndUse side $ TPtr $ TConst t
      HsHsSide ->
        case classHaskellConversionType <$> classHaskellConversion (classConversion cls) of
          Nothing ->
            throwError $ concat
            ["Expected a Haskell type for ", show cls, " but there isn't one"]
          Just t' -> t'
    TObjToHeap cls -> cppTypeToHsTypeAndUse side $ TPtr $ TObj cls
    TToGc t' -> case t' of
      TRef _ -> cppTypeToHsTypeAndUse side t'  -- References behave the same as pointers.
      TPtr _ -> cppTypeToHsTypeAndUse side t'
      TObj cls -> cppTypeToHsTypeAndUse side $ TPtr $ TObj cls
      _ -> throwError $ tToGcInvalidFormErrorMessage Nothing t'
    TConst t' -> cppTypeToHsTypeAndUse side t'

-- | Prints a value like 'P.prettyPrint', but removes newlines so that they
-- don't cause problems with this module's textual generation.  Should be mainly
-- used for printing types; stripping newlines from definitions for example
-- could go badly.
prettyPrint :: P.Pretty a => a -> String
prettyPrint = filter (/= '\n') . P.prettyPrint
