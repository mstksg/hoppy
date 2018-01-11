-- This file is part of Hoppy.
--
-- Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
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
  askInterface,
  askModule,
  askModuleName,
  getModuleForExtName,
  withErrorContext,
  inFunction,
  -- * Exports
  HsExport,
  addExport,
  addExport',
  addExports,
  -- * Imports
  addImports,
  -- * Language extensions
  addExtension,
  -- * Code generation
  sayLn,
  saysLn,
  ln,
  indent,
  indentSpaces,
  sayLet,
  toHsEnumTypeName,
  toHsEnumTypeName',
  toHsEnumCtorName,
  toHsEnumCtorName',
  toHsBitspaceTypeName,
  toHsBitspaceTypeName',
  toHsBitspaceValueName,
  toHsBitspaceValueName',
  toHsBitspaceToNumName,
  toHsBitspaceToNumName',
  toHsBitspaceClassName,
  toHsBitspaceClassName',
  toHsBitspaceFromValueName,
  toHsBitspaceFromValueName',
  toHsValueClassName,
  toHsValueClassName',
  toHsWithValuePtrName,
  toHsWithValuePtrName',
  toHsPtrClassName,
  toHsPtrClassName',
  toHsCastMethodName,
  toHsCastMethodName',
  toHsDownCastClassName,
  toHsDownCastClassName',
  toHsDownCastMethodName,
  toHsDownCastMethodName',
  toHsCastPrimitiveName,
  toHsCastPrimitiveName',
  toHsConstCastFnName,
  toHsConstCastFnName',
  toHsDataTypeName,
  toHsDataTypeName',
  toHsDataCtorName,
  toHsDataCtorName',
  toHsClassDeleteFnName',
  toHsClassDeleteFnPtrName',
  toHsCtorName,
  toHsCtorName',
  toHsMethodName,
  toHsMethodName',
  toHsClassEntityName,
  toHsClassEntityName',
  toHsCallbackCtorName,
  toHsCallbackCtorName',
  toHsCallbackNewFunPtrFnName,
  toHsCallbackNewFunPtrFnName',
  toHsFnName,
  toHsFnName',
  toArgName,
  HsTypeSide (..),
  cppTypeToHsTypeAndUse,
  getClassHaskellConversion,
  callbackToTFn,
  prettyPrint,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Arrow (first)
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
import qualified Data.Set as S
import Data.Tuple (swap)
import Foreign.Hoppy.Generator.Common
import Foreign.Hoppy.Generator.Spec.Base
import Foreign.Hoppy.Generator.Types
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
  , envModule :: Module
  , envModuleName :: String
  }

-- | Returns the currently generating interface.
askInterface :: Generator Interface
askInterface = envInterface <$> ask

-- | Returns the currently generating module.
askModule :: Generator Module
askModule = envModule <$> ask

-- | Returns the currently generating module's Haskell module name.
askModuleName :: Generator String
askModuleName = envModuleName <$> ask

-- | Looks up the 'Module' containing a given external name, throwing an error
-- if it can't be found.
getModuleForExtName :: ExtName -> Generator Module
getModuleForExtName extName = inFunction "getModuleForExtName" $ do
  iface <- askInterface
  case M.lookup extName $ interfaceNamesToModules iface of
    Just mod -> return mod
    Nothing -> throwError $ "Can't find module for " ++ show extName

-- | A partially-rendered 'Module'.  Contains all of the module's bindings, but
-- may be subject to further processing.
data Partial = Partial
  { partialModuleHsName :: String  -- ^ This is just the module name.
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
  , outputExtensions :: S.Set String
    -- ^ Language extensions to enable via the @{-# LANGUAGE #-}@ pragma for the
    -- whole module.
  }

instance Monoid Output where
  mempty = Output mempty mempty mempty mempty

  (Output e i b x) `mappend` (Output e' i' b' x') =
    Output (e `mappend` e') (i `mappend` i') (b `mappend` b') (x `mappend` x')

  mconcat os =
    Output (mconcat $ map outputExports os)
           (mconcat $ map outputImports os)
           (mconcat $ map outputBody os)
           (mconcat $ map outputExtensions os)

-- | Runs a generator action for the given interface and module name string.
-- Returns an error message if an error occurred, otherwise the action's output
-- together with its value.
runGenerator :: Interface -> Module -> Generator a -> Either ErrorMsg (Partial, a)
runGenerator iface mod generator =
  let modName = getModuleName iface mod
  in fmap (first (Partial modName) . swap) $
#if MIN_VERSION_mtl(2,2,1)
     runExcept $
#endif
     flip catchError (\msg -> throwError $ msg ++ ".") $
     runWriterT $ runReaderT generator $ Env iface mod modName

-- | Runs a generator action and returns the its value.
evalGenerator :: Interface -> Module -> Generator a -> Either ErrorMsg a
evalGenerator iface mod = fmap snd . runGenerator iface mod

-- | Runs a generator action and returns its output.
execGenerator :: Interface -> Module -> Generator a -> Either ErrorMsg Partial
execGenerator iface mod = fmap fst . runGenerator iface mod

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
        , case S.elems $ outputExtensions output of
            [] -> []
            extensions -> [ concat $ "{-# LANGUAGE " : intersperse ", " extensions ++ [" #-}"]
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

-- | Adds a Haskell language extension to the current module.
addExtension :: String -> Generator ()
addExtension extensionName =
  tell $ mempty { outputExtensions = S.singleton extensionName }

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

-- | Runs the given action, indenting all code output by the action N spaces.
indentSpaces :: Int -> Generator a -> Generator a
indentSpaces n = censor $ \o -> o { outputBody = map (\x -> indentation ++ x) $ outputBody o }
  where indentation = replicate n ' '

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

getExtNameModule :: ExtName -> Generator Module
getExtNameModule extName = inFunction "getExtNameModule" $ do
  iface <- askInterface
  fromMaybeM (throwError $ "Couldn't find module for " ++ show extName ++
              " (is it included in a module's export list?)") $
    M.lookup extName $
    interfaceNamesToModules iface

-- | Returns a module's unique short name that should be used for a qualified
-- import of the module.
getModuleImportName :: Module -> Generator String
getModuleImportName mod = do
  iface <- askInterface
  fromMaybeM (throwError $ "Couldn't find a Haskell import name for " ++ show mod ++
              " (is it included in the interface's module list?)") $
    M.lookup mod $
    interfaceHaskellModuleImportNames iface

-- | Adds a qualified import of the given external name's module into the current
-- module, and returns the qualified name of the import.  If the external name
-- is defined in the current module, then this is a no-op and 'Nothing' is
-- returned.
importHsModuleForExtName :: ExtName -> Generator (Maybe String)
importHsModuleForExtName extName = do
  currentModule <- askModule
  owningModule <- getExtNameModule extName
  if currentModule == owningModule
    then return Nothing
    else do iface <- askInterface
            let fullName = getModuleName iface owningModule
            qualifiedName <- getModuleImportName owningModule
            addImports $ hsQualifiedImport fullName qualifiedName
            return $ Just qualifiedName

-- | Used like @addExtNameModule extName hsEntity@.  @hsEntity@ is a name in
-- Haskell code that is generated from the definition of @extName@, and thus
-- lives in @extName@'s module.  This function adds imports and returns a
-- qualified name as necessary to refer to the given entity.
addExtNameModule :: ExtName -> String -> Generator String
addExtNameModule extName hsEntity = do
  maybeImportName <- importHsModuleForExtName extName
  return $ case maybeImportName of
    Nothing -> hsEntity  -- Same module.
    Just importName -> concat [importName, ".", hsEntity]  -- Different module.

-- | Internal helper function for constructing Haskell names from external
-- names.  Returns a name that is a suitable Haskell type name for the external
-- name, and if given 'Const', then with @\"Const\"@ appended.
toHsTypeName :: Constness -> ExtName -> Generator String
toHsTypeName cst extName =
  inFunction "toHsTypeName" $
  addExtNameModule extName $ toHsTypeName' cst extName

-- | Pure version of 'toHsTypeName' that doesn't create a qualified name.
toHsTypeName' :: Constness -> ExtName -> String
toHsTypeName' cst extName =
  (case cst of
      Const -> (++ "Const")
      Nonconst -> id) $
  case fromExtName extName of
    x:xs -> toUpper x:xs
    [] -> []

-- | Returns the Haskell name for an enum.
toHsEnumTypeName :: CppEnum -> Generator String
toHsEnumTypeName enum =
  inFunction "toHsEnumTypeName" $
  addExtNameModule (enumExtName enum) $ toHsEnumTypeName' enum

-- | Pure version of 'toHsEnumTypeName' that doesn't create a qualified name.
toHsEnumTypeName' :: CppEnum -> String
toHsEnumTypeName' = toHsTypeName' Nonconst . enumExtName

-- | Constructs the data constructor name for a value in an enum.  Like C++ and
-- unlike say Java, Haskell enum values aren't in a separate enum-specific
-- namespace, so we prepend the enum name to the value name to get the data
-- constructor name.  The value name is a list of words; see 'enumValueNames'.
toHsEnumCtorName :: CppEnum -> [String] -> Generator String
toHsEnumCtorName enum words =
  inFunction "toHsEnumCtorName" $
  addExtNameModule (enumExtName enum) $ toHsEnumCtorName' enum words

-- | Pure version of 'toHsEnumCtorName' that doesn't create a qualified name.
toHsEnumCtorName' :: CppEnum -> [String] -> String
toHsEnumCtorName' enum words =
  concat $ enumValuePrefix enum : map capitalize words

-- | Returns the Haskell name for a bitspace.  See 'toHsEnumTypeName'.
toHsBitspaceTypeName :: Bitspace -> Generator String
toHsBitspaceTypeName bitspace =
  inFunction "toHsBitspaceTypeName" $
  addExtNameModule (bitspaceExtName bitspace) $ toHsBitspaceTypeName' bitspace

-- | Pure version of 'toHsBitspaceTypeName' that doesn't create a qualified name.
toHsBitspaceTypeName' :: Bitspace -> String
toHsBitspaceTypeName' = toHsTypeName' Nonconst . bitspaceExtName

-- | Constructs the data constructor name for a value in a bitspace.  See
-- 'toHsEnumCtorName'.
toHsBitspaceValueName :: Bitspace -> [String] -> Generator String
toHsBitspaceValueName bitspace words =
  inFunction "toHsBitspaceValueName" $
  addExtNameModule (bitspaceExtName bitspace) $
  toHsBitspaceValueName' bitspace words

-- | Pure version of 'toHsBitspaceValueName' that doesn't create a qualified name.
toHsBitspaceValueName' :: Bitspace -> [String] -> String
toHsBitspaceValueName' bitspace words =
  lowerFirst $ concat $ bitspaceValuePrefix bitspace : map capitalize words

-- | Returns the name of the function that will convert a bitspace value into a
-- raw numeric value.
toHsBitspaceToNumName :: Bitspace -> Generator String
toHsBitspaceToNumName bitspace =
  inFunction "toHsBitspaceToNumName" $
  addExtNameModule (bitspaceExtName bitspace) $ toHsBitspaceToNumName' bitspace

-- | Pure version of 'toHsBitspaceToNumName' that doesn't create a qualified name.
toHsBitspaceToNumName' :: Bitspace -> String
toHsBitspaceToNumName' = ("from" ++) . toHsBitspaceTypeName'

-- | The name of the Haskell typeclass that contains a method for converting to
-- a bitspace value.
toHsBitspaceClassName :: Bitspace -> Generator String
toHsBitspaceClassName bitspace =
  inFunction "toHsBitspaceClassName" $
  addExtNameModule (bitspaceExtName bitspace) $ toHsBitspaceClassName' bitspace

-- | Pure version of 'toHsBitspaceClassName' that doesn't create a qualified name.
toHsBitspaceClassName' :: Bitspace -> String
toHsBitspaceClassName' bitspace = 'I':'s':toHsBitspaceTypeName' bitspace

-- | The name of the method in the 'toHsBitspaceClassName' typeclass that
-- constructs bitspace values.
toHsBitspaceFromValueName :: Bitspace -> Generator String
toHsBitspaceFromValueName bitspace =
  inFunction "toHsBitspaceFromValueName" $
  addExtNameModule (bitspaceExtName bitspace) $ toHsBitspaceFromValueName' bitspace

-- | Pure version of 'toHsBitspaceFromValueName' that doesn't create a qualified name.
toHsBitspaceFromValueName' :: Bitspace -> String
toHsBitspaceFromValueName' = ("to" ++) . toHsBitspaceTypeName'

-- | The name for the typeclass of types that can be represented as values of
-- the given C++ class.
toHsValueClassName :: Class -> Generator String
toHsValueClassName cls =
  inFunction "toHsValueClassName" $
  addExtNameModule (classExtName cls) $ toHsValueClassName' cls

-- | Pure version of 'toHsValueClassName' that doesn't create a qualified name.
toHsValueClassName' :: Class -> String
toHsValueClassName' cls = toHsDataTypeName' Nonconst cls ++ "Value"

-- | The name of the method within the 'toHsValueClassName' typeclass for
-- accessing an object of the type as a pointer.
toHsWithValuePtrName :: Class -> Generator String
toHsWithValuePtrName cls =
  inFunction "toHsWithValuePtrName" $
  addExtNameModule (classExtName cls) $ toHsWithValuePtrName' cls

-- | Pure version of 'toHsWithValuePtrName' that doesn't create a qualified name.
toHsWithValuePtrName' :: Class -> String
toHsWithValuePtrName' cls = concat ["with", toHsDataTypeName' Nonconst cls, "Ptr"]

-- | The name for the typeclass of types that are (possibly const) pointers to
-- objects of the given C++ class, or subclasses.
toHsPtrClassName :: Constness -> Class -> Generator String
toHsPtrClassName cst cls =
  inFunction "toHsPtrClassName" $
  addExtNameModule (classExtName cls) $ toHsPtrClassName' cst cls

-- | Pure version of 'toHsPtrClassName' that doesn't create a qualified name.
toHsPtrClassName' :: Constness -> Class -> String
toHsPtrClassName' cst cls = toHsDataTypeName' cst cls ++ "Ptr"

-- | The name of the function that upcasts pointers to the specific class type
-- and constness.
toHsCastMethodName :: Constness -> Class -> Generator String
toHsCastMethodName cst cls =
  inFunction "toHsCastMethodName" $
  addExtNameModule (classExtName cls) $ toHsCastMethodName' cst cls

-- | Pure version of 'toHsCastMethodName' that doesn't create a qualified name.
toHsCastMethodName' :: Constness -> Class -> String
toHsCastMethodName' cst cls = "to" ++ toHsDataTypeName' cst cls

-- | The name of the typeclass that provides a method to downcast to a specific
-- class type.  See 'toHsDownCastMethodName'.
toHsDownCastClassName :: Constness -> Class -> Generator String
toHsDownCastClassName cst cls =
  inFunction "toHsDownCastClassName" $
  addExtNameModule (classExtName cls) $ toHsDownCastClassName' cst cls

-- | Pure version of 'toHsDownCastClassName' that doesn't create a qualified
-- name.
toHsDownCastClassName' :: Constness -> Class -> String
toHsDownCastClassName' cst cls =
  concat [toHsDataTypeName' Nonconst cls,
          "Super",
          case cst of
            Const -> "Const"
            Nonconst -> ""]

-- | The name of the function that downcasts pointers to the specific class type
-- and constness.
toHsDownCastMethodName :: Constness -> Class -> Generator String
toHsDownCastMethodName cst cls =
  inFunction "toHsDownCastMethodName" $
  addExtNameModule (classExtName cls) $ toHsDownCastMethodName' cst cls

-- | Pure version of 'toHsDownCastMethodName' that doesn't create a qualified
-- name.
toHsDownCastMethodName' :: Constness -> Class -> String
toHsDownCastMethodName' cst cls = "downTo" ++ toHsDataTypeName' cst cls

-- | The import name for the foreign function that casts between two specific
-- pointer types.  Used for upcasting and downcasting.
--
-- We need to know which module the cast function resides in, and while we could
-- look this up, the caller always knows, so we just have them pass it in.
toHsCastPrimitiveName :: Class -> Class -> Class -> Generator String
toHsCastPrimitiveName descendentClass from to =
  inFunction "toHsCastPrimitiveName" $
  addExtNameModule (classExtName descendentClass) $ toHsCastPrimitiveName' from to

-- | Pure version of 'toHsCastPrimitiveName' that doesn't create a qualified
-- name.
toHsCastPrimitiveName' :: Class -> Class -> String
toHsCastPrimitiveName' from to =
  concat ["cast", toHsDataTypeName' Nonconst from, "To", toHsDataTypeName' Nonconst to]

-- | The name of one of the functions that add/remove const to/from a class's
-- pointer type.  Given 'Const', it will return the function that adds const,
-- and given 'Nonconst', it will return the function that removes const.
toHsConstCastFnName :: Constness -> Class -> Generator String
toHsConstCastFnName cst cls =
  inFunction "toHsConstCastFnName" $
  addExtNameModule (classExtName cls) $ toHsConstCastFnName' cst cls

-- | Pure version of 'toHsConstCastFnName' that doesn't create a qualified name.
toHsConstCastFnName' :: Constness -> Class -> String
toHsConstCastFnName' cst cls =
  concat ["cast", toHsDataTypeName' Nonconst cls,
          case cst of
            Const -> "ToConst"
            Nonconst -> "ToNonconst"]

-- | The name of the data type that represents a pointer to an object of the
-- given class and constness.
toHsDataTypeName :: Constness -> Class -> Generator String
toHsDataTypeName cst cls =
  inFunction "toHsDataTypeName" $
  addExtNameModule (classExtName cls) $ toHsDataTypeName' cst cls

-- | Pure version of 'toHsDataTypeName' that doesn't create a qualified name.
toHsDataTypeName' :: Constness -> Class -> String
toHsDataTypeName' cst cls = toHsTypeName' cst $ classExtName cls

-- | The name of a data constructor for one of the object pointer types.
toHsDataCtorName :: Managed -> Constness -> Class -> Generator String
toHsDataCtorName m cst cls =
  inFunction "toHsDataCtorName" $
  addExtNameModule (classExtName cls) $ toHsDataCtorName' m cst cls

-- | Pure version of 'toHsDataCtorName' that doesn't create a qualified name.
toHsDataCtorName' :: Managed -> Constness -> Class -> String
toHsDataCtorName' m cst cls = case m of
  Unmanaged -> base
  Managed -> base ++ "Gc"
  where base = toHsDataTypeName' cst cls

-- | The name of the foreign function import wrapping @delete@ for the given
-- class type.  This is in internal to the binding; normal users should use
-- 'Foreign.Hoppy.Runtime.delete'.
--
-- This is internal to a generated Haskell module, so it does not have a public
-- (qualified) form.
toHsClassDeleteFnName' :: Class -> String
toHsClassDeleteFnName' cls = 'd':'e':'l':'e':'t':'e':'\'':toHsDataTypeName' Nonconst cls

-- | The name of the foreign import that imports the same function as
-- 'toHsClassDeleteFnName', but as a 'Foreign.Ptr.FunPtr' rather than an actual
-- function.
--
-- This is internal to a generated Haskell module, so it does not have a public
-- (qualified) form.
toHsClassDeleteFnPtrName' :: Class -> String
toHsClassDeleteFnPtrName' cls =
  'd':'e':'l':'e':'t':'e':'P':'t':'r':'\'':toHsDataTypeName' Nonconst cls

-- | Returns the name of the Haskell function that invokes the given
-- constructor.
toHsCtorName :: Class -> Ctor -> Generator String
toHsCtorName cls ctor =
  inFunction "toHsCtorName" $
  toHsClassEntityName cls $ fromExtName $ ctorExtName ctor

-- | Pure version of 'toHsCtorName' that doesn't create a qualified name.
toHsCtorName' :: Class -> Ctor -> String
toHsCtorName' cls ctor =
  toHsClassEntityName' cls $ fromExtName $ ctorExtName ctor

-- | Returns the name of the Haskell function that invokes the given method.
toHsMethodName :: Class -> Method -> Generator String
toHsMethodName cls method =
  inFunction "toHsMethodName" $
  toHsClassEntityName cls $ fromExtName $ methodExtName method

-- | Pure version of 'toHsMethodName' that doesn't create a qualified name.
toHsMethodName' :: Class -> Method -> String
toHsMethodName' cls method =
  toHsClassEntityName' cls $ fromExtName $ methodExtName method

-- | Returns the name of the Haskell function for an entity in a class.
toHsClassEntityName :: IsFnName String name => Class -> name -> Generator String
toHsClassEntityName cls name =
  addExtNameModule (classExtName cls) $ toHsClassEntityName' cls name

-- | Pure version of 'toHsClassEntityName' that doesn't create a qualified name.
toHsClassEntityName' :: IsFnName String name => Class -> name -> String
toHsClassEntityName' cls name =
  lowerFirst $ fromExtName $
  classEntityForeignName' cls $
  case toFnName name of
    FnName name -> toExtName name
    FnOp op -> operatorPreferredExtName op

-- | The name of the function that takes a Haskell function and wraps it in a
-- callback object.  This is internal to the binding; normal users can pass
-- Haskell functions to be used as callbacks inplicitly.
toHsCallbackCtorName :: Callback -> Generator String
toHsCallbackCtorName callback =
  inFunction "toHsCallbackCtorName" $
  addExtNameModule (callbackExtName callback) $ toHsCallbackCtorName' callback

-- | Pure version of 'toHsCallbackCtorName' that doesn't create a qualified
-- name.
toHsCallbackCtorName' :: Callback -> String
toHsCallbackCtorName' callback =
  toHsFnName' $ toExtName $ fromExtName (callbackExtName callback) ++ "_new"

-- | The name of the function that takes a Haskell function with Haskell-side
-- types and wraps it in a 'Foreign.Ptr.FunPtr' that does appropriate
-- conversions to and from C-side types.
toHsCallbackNewFunPtrFnName :: Callback -> Generator String
toHsCallbackNewFunPtrFnName callback =
  inFunction "toHsCallbackNewFunPtrFnName" $
  addExtNameModule (callbackExtName callback) $ toHsCallbackNewFunPtrFnName' callback

-- | Pure version of 'toHsCallbackNewFunPtrFnName' that doesn't create a qualified
-- name.
toHsCallbackNewFunPtrFnName' :: Callback -> String
toHsCallbackNewFunPtrFnName' callback =
  toHsFnName' $ toExtName $ fromExtName (callbackExtName callback) ++ "_newFunPtr"

-- | Converts an external name into a name suitable for a Haskell function or
-- variable.
toHsFnName :: ExtName -> Generator String
toHsFnName extName =
  inFunction "toHsFnName" $
  addExtNameModule extName $ toHsFnName' extName

-- | Pure version of 'toHsFnName' that doesn't create a qualified name.
toHsFnName' :: ExtName -> String
toHsFnName' = lowerFirst . fromExtName

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
    Internal_TVoid -> return $ HsTyCon $ Special HsUnitCon
    -- C++ has sizeof(bool) == 1, whereas Haskell can > 1, so we have to convert.
    Internal_TBool -> case side of
      HsCSide -> addImports hsImportForRuntime $> HsTyCon (UnQual $ HsIdent "HoppyFHR.CBool")
      HsHsSide -> addImports hsImportForPrelude $> HsTyCon (UnQual $ HsIdent "HoppyP.Bool")
    Internal_TChar -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CChar")
    Internal_TUChar -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CUChar")
    Internal_TShort -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CShort")
    Internal_TUShort ->
      addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CUShort")
    Internal_TInt -> case side of
      HsCSide -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CInt")
      HsHsSide -> addImports hsImportForPrelude $> HsTyCon (UnQual $ HsIdent "HoppyP.Int")
    Internal_TUInt -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CUInt")
    Internal_TLong -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CLong")
    Internal_TULong -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CULong")
    Internal_TLLong -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CLLong")
    Internal_TULLong ->
      addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CULLong")
    Internal_TFloat -> case side of
      HsCSide -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CFloat")
      HsHsSide -> addImports hsImportForPrelude $> HsTyCon (UnQual $ HsIdent "HoppyP.Float")
    Internal_TDouble -> case side of
      HsCSide -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CDouble")
      HsHsSide -> addImports hsImportForPrelude $> HsTyCon (UnQual $ HsIdent "HoppyP.Double")
    Internal_TInt8 -> addImports hsImportForInt $> HsTyCon (UnQual $ HsIdent "HoppyDI.Int8")
    Internal_TInt16 -> addImports hsImportForInt $> HsTyCon (UnQual $ HsIdent "HoppyDI.Int16")
    Internal_TInt32 -> addImports hsImportForInt $> HsTyCon (UnQual $ HsIdent "HoppyDI.Int32")
    Internal_TInt64 -> addImports hsImportForInt $> HsTyCon (UnQual $ HsIdent "HoppyDI.Int64")
    Internal_TWord8 -> addImports hsImportForWord $> HsTyCon (UnQual $ HsIdent "HoppyDW.Word8")
    Internal_TWord16 -> addImports hsImportForWord $> HsTyCon (UnQual $ HsIdent "HoppyDW.Word16")
    Internal_TWord32 -> addImports hsImportForWord $> HsTyCon (UnQual $ HsIdent "HoppyDW.Word32")
    Internal_TWord64 -> addImports hsImportForWord $> HsTyCon (UnQual $ HsIdent "HoppyDW.Word64")
    Internal_TPtrdiff ->
      addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CPtrdiff")
    Internal_TSize -> addImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "HoppyFC.CSize")
    Internal_TSSize ->
      addImports hsImportForSystemPosixTypes $> HsTyCon (UnQual $ HsIdent "HoppySPT.CSsize")
    Internal_TEnum e -> HsTyCon . UnQual . HsIdent <$> case side of
      HsCSide -> addImports hsImportForForeignC $> "HoppyFC.CInt"
      HsHsSide -> toHsEnumTypeName e
    Internal_TBitspace b -> case side of
      HsCSide -> cppTypeToHsTypeAndUse side $ bitspaceType b
      HsHsSide -> do
        typeName <- toHsBitspaceTypeName b
        return $ HsTyCon $ UnQual $ HsIdent typeName
    Internal_TPtr (Internal_TObj cls) -> do
      -- Same as TPtr (TConst (TObj cls)), but nonconst.
      typeName <- toHsTypeName Nonconst $ classExtName cls
      let dataType = HsTyCon $ UnQual $ HsIdent typeName
      case side of
        HsCSide -> do
          addImports hsImportForForeign
          return $ HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.Ptr") dataType
        HsHsSide -> return dataType
    Internal_TPtr (Internal_TConst (Internal_TObj cls)) -> do
      -- Same as TPtr (TObj cls), but const.
      typeName <- toHsTypeName Const $ classExtName cls
      let dataType = HsTyCon $ UnQual $ HsIdent typeName
      case side of
        HsCSide -> do
          addImports hsImportForForeign
          return $ HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.Ptr") dataType
        HsHsSide -> return dataType
    Internal_TPtr fn@(Internal_TFn {}) -> do
      addImports hsImportForForeign
      HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.FunPtr") <$> cppTypeToHsTypeAndUse HsCSide fn
    Internal_TPtr t' -> do
      addImports hsImportForForeign
      -- Pointers to types not covered above point to raw C++ values, so we need
      -- to use the C-side type of the pointer target here.
      HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.Ptr") <$> cppTypeToHsTypeAndUse HsCSide t'
    Internal_TRef t' -> cppTypeToHsTypeAndUse side $ ptrT t'
    Internal_TFn paramTypes retType -> do
      paramHsTypes <- mapM (cppTypeToHsTypeAndUse side) paramTypes
      retHsType <- cppTypeToHsTypeAndUse side retType
      addImports hsImportForPrelude
      return $
        foldr HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") retHsType) paramHsTypes
    Internal_TCallback cb -> do
      hsType <- cppTypeToHsTypeAndUse side =<< callbackToTFn side cb
      case side of
        HsHsSide -> return hsType
        HsCSide -> do
          addImports hsImportForRuntime
          return $ HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyFHR.CCallback") hsType
    Internal_TObj cls -> case side of
      HsCSide -> cppTypeToHsTypeAndUse side $ ptrT $ constT t
      HsHsSide -> case classHaskellConversionType $ getClassHaskellConversion cls of
        Just typeGen -> typeGen
        Nothing ->
          throwError $ concat
          ["Expected a Haskell type for ", show cls, " but there isn't one"]
    Internal_TObjToHeap cls -> cppTypeToHsTypeAndUse side $ ptrT $ objT cls
    Internal_TToGc t' -> case t' of
      Internal_TRef _ -> cppTypeToHsTypeAndUse side t'  -- References behave the same as pointers.
      Internal_TPtr _ -> cppTypeToHsTypeAndUse side t'
      Internal_TObj cls -> cppTypeToHsTypeAndUse side $ ptrT $ objT cls
      _ -> throwError $ tToGcInvalidFormErrorMessage Nothing t'
    Internal_TConst t' -> cppTypeToHsTypeAndUse side t'

-- | Returns the 'ClassHaskellConversion' of a class.
getClassHaskellConversion :: Class -> ClassHaskellConversion
getClassHaskellConversion = classHaskellConversion . classConversion

-- | Constructs the function type for a callback.  For Haskell, the type depends
-- on the side; the C++ side has additional parameters.
--
-- Keep this in sync with the C++ generator's version.
callbackToTFn :: HsTypeSide -> Callback -> Generator Type
callbackToTFn side cb = do
  needsExcParams <- case side of
    HsCSide -> mayThrow
    HsHsSide -> return False
  return $ Internal_TFn ((if needsExcParams then addExcParams else id) $ callbackParams cb)
                        (callbackReturn cb)

  where mayThrow = case callbackThrows cb of
          Just t -> return t
          Nothing -> moduleCallbacksThrow <$> askModule >>= \mt -> case mt of
            Just t -> return t
            Nothing -> interfaceCallbacksThrow <$> askInterface

        addExcParams = (++ [ptrT intT, ptrT $ ptrT voidT])

-- | Prints a value like 'P.prettyPrint', but removes newlines so that they
-- don't cause problems with this module's textual generation.  Should be mainly
-- used for printing types; stripping newlines from definitions for example
-- could go badly.
prettyPrint :: P.Pretty a => a -> String
prettyPrint = collapseSpaces . filter (/= '\n') . P.prettyPrint
  where collapseSpaces (' ':xs) = ' ' : collapseSpaces (dropWhile (== ' ') xs)
        collapseSpaces (x:xs) = x : collapseSpaces xs
        collapseSpaces [] = []
