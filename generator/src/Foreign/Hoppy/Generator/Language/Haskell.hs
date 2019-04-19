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
  Env (..),
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
  SayExportMode (..),
  sayLn,
  saysLn,
  ln,
  indent,
  indentSpaces,
  sayLet,
  getExtNameModule,
  addExtNameModule,
  toHsTypeName,
  toHsTypeName',
  toHsFnName,
  toHsFnName',
  toArgName,
  HsTypeSide (..),
  cppTypeToHsTypeAndUse,
  getClassHaskellConversion,
  getEffectiveExceptionHandlers,
  prettyPrint,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Arrow (first)
import Control.Monad.Except (Except, catchError, runExcept, throwError)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Writer (WriterT, censor, runWriterT, tell)
import Data.Char (toUpper)
import Data.Foldable (forM_)
import Data.Function (on)
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid, mappend, mconcat, mempty)
#endif
import Data.Semigroup as Sem
import qualified Data.Set as S
import Data.Tuple (swap)
import Foreign.Hoppy.Generator.Common
import Foreign.Hoppy.Generator.Spec.Base
import {-# SOURCE #-} Foreign.Hoppy.Generator.Spec.Class (
  Class,
  ClassHaskellConversion,
  classConversion,
  classExtName,
  classHaskellConversion,
  classHaskellConversionType,
  )
import Foreign.Hoppy.Generator.Types (constT, objT, ptrT)
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
type Generator = ReaderT Env (WriterT Output (Except ErrorMsg))

-- | Context information for generating Haskell code.
data Env = Env
  { envInterface :: Interface
  , envModule :: Module
  , envModuleName :: String
  }

-- | Returns the currently generating interface.
askInterface :: Generator Interface
askInterface = asks envInterface

-- | Returns the currently generating module.
askModule :: Generator Module
askModule = asks envModule

-- | Returns the currently generating module's Haskell module name.
askModuleName :: Generator String
askModuleName = asks envModuleName

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

instance Sem.Semigroup Output where
  (Output e i b x) <> (Output e' i' b' x') =
    Output (e <> e') (i <> i') (b <> b') (x <> x')

instance Monoid Output where
  mempty = Output mempty mempty mempty mempty

  mappend = (<>)

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
     runExcept $
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

-- | The section of code that Hoppy is generating, for an export.
data SayExportMode =
    SayExportForeignImports
    -- ^ Hoppy is generating @foreign import@ statements for an export.  This is
    -- separate from the main 'SayExportDecls' phase because foreign import
    -- statements are emitted directly by a 'Generator', and these need to
    -- appear earlier in the code.
  | SayExportDecls
    -- ^ Hoppy is generating Haskell code to bind to the export.  This is the
    -- main step of Haskell code generation for an export.
    --
    -- Here, imports of Haskell modules should be added with 'LH.addImports'
    -- rather than emitting an @import@ statement yourself in the foreign import
    -- step.  'LH.addExtNameModule' may be used to import and reference the
    -- Haskell module of another export.
  | SayExportBoot
    -- ^ If Hoppy needs to generate @hs-boot@ files to break circular
    -- dependences between generated modules, then for each export in each
    -- module involved in a cycle, it will call the generator in this mode to
    -- produce @hs-boot@ code.  This code should provide a minimal declaration
    -- of Haskell entities generated by 'SayExportDecls', without providing any
    -- implementation.
    --
    -- For information on the special format of @hs-boot@ files, see the
    -- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html#how-to-compile-mutually-recursive-modules GHC User's Guide>.
  deriving (Eq, Show)

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

-- | Looks up the module that exports an external name.  Throws an error if the
-- external name is not exported.
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

-- | Constructs Haskell names from external names.  Returns a name that is a
-- suitable Haskell type name for the external name, and if given 'Const', then
-- with @\"Const\"@ appended.
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
    Internal_TFn params retType -> do
      paramHsTypes <- mapM (cppTypeToHsTypeAndUse side . parameterType) params
      retHsType <- cppTypeToHsTypeAndUse side retType
      addImports hsImportForPrelude
      return $
        foldr HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") retHsType) paramHsTypes
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
    Internal_TManual s -> case conversionSpecHaskell s of
      Just h -> case side of
        HsHsSide -> conversionSpecHaskellHsType h
        HsCSide -> fromMaybe (conversionSpecHaskellHsType h) $
                   conversionSpecHaskellCType h
      Nothing -> throwError $ show s ++ " defines no Haskell conversion"
    Internal_TConst t' -> cppTypeToHsTypeAndUse side t'

-- | Returns the 'ClassHaskellConversion' of a class.
getClassHaskellConversion :: Class -> ClassHaskellConversion
getClassHaskellConversion = classHaskellConversion . classConversion

-- | Combines the given exception handlers (from a particular exported entity)
-- with the handlers from the current module and interface.  The given handlers
-- have highest precedence, followed by module handlers, followed by interface
-- handlers.
getEffectiveExceptionHandlers :: ExceptionHandlers -> Generator ExceptionHandlers
getEffectiveExceptionHandlers handlers = do
  ifaceHandlers <- interfaceExceptionHandlers <$> askInterface
  moduleHandlers <- getExceptionHandlers <$> askModule
  -- Exception handlers declared lower in the hierarchy take precedence over
  -- those higher in the hierarchy; ExceptionHandlers is a left-biased monoid.
  return $ mconcat [handlers, moduleHandlers, ifaceHandlers]

-- | Prints a value like 'P.prettyPrint', but removes newlines so that they
-- don't cause problems with this module's textual generation.  Should be mainly
-- used for printing types; stripping newlines from definitions for example
-- could go badly.
prettyPrint :: P.Pretty a => a -> String
prettyPrint = collapseSpaces . filter (/= '\n') . P.prettyPrint
  where collapseSpaces (' ':xs) = ' ' : collapseSpaces (dropWhile (== ' ') xs)
        collapseSpaces (x:xs) = x : collapseSpaces xs
        collapseSpaces [] = []
