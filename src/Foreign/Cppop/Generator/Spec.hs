module Foreign.Cppop.Generator.Spec (
  -- * Interfaces
  Interface,
  ErrorMsg,
  HaskellImport,
  interface,
  interfaceName,
  interfaceModules,
  interfaceNamesToModules,
  interfaceHaskellModuleBase,
  addInterfaceHaskellModuleBase,
  -- * C++ includes
  Include,
  includeStd,
  includeLocal,
  includeToString,
  -- * Modules
  Module,
  moduleName,
  moduleHppPath,
  moduleCppPath,
  moduleExports,
  moduleHaskellName,
  moduleReqs,
  makeModule,
  modifyModule,
  modifyModule',
  setModuleHppPath,
  setModuleCppPath,
  addModuleExports,
  addModuleHaskellName,
  -- * Requirements
  Reqs,
  reqsIncludes,
  reqInclude,
  HasUseReqs (..),
  addReqIncludes,
  -- * Exports
  ExtName,
  toExtName,
  fromExtName,
  Export (..),
  exportExtName,
  Identifier,
  idNamespaces,
  idName,
  idToString,
  ident,
  ident',
  ident1,
  ident2,
  ident3,
  ident4,
  ident5,
  -- * Basic types
  Type (..),
  CppEnum, makeEnum, enumIdentifier, enumExtName, enumValueNames, enumUseReqs,
  Purity (..),
  Function, makeFn, fnIdentifier, fnExtName, fnPurity, fnParams, fnReturn, fnUseReqs,
  Class, makeClass, classIdentifier, classExtName, classSuperclasses, classCtors, classMethods,
  classConversions, classUseReqs,
  Ctor, makeCtor, ctorExtName, ctorParams,
  Method,
  MethodApplicability (..),
  Constness (..),
  Staticness (..),
  makeMethod, methodCName, methodExtName, methodApplicability, methodPurity, methodParams,
  methodReturn, methodConst, methodStatic,
  -- ** Conversions to and from foreign values
  ClassConversions (..),
  classConversionsNone,
  classModifyConversions,
  ClassHaskellConversion (..),
  -- * Callbacks
  Callback, makeCallback, callbackExtName, callbackParams, callbackReturn, callbackToTFn,
  -- * Haskell imports
  HsModuleName, HsImportSet, HsImportKey (..), HsImportSpecs (..), HsImportName, HsImportVal (..),
  hsWholeModuleImport, hsQualifiedImport, hsImport1, hsImport1', hsImports, hsImports',
  -- ** Internal to Cppop
  makeHsImportSet,
  getHsImportSet,
  hsImportForForeign,
  hsImportForForeignC,
  hsImportForPrelude,
  hsImportForSupport,
  hsImportForUnsafeIO,
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Control.Monad (liftM2, unless)
import Control.Monad.Except (MonadError, Except, runExcept, throwError)
import Control.Monad.State (MonadState, StateT, execStateT, get, modify)
import Data.Char (isAlpha, isAlphaNum)
import Data.Function (on)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mappend, mconcat, mempty)
import qualified Data.Set as S
import Language.Haskell.Syntax (HsType)

type ErrorMsg = String

type HaskellImport = String

-- | A complete specification of a C++ API.  Generators for different languages,
-- including the server generator for C++, use these to produce their output.
data Interface = Interface
  { interfaceName :: String
    -- ^ Textual name of the interface.
  , interfaceModules :: M.Map String Module
    -- ^ All of the individual modules, by name.
  , interfaceNamesToModules :: M.Map ExtName Module
    -- ^ Maps each 'ExtName' exported by some module to the module that exports
    -- the name.
  , interfaceHaskellModuleBase :: Maybe [String]
  }

instance Show Interface where
  show iface = concat ["<Interface ", show (interfaceName iface), ">"]

-- | An @#include@ directive in a C++ file.
data Include = Include { includeToString :: String }
             deriving (Eq, Ord, Show)

-- | Creates an @#include \<...\>@ directive.
includeStd :: String -> Include
includeStd path = Include $ "#include <" ++ path ++ ">\n"

-- | Creates an @#include "..."@ directive.
includeLocal :: String -> Include
includeLocal path = Include $ "#include \"" ++ path ++ "\"\n"

-- | Constructs an 'Interface' from the required parts.  Some validation is
-- performed; if the resulting interface would be invalid, an error message is
-- returned instead.
interface :: String  -- ^ 'interfaceName'
          -> [Module]  -- ^ 'interfaceModules'
          -> Either ErrorMsg Interface
interface ifName modules = do
  -- TODO Check for duplicate module names.
  -- TODO Check for duplicate module file paths.

  -- Check for multiple modules exporting an ExtName.
  let extNamesToModules :: M.Map ExtName [Module]
      extNamesToModules =
        M.unionsWith (++) $
        map (\m -> const [m] <$> moduleExports m) modules

      extNamesInMultipleModules :: [(ExtName, [Module])]
      extNamesInMultipleModules =
        M.toList $
        M.filter (\modules -> case modules of
                     _:_:_ -> True
                     _ -> False)
        extNamesToModules

  unless (null extNamesInMultipleModules) $
    Left $ unlines $
    "Some external name(s) are exported by multiple modules:" :
    map (\(extName, modules) ->
          concat $ "- " : show extName : ": " : map show modules)
        extNamesInMultipleModules

  return Interface
    { interfaceName = ifName
    , interfaceModules = M.fromList $ map (moduleName &&& id) modules
    , interfaceNamesToModules = M.map (\[x] -> x) extNamesToModules
    , interfaceHaskellModuleBase = Nothing
    }

addInterfaceHaskellModuleBase :: [String] -> Interface -> Either String Interface
addInterfaceHaskellModuleBase modulePath iface = case interfaceHaskellModuleBase iface of
  Nothing -> Right iface { interfaceHaskellModuleBase = Just modulePath }
  Just existingPath ->
    Left $ concat
    [ "addInterfaceHaskellModuleBase: Trying to add Haskell module base "
    , intercalate "." modulePath, " to ", show iface
    , " which already has a module base ", intercalate "." existingPath
    ]

data Module = Module
  { moduleName :: String
  , moduleHppPath :: String
  , moduleCppPath :: String
  , moduleExports :: M.Map ExtName Export
  , moduleReqs :: Reqs
  , moduleHaskellName :: Maybe [String]
  }

instance Eq Module where
  (==) = (==) `on` moduleName

instance Ord Module where
  compare = compare `on` moduleName

instance Show Module where
  show m = concat ["<Module ", moduleName m, ">"]

instance HasUseReqs Module where
  getUseReqs = moduleReqs
  setUseReqs reqs m = m { moduleReqs = reqs }

makeModule :: String  -- ^ The module name.
           -> String
           -- ^ The path within a project to a C++ header file to generate.
           -> String
           -- ^ The path within a project to a C++ source file to generate.
           -> Module
makeModule name hppPath cppPath = Module
  { moduleName = name
  , moduleHppPath = hppPath
  , moduleCppPath = cppPath
  , moduleExports = M.empty
  , moduleReqs = mempty
  , moduleHaskellName = Nothing
  }

modifyModule :: Module -> StateT Module (Except String) () -> Either String Module
modifyModule m action = runExcept $ execStateT action m

modifyModule' :: Module -> StateT Module (Except String) () -> Module
modifyModule' m action = case modifyModule m action of
  Left errorMsg ->
    error $ concat
    ["modifyModule' failed to modify ", show m, ": ", errorMsg]
  Right m' -> m'

setModuleHppPath :: MonadState Module m => String -> m ()
setModuleHppPath path = modify $ \m -> m { moduleHppPath = path }

setModuleCppPath :: MonadState Module m => String -> m ()
setModuleCppPath path = modify $ \m -> m { moduleCppPath = path }

addModuleExports :: (MonadError String m, MonadState Module m) => [Export] -> m ()
addModuleExports exports = do
  m <- get
  let existingExports = moduleExports m
      newExports = M.fromList $ map (exportExtName &&& id) exports
      duplicateNames = (S.intersection `on` M.keysSet) existingExports newExports
  if S.null duplicateNames
    then modify $ \m -> m { moduleExports = existingExports `mappend` newExports }
    else throwError $ concat
         ["addModuleExports: ", show m, " defines external names multiple times: ",
          show duplicateNames]

addModuleHaskellName :: (MonadError String m, MonadState Module m) => [String] -> m ()
addModuleHaskellName name = do
  m <- get
  case moduleHaskellName m of
    Nothing -> modify $ \m -> m { moduleHaskellName = Just name }
    Just name' ->
      throwError $ concat
      ["addModuleHaskellName: ", show m, " already has Haskell name ",
       show name', "; trying to add name ", show name, "."]

data Reqs = Reqs
  { reqsIncludes :: S.Set Include
  } deriving (Show)

instance Monoid Reqs where
  mempty = Reqs mempty

  mappend (Reqs incl) (Reqs incl') = Reqs $ mappend incl incl'

  mconcat reqs = Reqs $ mconcat $ map reqsIncludes reqs

reqInclude :: Include -> Reqs
reqInclude include = mempty { reqsIncludes = S.singleton include }

class HasUseReqs a where
  getUseReqs :: a -> Reqs

  setUseReqs :: Reqs -> a -> a
  setUseReqs = modifyUseReqs . const

  modifyUseReqs :: (Reqs -> Reqs) -> a -> a
  modifyUseReqs f x = setUseReqs (f $ getUseReqs x) x

addReqIncludes :: HasUseReqs a => [Include] -> a -> a
addReqIncludes includes =
  modifyUseReqs $ mappend mempty { reqsIncludes = S.fromList includes }

-- | An external name is a string that Cppop clients use to uniquely identify an
-- object to invoke at runtime.  An external name must start with an alphabetic
-- character, and may only contain alphanumeric characters and @'_'@.
newtype ExtName = ExtName
  { fromExtName :: String
    -- ^ Returns the string an an 'ExtName' contains.
  } deriving (Eq, Ord)

instance Show ExtName where
  show extName = concat ["$\"", fromExtName extName, "\"$"]

-- | Creates an 'ExtName' that contains the given string, erroring if the string
-- is an invalid 'ExtName'.
toExtName :: String -> ExtName
toExtName str = case str of
  [] -> error "An ExtName cannot be empty."
  c:cs -> if isAlpha c && all ((||) <$> isAlphaNum <*> (== '_')) cs
          then ExtName str
          else error $
               "An ExtName must start with a letter and only contain letters, numbers, and '_': " ++
               show str

-- | Generates an 'ExtName' from an 'Identifier', if the given name is absent.
extNameOrIdentifier :: Identifier -> Maybe ExtName -> ExtName
extNameOrIdentifier identifier =
  fromMaybe $ toExtName $ idName identifier

-- | Specifies some C++ object (function or class) to give access to.
data Export =
  ExportEnum CppEnum
  | ExportFn Function
  | ExportClass Class
  | ExportCallback Callback
  deriving (Show)

-- | Returns the external name of an export.
exportExtName :: Export -> ExtName
exportExtName export = case export of
  ExportEnum e -> enumExtName e
  ExportFn f -> fnExtName f
  ExportClass c -> classExtName c
  ExportCallback cb -> callbackExtName cb

-- | An absolute path from the top-level C++ namespace down to some named
-- object.
data Identifier = Identifier
  { idNamespaces :: [String]
    -- ^ Returns the namespaces of an identifier; i.e., all components except the last.
  , idName :: String
    -- ^ Returns the last component of the identifier.
  }
  deriving (Eq)

instance Show Identifier where
  show identifier = concat ["<Identifier ", idToString identifier, ">"]

-- | Converts an identifier to its C++ form.
idToString :: Identifier -> String
idToString identifier =
  intercalate "::" $ idNamespaces identifier ++ [idName identifier]

-- | Creates an identifier of the form @::a@.
ident :: String -> Identifier
ident = Identifier []

-- | Creates an identifier of the form @::a1::a2::...::aN::b@.
ident' :: [String] -> String -> Identifier
ident' = Identifier

-- | Creates an identifier of the form @::a::b@.
ident1 :: String -> String -> Identifier
ident1 ns1 = ident' [ns1]

-- | Creates an identifier of the form @::a::b::c@.
ident2 :: String -> String -> String -> Identifier
ident2 ns1 ns2 = ident' [ns1, ns2]

-- | Creates an identifier of the form @::a::b::c::d@.
ident3 :: String -> String -> String -> String -> Identifier
ident3 ns1 ns2 ns3 = ident' [ns1, ns2, ns3]

-- | Creates an identifier of the form @::a::b::c::d::e@.
ident4 :: String -> String -> String -> String -> String -> Identifier
ident4 ns1 ns2 ns3 ns4 = ident' [ns1, ns2, ns3, ns4]

-- | Creates an identifier of the form @::a::b::c::d::e::f@.
ident5 :: String -> String -> String -> String -> String -> String -> Identifier
ident5 ns1 ns2 ns3 ns4 ns5 = ident' [ns1, ns2, ns3, ns4, ns5]

-- | Concrete C++ types.  It is possible to represent invalid C++ types with
-- this, but that may result in undefined behaviour or invalid code generation.
--
-- TODO Support templated functions and classes.
data Type =
  TVoid  -- ^ @void@
  | TBool  -- ^ @bool@
  | TChar  -- ^ @char@
  | TUChar  -- ^ @unsigned char@
  | TShort  -- ^ @short int@
  | TUShort  -- ^ @unsigned short int@
  | TInt  -- ^ @int@
  | TUInt  -- ^ @unsigned int@
  | TLong  -- ^ @long int@
  | TULong  -- ^ @unsigned long int@
  | TLLong  -- ^ @long long int@
  | TULLong  -- ^ @unsigned long long int@
  | TFloat  -- ^ @float@
  | TDouble  -- ^ @double@
  | TSize  -- ^ @size_t@
  | TSSize  -- ^ @ssize_t@
  | TEnum CppEnum  -- ^ A C++ @enum@.
  | TPtr Type  -- ^ A poiner to another type.
  | TRef Type  -- ^ A reference to another type.
  | TFn [Type] Type
    -- ^ A function taking parameters and returning a value (or 'TVoid').
    -- Function declarations can use 'TFn' directly; but function pointers must
    -- wrap a 'TFn' in a 'TPtr'.
  | TCallback Callback
  | TObj Class
  | TConst Type
  deriving (Eq, Show)

-- | A C++ enum declaration.
data CppEnum = CppEnum
  { enumIdentifier :: Identifier
  , enumExtName :: ExtName
  , enumValueNames :: [(Int, [String])]
    -- ^ The numeric values and names of the enum values.  A single value's name
    -- is broken up into words.  How the words and ext name get combined to make
    -- a name in a particular foreign language depends on the language.
  , enumUseReqs :: Reqs
  }

instance Eq CppEnum where
  (==) = (==) `on` enumIdentifier

instance Show CppEnum where
  show e = concat ["<Enum ", show (enumExtName e), " ", show (enumIdentifier e), ">"]

instance HasUseReqs CppEnum where
  getUseReqs = enumUseReqs
  setUseReqs reqs e = e { enumUseReqs = reqs }

makeEnum :: Identifier  -- ^ 'enumIdentifier'
         -> Maybe ExtName
         -- ^ An optional external name; will be automatically derived from
         -- the identifier if absent.
         -> [(Int, [String])]  -- ^ 'enumValueNames'
         -> CppEnum
makeEnum identifier maybeExtName valueNames =
  CppEnum identifier (extNameOrIdentifier identifier maybeExtName) valueNames mempty

-- | Calls to pure functions will be executed non-strictly by Haskell.  Calls to
-- impure functions must execute in the IO monad.
--
-- Member functions for mutable classes should not be made pure, because it is
-- difficult in general to control when the call will be made.
data Purity = Nonpure | Pure
            deriving (Eq, Show)

-- | A C++ function declaration.
data Function = Function
  { fnIdentifier :: Identifier
  , fnExtName :: ExtName
  , fnPurity :: Purity
  , fnParams :: [Type]
  , fnReturn :: Type
  , fnUseReqs :: Reqs
    -- ^ Requirements for a binding to call the function.
  }

instance Show Function where
  show fn =
    concat ["<Function ", show (fnExtName fn), " ", show (fnIdentifier fn),
            show (fnParams fn), " ", show (fnReturn fn), ">"]

instance HasUseReqs Function where
  getUseReqs = fnUseReqs
  setUseReqs reqs fn = fn { fnUseReqs = reqs }

makeFn :: Identifier
       -> Maybe ExtName
       -- ^ An optional external name; will be automatically derived from
       -- the identifier if absent.
       -> Purity
       -> [Type]  -- ^ Parameter types.
       -> Type  -- ^ Return type.
       -> Function
makeFn identifier maybeExtName purity paramTypes retType =
  Function identifier
           (extNameOrIdentifier identifier maybeExtName)
           purity paramTypes retType mempty

-- | A C++ class declaration.
data Class = Class
  { classIdentifier :: Identifier
  , classExtName :: ExtName
  , classSuperclasses :: [Class]
  , classCtors :: [Ctor]
  , classMethods :: [Method]
  , classConversions :: ClassConversions
  , classUseReqs :: Reqs
    -- ^ Requirements for a 'Type' to reference this class.
  }

instance Eq Class where
  (==) = (==) `on` classIdentifier

instance Show Class where
  show cls =
    concat ["<Class ", show (classExtName cls), " ", show (classIdentifier cls), ">"]

instance HasUseReqs Class where
  getUseReqs = classUseReqs
  setUseReqs reqs cls = cls { classUseReqs = reqs }

makeClass :: Identifier
          -> Maybe ExtName
          -- ^ An optional external name; will be automatically derived from the
          -- identifier if absent.
          -> [Class]  -- ^ Superclasses.
          -> [Ctor]
          -> [Method]
          -> Class
makeClass identifier maybeExtName supers ctors methods = Class
  { classIdentifier = identifier
  , classExtName = extNameOrIdentifier identifier maybeExtName
  , classSuperclasses = supers
  , classCtors = ctors
  , classMethods = methods
  , classConversions = classConversionsNone
  , classUseReqs = mempty
  }

-- | When a class object is returned from a function or taken as a parameter by
-- value, it needs to be converted into a foreign (non-C++) object.  Conversion
-- may also be performed explicitly.  This data type describes how to perform
-- those conversions.  A class may or may not support conversion; what is said
-- below only applies to classes that are convertable.
--
-- When converting between a C++ value and a foreign value, an intermediate C
-- type is used.  For example, for a @std::string@, a regular C string (@char*@,
-- @'TPtr' 'TChar'@) allocated on the heap is used.  Since callbacks allow
-- calling back into foreign code, either language may call the other, so the
-- side that allocates memory on the heap transfers ownership of that memory to
-- the other language.
--
-- In foreign code, foreign values can be explicitly converted to new C++ (heap)
-- objects, and C++ object pointers can be explicitly converted to foreign
-- values, via special \"encode\" and \"decode\" functions generated for the
-- class.  In the context of these two functions, \"encode\" always converts
-- /to/ a C++ value and \"decode\" always converts /from/ one; this is separate
-- from the use of \"encode\" and \"decode\" in language-specific conversion
-- code (e.g. 'classCppDecoder', 'classCppEncoder', 'haskellEncodingDecoder',
-- 'haskellEncodingEncoder'), where encoding refers to converting a native value
-- to its C type, and decoding vice versa.
data ClassConversions = ClassConversions
  { classHaskellConversion :: Maybe ClassHaskellConversion
    -- ^ Controls how conversions happen in Haskell bindings.
  } deriving (Show)

-- | Encoding parameters for a class that is not encodable or decodable.
classConversionsNone :: ClassConversions
classConversionsNone = ClassConversions Nothing

-- | Modifies classes' 'ClassEncoding' structures with a given function.
classModifyConversions :: (ClassConversions -> ClassConversions) -> Class -> Class
classModifyConversions f cls = cls { classConversions = f $ classConversions cls }

data ClassHaskellConversion = ClassHaskellConversion
  { classHaskellConversionType :: HsType
  , classHaskellConversionTypeImports :: HsImportSet
  , classHaskellConversionToCppFn :: String
    -- ^ A Haskell expression that evaluates to a function that takes an object
    -- of type 'classHaskellConversionType', and returns a pointer to a new
    -- non-const C++ class object in IO.
  , classHaskellConversionToCppImports :: HsImportSet
  , classHaskellConversionFromCppFn :: String
    -- ^ A Haskell expression that evaluates to a function that takes a pointer
    -- to a const C++ class object, and returns an object of type
    -- 'classHaskellConversionType' in IO.
  , classHaskellConversionFromCppImports :: HsImportSet
  } deriving (Show)

-- | A C++ class constructor declaration.
data Ctor = Ctor
  { ctorExtName :: ExtName
  , ctorParams :: [Type]
  }

instance Show Ctor where
  show ctor = concat ["<Ctor ", show (ctorExtName ctor), " ", show (ctorParams ctor), ">"]

makeCtor :: ExtName
         -> [Type]  -- ^ Parameter types.
         -> Ctor
makeCtor = Ctor

-- | A C++ class method declaration.
data Method = Method
  { methodCName :: String
  , methodExtName :: ExtName
  , methodApplicability :: MethodApplicability
  , methodPurity :: Purity
  , methodParams :: [Type]
  , methodReturn :: Type
  }

instance Show Method where
  show method =
    concat ["<Method ", show (methodExtName method), " ", show (methodCName method), " ",
            show (methodApplicability method), " ", show (methodPurity method), " ",
            show (methodParams method), " ", show (methodReturn method), ">"]

data MethodApplicability = MNormal | MStatic | MConst
                         deriving (Eq, Show)

data Constness = Nonconst | Const
               deriving (Eq, Show)

data Staticness = Nonstatic | Static
               deriving (Eq, Show)

makeMethod :: String  -- ^ The C name of the method.
           -> ExtName
           -> MethodApplicability
           -> Purity
           -> [Type]  -- ^ Parameter types.
           -> Type  -- ^ Return type.
           -> Method
makeMethod = Method

methodConst :: Method -> Constness
methodConst method = case methodApplicability method of
  MConst -> Const
  _ -> Nonconst

methodStatic :: Method -> Staticness
methodStatic method = case methodApplicability method of
  MStatic -> Static
  _ -> Nonstatic

-- | A non-C++ function that can be invoked via a C++ functor.
data Callback = Callback
  { callbackExtName :: ExtName
  , callbackParams :: [Type]
  , callbackReturn :: Type
  }

instance Eq Callback where
  (==) = (==) `on` callbackExtName

instance Show Callback where
  show cb =
    concat ["<Callback ", show (callbackExtName cb), " ", show (callbackParams cb), " ",
            show (callbackReturn cb)]

makeCallback :: ExtName
             -> [Type]  -- ^ Parameter types.
             -> Type  -- ^ Return type.
             -> Callback
makeCallback = Callback

-- | Creates a 'TFn' from a callback's parameter and return types.
callbackToTFn :: Callback -> Type
callbackToTFn = TFn <$> callbackParams <*> callbackReturn

-- | A collection of imports for a Haskell module.  This is a monoid: import
-- Statements are merged to give the union of imported bindings.
--
-- This structure supports two specific types of imports:
--     - @import Foo (...)@
--     - @import qualified Foo as Bar@
-- Imports with @as@ but without @qualified@, and @qualified@ imports with a
-- spec list, are not supported.  This satisfies the needs of the code
-- generator, and keeps the merging logic simple.
newtype HsImportSet = HsImportSet { getHsImportSet :: M.Map HsImportKey HsImportSpecs }
                    deriving (Show)

instance Monoid HsImportSet where
  mempty = HsImportSet M.empty

  mappend (HsImportSet m) (HsImportSet m') =
    HsImportSet $ M.unionWith mergeImportSpecs m m'

  mconcat sets =
    HsImportSet $ M.unionsWith mergeImportSpecs $ map getHsImportSet sets

makeHsImportSet :: M.Map HsImportKey HsImportSpecs -> HsImportSet
makeHsImportSet = HsImportSet

-- | A Haskell module name.
type HsModuleName = String

-- | References an occurrence of an import statement, under which bindings can
-- be imported.  Only imported specs under equal 'HsImportKey's may be merged.
data HsImportKey = HsImportKey
  { hsImportModule :: HsModuleName
  , hsImportQualifiedName :: Maybe HsModuleName
  } deriving (Eq, Ord, Show)

-- | A specification of bindings to import from a module.  If 'Nothing', then
-- the entire module is imported.  If @'Just' 'M.empty'@, then only instances
-- are imported.
data HsImportSpecs = HsImportSpecs
  { getHsImportSpecs :: Maybe (M.Map HsImportName HsImportVal)
  , hsImportSource :: Bool
  } deriving (Show)

mergeImportSpecs :: HsImportSpecs -> HsImportSpecs -> HsImportSpecs
mergeImportSpecs (HsImportSpecs mm s) (HsImportSpecs mm' s') =
  HsImportSpecs (liftM2 mergeMaps mm mm') (s || s')
  where mergeMaps = M.unionWith mergeValues
        mergeValues v v' = case (v, v') of
          (HsImportValAll, _) -> HsImportValAll
          (_, HsImportValAll) -> HsImportValAll
          (HsImportValSome s, HsImportValSome s') -> HsImportValSome $ s ++ s'
          (x@(HsImportValSome _), _) -> x
          (_, x@(HsImportValSome _)) -> x
          (HsImportVal, HsImportVal) -> HsImportVal

-- | An identifier that can be imported from a module.  Symbols may be used here
-- when surrounded by parentheses.  Examples are @\"fmap\"@ and @\"(++)\"@.
type HsImportName = String

-- | Specifies how a name is imported.
data HsImportVal =
  HsImportVal
  -- ^ The name is imported, and nothing underneath it is.
  | HsImportValSome [HsImportName]
    -- ^ The name is imported, as are specific names underneath it.  This is a
    -- @X (a, b, c)@ import.
  | HsImportValAll
    -- ^ The name is imported, along with all names underneath it.  This is a @X
    -- (..)@ import.
  deriving (Show)

-- | An import for the entire contents of a Haskell module.
hsWholeModuleImport :: HsModuleName -> HsImportSet
hsWholeModuleImport moduleName =
  HsImportSet $ M.singleton (HsImportKey moduleName Nothing) $
  HsImportSpecs Nothing False

-- | A qualified import of a Haskell module.
hsQualifiedImport :: HsModuleName -> HsModuleName -> HsImportSet
hsQualifiedImport moduleName qualifiedName =
  HsImportSet $ M.singleton (HsImportKey moduleName $ Just qualifiedName) $
  HsImportSpecs Nothing False

-- | An import of a single name from a Haskell module.
hsImport1 :: HsModuleName -> HsImportName -> HsImportSet
hsImport1 moduleName valueName = hsImport1' moduleName valueName HsImportVal

-- | A detailed import of a single name from a Haskell module.
hsImport1' :: HsModuleName -> HsImportName -> HsImportVal -> HsImportSet
hsImport1' moduleName valueName valueType =
  HsImportSet $ M.singleton (HsImportKey moduleName Nothing) $
  HsImportSpecs (Just $ M.singleton valueName valueType) False

-- | An import of multiple names from a Haskell module.
hsImports :: HsModuleName -> [HsImportName] -> HsImportSet
hsImports moduleName names =
  hsImports' moduleName $ map (\name -> (name, HsImportVal)) names

-- | A detailed import of multiple names from a Haskell module.
hsImports' :: HsModuleName -> [(HsImportName, HsImportVal)] -> HsImportSet
hsImports' moduleName values =
  HsImportSet $ M.singleton (HsImportKey moduleName Nothing) $
  HsImportSpecs (Just $ M.fromList values) False

hsImportForForeign :: HsImportSet
hsImportForForeign = hsQualifiedImport "Foreign" "CppopF"

hsImportForForeignC :: HsImportSet
hsImportForForeignC = hsQualifiedImport "Foreign.C" "CppopFC"

hsImportForPrelude :: HsImportSet
hsImportForPrelude = hsQualifiedImport "Prelude" "CppopP"

hsImportForSupport :: HsImportSet
hsImportForSupport = hsQualifiedImport "Foreign.Cppop.Runtime.Support" "CppopFCRS"

hsImportForUnsafeIO :: HsImportSet
hsImportForUnsafeIO = hsQualifiedImport "System.IO.Unsafe" "CppopSIU"
