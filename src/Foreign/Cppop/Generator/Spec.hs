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
  classEncoding, classUseReqs,
  Ctor, makeCtor, ctorExtName, ctorParams,
  Method,
  MethodApplicability (..),
  Constness (..),
  Staticness (..),
  makeMethod, methodCName, methodExtName, methodApplicability, methodPurity, methodParams,
  methodReturn, methodConst, methodStatic,
  -- ** Encoding and decoding
  ClassEncoding (..),
  classEncodingNone,
  classModifyEncoding,
  classCopyEncodingFrom,
  CppCoder (..),
  cppCoderReqs,
  HaskellEncoding (..),
  -- * Callbacks
  Callback, makeCallback, callbackExtName, callbackParams, callbackReturn, callbackToTFn,
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Control.Monad (unless)
import Control.Monad.Except (MonadError, Except, runExcept, throwError)
import Control.Monad.State (MonadState, StateT, execStateT, gets, modify)
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
  } deriving (Show)

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
    "Some external name(s) were are exported by multiple modules:" :
    map (\(extName, modules) ->
          concat ["- ", fromExtName extName, ": ", show (map moduleName modules)])
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
    , intercalate "." modulePath, " to interface ", show (interfaceName iface)
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
    ["modifyModule' failed to modify module ", show (moduleName m), ": ", errorMsg]
  Right m' -> m'

addModuleExports :: (MonadError String m, MonadState Module m) => [Export] -> m ()
addModuleExports exports = do
  existingExports <- gets moduleExports
  let newExports = M.fromList $ map (exportExtName &&& id) exports
      duplicateNames = (S.intersection `on` M.keysSet) existingExports newExports
  if S.null duplicateNames
    then modify $ \m -> m { moduleExports = existingExports `mappend` newExports }
    else throwError $
         "addModuleExports: The following ExtNames are being defined multiple times: " ++
         show duplicateNames

addModuleHaskellName :: (MonadError String m, MonadState Module m) => [String] -> m ()
addModuleHaskellName name = do
  existingName <- gets moduleHaskellName
  case existingName of
    Nothing -> modify $ \m -> m { moduleHaskellName = Just name }
    Just name' ->
      throwError $ "addModuleHaskellName: Module already has Haskell name " ++
      show name' ++ "; trying to add name " ++ show name ++ "."

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
  } deriving (Eq, Ord, Show)

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
  deriving (Eq, Show)

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
  } deriving (Show)

instance Eq CppEnum where
  (==) = (==) `on` enumIdentifier

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
  deriving (Show)

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
  , classEncoding :: ClassEncoding
  , classUseReqs :: Reqs
    -- ^ Requirements for a 'Type' to reference this class.
  }
  deriving (Show)

instance Eq Class where
  (==) = (==) `on` classIdentifier

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
  , classEncoding = classEncodingNone
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
data ClassEncoding = ClassEncoding
  { classCppCType :: Maybe Type
    -- ^ The intermediate C type that will be used for conversions.  @Nothing@
    -- means that the class will not supported decoding from or encoding to a
    -- foreign type.
  , classCppCTypeReqs :: Reqs
    -- ^ Requirements for a binding to use the class's C type.
  , classCppDecoder :: Maybe CppCoder
    -- ^ The conversion process from a C type to a C++ object.  @Nothing@ means
    -- that the class will not support decoding from a foreign type.
  , classCppDecodeThenFree :: Bool
    -- ^ A convenience for writing interfaces; enables decoding with an existing
    -- function then automatically freeing the encoded value after the decoding
    -- is finished.
  , classCppEncoder :: Maybe CppCoder
    -- ^ The conversion process from a C++ object to a C type.  @Nothing@ means
    -- that the class will not support encoding to a foreign type.
  , classHaskellType :: Maybe HaskellEncoding
    -- ^ Controls how conversions happen in Haskell bindings.
  } deriving (Show)

-- | Encoding parameters for a class that is not encodable or decodable.
classEncodingNone :: ClassEncoding
classEncodingNone = ClassEncoding Nothing mempty Nothing False Nothing Nothing

-- | Modifies classes' 'ClassEncoding' structures with a given function.
classModifyEncoding :: (ClassEncoding -> ClassEncoding) -> Class -> Class
classModifyEncoding f cls = cls { classEncoding = f $ classEncoding cls }

-- | @classCopyEncodingFrom souce target@ copies the 'ClassEncoding' structure
-- from @source@ to @target@.
classCopyEncodingFrom :: Class -> Class -> Class
classCopyEncodingFrom source target = target { classEncoding = classEncoding source }

-- | The means of decoding and encoding a class in C++.
data CppCoder =
  CppCoderFn Identifier Reqs
  -- ^ The named function will be called to decode the C type (when decoding) or
  -- to encode the C++ type (when encoding).
  | CppCoderExpr [Maybe String] Reqs
    -- ^ A C++ expression made from the concatenation of all of the strings will
    -- be used.  For each @Nothing@, the name of an input variable will be
    -- substituted.
  deriving (Show)

cppCoderReqs :: CppCoder -> Reqs
cppCoderReqs (CppCoderFn _ reqs) = reqs
cppCoderReqs (CppCoderExpr _ reqs) = reqs

-- | Conversions of C++ class objects in Haskell.
data HaskellEncoding = HaskellEncoding
  { haskellEncodingType :: HsType
    -- ^ @ht@; the Haskell type to start with or end at, e.g. @String@.
  , haskellEncodingCType :: HsType
    -- ^ @ct@; the Haskell representation of the intermediate C type, e.g. @TPtr
    -- TChar@.
  , haskellEncodingDecoder :: String
    -- ^ Decoding function, of type @ct -> IO ht@.
  , haskellEncodingEncoder :: String
    -- ^ Encoding function, of type @ht -> IO ct@.
  , haskellEncodingTypeImports :: S.Set HaskellImport
    -- ^ Imports necessary to make use of @ht@.
  , haskellEncodingCTypeImports :: S.Set HaskellImport
    -- ^ Imports necessary to make use of @ct@.
  , haskellEncodingFnImports :: S.Set HaskellImport
    -- ^ Imports necessary to make use of @haskellEncodingDecoder@ and
    -- @haskellEncodingDecoder@.
  } deriving (Show)

-- | A C++ class constructor declaration.
data Ctor = Ctor
  { ctorExtName :: ExtName
  , ctorParams :: [Type]
  }
  deriving (Show)

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
  deriving (Show)

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
  } deriving (Show)

instance Eq Callback where
  (==) = (==) `on` callbackExtName

makeCallback :: ExtName
             -> [Type]  -- ^ Parameter types.
             -> Type  -- ^ Return type.
             -> Callback
makeCallback = Callback

-- | Creates a 'TFn' from a callback's parameter and return types.
callbackToTFn :: Callback -> Type
callbackToTFn = TFn <$> callbackParams <*> callbackReturn
