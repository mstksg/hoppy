module Foreign.Cppop.Generator.Spec (
  -- * Interfaces
  Interface,
  ErrorMsg,
  interface,
  interfaceName,
  interfaceBindingsCppPath,
  interfaceBindingsHppPath,
  interfaceBindingsIncludes,
  interfaceCallbacksCppPath,
  interfaceCallbacksHppPath,
  interfaceCallbacksIncludes,
  interfaceHaskellImports,
  interfaceExports,
  interfaceExportsByName,
  -- * C++ includes
  Include,
  includeStd,
  includeLocal,
  includeToString,
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
  CppEnum, makeEnum, enumIdentifier, enumExtName, enumValueNames,
  Purity (..),
  Function, makeFn, fnIdentifier, fnExtName, fnPurity, fnParams, fnReturn,
  Class, makeClass, classIdentifier, classExtName, classSuperclasses, classCtors, classMethods,
  classEncoding,
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
  HaskellEncoding (..),
  -- * Callbacks
  Callback, makeCallback, callbackExtName, callbackParams, callbackReturn, callbackToTFn,
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Data.Char (isAlpha, isAlphaNum)
import Data.Function (on)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (mappend)
import Language.Haskell.Syntax (HsType)

type ErrorMsg = String

-- | A complete specification of a C++ API.  Generators for different languages,
-- including the server generator for C++, use these to produce their output.
data Interface = Interface
  { interfaceName :: String
    -- ^ Textual name of the interface.
  , interfaceBindingsCppPath :: FilePath
    -- ^ A path to a file into which to write C++ bindings.  This path will be
    -- created relative to a root C++ path passed into the generator at runtime.
  , interfaceBindingsHppPath :: FilePath
    -- ^ The corresponding header file for 'interfaceBindingsCppPath'.
  , interfaceBindingsIncludes :: [Include]
    -- ^ Includes that will be added to the binding header and source files.
  , interfaceCallbacksCppPath :: Maybe FilePath
    -- ^ A path to a file into which to write C++ callbacks.  This path will be
    -- created relative to a root C++ path passed into the generator at runtime.
  , interfaceCallbacksHppPath :: Maybe FilePath
    -- ^ The corresponding header file for 'interfaceCallbacksHppPath'.
  , interfaceCallbacksIncludes :: [Include]
    -- ^ Includes that will be added to the callback header and source files.
  , interfaceHaskellImports :: [String]
    -- ^ Imports that will be added to the generated Haskell bindings.
  , interfaceExports :: [Export]
    -- ^ The complete list of bindings to be generated.
  , interfaceExportsByName :: Map ExtName Export
    -- ^ A map of the bindings to be generated.
  } deriving (Show)

-- | An @#include@ directive in a C++ file.
data Include = Include { includeToString :: String }
             deriving (Show)

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
          -> FilePath  -- ^ 'interfaceBindingsCppPath'
          -> FilePath  -- ^ 'interfaceBindingsHppPath'
          -> [Include]  -- ^ 'interfaceBindingsIncludes'
          -> Maybe (FilePath, FilePath, [Include])
          -- ^ If present, then @('interfaceCallbacksCppPath',
          -- 'interfaceCallbacksHppPath', 'interfaceCallbacksIncludes')@.
          -> [String]  -- ^ 'interfaceHaskellImports'
          -> [Export]  -- ^ 'interfaceExports'
          -> Either ErrorMsg Interface
interface ifName bindingsCppPath bindingsHppPath bindingsIncludes
          maybeCallbacksPathsAndIncludes haskellImports exports = do
  -- Check for multiple definitions of a single external name.
  let directory = Map.fromListWith mappend $ flip map exports $
                  \e -> (exportExtName e, [e])
      dupErrorMsgs = catMaybes $ flip map (Map.assocs directory) $ \(name, exports) ->
        let erroneous = case exports of
              _:_:_ -> True  -- Multiple exports.
              _ -> False
        in if erroneous
           then Just $ unlines $
                ("- " ++ show name) :
                map (("  - " ++) . show) exports
           else Nothing
  case dupErrorMsgs of
    _:_:_ -> Left $ unlines $ "Some external names were declared multiple times:" : dupErrorMsgs
    _:_ -> Left $ unlines $ "An external name was declared multiple times:" : dupErrorMsgs
    _ -> return ()

  return $ Interface
    { interfaceName = ifName
    , interfaceBindingsCppPath = bindingsCppPath
    , interfaceBindingsHppPath = bindingsHppPath
    , interfaceBindingsIncludes = bindingsIncludes
    , interfaceCallbacksCppPath = fmap (\(a,_,_) -> a) maybeCallbacksPathsAndIncludes
    , interfaceCallbacksHppPath = fmap (\(_,b,_) -> b) maybeCallbacksPathsAndIncludes
    , interfaceCallbacksIncludes = maybe [] (\(_,_,c) -> c) maybeCallbacksPathsAndIncludes
    , interfaceHaskellImports = haskellImports
    , interfaceExports = exports
    , interfaceExportsByName = Map.fromList $ map (exportExtName &&& id) exports
    }

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
extNameOrIdentifier identifier maybeExtName =
  fromMaybe (toExtName $ idName identifier) maybeExtName

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
  ExportCallback c -> callbackExtName c

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
ident1 ns1 name = ident' [ns1] name

-- | Creates an identifier of the form @::a::b::c@.
ident2 :: String -> String -> String -> Identifier
ident2 ns1 ns2 name = ident' [ns1, ns2] name

-- | Creates an identifier of the form @::a::b::c::d@.
ident3 :: String -> String -> String -> String -> Identifier
ident3 ns1 ns2 ns3 name = ident' [ns1, ns2, ns3] name

-- | Creates an identifier of the form @::a::b::c::d::e@.
ident4 :: String -> String -> String -> String -> String -> Identifier
ident4 ns1 ns2 ns3 ns4 name = ident' [ns1, ns2, ns3, ns4] name

-- | Creates an identifier of the form @::a::b::c::d::e::f@.
ident5 :: String -> String -> String -> String -> String -> String -> Identifier
ident5 ns1 ns2 ns3 ns4 ns5 name = ident' [ns1, ns2, ns3, ns4, ns5] name

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
  | TArray (Maybe Int) Type  -- ^ An array, optionally with finite size.
  | TPtr Type  -- ^ A poiner to another type.
  | TRef Type  -- ^ A reference to another type.
  | TFn [Type] Type
    -- ^ A function taking parameters and returning a value (or 'TVoid').
    -- Function declarations can use 'TFn' directly; but function pointers must
    -- wrap a 'TFn' in a 'TPtr'.
  | TCallback Callback
  | TObj Class
  | TOpaque String
  | TBlob
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
  } deriving (Show)

instance Eq CppEnum where
  (==) = (==) `on` enumIdentifier

makeEnum :: Identifier  -- ^ 'enumIdentifier'
         -> Maybe ExtName
         -- ^ An optional external name; will be automatically derived from
         -- the identifier if absent.
         -> [(Int, [String])]  -- ^ 'enumValueNames'
         -> CppEnum
makeEnum identifier maybeExtName =
  CppEnum identifier $ extNameOrIdentifier identifier maybeExtName

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
  }
  deriving (Show)

makeFn :: Identifier
       -> Maybe ExtName
       -- ^ An optional external name; will be automatically derived from
       -- the identifier if absent.
       -> Purity
       -> [Type]  -- ^ Parameter types.
       -> Type  -- ^ Return type.
       -> Function
makeFn identifier maybeExtName =
  Function identifier $ extNameOrIdentifier identifier maybeExtName

-- | A C++ class declaration.
data Class = Class
  { classIdentifier :: Identifier
  , classExtName :: ExtName
  , classSuperclasses :: [Class]
  , classCtors :: [Ctor]
  , classMethods :: [Method]
  , classEncoding :: ClassEncoding
  }
  deriving (Show)

instance Eq Class where
  (==) = (==) `on` classIdentifier

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
  }

-- | When a class object is returned from a function or taken as a parameter by
-- value, it needs to be converted into a foreign (non-C++) object.  This data
-- type describes how to perform those conversions.
--
-- When converting between a C++ value and a foreign value, an intermediate C
-- type is used.  For example, for a @std::string@, a regular C string (@char*@,
-- @'TPtr' 'TChar'@) allocated on the heap is used.  Since callbacks allow
-- calling back into foreign code, either language may call the other, so the
-- side that allocates memory on the heap transfers ownership of that memory to
-- the other language.
data ClassEncoding = ClassEncoding
  { classCppCType :: Maybe Type
    -- ^ The intermediate C type that will be used for conversions.  @Nothing@
    -- means that the class will not supported decoding from or encoding to a
    -- foreign type.
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
classEncodingNone = ClassEncoding Nothing Nothing False Nothing Nothing

-- | Modifies classes' 'ClassEncoding' structures with a given function.
classModifyEncoding :: (ClassEncoding -> ClassEncoding) -> Class -> Class
classModifyEncoding f cls = cls { classEncoding = f $ classEncoding cls }

-- | @classCopyEncodingFrom souce target@ copies the 'ClassEncoding' structure
-- from @source@ to @target@.
classCopyEncodingFrom :: Class -> Class -> Class
classCopyEncodingFrom source target = target { classEncoding = classEncoding source }

-- | The means of decoding and encoding a class in C++.
data CppCoder =
  CppCoderFn Identifier
  -- ^ The named function will be called to decode the C type (when decoding) or
  -- to encode the C++ type (when encoding).
  | CppCoderExpr [Maybe String]
    -- ^ A C++ expression made from the concatenation of all of the strings will
    -- be used.  For each @Nothing@, the name of an input variable will be
    -- substituted.
  deriving (Show)

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
