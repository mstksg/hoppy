module Foreign.Cppop.Generator.Spec (
  -- * Interfaces
  Interface,
  interface,
  interfaceName,
  interfaceBindingsCppPath,
  interfaceBindingsHppPath,
  interfaceBindingsIncludes,
  interfaceCallbacksCppPath,
  interfaceCallbacksHppPath,
  interfaceCallbacksIncludes,
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
  Purity (..),
  Function (..),
  Class (..),
  makeClass,
  Ctor (..),
  Method (..),
  MethodApplicability (..),
  Constness (..),
  Staticness (..),
  methodConst,
  methodStatic,
  -- ** Encoding and decoding
  ClassEncoding (..),
  classEncodingNone,
  classModifyEncoding,
  classCopyEncodingFrom,
  CppCoder (..),
  HaskellEncoding (..),
  -- * Callbacks
  Callback (..),
  callbackToTFn,
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
  , interfaceBindingsCppPath :: FilePath
  , interfaceBindingsHppPath :: FilePath
  , interfaceBindingsIncludes :: [Include]
  , interfaceCallbacksCppPath :: Maybe FilePath
  , interfaceCallbacksHppPath :: Maybe FilePath
  , interfaceCallbacksIncludes :: [Include]
  , interfaceExports :: [Export]
  , interfaceExportsByName :: Map ExtName Export
  } deriving (Show)

data Include = Include { includeToString :: String }
             deriving (Show)

includeStd :: String -> Include
includeStd path = Include $ "#include <" ++ path ++ ">\n"

includeLocal :: String -> Include
includeLocal path = Include $ "#include \"" ++ path ++ "\"\n"

-- | Constructs an 'Interface' from the required parts.  Some validation is
-- performed; if the resulting interface would be invalid, an error message is
-- returned instead.
interface :: String
          -> FilePath
          -> FilePath
          -> [Include]
          -> Maybe (FilePath, FilePath, [Include])
          -> [Export]
          -> Either ErrorMsg Interface
interface ifName bindingsCppPath bindingsHppPath bindingsIncludes
          maybeCallbacksPathsAndIncludes exports = do
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
    , interfaceExports = exports
    , interfaceExportsByName = Map.fromList $ map (exportExtName &&& id) exports
    }

-- | An external name is a string that Cppop clients use to uniquely identify an
-- object to invoke at runtime.
newtype ExtName = ExtName { fromExtName :: String }
                deriving (Eq, Ord, Show)

toExtName :: String -> ExtName
toExtName str = case str of
  [] -> error "An ExtName cannot be empty."
  c:cs -> if isAlpha c && all ((||) <$> isAlphaNum <*> (== '_')) cs
          then ExtName str
          else error $
               "An ExtName must start with a letter and only contain letters, numbers, and '_': " ++
               show str

-- | Specifies some C++ object (function or class) to give access to.
data Export =
  ExportFn Function
  | ExportClass Class
  | ExportCallback Callback
  deriving (Show)

-- | Returns the external name of an export.
exportExtName :: Export -> ExtName
exportExtName export = case export of
  ExportFn f -> fnExtName f
  ExportClass c -> classExtName c
  ExportCallback c -> callbackExtName c

-- | An absolute path from the top-level C++ namespace down to some named
-- object.
data Identifier = Identifier
  { idNamespaces :: [String]
  , idName :: String
  }
  deriving (Eq, Show)

idToString :: Identifier -> String
idToString identifier =
  ':':':':(intercalate "::" $ idNamespaces identifier ++ [idName identifier])

ident :: String -> Identifier
ident = Identifier []

ident' :: [String] -> String -> Identifier
ident' = Identifier

ident1 :: String -> String -> Identifier
ident1 ns1 name = ident' [ns1] name

ident2 :: String -> String -> String -> Identifier
ident2 ns1 ns2 name = ident' [ns1, ns2] name

ident3 :: String -> String -> String -> String -> Identifier
ident3 ns1 ns2 ns3 name = ident' [ns1, ns2, ns3] name

ident4 :: String -> String -> String -> String -> String -> Identifier
ident4 ns1 ns2 ns3 ns4 name = ident' [ns1, ns2, ns3, ns4] name

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

makeClass :: Identifier -> Maybe ExtName -> [Class] -> [Ctor] -> [Method] -> Class
makeClass identifier maybeExtName supers ctors methods = Class
  { classIdentifier = identifier
  , classExtName = fromMaybe (toExtName $ idName identifier) maybeExtName
  , classSuperclasses = supers
  , classCtors = ctors
  , classMethods = methods
  , classEncoding = classEncodingNone
  }

data ClassEncoding = ClassEncoding
  { classCppCType :: Maybe Type
  , classCppDecoder :: Maybe CppCoder
  , classCppDecodeThenFree :: Bool
    -- ^ A convenience for writing interfaces; enables decoding with an existing
    -- function then automatically freeing the encoded value after the decoding
    -- is finished.
  , classCppEncoder :: Maybe CppCoder
  , classHaskellType :: Maybe HaskellEncoding
  } deriving (Show)

classEncodingNone :: ClassEncoding
classEncodingNone = ClassEncoding Nothing Nothing False Nothing Nothing

classModifyEncoding :: (ClassEncoding -> ClassEncoding) -> Class -> Class
classModifyEncoding f cls = cls { classEncoding = f $ classEncoding cls }

classCopyEncodingFrom :: Class -> Class -> Class
classCopyEncodingFrom source target = target { classEncoding = classEncoding source }

data CppCoder = CppCoderFn Identifier | CppCoderExpr [Maybe String]
              deriving (Show)

data HaskellEncoding = HaskellEncoding
  { haskellEncodingType :: HsType  -- ^ @ht@; e.g. @String@.
  , haskellEncodingCType :: HsType  -- ^ @ct@; e.g. @TPtr TChar@.
  , haskellEncodingDecoder :: String  -- ^ Decoding function, of type @ct -> IO ht@.
  , haskellEncodingEncoder :: String  -- ^ Encoding function, of type @ht -> IO ct@.
  } deriving (Show)

-- | A C++ class constructor declaration.
data Ctor = Ctor
  { ctorExtName :: ExtName
  , ctorParams :: [Type]
  }
  deriving (Show)

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

callbackToTFn :: Callback -> Type
callbackToTFn = TFn <$> callbackParams <*> callbackReturn
