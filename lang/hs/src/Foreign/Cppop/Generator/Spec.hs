module Foreign.Cppop.Generator.Spec (
  -- * Interfaces
  Interface,
  interface,
  interfaceName,
  interfaceIncludes,
  interfaceExports,
  --interfaceConvs,
  interfaceExportsByName,
  --interfaceConvsByName,
  --interfaceDefaultConvsByFromType,
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
  --EType (..),
  --etypeFrom,
  --etypeTo,
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
  ForeignType (..),
  ClassEncoding (..),
  classEncodingNone,
  classModifyEncoding,
  classCopyEncodingFrom,
  ---- * Conversions
  --Conv (..),
  ) where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Monoid (mappend)
import Language.Haskell.Syntax (HsType)

type ErrorMsg = String

-- | A complete specification of a C++ API.  Generators for different languages,
-- including the server generator for C++, use these to produce their output.
data Interface = Interface
  { interfaceName :: String
  , interfaceIncludes :: [Include]
  , interfaceExports :: [Export]
  --, interfaceConvs :: [Conv]
  , interfaceExportsByName :: Map ExtName Export
  --, interfaceConvsByName :: Map ExtName Conv
  --, interfaceDefaultConvsByFromType :: Map Type Conv
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
interface :: String -> [Include] -> [Export] -> Either ErrorMsg Interface
interface ifName includes exports = do
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
    , interfaceIncludes = includes
    , interfaceExports = exports
    --, interfaceConvs = convs
    , interfaceExportsByName = Map.fromList $ map (exportExtName &&& id) exports
    --, interfaceConvsByName = Map.fromList $ map (convExtName &&& id) namedConvs
    --, interfaceDefaultConvsByFromType = Map.fromList $ map (convFrom &&& id) convs
    }

-- | An external name is a string that Cppop clients use to uniquely identify an
-- object to invoke at runtime.
newtype ExtName = ExtName { fromExtName :: String }
                deriving (Eq, Ord, Show)

toExtName :: String -> ExtName
toExtName = ExtName

-- | Specifies some C++ object (function or class) to give access to.
data Export =
  ExportFn Function
  | ExportClass Class
  deriving (Show)

-- | Returns the external name of an export.
exportExtName :: Export -> ExtName
exportExtName export = case export of
  ExportFn f -> fnExtName f
  ExportClass c -> classExtName c

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
  | TObj Class
  | TOpaque String
  | TBlob
  | TConst Type
  deriving (Eq, Show)

--instance Eq Type where
--  TVoid == TVoid = True
--  TBool == TBool = True
--  TChar == TChar = True
--  TUChar == TUChar = True
--  TShort == TShort = True
--  TUShort == TUShort = True
--  TInt == TInt = True
--  TUInt == TUInt = True
--  TLong == TLong = True
--  TULong == TULong = True
--  TLLong == TLLong = True
--  TULLong == TULLong = True
--  TFloat == TFloat = True
--  TDouble == TDouble = True
--  TArray n t == TArray n' t' = n == n' && t == t'
--  TPtr t == TPtr t' = t == t'
--  TRef t == TRef t' = t == t'
--  TFn ps r == TFn ps' r' = ps == ps' && r == r'
--  TObj cls == TObj cls' = classIdentifier cls == classIdentifier cls'
--  TOpaque str == TOpaque str' = str == str'
--  TBlob == TBlob = True
--  TConst t == TConst t' = t == t'
--  _ == _ = False

---- | Effective type: Either a regular (unconverted) or a converted type.
---- Exported functions return and take as paramters 'EType's to allow conversion
---- between types that Cppop knows how to serialize, and types it doesn't.  When
---- an 'EConv' is given as a function parameter type, then the from-type is the
---- type of then wire type, and the to-type is the type that the C++ function
---- accepts.  In a return type it's the reverse: the from-type is the type the
---- function returns and the to-type is the wire type.
--data EType = EType Type | EConv Conv
--           deriving (Eq, Show)
--
---- | The from-type of a converted type, or the type itself in the case of an
---- unconverted type.
--etypeFrom :: EType -> Type
--etypeFrom et = case et of
--  EType t -> t
--  EConv c -> convFrom c
--
---- | The to-type of a converted type, or the type itself in the case of an
---- unconverted type.
--etypeTo :: EType -> Type
--etypeTo et = case et of
--  EType t -> t
--  EConv c -> convTo c

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

--instance Ord Class where
--  compare = comparing (idToString . classIdentifier)

makeClass :: Identifier -> [Class] -> [Ctor] -> [Method] -> Class
makeClass identifier supers ctors methods = Class
  { classIdentifier = identifier
  , classExtName = toExtName $ idName identifier
  , classSuperclasses = supers
  , classCtors = ctors
  , classMethods = methods
  , classEncoding = classEncodingNone
  }

data ForeignType t f = ForeignType
  { coderType :: t
  , coderDecoder :: f
  , coderEncoder :: f
  } deriving (Show)

data ClassEncoding = ClassEncoding
  { classCppDecoder :: Maybe Identifier
  , classCppEncoder :: Maybe Identifier
  , classHaskellType :: Maybe (ForeignType HsType String)
  } deriving (Show)

classEncodingNone :: ClassEncoding
classEncodingNone = ClassEncoding Nothing Nothing Nothing

classModifyEncoding :: (ClassEncoding -> ClassEncoding) -> Class -> Class
classModifyEncoding f cls = cls { classEncoding = f $ classEncoding cls }

classCopyEncodingFrom :: Class -> Class -> Class
classCopyEncodingFrom source target = target { classEncoding = classEncoding source }

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

---- | A conversion between C++ types.
--data Conv =
--    -- | Conversion via existing function.
--    ConvFn
--    { convFrom :: Type
--    , convTo :: Type
--    , convId :: Identifier
--    }
--  | -- | Conversion via a inline string.  The name of a variable containing the
--    -- value to convert will be interspersed between each code segment to do the
--    -- conversion.
--    ConvInline
--    { convFrom :: Type
--    , convTo :: Type
--    , convSegments :: [String]
--    }
--  | -- | Conversion via custom function definition.
--    ConvCustom
--    { convFrom :: Type
--    , convTo :: Type
--    , convExtName :: ExtName
--    , convBody :: String
--    }
--  deriving (Show)
--
--instance Eq Conv where
--  (==) = const $ const False  -- TODO

--data Decoder = Decoder
--  { decoderType :: Type
--  , decoderFunction :: Identifier
--  }
--
--data Encoder = Encoder
--  { encoderType :: Type
--  , encoderFunction :: Identifier
--  }

--data ConvPair = ConvPair
--  { deserializingConv :: Conv
--  , serializingConv :: Conv
--  }
--
--deserialierSerializerPair :: Conv -> Conv -> Either ErrorMsg ConvPair
--deserialierSerializerPair deserializer serializer =
--  if (convFrom deserializer /= convTo serializer ||
--        convFrom serializer /= convTo deserializer)
--  then Left $ "Conv types are not inverses.  Deserializer: " ++
--       show (convFrom deserializer) ++ " -> " ++ show (convTo deserializer) ++
--       "Serializer: " ++ show (convFrom serializer) ++ " -> " ++
--       show (convTo serializer) ++ "."
--  else Right $ ConvPair deserializer serializer
