{-# LANGUAGE CPP #-}

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
  addUseReqs,
  addReqIncludes,
  -- * Names and exports
  ExtName,
  toExtName,
  fromExtName,
  FnName (..),
  IsFnName (..),
  Operator (..),
  OperatorType (..),
  operatorPreferredExtName,
  operatorType,
  Export (..),
  exportExtName,
  Identifier,
  identifierParts,
  IdPart,
  idPartBase,
  idPartArgs,
  ident, ident', ident1, ident2, ident3, ident4, ident5,
  identT, identT', ident1T, ident2T, ident3T, ident4T, ident5T,
  -- * Basic types
  Type (..),
  HasTVars (..),
  -- ** Enums
  CppEnum, makeEnum, enumIdentifier, enumExtName, enumValueNames, enumUseReqs,
  Purity (..),
  -- ** Functions
  Function, makeFn, fnCName, fnExtName, fnPurity, fnParams, fnReturn, fnUseReqs,
  -- *** Function templates
  FnTemplate, makeFnTemplate, instantiateFnTemplate, instantiateFnTemplate',
  fnTemplateIdentifier, fnTemplateExtNamePrefix, fnTemplateVars, fnTemplatePurity, fnTemplateParams,
  fnTemplateReturn, fnTemplateUseReqs,
  -- ** Classes
  Class, makeClass, classIdentifier, classExtName, classSuperclasses, classCtors, classMethods,
  classConversions, classUseReqs,
  HasClassyExtName (..),
  Ctor, makeCtor, mkCtor, ctorExtName, ctorParams,
  Method,
  MethodApplicability (..),
  Constness (..),
  Staticness (..),
  makeMethod, mkMethod, mkMethod', mkConstMethod, mkConstMethod',
  mkStaticMethod, mkStaticMethod',
  mkProps, mkProp, mkStaticProp, mkBoolIsProp, mkBoolHasProp,
  methodCName, methodExtName, methodApplicability, methodPurity, methodParams,
  methodReturn, methodConst, methodStatic,
  -- *** Conversions to and from foreign values
  ClassConversions (..),
  classConversionsNone,
  classModifyConversions,
  ClassHaskellConversion (..),
  -- *** Class templates
  ClassTemplate, ClassTemplateSuper (..),
  ClassTemplateConversionsGen, ClassTemplateConversionsEnv, askTypeArgs, askMethodPrefix,
  makeClassTemplate, instantiateClassTemplate, instantiateClassTemplate',
  addClassTemplateConversions,
  classTemplateIdentifier, classTemplateExtNamePrefix, classTemplateVars, classTemplateSuperclasses,
  classTemplateCtors, classTemplateMethods, classTemplateUseReqs,
  -- ** Callbacks
  Callback, makeCallback, callbackExtName, callbackParams, callbackReturn, callbackUseReqs,
  callbackToTFn,
  -- * Haskell imports
  HsModuleName, HsImportSet, HsImportKey (..), HsImportSpecs (..), HsImportName, HsImportVal (..),
  hsWholeModuleImport, hsQualifiedImport, hsImport1, hsImport1', hsImports, hsImports',
  -- * Internal to Cppop
  -- ** Haskell imports
  internalToHsFnName,
  makeHsImportSet,
  getHsImportSet,
  hsImportForForeign,
  hsImportForForeignC,
  hsImportForPrelude,
  hsImportForSupport,
  hsImportForSystemPosixTypes,
  hsImportForUnsafeIO,
  -- ** Error messages
  freeVarErrorMsg,
  tObjToHeapWrongDirectionErrorMsg,
  ) where

import Control.Arrow ((&&&))
import Control.Monad (forM, liftM2, unless, when)
#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except (MonadError, throwError)
#else
import Control.Monad.Error (MonadError, throwError)
#endif
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.State (MonadState, StateT, execStateT, get, modify)
import Control.Monad.Trans (lift)
import Data.Char (isAlpha, isAlphaNum, toLower, toUpper)
import Data.Function (on)
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Foreign.Cppop.Common
import Foreign.Cppop.Common.Consume
import {-# SOURCE #-} qualified Foreign.Cppop.Generator.Language.Haskell.General as Haskell
import Language.Haskell.Syntax (HsType)

type ErrorMsg = String

type HaskellImport = String

-- | A complete specification of a C++ API.  Generators for different languages,
-- including the binding generator for C++, use these to produce their output.
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
          concat $ "- " : show extName : ": " : intersperse ", " (map show modules))
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

modifyModule :: Module -> StateT Module (Either String) () -> Either String Module
modifyModule = flip execStateT

modifyModule' :: Module -> StateT Module (Either String) () -> Module
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

-- | A set of requirements of needed to use an identifier in C++ (function,
-- type, etc.), via a set of 'Include's.  The monoid instance has 'mempty' as an
-- empty set of includes, and 'mappend' unions two include sets.
data Reqs = Reqs
  { reqsIncludes :: S.Set Include
    -- ^ The includes specified by a 'Reqs'.
  } deriving (Show)

instance Monoid Reqs where
  mempty = Reqs mempty

  mappend (Reqs incl) (Reqs incl') = Reqs $ mappend incl incl'

  mconcat reqs = Reqs $ mconcat $ map reqsIncludes reqs

-- | Creates a 'Reqs' that contains the given include.
reqInclude :: Include -> Reqs
reqInclude include = mempty { reqsIncludes = S.singleton include }

-- | C++ types that have requirements in order to use them.
class HasUseReqs a where
  getUseReqs :: a -> Reqs

  setUseReqs :: Reqs -> a -> a
  setUseReqs = modifyUseReqs . const

  modifyUseReqs :: (Reqs -> Reqs) -> a -> a
  modifyUseReqs f x = setUseReqs (f $ getUseReqs x) x

-- | Adds to a type's requirements.
addUseReqs :: HasUseReqs a => Reqs -> a -> a
addUseReqs reqs = modifyUseReqs $ mappend reqs

-- | Adds a list of includes to the requirements of a type.
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
extNameOrIdentifier ident = fromMaybe $ case identifierParts ident of
  [] -> error "extNameOrIdentifier: Invalid empty identifier."
  parts -> toExtName $ idPartBase $ last parts

-- | Like 'extNameOrIdentifier', but works with strings rather than 'ExtName's.
stringOrIdentifier :: Identifier -> Maybe String -> String
stringOrIdentifier ident = fromMaybe $ case identifierParts ident of
  [] -> error "stringOrIdentifier: Invalid empty identifier."
  parts -> idPartBase $ last parts

-- | This is here because of module circular dependencies; see
-- 'Foreign.Cppop.Generator.Language.Haskell.General.toHsFnName'.
internalToHsFnName :: ExtName -> String
internalToHsFnName extName = case fromExtName extName of
  x:xs -> toLower x:xs
  [] -> []

-- | Generates an 'ExtName' from an @'FnName' 'Identifier'@, if the given name
-- is absent.
extNameOrFnIdentifier :: FnName Identifier -> Maybe ExtName -> ExtName
extNameOrFnIdentifier name =
  fromMaybe $ case name of
    FnName identifier -> case identifierParts identifier of
      [] -> error "extNameOrFnIdentifier: Empty idenfitier."
      parts -> toExtName $ idPartBase $ last parts
    FnOp op -> operatorPreferredExtName op

-- | The name of a function or method.  May be either an alphanumeric string
-- ('FnName') or an operator ('FnOp').
data FnName name =
  FnName name
  | FnOp Operator
  deriving (Eq, Ord)

instance Show name => Show (FnName name) where
  show (FnName name) = concat ["<FnName ", show name, ">"]
  show (FnOp op) = concat ["<FnOp ", show op, ">"]

-- | Enables implementing automatic conversions to a @'FnName' t@.
class IsFnName t a where
  toFnName :: a -> FnName t

instance IsFnName t (FnName t) where
  toFnName = id

instance IsFnName t t where
  toFnName = FnName

instance IsFnName t Operator where
  toFnName = FnOp

data Operator =
  OpCall  -- ^ @x(...)@
  | OpComma -- ^ @x, y@
  | OpAssign  -- ^ @x = y@
  | OpArray  -- ^ @x[y]@
  | OpDeref  -- ^ @*x@
  | OpAddress  -- ^ @&x@
  | OpAdd  -- ^ @x + y@
  | OpAddAssign  -- ^ @x += y@
  | OpSubtract  -- ^ @x - y@
  | OpSubtractAssign  -- ^ @x -= y@
  | OpMultiply  -- ^ @x * y@
  | OpMultiplyAssign  -- ^ @x *= y@
  | OpDivide  -- ^ @x / y@
  | OpDivideAssign  -- ^ @x /= y@
  | OpModulo  -- ^ @x % y@
  | OpModuloAssign  -- ^ @x %= y@
  | OpPlus  -- ^ @+x@
  | OpMinus  -- ^ @-x@
  | OpIncPre  -- ^ @++x@
  | OpIncPost  -- ^ @x++@
  | OpDecPre  -- ^ @--x@
  | OpDecPost  -- ^ @x--@
  | OpEq  -- ^ @x == y@
  | OpNe  -- ^ @x != y@
  | OpLt  -- ^ @x < y@
  | OpLe  -- ^ @x <= y@
  | OpGt  -- ^ @x > y@
  | OpGe  -- ^ @x >= y@
  | OpNot  -- ^ @!x@
  | OpAnd  -- ^ @x && y@
  | OpOr  -- ^ @x || y@
  | OpBitNot  -- ^ @~x@
  | OpBitAnd  -- ^ @x & y@
  | OpBitAndAssign  -- ^ @x &= y@
  | OpBitOr  -- ^ @x | y@
  | OpBitOrAssign  -- ^ @x |= y@
  | OpBitXor  -- ^ @x ^ y@
  | OpBitXorAssign  -- ^ @x ^= y@
  | OpShl  -- ^ @x << y@
  | OpShlAssign  -- ^ @x <<= y@
  | OpShr  -- ^ @x >> y@
  | OpShrAssign  -- ^ @x >>= y@
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | The arity and syntax of an operator.
data OperatorType =
  UnaryPrefixOperator String  -- ^ Prefix unary operators.  Examples: @!x@, @*x@, @++x@.
  | UnaryPostfixOperator String  -- ^ Postfix unary operators.  Examples: @x--, x++@.
  | BinaryOperator String  -- ^ Infix binary operators.  Examples: @x * y@, @x >>= y@.
  | CallOperator  -- ^ @bx(...)@ with arbitrary arity.
  | ArrayOperator  -- ^ @x[y]@, a binary operator with non-infix syntax.

data OperatorInfo = OperatorInfo
  { operatorPreferredExtName' :: ExtName
  , operatorType' :: OperatorType
  }

makeOperatorInfo :: String -> OperatorType -> OperatorInfo
makeOperatorInfo = OperatorInfo . toExtName

-- | Returns a conventional string to use for the 'ExtName' of an operator.
operatorPreferredExtName :: Operator -> ExtName
operatorPreferredExtName op = case M.lookup op operatorInfo of
  Just info -> operatorPreferredExtName' info
  Nothing ->
    error $ concat
    ["operatorPreferredExtName: Internal error, missing info for operator ", show op, "."]

-- | Returns the type of an operator.
operatorType :: Operator -> OperatorType
operatorType op = case M.lookup op operatorInfo of
  Just info -> operatorType' info
  Nothing ->
    error $ concat
    ["operatorType: Internal error, missing info for operator ", show op, "."]

-- | Metadata for operators.
--
-- TODO Test out this missing data.
operatorInfo :: M.Map Operator OperatorInfo
operatorInfo =
  let input =
        [ (OpCall, makeOperatorInfo "CALL" CallOperator)
        , (OpComma, makeOperatorInfo "COMMA" $ BinaryOperator ",")
        , (OpAssign, makeOperatorInfo "ASSIGN" $ BinaryOperator "=")
        , (OpArray, makeOperatorInfo "ARRAY" ArrayOperator)
        , (OpDeref, makeOperatorInfo "DEREF" $ UnaryPrefixOperator "*")
        , (OpAddress, makeOperatorInfo "ADDRESS" $ UnaryPrefixOperator "&")
        , (OpAdd, makeOperatorInfo "ADD" $ BinaryOperator "+")
        , (OpAddAssign, makeOperatorInfo "ADDA" $ BinaryOperator "+=")
        , (OpSubtract, makeOperatorInfo "SUB" $ BinaryOperator "-")
        , (OpSubtractAssign, makeOperatorInfo "SUBA" $ BinaryOperator "-=")
        , (OpMultiply, makeOperatorInfo "MUL" $ BinaryOperator "*")
        , (OpMultiplyAssign, makeOperatorInfo "MULA" $ BinaryOperator "*=")
        , (OpDivide, makeOperatorInfo "DIV" $ BinaryOperator "/")
        , (OpDivideAssign, makeOperatorInfo "DIVA" $ BinaryOperator "/=")
        , (OpModulo, makeOperatorInfo "MOD" $ BinaryOperator "%")
        , (OpModuloAssign, makeOperatorInfo "MODA" $ BinaryOperator "%=")
        , (OpPlus, makeOperatorInfo "PLUS" $ UnaryPrefixOperator "+")
        , (OpMinus, makeOperatorInfo "NEG" $ UnaryPrefixOperator "-")
        , (OpIncPre, makeOperatorInfo "INC" $ UnaryPrefixOperator "++")
        , (OpIncPost, makeOperatorInfo "INCPOST" $ UnaryPostfixOperator "++")
        , (OpDecPre, makeOperatorInfo "DEC" $ UnaryPrefixOperator "--")
        , (OpDecPost, makeOperatorInfo "DECPOST" $ UnaryPostfixOperator "--")
        , (OpEq, makeOperatorInfo "EQ" $ BinaryOperator "==")
        , (OpNe, makeOperatorInfo "NE" $ BinaryOperator "!=")
        , (OpLt, makeOperatorInfo "LT" $ BinaryOperator "<")
        , (OpLe, makeOperatorInfo "LE" $ BinaryOperator "<=")
        , (OpGt, makeOperatorInfo "GT" $ BinaryOperator ">")
        , (OpGe, makeOperatorInfo "GE" $ BinaryOperator ">=")
        , (OpNot, makeOperatorInfo "NOT" $ UnaryPrefixOperator "!")
        , (OpAnd, makeOperatorInfo "AND" $ BinaryOperator "&&")
        , (OpOr, makeOperatorInfo "OR" $ BinaryOperator "||")
        , (OpBitNot, makeOperatorInfo "BNOT" $ UnaryPrefixOperator "~")
        , (OpBitAnd, makeOperatorInfo "BAND" $ BinaryOperator "&")
        , (OpBitAndAssign, makeOperatorInfo "BANDA" $ BinaryOperator "&=")
        , (OpBitOr, makeOperatorInfo "BOR" $ BinaryOperator "|")
        , (OpBitOrAssign, makeOperatorInfo "BORA" $ BinaryOperator "|=")
        , (OpBitXor, makeOperatorInfo "BXOR" $ BinaryOperator "^")
        , (OpBitXorAssign, makeOperatorInfo "BXORA" $ BinaryOperator "^=")
        , (OpShl, makeOperatorInfo "SHL" $ BinaryOperator "<<")
        , (OpShlAssign, makeOperatorInfo "SHLA" $ BinaryOperator "<<=")
        , (OpShr, makeOperatorInfo "SHR" $ BinaryOperator ">>")
        , (OpShrAssign, makeOperatorInfo "SHR" $ BinaryOperator ">>=")
        ]
  in if map fst input == [minBound..]
     then M.fromList input
     else error "operatorInfo: Operator info list is out of sync with Operator data type."

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
newtype Identifier = Identifier { identifierParts :: [IdPart] }
                   deriving (Eq)

instance Show Identifier where
  show ident =
    (\words -> concat $ "<Identifier " : words ++ [">"]) $
    intersperse "::" $
    map (\part -> case idPartArgs part of
            Nothing -> idPartBase part
            Just args ->
              concat $
              idPartBase part : "<" :
              intersperse ", " (map show args) ++ [">"]) $
    identifierParts ident

instance HasTVars Identifier where
  substTVar var val =
    Identifier .
    map (\part -> part { idPartArgs =
                            map (substTVar var val) <$>
                            idPartArgs part
                       }) .
    identifierParts

data IdPart = IdPart
  { idPartBase :: String
  , idPartArgs :: Maybe [Type]
  } deriving (Eq, Show)

-- | Creates an identifier of the form @a@.
ident :: String -> Identifier
ident a = Identifier [IdPart a Nothing]

-- | Creates an identifier of the form @a1::a2::...::aN@.
ident' :: [String] -> Identifier
ident' = Identifier . map (\x -> IdPart x Nothing)

-- | Creates an identifier of the form @a::b@.
ident1 :: String -> String -> Identifier
ident1 a b = ident' [a, b]

-- | Creates an identifier of the form @a::b::c@.
ident2 :: String -> String -> String -> Identifier
ident2 a b c = ident' [a, b, c]

-- | Creates an identifier of the form @a::b::c::d@.
ident3 :: String -> String -> String -> String -> Identifier
ident3 a b c d = ident' [a, b, c, d]

-- | Creates an identifier of the form @a::b::c::d::e@.
ident4 :: String -> String -> String -> String -> String -> Identifier
ident4 a b c d e = ident' [a, b, c, d, e]

-- | Creates an identifier of the form @a::b::c::d::e::f@.
ident5 :: String -> String -> String -> String -> String -> String -> Identifier
ident5 a b c d e f = ident' [a, b, c, d, e, f]

-- | Creates an identifier of the form @a<...>@.
identT :: String -> [Type] -> Identifier
identT a ts = Identifier [IdPart a $ Just ts]

-- | Creates an identifier with arbitrary many templated and non-templated
-- parts.
identT' :: [(String, Maybe [Type])] -> Identifier
identT' = Identifier . map (uncurry IdPart)

-- | Creates an identifier of the form @a::b<...>@.
ident1T :: String -> String -> [Type] -> Identifier
ident1T a b ts = Identifier [IdPart a Nothing, IdPart b $ Just ts]

-- | Creates an identifier of the form @a::b::c<...>@.
ident2T :: String -> String -> String -> [Type] -> Identifier
ident2T a b c ts = Identifier [IdPart a Nothing, IdPart b Nothing, IdPart c $ Just ts]

-- | Creates an identifier of the form @a::b::c::d<...>@.
ident3T :: String -> String -> String -> String -> [Type] -> Identifier
ident3T a b c d ts =
  Identifier [IdPart a Nothing, IdPart b Nothing, IdPart c Nothing,
              IdPart d $ Just ts]

-- | Creates an identifier of the form @a::b::c::d::e<...>@.
ident4T :: String -> String -> String -> String -> String -> [Type] -> Identifier
ident4T a b c d e ts =
  Identifier [IdPart a Nothing, IdPart b Nothing, IdPart c Nothing,
              IdPart d Nothing, IdPart e $ Just ts]

-- | Creates an identifier of the form @a::b::c::d::e::f<...>@.
ident5T :: String -> String -> String -> String -> String -> String -> [Type] -> Identifier
ident5T a b c d e f ts =
  Identifier [IdPart a Nothing, IdPart b Nothing, IdPart c Nothing,
              IdPart d Nothing, IdPart e Nothing, IdPart f $ Just ts]

-- | Concrete C++ types.  It is possible to represent invalid C++ types with
-- this, but that may result in undefined behaviour or invalid code generation.
data Type =
  TVar String  -- ^ A type variable.  May appear within a template.
  | TVoid  -- ^ @void@
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
  | TPtrdiff  -- ^ @ptrdiff_t@
  | TSize  -- ^ @size_t@
  | TSSize  -- ^ @ssize_t@
  | TEnum CppEnum  -- ^ A C++ @enum@.
  | TPtr Type  -- ^ A poiner to another type.
  | TRef Type  -- ^ A reference to another type.
  | TFn [Type] Type
    -- ^ A function taking parameters and returning a value (or 'TVoid').
    -- Function pointers must wrap a 'TFn' in a 'TPtr'.
  | TCallback Callback  -- ^ A handle for calling foreign code from C++.
  | TObj Class  -- ^ An instance of a class.
  | TObjToHeap Class
    -- ^ A special case of 'TObj' that is only allowed when passing values from
    -- C++ to a foreign language.  Rather than looking at the object's
    -- 'ClassConversions', the object will be copied to the heap, and a pointer
    -- to the new object will be passed.  The object must be copy-constructable.
    --
    -- __The foreign language owns the pointer, even for callback arguments.__
  | TConst Type  -- ^ A @const@ version of another type.
  deriving (Eq, Show)

instance HasTVars Type where
  substTVar var val t = case t of
    TVar v | v == var -> val
           | otherwise -> t
    TVoid -> t
    TBool -> t
    TChar -> t
    TUChar -> t
    TShort -> t
    TUShort -> t
    TInt -> t
    TUInt -> t
    TLong -> t
    TULong -> t
    TLLong -> t
    TULLong -> t
    TFloat -> t
    TDouble -> t
    TPtrdiff -> t
    TSize -> t
    TSSize -> t
    TEnum _ -> t
    TPtr t' -> recur t'
    TRef t' -> recur t'
    TFn paramTypes retType -> TFn (map recur paramTypes) $ recur retType
    TCallback _ -> t
    TObj _ -> t
    TObjToHeap _ -> t
    TConst t' -> recur t'
    where recur = substTVar var val

typeIsConcrete :: Type -> Bool
typeIsConcrete t = case t of
  TVar _ -> False
  TVoid -> True
  TBool -> True
  TChar -> True
  TUChar -> True
  TShort -> True
  TUShort -> True
  TInt -> True
  TUInt -> True
  TLong -> True
  TULong -> True
  TLLong -> True
  TULLong -> True
  TFloat -> True
  TDouble -> True
  TPtrdiff -> True
  TSize -> True
  TSSize -> True
  TEnum _ -> True
  TPtr t' -> typeIsConcrete t'
  TRef t' -> typeIsConcrete t'
  TFn paramTypes retType -> all typeIsConcrete paramTypes && typeIsConcrete retType
  TCallback _ -> True
  TObj _ -> True
  TObjToHeap _ -> True
  TConst t' -> typeIsConcrete t'

class HasTVars a where
  -- @substTVar var val x@ replaces all occurrences of @'TVar' var@ in @x@ with
  -- @val@.  (Classes and callbacks pointed to by 'TCallback' and 'TObj' are not
  -- recurred into.)
  substTVar :: String -> Type -> a -> a

  substTVars :: [(String, Type)] -> a -> a
  substTVars vs x = foldr (uncurry substTVar) x vs

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

-- | Whether or not a function may cause side-effects.
--
-- Haskell bindings for pure functions will not be in 'IO', calls to pure
-- functions will be executed non-strictly.  Calls to impure functions will
-- execute in the IO monad.
--
-- Member functions for mutable classes should not be made pure, because it is
-- difficult in general to control when the call will be made.
data Purity = Nonpure  -- ^ Side-affects are possible.
            | Pure  -- ^ Side-affects will not happen.
            deriving (Eq, Show)

-- | A C++ function declaration.
data Function = Function
  { fnCName :: FnName Identifier
  , fnExtName :: ExtName
  , fnPurity :: Purity
  , fnParams :: [Type]
  , fnReturn :: Type
  , fnUseReqs :: Reqs
    -- ^ Requirements for a binding to call the function.
  }

instance Show Function where
  show fn =
    concat ["<Function ", show (fnExtName fn), " ", show (fnCName fn),
            show (fnParams fn), " ", show (fnReturn fn), ">"]

instance HasUseReqs Function where
  getUseReqs = fnUseReqs
  setUseReqs reqs fn = fn { fnUseReqs = reqs }

instance HasTVars Function where
  substTVar var val fn =
    fn { fnCName = case fnCName fn of
           FnName identifier -> FnName $ substTVar var val identifier
           x@(FnOp _) -> x
       , fnParams = map subst $ fnParams fn
       , fnReturn = subst $ fnReturn fn
       }
    where subst = substTVar var val

makeFn :: IsFnName Identifier name
       => name
       -> Maybe ExtName
       -- ^ An optional external name; will be automatically derived from
       -- the identifier if absent.
       -> Purity
       -> [Type]  -- ^ Parameter types.
       -> Type  -- ^ Return type.
       -> Function
makeFn cName maybeExtName purity paramTypes retType =
  let fnName = toFnName cName
  in Function fnName
              (extNameOrFnIdentifier fnName maybeExtName)
              purity paramTypes retType mempty

data FnTemplate = FnTemplate
  { fnTemplateIdentifier :: Identifier
  , fnTemplateExtNamePrefix :: String
  , fnTemplateVars :: [String]
  , fnTemplatePurity :: Purity
  , fnTemplateParams :: [Type]
  , fnTemplateReturn :: Type
  , fnTemplateUseReqs :: Reqs
  }

instance Show FnTemplate where
  show tmpl =
    concat ["<FnTemplate ", show (fnTemplateIdentifier tmpl), ">"]

instance HasUseReqs FnTemplate where
  getUseReqs = fnTemplateUseReqs
  setUseReqs reqs fnTemplate = fnTemplate { fnTemplateUseReqs = reqs }

makeFnTemplate ::
  Identifier
  -> Maybe String
  -- ^ An optional prefix for the external name of functions instantiated from
  -- this template.  Will use the last component of the identifier if absent.
  -> [String]  -- ^ The names of type variables.
  -> Purity
  -> [Type]  -- ^ Parameter types.
  -> Type  -- ^ Return type.
  -> FnTemplate
makeFnTemplate ident maybeExtNamePrefix vars purity paramTypes retType =
  FnTemplate ident (stringOrIdentifier ident maybeExtNamePrefix)
             vars purity paramTypes retType mempty

instantiateFnTemplate
  :: FnTemplate  -- ^ The template to instantiate.
  -> String
  -- ^ A suffix to append to the prefix given to 'makeFnTemplate' to form a
  -- complete 'ExtName' for the function.
  -> [Type]  -- ^ Types to substitute for the type variables in the template.
  -> Reqs  -- ^ Requirements for the type variables.
  -> Either String Function
instantiateFnTemplate tmpl extNameSuffix typeArgs typeArgUseReqs = do
  -- Ensure that the right number of type arguments are passed in.
  let ident = fnTemplateIdentifier tmpl
      vars = fnTemplateVars tmpl
      varCount = length vars
      argCount = length typeArgs
  when (argCount /= varCount) $
    Left $ concat
    ["instantiateFnTemplate: ", show argCount, " argument(s) given for ", show varCount,
     "-parameter function template ", show ident, "."]

  Right $
    addUseReqs (fnTemplateUseReqs tmpl `mappend` typeArgUseReqs) $
    substTVars (zip vars typeArgs) $
    makeFn ident
           (Just $ toExtName $ fnTemplateExtNamePrefix tmpl ++ extNameSuffix)
           (fnTemplatePurity tmpl)
           (fnTemplateParams tmpl)
           (fnTemplateReturn tmpl)

instantiateFnTemplate' :: FnTemplate -> String -> [Type] -> Reqs -> Function
instantiateFnTemplate' tmpl extNameSuffix typeArgs typeArgUseReqs =
  either error id $
  instantiateFnTemplate tmpl extNameSuffix typeArgs typeArgUseReqs

-- | A C++ class declaration.  A class's external name is automatically combined
-- with the external names of things inside the class, by way of
-- 'HasClassyExtName'.
data Class = Class
  { classIdentifier :: Identifier
  , classExtName :: ExtName
  , classSuperclasses :: [Class]
  , classCtors :: [Ctor]
  , classMethods :: [Method]
  , classConversions :: ClassConversions
  , classUseReqs :: Reqs
    -- ^ Requirements for a 'Type' to reference this class.
  , classInstantiationInfo :: Maybe ClassInstantiationInfo
  }

instance Eq Class where
  (==) = (==) `on` classIdentifier

instance Show Class where
  show cls =
    concat ["<Class ", show (classExtName cls), " ", show (classIdentifier cls), ">"]

instance HasUseReqs Class where
  getUseReqs = classUseReqs
  setUseReqs reqs cls = cls { classUseReqs = reqs }

instance HasTVars Class where
  substTVar var val cls =
    cls { classIdentifier = substTVar var val $ classIdentifier cls
        , classCtors = map doCtor $ classCtors cls
        , classMethods = map doMethod $ classMethods cls
        }
    where doCtor ctor =
            ctor { ctorParams = map subst $ ctorParams ctor }
          doMethod method =
            method { methodParams = map subst $ methodParams method
                   , methodReturn = subst $ methodReturn method
                   }
          subst = substTVar var val

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
  , classInstantiationInfo = Nothing
  }

-- | When a class object is returned from a function or taken as a parameter by
-- value (i.e. with 'TObj'), it will be converted to or from a foreign (non-C++)
-- object.  Conversion may also be performed explicitly.  This data type
-- describes how to perform those conversions.  A class may or may not support
-- conversion, for any particular foreign language; what is said below only
-- applies to classes that are convertible for a language.
--
-- When converting between a C++ value and a foreign value, a pointer to the
-- object is passed between C++ and the foreign language.  Then, for each
-- foreign language, a binding author can provide pieces of code in that
-- language to translate between the pointer and a foreign value (usually by
-- invoking the FFI functions generated by Cppop), and generated bindings will
-- perform these conversions automatically.  The code supplied to convert in
-- each direction should leave the original object unchanged (and alive, in case
-- of manual memory management).  (Internally, during a function call in either
-- direction, the side that creates a value is in charge of its lifetime, but
-- this is managed by Cppop.)
--
-- In foreign code, foreign values can be explicitly converted to new C++ (heap)
-- objects, and C++ object pointers can be explicitly converted to foreign
-- values, via special functions generated for the class.
data ClassConversions = ClassConversions
  { classHaskellConversion :: Maybe ClassHaskellConversion
  }

-- | Encoding parameters for a class that is not encodable or decodable.
classConversionsNone :: ClassConversions
classConversionsNone = ClassConversions Nothing

-- | Modifies classes' 'ClassEncoding' structures with a given function.
classModifyConversions :: (ClassConversions -> ClassConversions) -> Class -> Class
classModifyConversions f cls = cls { classConversions = f $ classConversions cls }

-- | Controls how conversions between C++ objects and Haskell values happen in
-- Haskell bindings.
data ClassHaskellConversion = ClassHaskellConversion
  { classHaskellConversionType :: Haskell.Generator HsType
    -- ^ Produces the Haskell type that represents a value of the corresponding
    -- C++ class.  This generator may add imports, but must not output code or
    -- add exports.
  , classHaskellConversionToCppFn :: Haskell.Generator ()
    -- ^ Produces a Haskell expression that evaluates to a function that takes
    -- an object of the type that 'classHaskellConversionType' generates, and
    -- returns a pointer to a new non-const C++ class object in IO.  The
    -- generator must output code and may add imports, but must not add exports.
  , classHaskellConversionFromCppFn :: Haskell.Generator ()
    -- ^ Produces a Haskell expression that evaluates to a function that takes a
    -- pointer to a const C++ class object, and returns an object of the type
    -- that 'classHaskellConversionType' generates, in IO.  The generator must
    -- output code and may add imports, but must not add exports.
  }

-- | Things that live inside of a class, and have the class's external name
-- prepended to their own in generated code.  With an external name of @\"bar\"@
-- and a class with external name @\"foo\"@, the resulting name will be
-- @\"foo_bar\"@.
class HasClassyExtName a where
  -- | Extracts the external name of the object, without the class name added.
  getClassyExtNameSuffix :: a -> ExtName

  -- | Computes the external name to use in generated code, containing both the
  -- class's and object's external names.
  getClassyExtName :: Class -> a -> ExtName
  getClassyExtName cls x =
    toExtName $ concat [fromExtName $ classExtName cls, "_", fromExtName $ getClassyExtNameSuffix x]

-- | A C++ class constructor declaration.
data Ctor = Ctor
  { ctorExtName :: ExtName
  , ctorParams :: [Type]
  }

instance Show Ctor where
  show ctor = concat ["<Ctor ", show (ctorExtName ctor), " ", show (ctorParams ctor), ">"]

instance HasClassyExtName Ctor where
  getClassyExtNameSuffix = ctorExtName

-- | Creates a 'Ctor' with full generality.
makeCtor :: ExtName
         -> [Type]  -- ^ Parameter types.
         -> Ctor
makeCtor = Ctor

-- | @mkCtor name@ creates a 'Ctor' whose external name is @className_name@.
mkCtor :: String
       -> [Type]  -- ^ Parameter types.
       -> Ctor
mkCtor = makeCtor . toExtName

-- | A C++ class method declaration.
--
-- Any operator function that can be written as a method may have its binding be
-- written either as part of the associated class or as a separate entity,
-- independently of how the function is declared in C++.
data Method = Method
  { methodCName :: FnName String
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

instance HasClassyExtName Method where
  getClassyExtNameSuffix = methodExtName

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

-- | Creates a 'Method' with full generality and manual name specification.
makeMethod :: IsFnName String name
           => name  -- ^ The C++ name of the method.
           -> ExtName  -- ^ The external name of the method.
           -> MethodApplicability
           -> Purity
           -> [Type]  -- ^ Parameter types.
           -> Type  -- ^ Return type.
           -> Method
makeMethod = Method . toFnName

-- | This function is internal.
--
-- Creates a method similar to 'makeMethod', but with automatic naming.  The
-- method's external name will be @className ++ \"_\" ++ cppMethodName@.  If the
-- method name is a 'FnOp' then the 'operatorPreferredExtName' will be appeneded
-- to the class name.
--
-- For creating multiple bindings to a method, see 'makeMethod''.
makeMethod' :: IsFnName String name
            => name  -- ^ The C++ name of the method.
            -> MethodApplicability
            -> Purity
            -> [Type]  -- ^ Parameter types.
            -> Type  -- ^ Return type.
            -> Method
makeMethod' name = makeMethod''' (toFnName name) Nothing

-- | This function is internal.
--
-- Creates a method similar to 'makeMethod'', but with an custom string that
-- will be appended to the class name to form the method's external name.  This
-- is useful for making multiple bindings to a method, e.g. for overloading and
-- optional arguments.
makeMethod'' :: IsFnName String name
             => name  -- ^ The C++ name of the method.
             -> String  -- ^ A foreign name for the method.
             -> MethodApplicability
             -> Purity
             -> [Type]  -- ^ Parameter types.
             -> Type  -- ^ Return type.
             -> Method
makeMethod'' name foreignName = makeMethod''' (toFnName name) $ Just foreignName

-- | The implementation of 'makeMethod'' and 'makeMethod'''.
makeMethod''' :: FnName String  -- ^ The C++ name of the method.
              -> Maybe String  -- ^ A foreign name for the method.
              -> MethodApplicability
              -> Purity
              -> [Type]  -- ^ Parameter types.
              -> Type  -- ^ Return type.
              -> Method
makeMethod''' (FnName "") maybeForeignName _ _ paramTypes retType =
  error $ concat ["makeMethod''': Given an empty method name with foreign name ",
                  show maybeForeignName, ", parameter types ", show paramTypes,
                  ", and return type ", show retType, "."]
makeMethod''' name (Just "") _ _ paramTypes retType =
  error $ concat ["makeMethod''': Given an empty foreign name with method ",
                  show name, ", parameter types ", show paramTypes, ", and return type ",
                  show retType, "."]
makeMethod''' name maybeForeignName appl purity paramTypes retType =
  let extName = flip fromMaybe (toExtName <$> maybeForeignName) $ case name of
        FnName s -> toExtName s
        FnOp op -> operatorPreferredExtName op
  in makeMethod name extName appl purity paramTypes retType

-- | Creates a nonconst, nonstatic 'Method' for @class::methodName@ and whose
-- external name is @class_methodName@.  If the name is an operator, then the
-- 'operatorPreferredExtName' will be used in the external name.
--
-- For creating multiple bindings to a method, see 'mkMethod''.
mkMethod :: IsFnName String name
         => name  -- ^ The C++ name of the method.
         -> [Type]  -- ^ Parameter types.
         -> Type  -- ^ Return type.
         -> Method
mkMethod name = makeMethod' name MNormal Nonpure

-- | Creates a nonconst, nonstatic 'Method' for method @class::methodName@ and
-- whose external name is @class_methodName@.  This enables multiple 'Method's
-- with different foreign names (and hence different external names) to bind to
-- the same method, e.g. to make use of optional arguments or overloading.  See
-- 'mkMethod' for a simpler form.
mkMethod' :: IsFnName String name
          => name  -- ^ The C++ name of the method.
          -> String  -- ^ A foreign name for the method.
          -> [Type]  -- ^ Parameter types.
          -> Type  -- ^ Return type.
          -> Method
mkMethod' cName foreignName = makeMethod'' cName foreignName MNormal Nonpure

-- | Same as 'mkMethod', but returns an 'MConst' method.
mkConstMethod :: IsFnName String name => name -> [Type] -> Type -> Method
mkConstMethod name = makeMethod' name MConst Nonpure

-- | Same as 'mkMethod'', but returns an 'MConst' method.
mkConstMethod' :: IsFnName String name => name -> String -> [Type] -> Type -> Method
mkConstMethod' cName foreignName = makeMethod'' cName foreignName MConst Nonpure

-- | Same as 'mkMethod', but returns an 'MStatic' method.
mkStaticMethod :: IsFnName String name => name -> [Type] -> Type -> Method
mkStaticMethod name = makeMethod' name MStatic Nonpure

-- | Same as 'mkMethod'', but returns an 'MStatic' method.
mkStaticMethod' :: IsFnName String name => name -> String -> [Type] -> Type -> Method
mkStaticMethod' cName foreignName = makeMethod'' cName foreignName MStatic Nonpure

-- | Used in conjunction with 'mkProp' and friends, this creates a list of
-- 'Method's for binding to getter/setter method pairs.  This can be used as
-- follows:
--
-- > myClass =
-- >   makeClass ... $
-- >   [ methods... ] ++
-- >   mkProps
-- >   [ mkBoolIsProp myClass "adjustable"
-- >   , mkProp myClass "maxWidth" TInt
-- >   ]
mkProps :: [[Method]] -> [Method]
mkProps = concat

-- | Creates a getter/setter binding pair for methods:
--
-- > T getFoo() const
-- > void setFoo(T)
mkProp :: String -> Type -> [Method]
mkProp name t =
  let c:cs = name
      setName = 's' : 'e' : 't' : toUpper c : cs
  in [ mkConstMethod name [] t
     , mkMethod setName [t] TVoid
     ]

-- | Creates a getter/setter binding pair for static methods:
--
-- > static T getFoo() const
-- > static void setFoo(T)
mkStaticProp :: String -> Type -> [Method]
mkStaticProp name t =
  let c:cs = name
      setName = 's' : 'e' : 't' : toUpper c : cs
  in [ mkStaticMethod name [] t
     , mkStaticMethod setName [t] TVoid
     ]

-- | Creates a getter/setter binding pair for boolean methods, where the getter
-- is prefixed with @is@:
--
-- > bool isFoo() const
-- > void setFoo(bool)
mkBoolIsProp :: String -> [Method]
mkBoolIsProp name =
  let c:cs = name
      name' = toUpper c : cs
      isName = 'i':'s':name'
      setName = 's':'e':'t':name'
  in [ mkConstMethod isName [] TBool
     , mkMethod setName [TBool] TVoid
     ]

-- | Creates a getter/setter binding pair for boolean methods, where the getter
-- is prefixed with @has@:
--
-- > bool hasFoo() const
-- > void setFoo(bool)
mkBoolHasProp :: String -> [Method]
mkBoolHasProp name =
  let c:cs = name
      name' = toUpper c : cs
      hasName = 'h':'a':'s':name'
      setName = 's':'e':'t':name'
  in [ mkConstMethod hasName [] TBool
     , mkMethod setName [TBool] TVoid
     ]

data ClassTemplate = ClassTemplate
  { classTemplateIdentifier :: Identifier
  , classTemplateExtNamePrefix :: String
  , classTemplateVars :: [String]
  , classTemplateSuperclasses :: [ClassTemplateSuper]
  , classTemplateCtors :: [Ctor]
  , classTemplateMethods :: [Method]
  , classTemplateConversions :: Maybe ClassTemplateConversionsGen
  , classTemplateUseReqs :: Reqs
  }

instance Eq ClassTemplate where
  (==) = (==) `on` classTemplateIdentifier

instance Show ClassTemplate where
  show tmpl =
    concat ["<ClassTemplate ", show (classTemplateIdentifier tmpl), ">"]

instance HasUseReqs ClassTemplate where
  getUseReqs = classTemplateUseReqs
  setUseReqs reqs classTemplate = classTemplate { classTemplateUseReqs = reqs }

data ClassTemplateSuper =
  ClassTemplateSuperClass Class
  -- ^ A non-templated superclass for a templated class.
  | ClassTemplateSuperTemplate ClassTemplate [Type]
    -- ^ A templated superclass for a templated class.

type ClassTemplateConversionsGen = Reader ClassTemplateConversionsEnv ClassConversions

data ClassTemplateConversionsEnv = ClassTemplateConversionsEnv
  { classTemplateConversionsEnvTypeArgs :: [Type]
  , classTemplateConversionsEnvMethodPrefix :: String
  }

makeClassTemplateConversionsEnv :: Class -> [Type] -> ClassTemplateConversionsEnv
makeClassTemplateConversionsEnv cls typeArgs =
  ClassTemplateConversionsEnv
  { classTemplateConversionsEnvTypeArgs = typeArgs
  , classTemplateConversionsEnvMethodPrefix = internalToHsFnName (classExtName cls) ++ "_"
  }

askTypeArgs :: Reader ClassTemplateConversionsEnv [Type]
askTypeArgs = asks classTemplateConversionsEnvTypeArgs

askMethodPrefix :: Reader ClassTemplateConversionsEnv String
askMethodPrefix = asks classTemplateConversionsEnvMethodPrefix

makeClassTemplate ::
  Identifier
  -- ^ Identifier for the class template.  Should include 'TVar's for all type
  -- variables.
  -> Maybe String
  -- ^ An optional prefix for the external name of classes instantiated from
  -- this template.  Will use the last component of the identifier if absent.
  -> [String]  -- ^ The names of type variables.
  -> [ClassTemplateSuper]  -- ^ Superclasses.
  -> [Ctor]  -- ^ Constructors, as in a non-template class.
  -> [Method]  -- ^ Methods, as in a non-template class.
  -> ClassTemplate
makeClassTemplate ident maybeExtNamePrefix vars supers ctors methods =
  ClassTemplate ident (stringOrIdentifier ident maybeExtNamePrefix)
                vars supers ctors methods Nothing mempty

addClassTemplateConversions :: ClassTemplateConversionsGen -> ClassTemplate -> ClassTemplate
addClassTemplateConversions convs tmpl = case classTemplateConversions tmpl of
  Nothing -> tmpl { classTemplateConversions = Just convs }
  Just _ ->
    error $ concat
    ["addClassTemplateEncoding: ", show tmpl, " already has conversions, trying to add again."]

instantiateClassTemplate ::
  ClassTemplate  -- ^ The template to instantiate.
  -> String
  -- ^ A suffix to append to the prefix given to 'makeClassTemplate' to form a
  -- complete 'ExtName' for the class.
  -> [Type]  -- ^ Types to substitute for the type variables in the template.
  -> [Class]
  -- ^ Instantiated class templates for all derived class templates
  -- ('ClassTemplateSuperTemplate').  Derived templates are not instantiated
  -- automatically because things like the 'ExtName' must be set manually, so
  -- the manually instantiated supers must be passed in here.  This list should
  -- not contain any entries for non-template supers
  -- ('ClassTemplateSuperClass').
  -> Reqs  -- ^ Requirements for the type variables.
  -> Either String Class
instantiateClassTemplate tmpl extNameSuffix typeArgs instantiatedSupers typeArgUseReqs = do
  -- Ensure that the right number of type arguments are passed in.
  let ident = classTemplateIdentifier tmpl
      supers = classTemplateSuperclasses tmpl
      vars = classTemplateVars tmpl
      varCount = length vars
      argCount = length typeArgs
  when (argCount /= varCount) $
    Left $ concat
    ["instantiateClassTemplate: ", show argCount, " argument(s) given for ", show varCount,
     "-parameter class template ", show ident, "."]

  -- Ensure that 'typeArgs' contains no type variables (since in this context
  -- they would be free).
  case filter (not . typeIsConcrete) typeArgs of
    [] -> return ()
    ts -> Left $ concat
          ["instantiateClassTemplate: Can't instantiate template ", show ident,
           " with types that contain free variables: ", show ts]

  -- Build the list of concrete superclasses (no templates), ensuring that the
  -- instantiated superclasses given for any template superclasses are
  -- instantiated from the expected templates.
  let templateSuperCount = length $
                           filter (\super -> case super of
                                      ClassTemplateSuperClass {} -> False
                                      ClassTemplateSuperTemplate {} -> True)
                           supers
      instantiatedSuperCount = length instantiatedSupers
  when (instantiatedSuperCount /= templateSuperCount) $
    Left $ concat
    ["instantiateClassTemplate: Template ", show ident, " has ", show templateSuperCount,
     " template superclass(es), but was given ", show instantiatedSuperCount,
     " instantiated superclass(es) during instantiation."]
  (unusedInstantiatedSupers, concreteSupers) <-
    runConsumeT instantiatedSupers $ forM supers $ \super -> case super of
      ClassTemplateSuperClass cls -> return cls
      ClassTemplateSuperTemplate superTmpl superTypeArgs -> do
        -- Read the already-instaniated superclass from the supplied list.  We
        -- checked counts above, so there should be one.
        superCls <-
          fromMaybeM (error $ concat
                      ["instantiateClassTemplate: Internal error, ran out of instantiated ",
                       "superclasses with template ", show ident, "."]) =<<
          next

        -- Ensure that 'superCls' is instantiated from 'superTmpl' with the
        -- correct type arguments.
        unless (classIsInstantiatedFromTemplate superCls superTmpl superTypeArgs) $
          lift $ Left $ concat
          ["instantiateClassTemplate: Superclass ", show (classExtName superCls),
           " is not an instantiation of template ", show (classTemplateIdentifier superTmpl),
           " with types ", show superTypeArgs, ", while instantiating template ",
           show ident, "."]

        return superCls
  unless (null unusedInstantiatedSupers) $
    Left $ concat
    ["instantiateClassTemplate: Internal error, unused instantiated superclasses with template ",
     show ident, "."]

  let clsExtName = classTemplateExtNamePrefix tmpl ++ extNameSuffix :: String
      result =
        (case classTemplateConversions tmpl of
            Nothing -> id
            Just convs -> classModifyConversions $ const $
                          runReader convs $ makeClassTemplateConversionsEnv result typeArgs) $
        addClassInstantiationInfo tmpl typeArgs $
        addUseReqs (classTemplateUseReqs tmpl `mappend` typeArgUseReqs) $
        substTVars (zip vars typeArgs) $
        makeClass ident
                  (Just $ toExtName clsExtName)
                  concreteSupers
                  (classTemplateCtors tmpl)
                  (classTemplateMethods tmpl)

  return result

instantiateClassTemplate' :: ClassTemplate -> String -> [Type] -> [Class] -> Reqs -> Class
instantiateClassTemplate' tmpl extNameSuffix typeArgs instantiatedSupers typeArgUseReqs =
  either error id $
  instantiateClassTemplate tmpl extNameSuffix typeArgs instantiatedSupers typeArgUseReqs

data ClassInstantiationInfo = ClassInstantiationInfo
  { classInstantiationTemplate :: ClassTemplate
  , classInstantiationTypeArgs :: [Type]
  } deriving (Eq)

instance Show ClassInstantiationInfo where
  show info =
    concat ["<ClassInstantiationInfo ", show (classInstantiationTemplate info),
            " ", show (classInstantiationTypeArgs info)]

addClassInstantiationInfo :: ClassTemplate -> [Type] -> Class -> Class
addClassInstantiationInfo tmpl typeArgs cls = case classInstantiationInfo cls of
  Nothing -> cls { classInstantiationInfo = Just $ ClassInstantiationInfo tmpl typeArgs }
  Just info ->
    error $ concat
    ["addClassInstantiationInfo: ", show cls, " already has ", show info,
     ", trying to add ", show tmpl, " and ", show typeArgs, "."]

classIsInstantiatedFromTemplate :: Class -> ClassTemplate -> [Type] -> Bool
classIsInstantiatedFromTemplate cls tmpl typeArgs = case classInstantiationInfo cls of
  Nothing -> False
  Just info -> classInstantiationTemplate info == tmpl &&
               classInstantiationTypeArgs info == typeArgs

-- | A non-C++ function that can be invoked via a C++ functor.
data Callback = Callback
  { callbackExtName :: ExtName
  , callbackParams :: [Type]
  , callbackReturn :: Type
  , callbackUseReqs :: Reqs
  }

instance Eq Callback where
  (==) = (==) `on` callbackExtName

instance Show Callback where
  show cb =
    concat ["<Callback ", show (callbackExtName cb), " ", show (callbackParams cb), " ",
            show (callbackReturn cb)]

instance HasUseReqs Callback where
  getUseReqs = callbackUseReqs
  setUseReqs reqs cb = cb { callbackUseReqs = reqs }

makeCallback :: ExtName
             -> [Type]  -- ^ Parameter types.
             -> Type  -- ^ Return type.
             -> Callback
makeCallback extName paramTypes retType = Callback extName paramTypes retType mempty

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

hsImportForSystemPosixTypes :: HsImportSet
hsImportForSystemPosixTypes = hsQualifiedImport "System.Posix.Types" "CppopSPT"

hsImportForUnsafeIO :: HsImportSet
hsImportForUnsafeIO = hsQualifiedImport "System.IO.Unsafe" "CppopSIU"

-- | Returns an error message that indicates that @caller@ received a 'TVar'
-- where one is not accepted.
freeVarErrorMsg :: String -> Type -> String
freeVarErrorMsg caller t = concat $ case t of
  TVar v -> [caller, ": Unexpected free template type variable ", show v, "."]
  _ -> ["freeVarErrorMsg: Expected a TVar from caller ", show caller,
        " but instead received ", show t, "."]

tObjToHeapWrongDirectionErrorMsg :: String -> Class -> String
tObjToHeapWrongDirectionErrorMsg caller cls =
  concat [caller, ": (TObjToHeap ", show cls, ") is not allowed to be passed into C++."]
