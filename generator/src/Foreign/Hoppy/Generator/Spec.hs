-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

-- | The primary data types for specifying C++ interfaces.
--
-- 'Show' instances in this module produce strings of the form @\"\<TypeOfObject
-- nameOfObject otherInfo...\>\"@.  They can be used in error messages without
-- specifying a noun separately, i.e. write @show cls@ instead of @\"the class
-- \" ++ show cls@.
module Foreign.Hoppy.Generator.Spec (
  -- * Interfaces
  Interface,
  ErrorMsg,
  interface,
  interfaceName,
  interfaceModules,
  interfaceNamesToModules,
  interfaceHaskellModuleBase,
  interfaceDefaultHaskellModuleBase,
  interfaceAddHaskellModuleBase,
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
  moduleReqs,
  moduleHaskellName,
  makeModule,
  moduleModify,
  moduleModify',
  moduleSetHppPath,
  moduleSetCppPath,
  moduleAddExports,
  moduleAddHaskellName,
  -- * Requirements
  Reqs,
  reqsIncludes,
  reqInclude,
  HasReqs (..),
  addReqs,
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
  operatorPreferredExtName',
  operatorType,
  Export (..),
  exportExtName,
  exportAddendum,
  Identifier,
  identifierParts,
  IdPart,
  idPartBase,
  idPartArgs,
  ident, ident', ident1, ident2, ident3, ident4, ident5,
  identT, identT', ident1T, ident2T, ident3T, ident4T, ident5T,
  -- * Basic types
  Type (..),
  normalizeType,
  stripConst,
  -- ** Variables
  Variable, makeVariable, varIdentifier, varExtName, varType, varReqs,
  varIsConst, varGetterExtName, varSetterExtName,
  -- ** Enums
  CppEnum, makeEnum, enumIdentifier, enumExtName, enumValueNames, enumReqs,
  -- ** Bitspaces
  Bitspace, makeBitspace, bitspaceExtName, bitspaceType, bitspaceValueNames, bitspaceEnum,
  bitspaceAddEnum, bitspaceCppTypeIdentifier, bitspaceFromCppValueFn, bitspaceToCppValueFn,
  bitspaceAddCppType, bitspaceReqs,
  -- ** Functions
  Purity (..),
  Function, makeFn, fnCName, fnExtName, fnPurity, fnParams, fnReturn, fnReqs,
  -- ** Classes
  Class, makeClass, classIdentifier, classExtName, classSuperclasses, classCtors, classDtorIsPublic,
  classMethods, classConversion, classReqs, classAddCtors, classSetDtorPrivate, classAddMethods,
  HasClassyExtName (..),
  Ctor, makeCtor, mkCtor, ctorExtName, ctorParams,
  Method,
  MethodImpl (..),
  MethodApplicability (..),
  Constness (..),
  constNegate,
  Staticness (..),
  makeMethod, makeFnMethod, mkMethod, mkMethod', mkConstMethod, mkConstMethod',
  mkStaticMethod, mkStaticMethod',
  mkProps, mkProp, mkStaticProp, mkBoolIsProp, mkBoolHasProp,
  methodImpl, methodExtName, methodApplicability, methodPurity, methodParams,
  methodReturn, methodConst, methodStatic,
  -- *** Conversion to and from foreign values
  ClassConversion (..),
  classConversionNone,
  classModifyConversion,
  ClassHaskellConversion (..),
  -- ** Callbacks
  Callback, makeCallback, callbackExtName, callbackParams, callbackReturn, callbackReqs,
  callbackToTFn,
  -- * Addenda
  Addendum (..),
  HasAddendum,
  addAddendumHaskell,
  -- * Haskell imports
  HsModuleName, HsImportSet, HsImportKey (..), HsImportSpecs (..), HsImportName, HsImportVal (..),
  hsWholeModuleImport, hsQualifiedImport, hsImport1, hsImport1', hsImports, hsImports',
  -- * Internal to Hoppy
  stringOrIdentifier,
  -- ** Haskell imports
  makeHsImportSet,
  getHsImportSet,
  hsImportForBits,
  hsImportForInt,
  hsImportForWord,
  hsImportForForeign,
  hsImportForForeignC,
  hsImportForPrelude,
  hsImportForRuntime,
  hsImportForSystemPosixTypes,
  hsImportForUnsafeIO,
  -- ** Error messages
  tObjToHeapWrongDirectionErrorMsg,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Arrow ((&&&))
import Control.Monad (liftM2, unless)
#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except (MonadError, throwError)
#else
import Control.Monad.Error (MonadError, throwError)
#endif
import Control.Monad.State (MonadState, StateT, execStateT, get, modify)
import Data.Char (isAlpha, isAlphaNum, toUpper)
import Data.Function (on)
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid, mappend, mconcat, mempty)
#endif
import qualified Data.Set as S
import {-# SOURCE #-} qualified Foreign.Hoppy.Generator.Language.Haskell as Haskell
import Language.Haskell.Syntax (HsType)

-- | Indicates strings that are error messages.
type ErrorMsg = String

-- | A complete specification of a C++ API.  Generators for different languages,
-- including the binding generator for C++, use these to produce their output.
data Interface = Interface
  { interfaceName :: String
    -- ^ The textual name of the interface.
  , interfaceModules :: M.Map String Module
    -- ^ All of the individual modules, by 'moduleName'.
  , interfaceNamesToModules :: M.Map ExtName Module
    -- ^ Maps each 'ExtName' exported by some module to the module that exports
    -- the name.
  , interfaceHaskellModuleBase' :: Maybe [String]
    -- ^ See 'interfaceHaskellModuleBase'.
  }

instance Show Interface where
  show iface = concat ["<Interface ", show (interfaceName iface), ">"]

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
    , interfaceHaskellModuleBase' = Nothing
    }

-- | The name of the parent Haskell module under which a Haskell module will be
-- generated for a Hoppy 'Module'.  This is a list of Haskell module path
-- components, in other words, @'Data.List.intercalate' "."@ on the list
-- produces a Haskell module name.  Defaults to
-- 'interfaceDefaultHaskellModuleBase', and may be overridden with
-- 'interfaceAddHaskellModuleBase'.
interfaceHaskellModuleBase :: Interface -> [String]
interfaceHaskellModuleBase =
  fromMaybe interfaceDefaultHaskellModuleBase . interfaceHaskellModuleBase'

-- | The default Haskell module under which Hoppy modules will be generated.
-- This is @Foreign.Hoppy.Generated@, that is:
--
-- > ["Foreign", "Hoppy", "Generated"]
interfaceDefaultHaskellModuleBase :: [String]
interfaceDefaultHaskellModuleBase = ["Foreign", "Hoppy", "Generated"]

-- | Sets an interface to generate all of its modules under the given Haskell
-- module prefix.  See 'interfaceHaskellModuleBase'.
interfaceAddHaskellModuleBase :: [String] -> Interface -> Either String Interface
interfaceAddHaskellModuleBase modulePath iface = case interfaceHaskellModuleBase' iface of
  Nothing -> Right iface { interfaceHaskellModuleBase' = Just modulePath }
  Just existingPath ->
    Left $ concat
    [ "addInterfaceHaskellModuleBase: Trying to add Haskell module base "
    , intercalate "." modulePath, " to ", show iface
    , " which already has a module base ", intercalate "." existingPath
    ]

-- | An @#include@ directive in a C++ file.
data Include = Include
  { includeToString :: String
    -- ^ Returns the complete @#include ...@ line for an include, including
    -- trailing newline.
  } deriving (Eq, Ord, Show)

-- | Creates an @#include \<...\>@ directive.
includeStd :: String -> Include
includeStd path = Include $ "#include <" ++ path ++ ">\n"

-- | Creates an @#include "..."@ directive.
includeLocal :: String -> Include
includeLocal path = Include $ "#include \"" ++ path ++ "\"\n"

-- | A portion of functionality in a C++ API.  An 'Interface' is composed of
-- multiple modules.  A module will generate a single compilation unit
-- containing bindings for all of the module's exports.  The C++ code for a
-- generated module will @#include@ everything necessary for what is written to
-- the header and source files separately.  You can declare include dependencies
-- with e.g. 'addReqIncludes', either for individual exports or at the module
-- level.  Dependencies between modules are handled automatically, and
-- circularity is supported to a certain extent.  See the documentation for the
-- individual language modules for further details.
data Module = Module
  { moduleName :: String
    -- ^ The module's name.  A module name must identify a unique module within
    -- an 'Interface'.
  , moduleHppPath :: String
    -- ^ A relative path under a C++ sources root to which the generator will
    -- write a header file for the module's C++ bindings.
  , moduleCppPath :: String
    -- ^ A relative path under a C++ sources root to which the generator will
    -- write a source file for the module's C++ bindings.
  , moduleExports :: M.Map ExtName Export
    -- ^ All of the exports in a module.
  , moduleReqs :: Reqs
    -- ^ Module-level requirements.
  , moduleHaskellName :: Maybe [String]
    -- ^ The generated Haskell module name, underneath the
    -- 'interfaceHaskellModuleBase'.  If absent (by default), the 'moduleName'
    -- is used.  May be modified with 'moduleAddHaskellName'.
  }

instance Eq Module where
  (==) = (==) `on` moduleName

instance Ord Module where
  compare = compare `on` moduleName

instance Show Module where
  show m = concat ["<Module ", moduleName m, ">"]

instance HasReqs Module where
  getReqs = moduleReqs
  setReqs reqs m = m { moduleReqs = reqs }

-- | Creates an empty module, ready to be configured with 'moduleModify'.
makeModule :: String  -- ^ 'moduleName'
           -> String  -- ^ 'moduleHppPath'
           -> String  -- ^ 'moduleCppPath'
           -> Module
makeModule name hppPath cppPath = Module
  { moduleName = name
  , moduleHppPath = hppPath
  , moduleCppPath = cppPath
  , moduleExports = M.empty
  , moduleReqs = mempty
  , moduleHaskellName = Nothing
  }

-- | Extends a module.  To be used with the module state-monad actions in this
-- package.
moduleModify :: Module -> StateT Module (Either String) () -> Either ErrorMsg Module
moduleModify = flip execStateT

-- | Same as 'moduleModify', but calls 'error' in the case of failure, which is
-- okay in for a generator which would abort in this case anyway.
moduleModify' :: Module -> StateT Module (Either String) () -> Module
moduleModify' m action = case moduleModify m action of
  Left errorMsg ->
    error $ concat
    ["moduleModify' failed to modify ", show m, ": ", errorMsg]
  Right m' -> m'

-- | Replaces a module's 'moduleHppPath'.
moduleSetHppPath :: MonadState Module m => String -> m ()
moduleSetHppPath path = modify $ \m -> m { moduleHppPath = path }

-- | Replaces a module's 'moduleCppPath'.
moduleSetCppPath :: MonadState Module m => String -> m ()
moduleSetCppPath path = modify $ \m -> m { moduleCppPath = path }

-- | Adds exports to a module.  An export must only be added to any module at
-- most once, and must not be added to multiple modules.
moduleAddExports :: (MonadError String m, MonadState Module m) => [Export] -> m ()
moduleAddExports exports = do
  m <- get
  let existingExports = moduleExports m
      newExports = M.fromList $ map (exportExtName &&& id) exports
      duplicateNames = (S.intersection `on` M.keysSet) existingExports newExports
  if S.null duplicateNames
    then modify $ \m -> m { moduleExports = existingExports `mappend` newExports }
    else throwError $ concat
         ["moduleAddExports: ", show m, " defines external names multiple times: ",
          show duplicateNames]

-- | Changes a module's 'moduleHaskellName' from the default.  This can only be
-- called once on a module.
moduleAddHaskellName :: (MonadError String m, MonadState Module m) => [String] -> m ()
moduleAddHaskellName name = do
  m <- get
  case moduleHaskellName m of
    Nothing -> modify $ \m -> m { moduleHaskellName = Just name }
    Just name' ->
      throwError $ concat
      ["moduleAddHaskellName: ", show m, " already has Haskell name ",
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

-- | C++ types that have requirements in order to use them in generated
-- bindings.
class HasReqs a where
  {-# MINIMAL getReqs, (setReqs | modifyReqs) #-}

  -- | Returns an object's requirements.
  getReqs :: a -> Reqs

  -- | Replaces an object's requirements with new ones.
  setReqs :: Reqs -> a -> a
  setReqs = modifyReqs . const

  -- | Modifies an object's requirements.
  modifyReqs :: (Reqs -> Reqs) -> a -> a
  modifyReqs f x = setReqs (f $ getReqs x) x

-- | Adds to a object's requirements.
addReqs :: HasReqs a => Reqs -> a -> a
addReqs reqs = modifyReqs $ mappend reqs

-- | Adds a list of includes to the requirements of an object.
addReqIncludes :: HasReqs a => [Include] -> a -> a
addReqIncludes includes =
  modifyReqs $ mappend mempty { reqsIncludes = S.fromList includes }

-- | An external name is a string that generated bindings use to uniquely
-- identify an object at runtime.  An external name must start with an
-- alphabetic character, and may only contain alphanumeric characters and @'_'@.
-- You are free to use whatever naming style you like; case conversions will be
-- performed automatically when required.  Hoppy does make use of some
-- conventions though, for example with 'Operator's and in the provided bindings
-- for the C++ standard library.
--
-- External names must be unique within an interface.  They may not be reused
-- between modules.  This assumption is used for symbol naming in compiled
-- shared objects and to freely import modules in Haskell bindings.
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

-- | Generates an 'ExtName' from an @'FnName' 'Identifier'@, if the given name
-- is absent.
extNameOrFnIdentifier :: FnName Identifier -> Maybe ExtName -> ExtName
extNameOrFnIdentifier name =
  fromMaybe $ case name of
    FnName identifier -> case identifierParts identifier of
      [] -> error "extNameOrFnIdentifier: Empty idenfitier."
      parts -> toExtName $ idPartBase $ last parts
    FnOp op -> operatorPreferredExtName op

-- | The C++ name of a function or method.
data FnName name =
  FnName name
  -- ^ A regular, \"alphanumeric\" name.  The exact type depends on what kind of
  -- object is being named.
  | FnOp Operator
    -- ^ An operator name.
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

-- | Overloadable C++ operators.
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
  | CallOperator  -- ^ @x(...)@ with arbitrary arity.
  | ArrayOperator  -- ^ @x[y]@, a binary operator with non-infix syntax.

data OperatorInfo = OperatorInfo
  { operatorPreferredExtName'' :: ExtName
  , operatorType' :: OperatorType
  }

makeOperatorInfo :: String -> OperatorType -> OperatorInfo
makeOperatorInfo = OperatorInfo . toExtName

-- | Returns a conventional string to use for the 'ExtName' of an operator.
operatorPreferredExtName :: Operator -> ExtName
operatorPreferredExtName op = case M.lookup op operatorInfo of
  Just info -> operatorPreferredExtName'' info
  Nothing ->
    error $ concat
    ["operatorPreferredExtName: Internal error, missing info for operator ", show op, "."]

-- | Returns a conventional name for an operator, as with
-- 'operatorPreferredExtName', but as a string.
operatorPreferredExtName' :: Operator -> String
operatorPreferredExtName' = fromExtName . operatorPreferredExtName

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
  ExportVariable Variable  -- ^ Exports a variable.
  | ExportEnum CppEnum  -- ^ Exports an enum.
  | ExportBitspace Bitspace  -- ^ Exports a bitspace.
  | ExportFn Function  -- ^ Exports a function.
  | ExportClass Class  -- ^ Exports a class with all of its contents.
  | ExportCallback Callback  -- ^ Exports a callback.
  deriving (Show)

-- | Returns the external name of an export.
exportExtName :: Export -> ExtName
exportExtName export = case export of
  ExportVariable v -> varExtName v
  ExportEnum e -> enumExtName e
  ExportBitspace b -> bitspaceExtName b
  ExportFn f -> fnExtName f
  ExportClass c -> classExtName c
  ExportCallback cb -> callbackExtName cb

-- | Returns the export's addendum.  'Export' doesn't have a 'HasAddendum'
-- instance because you normally wouldn't want to modify the addendum of one.
exportAddendum export = case export of
  ExportVariable v -> getAddendum v
  ExportEnum e -> getAddendum e
  ExportBitspace bs -> getAddendum bs
  ExportFn f -> getAddendum f
  ExportClass cls -> getAddendum cls
  ExportCallback cb -> getAddendum cb

-- | A path to some C++ object, including namespaces.  An identifier consists of
-- multiple parts separated by @\"::\"@.  Each part has a name string followed
-- by an optional template argument list, where each argument gets rendered from
-- a 'Type' (non-type arguments for template metaprogramming are not supported).
newtype Identifier = Identifier
  { identifierParts :: [IdPart]
    -- ^ The separate parts of the identifier, between @::@s.
  } deriving (Eq)

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

-- | A single component of an 'Identifier', between @::@s.
data IdPart = IdPart
  { idPartBase :: String
    -- ^ The name within the enclosing scope.
  , idPartArgs :: Maybe [Type]
    -- ^ Template arguments, if present.
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

-- | Creates an identifier of the form @a\<...\>@.
identT :: String -> [Type] -> Identifier
identT a ts = Identifier [IdPart a $ Just ts]

-- | Creates an identifier with arbitrary many templated and non-templated
-- parts.
identT' :: [(String, Maybe [Type])] -> Identifier
identT' = Identifier . map (uncurry IdPart)

-- | Creates an identifier of the form @a::b\<...\>@.
ident1T :: String -> String -> [Type] -> Identifier
ident1T a b ts = Identifier [IdPart a Nothing, IdPart b $ Just ts]

-- | Creates an identifier of the form @a::b::c\<...\>@.
ident2T :: String -> String -> String -> [Type] -> Identifier
ident2T a b c ts = Identifier [IdPart a Nothing, IdPart b Nothing, IdPart c $ Just ts]

-- | Creates an identifier of the form @a::b::c::d\<...\>@.
ident3T :: String -> String -> String -> String -> [Type] -> Identifier
ident3T a b c d ts =
  Identifier [IdPart a Nothing, IdPart b Nothing, IdPart c Nothing,
              IdPart d $ Just ts]

-- | Creates an identifier of the form @a::b::c::d::e\<...\>@.
ident4T :: String -> String -> String -> String -> String -> [Type] -> Identifier
ident4T a b c d e ts =
  Identifier [IdPart a Nothing, IdPart b Nothing, IdPart c Nothing,
              IdPart d Nothing, IdPart e $ Just ts]

-- | Creates an identifier of the form @a::b::c::d::e::f\<...\>@.
ident5T :: String -> String -> String -> String -> String -> String -> [Type] -> Identifier
ident5T a b c d e f ts =
  Identifier [IdPart a Nothing, IdPart b Nothing, IdPart c Nothing,
              IdPart d Nothing, IdPart e Nothing, IdPart f $ Just ts]

-- | Concrete C++ types.  It is possible to represent invalid C++ types with
-- this, but we try to catch these and fail cleanly as much as possible.
data Type =
    TVoid  -- ^ C++ @void@, Haskell @()@.
  | TBool  -- ^ C++ @bool@, Haskell 'Bool'.
  | TChar  -- ^ C++ @char@, Haskell 'Foreign.C.CChar'.
  | TUChar  -- ^ C++ @unsigned char@, Haskell 'Foreign.C.CUChar'.
  | TShort  -- ^ C++ @short int@, Haskell 'Foreign.C.CShort'.
  | TUShort  -- ^ C++ @unsigned short int@, Haskell 'Foreign.C.CUShort'.
  | TInt  -- ^ C++ @int@, Haskell 'Foreign.C.CInt'.
  | TUInt  -- ^ C++ @unsigned int@, Haskell 'Foreign.C.CUInt'.
  | TLong  -- ^ C++ @long int@, Haskell 'Foreign.C.CLong'.
  | TULong  -- ^ C++ @unsigned long int@, Haskell 'Foreign.C.CULong'.
  | TLLong  -- ^ C++ @long long int@, Haskell 'Foreign.C.CLLong'.
  | TULLong  -- ^ C++ @unsigned long long int@, Haskell 'Foreign.C.CULLong'.
  | TFloat  -- ^ C++ @float@, Haskell 'Foreign.C.CFloat'.
  | TDouble  -- ^ C++ @double@, Haskell 'Foreign.C.CDouble'.
  | TInt8  -- ^ C++ @int8_t@, Haskell 'Data.Int.Int8'.
  | TInt16  -- ^ C++ @int16_t@, Haskell 'Data.Int.Int16'.
  | TInt32  -- ^ C++ @int32_t@, Haskell 'Data.Int.Int32'.
  | TInt64  -- ^ C++ @int64_t@, Haskell 'Data.Int.Int64'.
  | TWord8  -- ^ C++ @uint8_t@, Haskell 'Data.Word.Word8'.
  | TWord16  -- ^ C++ @uint16_t@, Haskell 'Data.Word.Word16'.
  | TWord32  -- ^ C++ @uint32_t@, Haskell 'Data.Word.Word32'.
  | TWord64  -- ^ C++ @uint64_t@, Haskell 'Data.Word.Word64'.
  | TPtrdiff  -- ^ C++ @ptrdiff_t@, Haskell 'Foreign.C.CPtrdiff'.
  | TSize  -- ^ C++ @size_t@, Haskell 'Foreign.C.CSize'.
  | TSSize  -- ^ C++ @ssize_t@, Haskell 'System.Posix.Types.CSsize'.
  | TEnum CppEnum  -- ^ A C++ @enum@ value.
  | TBitspace Bitspace  -- ^ A C++ bitspace value.
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
    -- 'ClassConversion', the object will be copied to the heap, and a pointer
    -- to the new object will be passed.  The object must be copy-constructable.
    --
    -- __The foreign language owns the pointer, even for callback arguments.__
  | TConst Type  -- ^ A @const@ version of another type.
  deriving (Eq, Show)

-- | Canonicalizes a 'Type' without changing its meaning.  Multiple nested
-- 'TConst's are collapsed into a single one.
normalizeType :: Type -> Type
normalizeType t = case t of
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
  TInt8 -> t
  TInt16 -> t
  TInt32 -> t
  TInt64 -> t
  TWord8 -> t
  TWord16 -> t
  TWord32 -> t
  TWord64 -> t
  TPtrdiff -> t
  TSize -> t
  TSSize -> t
  TEnum _ -> t
  TBitspace _ -> t
  TPtr t' -> TPtr $ normalizeType t'
  TRef t' -> TRef $ normalizeType t'
  TFn paramTypes retType -> TFn (map normalizeType paramTypes) $ normalizeType retType
  TCallback _ -> t
  TObj _ -> t
  TObjToHeap _ -> t
  TConst (TConst t') -> normalizeType $ TConst t'
  TConst _ -> t

-- | Strips leading 'TConst's off of a type.
stripConst :: Type -> Type
stripConst t = case t of
  TConst t' -> stripConst t'
  _ -> t

-- | A C++ variable.
data Variable = Variable
  { varIdentifier :: Identifier
    -- ^ The identifier used to refer to the variable.
  , varExtName :: ExtName
    -- ^ The variable's external name.
  , varType :: Type
    -- ^ The type of the variable.  This may be 'TConst' to indicate that the
    -- variable is read-only.
  , varReqs :: Reqs
    -- ^ Requirements for bindings to use this variable.
  , varAddendum :: Addendum
    -- ^ The variable's addendum.
  }

instance Eq Variable where
  (==) = (==) `on` varIdentifier

instance Show Variable where
  show v = concat ["<Variable ", show (varExtName v), " ", show (varType v), ">"]

instance HasReqs Variable where
  getReqs = varReqs
  setReqs reqs v = v { varReqs = reqs }

instance HasAddendum Variable where
  getAddendum = varAddendum
  setAddendum addendum v = v { varAddendum = addendum }

-- | Creates a binding for a C++ variable.
makeVariable :: Identifier -> Maybe ExtName -> Type -> Variable
makeVariable identifier maybeExtName t =
  Variable identifier (extNameOrIdentifier identifier maybeExtName) t mempty mempty

-- | Returns whether the variable is constant, i.e. whether its type is
-- @'TConst' ...@.
varIsConst :: Variable -> Bool
varIsConst v = case varType v of
  TConst _ -> True
  _ -> False

-- | Returns the external name of the getter function for the variable.
varGetterExtName :: Variable -> ExtName
varGetterExtName = toExtName . (++ "_get") . fromExtName . varExtName

-- | Returns the external name of the setter function for the variable.
varSetterExtName :: Variable -> ExtName
varSetterExtName = toExtName . (++ "_set") . fromExtName . varExtName

-- | A C++ enum declaration.  An enum should actually be enumerable (in the
-- sense of Haskell's 'Enum'); if it's not, consider using a 'Bitspace' instead.
data CppEnum = CppEnum
  { enumIdentifier :: Identifier
    -- ^ The identifier used to refer to the enum.
  , enumExtName :: ExtName
    -- ^ The enum's external name.
  , enumValueNames :: [(Int, [String])]
    -- ^ The numeric values and names of the enum values.  A single value's name
    -- is broken up into words.  How the words and ext name get combined to make
    -- a name in a particular foreign language depends on the language.
  , enumReqs :: Reqs
    -- ^ Requirements for a 'Type' to reference this enum.
  , enumAddendum :: Addendum
    -- ^ The enum's addendum.
  }

instance Eq CppEnum where
  (==) = (==) `on` enumIdentifier

instance Show CppEnum where
  show e = concat ["<Enum ", show (enumExtName e), " ", show (enumIdentifier e), ">"]

instance HasReqs CppEnum where
  getReqs = enumReqs
  setReqs reqs e = e { enumReqs = reqs }

instance HasAddendum CppEnum where
  getAddendum = enumAddendum
  setAddendum addendum e = e { enumAddendum = addendum }

-- | Creates a binding for a C++ enum.
makeEnum :: Identifier  -- ^ 'enumIdentifier'
         -> Maybe ExtName
         -- ^ An optional external name; will be automatically derived from
         -- the identifier if absent.
         -> [(Int, [String])]  -- ^ 'enumValueNames'
         -> CppEnum
makeEnum identifier maybeExtName valueNames =
  CppEnum identifier (extNameOrIdentifier identifier maybeExtName) valueNames mempty mempty

-- | A C++ numeric space with bitwise operations.  This is similar to a
-- 'CppEnum', but in addition to the extra operations, this differs in that
-- these values aren't enumerable.
--
-- Additionally, as a kludge for Qtah, a bitspace may have a C++ type
-- ('bitspaceCppTypeIdentifier') separate from its numeric type
-- ('bitspaceType').  Qt bitspaces aren't raw numbers but are instead type-safe
-- @QFlags@ objects that don't implicitly convert from integers, so we need a
-- means to do so manually.  Barring general ad-hoc argument and return value
-- conversion support, we allow this as follows: when given a C++ type, then a
-- bitspace may also have a conversion function between the numeric and C++
-- type, in each direction.  If a conversion function is present, it will be
-- used for conversions in its respective direction.  The C++ type is not a full
-- 'Type', but only an 'Identifier', since additional information is not needed.
-- See 'bitspaceAddCppType'.
data Bitspace = Bitspace
  { bitspaceExtName :: ExtName
    -- ^ The bitspace's external name.
  , bitspaceType :: Type
    -- ^ The C++ type used for bits values.  This should be a primitive numeric
    -- type.
  , bitspaceValueNames :: [(Int, [String])]
    -- ^ The numeric values and names of the bitspace values.  See
    -- 'enumValueNames'.
  , bitspaceEnum :: Maybe CppEnum
    -- ^ An associated enum, whose values may be converted to values in the
    -- bitspace.
  , bitspaceCppTypeIdentifier :: Maybe Identifier
    -- ^ The optional C++ type for a bitspace.
  , bitspaceToCppValueFn :: Maybe String
    -- ^ The name of a C++ function to convert from 'bitspaceType' to the
    -- bitspace's C++ type.
  , bitspaceFromCppValueFn :: Maybe String
    -- ^ The name of a C++ function to convert from the bitspace's C++ type to
    -- 'bitspaceType'.
  , bitspaceReqs :: Reqs
    -- ^ Requirements for emitting the bindings for a bitspace, i.e. what's
    -- necessary to reference 'bitspaceCppTypeIdentifier',
    -- 'bitspaceFromCppValueFn', and 'bitspaceToCppValueFn'.  'bitspaceType' can
    -- take some numeric types that require includes as well, but you don't need
    -- to list these here.
  , bitspaceAddendum :: Addendum
    -- ^ The bitspace's addendum.
  }

instance Eq Bitspace where
  (==) = (==) `on` bitspaceExtName

instance Show Bitspace where
  show e = concat ["<Bitspace ", show (bitspaceExtName e), " ", show (bitspaceType e), ">"]

instance HasReqs Bitspace where
  getReqs = bitspaceReqs
  setReqs reqs b = b { bitspaceReqs = reqs }

instance HasAddendum Bitspace where
  getAddendum = bitspaceAddendum
  setAddendum addendum bs = bs { bitspaceAddendum = addendum }

-- | Creates a binding for a C++ bitspace.
makeBitspace :: ExtName  -- ^ 'bitspaceExtName'
             -> Type  -- ^ 'bitspaceType'
             -> [(Int, [String])]  -- ^ 'bitspaceValueNames'
             -> Bitspace
makeBitspace extName t valueNames =
  Bitspace extName t valueNames Nothing Nothing Nothing Nothing mempty mempty

-- | Associates an enum with the bitspace.  See 'bitspaceEnum'.
bitspaceAddEnum :: CppEnum -> Bitspace -> Bitspace
bitspaceAddEnum enum bitspace = case bitspaceEnum bitspace of
  Just enum' ->
    error $ concat
    ["bitspaceAddEnum: Adding ", show enum, " to ", show bitspace,
     ", but it already has ", show enum', "."]
  Nothing ->
    if bitspaceValueNames bitspace /= enumValueNames enum
    then error $ concat
         ["bitspaceAddEnum: Trying to add ", show enum, " to ", show bitspace,
          ", but the values aren't equal.\nBitspace values: ", show $ bitspaceValueNames bitspace,
          "\n    Enum values: ", show $ enumValueNames enum]
    else bitspace { bitspaceEnum = Just enum }

-- | @bitspaceAddCppType cppTypeIdentifier toCppValueFn fromCppValueFn@
-- associates a C++ type (plus optional conversion functions) with a bitspace.
-- At least one conversion should be specified, otherwise adding the C++ type
-- will mean nothing.  You should also add use requirements to the bitspace for
-- all of these arguments; see 'HasReqs'.
bitspaceAddCppType :: Identifier -> Maybe String -> Maybe String -> Bitspace -> Bitspace
bitspaceAddCppType cppTypeId toCppValueFnMaybe fromCppValueFnMaybe b =
  case bitspaceCppTypeIdentifier b of
    Just cppTypeId' ->
      error $ concat
      ["bitspaceAddCppType: Adding C++ type ", show cppTypeId,
       " to ", show b, ", but it already has ", show cppTypeId', "."]
    Nothing ->
      b { bitspaceCppTypeIdentifier = Just cppTypeId
        , bitspaceToCppValueFn = toCppValueFnMaybe
        , bitspaceFromCppValueFn = fromCppValueFnMaybe
        }

-- | Whether or not a function may cause side-effects.
--
-- Haskell bindings for pure functions will not be in 'IO', and calls to pure
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
    -- ^ The identifier used to call the function.
  , fnExtName :: ExtName
    -- ^ The function's external name.
  , fnPurity :: Purity
    -- ^ Whether the function is pure.
  , fnParams :: [Type]
    -- ^ The function's parameter types.
  , fnReturn :: Type
    -- ^ The function's return type.
  , fnReqs :: Reqs
    -- ^ Requirements for a binding to call the function.
  , fnAddendum :: Addendum
    -- ^ The function's addendum.
  }

instance Show Function where
  show fn =
    concat ["<Function ", show (fnExtName fn), " ", show (fnCName fn),
            show (fnParams fn), " ", show (fnReturn fn), ">"]

instance HasReqs Function where
  getReqs = fnReqs
  setReqs reqs fn = fn { fnReqs = reqs }

instance HasAddendum Function where
  getAddendum = fnAddendum
  setAddendum addendum fn = fn { fnAddendum = addendum }

-- | Creates a binding for a C++ function.
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
              purity paramTypes retType mempty mempty

-- | A C++ class declaration.  A class's external name is automatically combined
-- with the external names of things inside the class, by way of
-- 'HasClassyExtName'.
data Class = Class
  { classIdentifier :: Identifier
    -- ^ The identifier used to refer to the class.
  , classExtName :: ExtName
    -- ^ The class's external name.
  , classSuperclasses :: [Class]
    -- ^ The class's public superclasses.
  , classCtors :: [Ctor]
    -- ^ The class's constructors.
  , classDtorIsPublic :: Bool
    -- ^ Whether the class's destructor has public visibility.
  , classMethods :: [Method]
    -- ^ The class's methods.
  , classConversion :: ClassConversion
    -- ^ Behaviour for converting objects to and from foriegn values.
  , classReqs :: Reqs
    -- ^ Requirements for a 'Type' to reference this class.
  , classAddendum :: Addendum
    -- ^ The class's addendum.
  }

instance Eq Class where
  (==) = (==) `on` classIdentifier

instance Show Class where
  show cls =
    concat ["<Class ", show (classExtName cls), " ", show (classIdentifier cls), ">"]

instance HasReqs Class where
  getReqs = classReqs
  setReqs reqs cls = cls { classReqs = reqs }

instance HasAddendum Class where
  getAddendum = classAddendum
  setAddendum addendum cls = cls { classAddendum = addendum }

-- | Creates a binding for a C++ class and its contents.
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
  , classDtorIsPublic = True
  , classMethods = methods
  , classConversion = classConversionNone
  , classReqs = mempty
  , classAddendum = mempty
  }

-- | Adds constructors to a class.
classAddCtors :: [Ctor] -> Class -> Class
classAddCtors ctors cls =
  if null ctors then cls else cls { classCtors = classCtors cls ++ ctors }

-- | Marks a class's destructor as private, so that a binding for it won't be
-- generated.
classSetDtorPrivate :: Class -> Class
classSetDtorPrivate cls = cls { classDtorIsPublic = False }

-- | Adds methods to a class.
classAddMethods :: [Method] -> Class -> Class
classAddMethods methods cls =
  if null methods then cls else cls { classMethods = classMethods cls ++ methods }

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
-- invoking the FFI functions generated by Hoppy), and generated bindings will
-- perform these conversions automatically.  The code supplied to convert in
-- each direction should leave the original object unchanged (and alive, in case
-- of manual memory management).  (Internally, during a function call in either
-- direction, the side that creates a value is in charge of its lifetime, but
-- this is managed by Hoppy.)
--
-- In foreign code, foreign values can be explicitly converted to new C++ (heap)
-- objects, and C++ object pointers can be explicitly converted to foreign
-- values, via special functions generated for the class.
data ClassConversion = ClassConversion
  { classHaskellConversion :: Maybe ClassHaskellConversion
    -- ^ Conversions to and from Haskell.
  }

-- | Encoding parameters for a class that is not encodable or decodable.
classConversionNone :: ClassConversion
classConversionNone = ClassConversion Nothing

-- | Modifies classes' 'ClassEncoding' structures with a given function.
classModifyConversion :: (ClassConversion -> ClassConversion) -> Class -> Class
classModifyConversion f cls = cls { classConversion = f $ classConversion cls }

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
  --
  -- See also 'Foreign.Hoppy.Generator.Language.Haskell.General.toHsMethodName'.
  getClassyExtName :: Class -> a -> ExtName
  getClassyExtName cls x =
    toExtName $ concat [fromExtName $ classExtName cls, "_", fromExtName $ getClassyExtNameSuffix x]

-- | A C++ class constructor declaration.
data Ctor = Ctor
  { ctorExtName :: ExtName
    -- ^ The constructor's external name.
  , ctorParams :: [Type]
    -- ^ The constructor's parameter types.
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
  { methodImpl :: MethodImpl
    -- ^ The underlying code that the binding calls.
  , methodExtName :: ExtName
    -- ^ The method's external name.
  , methodApplicability :: MethodApplicability
    -- ^ How the method is associated to its class.
  , methodPurity :: Purity
    -- ^ Whether the method is pure.
  , methodParams :: [Type]
    -- ^ The method's parameter types.
  , methodReturn :: Type
    -- ^ The method's return type.
  }

instance Show Method where
  show method =
    concat ["<Method ", show (methodExtName method), " ",
            case methodImpl method of
              RealMethod name -> show name
              FnMethod name -> show name, " ",
            show (methodApplicability method), " ",
            show (methodPurity method), " ",
            show (methodParams method), " ",
            show (methodReturn method), ">"]

instance HasClassyExtName Method where
  getClassyExtNameSuffix = methodExtName

-- | The C++ code to which a 'Method' is bound.
data MethodImpl =
  RealMethod (FnName String)
  -- ^ The 'Method' is bound to an actual class method.
  | FnMethod (FnName Identifier)
    -- ^ The 'Method' is bound to a wrapper function.  When wrapping a method
    -- with another function, this is preferrable to just using a 'Function'
    -- binding because a method will still appear to be part of the class in
    -- foreign bindings.
  deriving (Eq, Show)

-- | How a method is associated to its class.  A method may be static, const, or
-- neither (a regular method).
data MethodApplicability = MNormal | MStatic | MConst
                         deriving (Eq, Show)

-- | Whether or not a method is const.
data Constness = Nonconst | Const
               deriving (Eq, Show)

-- | Returns the opposite constness value.
constNegate :: Constness -> Constness
constNegate Nonconst = Const
constNegate Const = Nonconst

-- | Whether or not a method is static.
data Staticness = Nonstatic | Static
               deriving (Eq, Show)

-- | Returns the constness of a method, based on its 'methodApplicability'.
methodConst :: Method -> Constness
methodConst method = case methodApplicability method of
  MConst -> Const
  _ -> Nonconst

-- | Returns the staticness of a method, based on its 'methodApplicability'.
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
makeMethod name = Method $ RealMethod $ toFnName name

-- | Creates a 'Method' that is in fact backed by a C++ non-member function (a
-- la 'makeFn'), but appears to be a regular method.  This is useful for
-- wrapping a method on the C++ side when its arguments aren't right for binding
-- directly.
--
-- A @this@ pointer parameter is __not__ automatically added to the parameter
-- list for non-static methods created with @makeFnMethod@.
makeFnMethod :: IsFnName Identifier name
             => name
             -> String
             -> MethodApplicability
             -> Purity
             -> [Type]
             -> Type
             -> Method
makeFnMethod cName foreignName = Method (FnMethod $ toFnName cName) (toExtName foreignName)

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

-- | A non-C++ function that can be invoked via a C++ functor.
data Callback = Callback
  { callbackExtName :: ExtName
    -- ^ The callback's external name.
  , callbackParams :: [Type]
    -- ^ The callback's parameter types.
  , callbackReturn :: Type
    -- ^ The callback's return type.
  , callbackReqs :: Reqs
    -- ^ Requirements for the callback.
  , callbackAddendum :: Addendum
    -- ^ The callback's addendum.
  }

instance Eq Callback where
  (==) = (==) `on` callbackExtName

instance Show Callback where
  show cb =
    concat ["<Callback ", show (callbackExtName cb), " ", show (callbackParams cb), " ",
            show (callbackReturn cb)]

instance HasReqs Callback where
  getReqs = callbackReqs
  setReqs reqs cb = cb { callbackReqs = reqs }

instance HasAddendum Callback where
  getAddendum = callbackAddendum
  setAddendum addendum cb = cb { callbackAddendum = addendum }

-- | Creates a binding for constructing callbacks into foreign code.
makeCallback :: ExtName
             -> [Type]  -- ^ Parameter types.
             -> Type  -- ^ Return type.
             -> Callback
makeCallback extName paramTypes retType = Callback extName paramTypes retType mempty mempty

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
newtype HsImportSet = HsImportSet
  { getHsImportSet :: M.Map HsImportKey HsImportSpecs
    -- ^ Returns the import set's internal map from module names to imported
    -- bindings.
  } deriving (Show)

instance Monoid HsImportSet where
  mempty = HsImportSet M.empty

  mappend (HsImportSet m) (HsImportSet m') =
    HsImportSet $ M.unionWith mergeImportSpecs m m'

  mconcat sets =
    HsImportSet $ M.unionsWith mergeImportSpecs $ map getHsImportSet sets

-- | A literal piece of code that will be inserted into a generated source file
-- after the regular binding glue.  The 'Monoid' instance concatenates code
-- (actions).
data Addendum = Addendum
  { addendumHaskell :: Haskell.Generator ()
    -- ^ Code to be output into the Haskell binding.  May also add imports and
    -- exports.
  }

instance Monoid Addendum where
  mempty = Addendum $ return ()
  mappend (Addendum a) (Addendum b) = Addendum $ a >> b

-- | A typeclass for types that have an addendum.
class HasAddendum a where
  {-# MINIMAL getAddendum, (setAddendum | modifyAddendum) #-}

  -- | Returns an object's addendum.
  getAddendum :: a -> Addendum

  -- | Replaces and object's addendum with another.
  setAddendum :: Addendum -> a -> a
  setAddendum addendum = modifyAddendum $ const addendum

  -- | Modified an object's addendum.
  modifyAddendum :: (Addendum -> Addendum) -> a -> a
  modifyAddendum f x = setAddendum (f $ getAddendum x) x

-- | Adds a Haskell addendum to an object.
addAddendumHaskell :: HasAddendum a => Haskell.Generator () -> a -> a
addAddendumHaskell gen = modifyAddendum $ \addendum ->
  addendum `mappend` mempty { addendumHaskell = gen }

-- | Constructor for an import set.
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

-- | Combines two 'HsImportSpecs's into one that imports everything that the two
-- did separately.
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

-- | Imports "Data.Bits" qualified as @HoppyDB@.
hsImportForBits :: HsImportSet
hsImportForBits = hsQualifiedImport "Data.Bits" "HoppyDB"

-- | Imports "Data.Int" qualified as @HoppyDI@.
hsImportForInt :: HsImportSet
hsImportForInt = hsQualifiedImport "Data.Int" "HoppyDI"

-- | Imports "Data.Word" qualified as @HoppyDW@.
hsImportForWord :: HsImportSet
hsImportForWord = hsQualifiedImport "Data.Word" "HoppyDW"

-- | Imports "Foreign" qualified as @HoppyF@.
hsImportForForeign :: HsImportSet
hsImportForForeign = hsQualifiedImport "Foreign" "HoppyF"

-- | Imports "Foreign.C" qualified as @HoppyFC@.
hsImportForForeignC :: HsImportSet
hsImportForForeignC = hsQualifiedImport "Foreign.C" "HoppyFC"

-- | Imports "Prelude" qualified as @HoppyP@.
hsImportForPrelude :: HsImportSet
hsImportForPrelude = hsQualifiedImport "Prelude" "HoppyP"

-- | Imports "Foreign.Hoppy.Runtime" qualified as @HoppyFHR@.
hsImportForRuntime :: HsImportSet
hsImportForRuntime = hsQualifiedImport "Foreign.Hoppy.Runtime" "HoppyFHR"

-- | Imports "System.Posix.Types" qualified as @HoppySPT@.
hsImportForSystemPosixTypes :: HsImportSet
hsImportForSystemPosixTypes = hsQualifiedImport "System.Posix.Types" "HoppySPT"

-- | Imports "System.IO.Unsafe" qualified as @HoppySIU@.
hsImportForUnsafeIO :: HsImportSet
hsImportForUnsafeIO = hsQualifiedImport "System.IO.Unsafe" "HoppySIU"

-- | Returns an error message indicating that 'TObjToHeap' is used where data is
-- going from a foreign langauge into C++.
tObjToHeapWrongDirectionErrorMsg :: Maybe String -> Class -> String
tObjToHeapWrongDirectionErrorMsg maybeCaller cls =
  concat [maybe "" (++ ": ") maybeCaller,
          "(TObjToHeap ", show cls, ") cannot be passed into C++",
          maybe "" (const ".") maybeCaller]
