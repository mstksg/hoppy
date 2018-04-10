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

{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}

module Foreign.Hoppy.Generator.Spec.Base (
  -- * Interfaces
  Interface,
  ErrorMsg,
  InterfaceOptions (..),
  defaultInterfaceOptions,
  interface,
  interface',
  interfaceName,
  interfaceModules,
  interfaceNamesToModules,
  interfaceHaskellModuleBase,
  interfaceDefaultHaskellModuleBase,
  interfaceAddHaskellModuleBase,
  interfaceHaskellModuleImportNames,
  interfaceExceptionHandlers,
  interfaceCallbacksThrow,
  interfaceSetCallbacksThrow,
  interfaceExceptionClassId,
  interfaceExceptionSupportModule,
  interfaceSetExceptionSupportModule,
  interfaceSetSharedPtr,
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
  moduleExceptionHandlers,
  moduleCallbacksThrow,
  moduleSetCallbacksThrow,
  moduleAddendum,
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
  isValidExtName,
  fromExtName,
  HasExtNames (..),
  getAllExtNames,
  FnName (..),
  IsFnName (..),
  Operator (..),
  OperatorType (..),
  operatorPreferredExtName,
  operatorPreferredExtName',
  operatorType,
  Export (..),
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
  enumValuePrefix, enumSetValuePrefix,
  -- ** Bitspaces
  Bitspace, makeBitspace, bitspaceExtName, bitspaceType, bitspaceValueNames, bitspaceEnum,
  bitspaceAddEnum, bitspaceCppTypeIdentifier, bitspaceFromCppValueFn, bitspaceToCppValueFn,
  bitspaceAddCppType, bitspaceReqs,
  bitspaceValuePrefix, bitspaceSetValuePrefix,
  -- ** Functions
  Purity (..),
  Function, makeFn, fnCName, fnExtName, fnPurity, fnParams, fnReturn, fnReqs, fnExceptionHandlers,
  -- ** Classes
  Class, makeClass, classIdentifier, classExtName, classSuperclasses,
  classEntities, classAddEntities, classVariables, classCtors, classMethods,
  classDtorIsPublic, classSetDtorPrivate,
  classConversion, classReqs, classEntityPrefix, classSetEntityPrefix,
  classIsMonomorphicSuperclass, classSetMonomorphicSuperclass,
  classIsSubclassOfMonomorphic, classSetSubclassOfMonomorphic,
  classIsException, classMakeException,
  ClassEntity (..),
  IsClassEntity (..), classEntityExtName, classEntityForeignName, classEntityForeignName',
  ClassVariable,
  makeClassVariable, makeClassVariable_,
  mkClassVariable, mkClassVariable_,
  mkStaticClassVariable, mkStaticClassVariable_,
  classVarCName, classVarExtName, classVarType, classVarStatic, classVarGettable,
  classVarGetterExtName, classVarGetterForeignName,
  classVarSetterExtName, classVarSetterForeignName,
  Ctor, makeCtor, makeCtor_, mkCtor, mkCtor_, ctorExtName, ctorParams, ctorExceptionHandlers,
  Method,
  MethodImpl (..),
  MethodApplicability (..),
  Constness (..),
  constNegate,
  Staticness (..),
  makeMethod, makeMethod_,
  makeFnMethod, makeFnMethod_,
  mkMethod, mkMethod_, mkMethod', mkMethod'_,
  mkConstMethod, mkConstMethod_, mkConstMethod', mkConstMethod'_,
  mkStaticMethod, mkStaticMethod_, mkStaticMethod', mkStaticMethod'_,
  Prop,  -- The data constructor is private.
  mkProp, mkProp_,
  mkStaticProp, mkStaticProp_,
  mkBoolIsProp, mkBoolIsProp_,
  mkBoolHasProp, mkBoolHasProp_,
  methodImpl, methodExtName, methodApplicability, methodPurity, methodParams,
  methodReturn, methodExceptionHandlers, methodConst, methodStatic,
  -- *** Conversion to and from foreign values
  ClassConversion (..),
  classConversionNone,
  classModifyConversion,
  classSetConversion,
  ClassHaskellConversion (..),
  classHaskellConversionNone,
  classSetHaskellConversion,
  -- ** Callbacks
  Callback, makeCallback,
  callbackExtName, callbackParams, callbackReturn, callbackThrows, callbackReqs,
  callbackSetThrows,
  -- * Exceptions
  ExceptionId (..),
  exceptionCatchAllId,
  ExceptionHandler (..),
  ExceptionHandlers (..),
  HandlesExceptions (getExceptionHandlers),
  handleExceptions,
  -- * Addenda
  Addendum (..),
  HasAddendum (..),
  addAddendumHaskell,
  -- * Haskell imports
  HsModuleName, HsImportSet, HsImportKey (..), HsImportSpecs (..), HsImportName, HsImportVal (..),
  hsWholeModuleImport, hsQualifiedImport, hsImport1, hsImport1', hsImports, hsImports',
  hsImportSetMakeSource,
  -- * Internal to Hoppy
  interfaceAllExceptionClasses,
  interfaceSharedPtr,
  classFindCopyCtor,
  -- ** Haskell imports
  makeHsImportSet,
  getHsImportSet,
  hsImportForBits,
  hsImportForException,
  hsImportForInt,
  hsImportForWord,
  hsImportForForeign,
  hsImportForForeignC,
  hsImportForMap,
  hsImportForPrelude,
  hsImportForRuntime,
  hsImportForSystemPosixTypes,
  hsImportForTypeable,
  hsImportForUnsafeIO,
  -- ** Error messages
  objToHeapTWrongDirectionErrorMsg,
  tToGcInvalidFormErrorMessage,
  toGcTWrongDirectionErrorMsg,
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
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid, mappend, mconcat, mempty)
#endif
import qualified Data.Set as S
import Foreign.Hoppy.Generator.Common
import {-# SOURCE #-} qualified Foreign.Hoppy.Generator.Language.Haskell as Haskell
import Language.Haskell.Syntax (HsType)

-- | Indicates strings that are error messages.
type ErrorMsg = String

-- | A complete specification of a C++ API.  Generators for different languages,
-- including the binding generator for C++, use these to produce their output.
--
-- 'Interface' does not have a 'HandlesExceptions' instance because
-- 'modifyExceptionHandlers' does not work for it (handled exceptions cannot be
-- modified after an 'Interface' is constructed).
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
  , interfaceHaskellModuleImportNames :: M.Map Module String
    -- ^ Short qualified module import names that generated modules use to refer
    -- to each other tersely.
  , interfaceExceptionHandlers :: ExceptionHandlers
    -- ^ Exceptions that all functions in the interface may throw.
  , interfaceCallbacksThrow :: Bool
    -- ^ Whether callbacks within the interface support throwing C++ exceptions
    -- from Haskell into C++ during their execution.  This may be overridden by
    -- 'moduleCallbacksThrow' and 'callbackThrows'.
  , interfaceExceptionNamesToIds :: M.Map ExtName ExceptionId
    -- ^ Maps from external names of exception classes to their exception IDs.
  , interfaceExceptionSupportModule :: Maybe Module
    -- ^ When an interface uses C++ exceptions, then one module needs to
    -- manually be selected to contain some interface-specific runtime support.
    -- This is the selected module.
  , interfaceSharedPtr :: (Reqs, String)
    -- ^ The name of the @shared_ptr@ class to use, and the requirements to use
    -- it.  This defaults to using @std::shared_ptr@ from @<memory>@, but can be
    -- changed if necessary via 'interfaceSetSharedPtr'.
  }

instance Show Interface where
  show iface = concat ["<Interface ", show (interfaceName iface), ">"]

-- | Optional parameters when constructing an 'Interface' with 'interface'.
data InterfaceOptions = InterfaceOptions
  { interfaceOptionsExceptionHandlers :: ExceptionHandlers
  }

-- | Options used by 'interface'.  This contains no exception handlers.
defaultInterfaceOptions :: InterfaceOptions
defaultInterfaceOptions = InterfaceOptions mempty

-- | Constructs an 'Interface' from the required parts.  Some validation is
-- performed; if the resulting interface would be invalid, an error message is
-- returned instead.
--
-- This function passes 'defaultInterfaceOptions' to 'interface''.
interface :: String  -- ^ 'interfaceName'
          -> [Module]  -- ^ 'interfaceModules'
          -> Either ErrorMsg Interface
interface ifName modules = interface' ifName modules defaultInterfaceOptions

-- | Same as 'interface', but accepts some optional arguments.
interface' :: String  -- ^ 'interfaceName'
           -> [Module]  -- ^ 'interfaceModules'
           -> InterfaceOptions
           -> Either ErrorMsg Interface
interface' ifName modules options = do
  -- TODO Check for duplicate module names.
  -- TODO Check for duplicate module file paths.

  -- Check for multiple modules exporting an ExtName.
  let extNamesToModules :: M.Map ExtName [Module]
      extNamesToModules =
        M.unionsWith (++) $
        for modules $ \mod ->
        let extNames = concatMap getAllExtNames $ M.elems $ moduleExports mod
        in M.fromList $ zip extNames $ repeat [mod]

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

  let haskellModuleImportNames =
        M.fromList $
        (\a b f -> zipWith f a b) modules [1..] $
        \mod index -> (mod, 'M' : show index)

  -- Generate a unique exception ID integer for each exception class.  IDs 0 and
  -- 1 are reserved.
  let exceptionNamesToIds =
        M.fromList $
        zip (map classExtName $ interfaceAllExceptionClasses' modules)
            (map ExceptionId [exceptionFirstFreeId..])

  return Interface
    { interfaceName = ifName
    , interfaceModules = M.fromList $ map (moduleName &&& id) modules
    , interfaceNamesToModules = M.map (\[x] -> x) extNamesToModules
    , interfaceHaskellModuleBase' = Nothing
    , interfaceHaskellModuleImportNames = haskellModuleImportNames
    , interfaceExceptionHandlers = interfaceOptionsExceptionHandlers options
    , interfaceCallbacksThrow = False
    , interfaceExceptionNamesToIds = exceptionNamesToIds
    , interfaceExceptionSupportModule = Nothing
    , interfaceSharedPtr = (reqInclude $ includeStd "memory", "std::shared_ptr")
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

-- | Returns the the exception ID for a class in an interface, if it has one
-- (i.e. if it's been marked as an exception class with 'classMakeException').
interfaceExceptionClassId :: Interface -> Class -> Maybe ExceptionId
interfaceExceptionClassId iface cls =
  M.lookup (classExtName cls) $ interfaceExceptionNamesToIds iface

-- | Returns all of the exception classes in an interface.
interfaceAllExceptionClasses :: Interface -> [Class]
interfaceAllExceptionClasses = interfaceAllExceptionClasses' . M.elems . interfaceModules

interfaceAllExceptionClasses' :: [Module] -> [Class]
interfaceAllExceptionClasses' modules =
  flip concatMap modules $ \mod ->
  catMaybes $
  for (M.elems $ moduleExports mod) $ \export -> case export of
    ExportClass cls | classIsException cls -> Just cls
    _ -> Nothing

-- | Changes 'callbackThrows' for all callbacks in an interface that don't have it
-- set explicitly at the module or callback level.
interfaceSetCallbacksThrow :: Bool -> Interface -> Interface
interfaceSetCallbacksThrow b iface = iface { interfaceCallbacksThrow = b }

-- | Sets an interface's exception support module, for interfaces that use
-- exceptions.
interfaceSetExceptionSupportModule :: Module -> Interface -> Interface
interfaceSetExceptionSupportModule mod iface = case interfaceExceptionSupportModule iface of
  Nothing -> iface { interfaceExceptionSupportModule = Just mod }
  Just existingMod ->
    if mod == existingMod
    then iface
    else error $ "interfaceSetExceptionSupportModule: " ++ show iface ++
         " already has exception support module " ++ show existingMod ++
         ", trying to set " ++ show mod ++ "."

-- | Installs a custom @std::shared_ptr@ implementation for use by an interface.
-- Hoppy uses shared pointers for generated callback code.  This function is
-- useful for building code with compilers that don't provide a conforming
-- @std::shared_ptr@ implementation.
--
-- @interfaceSetSharedPtr ident reqs iface@ modifies @iface@ to use as a
-- @shared_ptr@ class the C++ identifier @ident@, which needs @reqs@ in order to
-- be accessed.  @ident@ should be the name of a template to which an arbitrary
-- @\<T\>@ can be appended, for example @"std::shared_ptr"@.
--
-- A @shared_ptr\<T\>@ implementation @foo@ must at least provide the following
-- interface:
--
-- > foo();  // Initialization with a null pointer.
-- > foo(T*);  // Initialization with a given pointer.
-- > foo(const foo&);  // Copy-construction.
-- > T& operator*() const;  // Dereferencing (when non-null).
-- > T* operator->() const;  // Dereferencing and invocation (when non-null).
-- > explicit operator bool() const;  // Is the target object null?
interfaceSetSharedPtr :: String -> Reqs -> Interface -> Interface
interfaceSetSharedPtr identifier reqs iface =
  iface { interfaceSharedPtr = (reqs, identifier) }

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
-- level (via the @'HasReqs' 'Module'@ instance).  Dependencies between modules
-- are handled automatically, and circularity is supported to a certain extent.
-- See the documentation for the individual language modules for further
-- details.
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
  , moduleExceptionHandlers :: ExceptionHandlers
    -- ^ Exceptions that all functions in the module may throw.
  , moduleCallbacksThrow :: Maybe Bool
    -- ^ Whether callbacks exported from the module support exceptions being
    -- thrown during their execution.  When present, this overrides
    -- 'interfaceCallbacksThrow'.  This maybe overridden by 'callbackThrows'.
  , moduleAddendum :: Addendum
    -- ^ The module's addendum.
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

instance HasAddendum Module where
  getAddendum = moduleAddendum
  setAddendum addendum m = m { moduleAddendum = addendum }

instance HandlesExceptions Module where
  getExceptionHandlers = moduleExceptionHandlers
  modifyExceptionHandlers f m = m { moduleExceptionHandlers = f $ moduleExceptionHandlers m }

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
  , moduleExceptionHandlers = mempty
  , moduleCallbacksThrow = Nothing
  , moduleAddendum = mempty
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
      newExports = M.fromList $ map (getPrimaryExtName &&& id) exports
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

-- | Changes 'callbackThrows' for all callbacks in a module that don't have it
-- set explicitly.
moduleSetCallbacksThrow :: MonadState Module m => Maybe Bool -> m ()
moduleSetCallbacksThrow b = modify $ \m -> m { moduleCallbacksThrow = b }

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

-- | Contains the data types for bindings to C++ entities: 'Function', 'Class',
-- etc.  Use 'addReqs' or 'addReqIncludes' to specify requirements for these
-- entities, e.g. header files that must be included in order to access the
-- underlying entities that are being bound.

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
  } deriving (Eq, Monoid, Ord)

instance Show ExtName where
  show extName = concat ["$\"", fromExtName extName, "\"$"]

-- | Creates an 'ExtName' that contains the given string, erroring if the string
-- is an invalid 'ExtName'.
toExtName :: String -> ExtName
toExtName str = case str of
  -- Keep this logic in sync with isValidExtName.
  [] -> error "An ExtName cannot be empty."
  _ -> if isValidExtName str
       then ExtName str
       else error $
            "An ExtName must start with a letter and only contain letters, numbers, and '_': " ++
            show str

-- | Returns true if the given string is represents a valid 'ExtName'.
isValidExtName :: String -> Bool
isValidExtName str = case str of
  -- Keep this logic in sync with toExtName.
  [] -> False
  c:cs -> isAlpha c && all ((||) <$> isAlphaNum <*> (== '_')) cs

-- | Generates an 'ExtName' from an 'Identifier', if the given name is absent.
extNameOrIdentifier :: Identifier -> Maybe ExtName -> ExtName
extNameOrIdentifier ident = fromMaybe $ case identifierParts ident of
  [] -> error "extNameOrIdentifier: Invalid empty identifier."
  parts -> toExtName $ idPartBase $ last parts

-- | Generates an 'ExtName' from an @'FnName' 'Identifier'@, if the given name
-- is absent.
extNameOrFnIdentifier :: FnName Identifier -> Maybe ExtName -> ExtName
extNameOrFnIdentifier name =
  fromMaybe $ case name of
    FnName identifier -> case identifierParts identifier of
      [] -> error "extNameOrFnIdentifier: Empty idenfitier."
      parts -> toExtName $ idPartBase $ last parts
    FnOp op -> operatorPreferredExtName op

-- | Generates an 'ExtName' from a string, if the given name is absent.
extNameOrString :: String -> Maybe ExtName -> ExtName
extNameOrString str = fromMaybe $ toExtName str

-- | Types that have an external name, and also optionally have nested entities
-- with external names as well.  See 'getAllExtNames'.
class HasExtNames a where
  -- | Returns the external name by which a given entity is referenced.
  getPrimaryExtName :: a -> ExtName

  -- | Returns external names nested within the given entity.  Does not include
  -- the primary external name.
  getNestedExtNames :: a -> [ExtName]
  getNestedExtNames _ = []

-- | Returns a list of all of the external names an entity contains.  This
-- combines both 'getPrimaryExtName' and 'getNestedExtNames'.
getAllExtNames :: HasExtNames a => a -> [ExtName]
getAllExtNames x = getPrimaryExtName x : getNestedExtNames x

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

instance HasExtNames Export where
  getPrimaryExtName x = case x of
    ExportVariable v -> getPrimaryExtName v
    ExportEnum e -> getPrimaryExtName e
    ExportBitspace b -> getPrimaryExtName b
    ExportFn f -> getPrimaryExtName f
    ExportClass cls -> getPrimaryExtName cls
    ExportCallback cb -> getPrimaryExtName cb

  getNestedExtNames x = case x of
    ExportVariable v -> getNestedExtNames v
    ExportEnum e -> getNestedExtNames e
    ExportBitspace b -> getNestedExtNames b
    ExportFn f -> getNestedExtNames f
    ExportClass cls -> getNestedExtNames cls
    ExportCallback cb -> getNestedExtNames cb

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

-- | A concrete C++ type.  Use the bindings in "Foreign.Hoppy.Generator.Types"
-- for values of this type; these data constructors are subject to change
-- without notice.
data Type =
    Internal_TVoid
  | Internal_TBool
  | Internal_TChar
  | Internal_TUChar
  | Internal_TShort
  | Internal_TUShort
  | Internal_TInt
  | Internal_TUInt
  | Internal_TLong
  | Internal_TULong
  | Internal_TLLong
  | Internal_TULLong
  | Internal_TFloat
  | Internal_TDouble
  | Internal_TInt8
  | Internal_TInt16
  | Internal_TInt32
  | Internal_TInt64
  | Internal_TWord8
  | Internal_TWord16
  | Internal_TWord32
  | Internal_TWord64
  | Internal_TPtrdiff
  | Internal_TSize
  | Internal_TSSize
  | Internal_TEnum CppEnum
  | Internal_TBitspace Bitspace
  | Internal_TPtr Type
  | Internal_TRef Type
  | Internal_TFn [Type] Type
  | Internal_TCallback Callback
  | Internal_TObj Class
  | Internal_TObjToHeap Class
  | Internal_TToGc Type
  | Internal_TConst Type
  deriving (Eq, Show)

-- | Canonicalizes a 'Type' without changing its meaning.  Multiple nested
-- 'Internal_TConst's are collapsed into a single one.
normalizeType :: Type -> Type
normalizeType t = case t of
  Internal_TVoid -> t
  Internal_TBool -> t
  Internal_TChar -> t
  Internal_TUChar -> t
  Internal_TShort -> t
  Internal_TUShort -> t
  Internal_TInt -> t
  Internal_TUInt -> t
  Internal_TLong -> t
  Internal_TULong -> t
  Internal_TLLong -> t
  Internal_TULLong -> t
  Internal_TFloat -> t
  Internal_TDouble -> t
  Internal_TInt8 -> t
  Internal_TInt16 -> t
  Internal_TInt32 -> t
  Internal_TInt64 -> t
  Internal_TWord8 -> t
  Internal_TWord16 -> t
  Internal_TWord32 -> t
  Internal_TWord64 -> t
  Internal_TPtrdiff -> t
  Internal_TSize -> t
  Internal_TSSize -> t
  Internal_TEnum _ -> t
  Internal_TBitspace _ -> t
  Internal_TPtr t' -> Internal_TPtr $ normalizeType t'
  Internal_TRef t' -> Internal_TRef $ normalizeType t'
  Internal_TFn paramTypes retType ->
    Internal_TFn (map normalizeType paramTypes) $ normalizeType retType
  Internal_TCallback _ -> t
  Internal_TObj _ -> t
  Internal_TObjToHeap _ -> t
  Internal_TToGc _ -> t
  Internal_TConst (Internal_TConst t') -> normalizeType $ Internal_TConst t'
  Internal_TConst _ -> t

-- | Strips leading 'Internal_TConst's off of a type.
stripConst :: Type -> Type
stripConst t = case t of
  Internal_TConst t' -> stripConst t'
  _ -> t

-- | A C++ variable.
--
-- Use this data type's 'HasReqs' instance to make the variable accessible.
data Variable = Variable
  { varIdentifier :: Identifier
    -- ^ The identifier used to refer to the variable.
  , varExtName :: ExtName
    -- ^ The variable's external name.
  , varType :: Type
    -- ^ The variable's type.  This may be
    -- 'Foreign.Hoppy.Generator.Types.constT' to indicate that the variable is
    -- read-only.
  , varReqs :: Reqs
    -- ^ Requirements for bindings to access this variable.
  , varAddendum :: Addendum
    -- ^ The variable's addendum.
  }

instance Eq Variable where
  (==) = (==) `on` varExtName

instance Show Variable where
  show v = concat ["<Variable ", show (varExtName v), " ", show (varType v), ">"]

instance HasExtNames Variable where
  getPrimaryExtName = varExtName
  getNestedExtNames v = [varGetterExtName v, varSetterExtName v]

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
-- @'Foreign.Hoppy.Generator.Types.constT' ...@.
varIsConst :: Variable -> Bool
varIsConst v = case varType v of
  Internal_TConst _ -> True
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
    -- ^ Requirements for bindings to access this enum.  Currently unused, but
    -- will be in the future.
  , enumAddendum :: Addendum
    -- ^ The enum's addendum.
  , enumValuePrefix :: String
    -- ^ The prefix applied to value names ('enumValueNames') when determining
    -- the names of values in foreign languages.  This defaults to the external
    -- name of the enum, plus an underscore.
    --
    -- See 'enumSetValuePrefix'.
  }

instance Eq CppEnum where
  (==) = (==) `on` enumExtName

instance Show CppEnum where
  show e = concat ["<Enum ", show (enumExtName e), " ", show (enumIdentifier e), ">"]

instance HasExtNames CppEnum where
  getPrimaryExtName = enumExtName

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
  let extName = extNameOrIdentifier identifier maybeExtName
  in CppEnum
     identifier
     extName
     valueNames
     mempty
     mempty
     (fromExtName extName ++ "_")

-- | Sets the prefix applied to the names of enum values' identifiers in foreign
-- languages.
--
-- See 'enumValuePrefix'.
enumSetValuePrefix :: String -> CppEnum -> CppEnum
enumSetValuePrefix prefix enum = enum { enumValuePrefix = prefix }

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
    -- type, usually 'Foreign.Hoppy.Generator.Types.intT'.
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
  , bitspaceValuePrefix :: String
    -- ^ The prefix applied to value names ('bitspaceValueNames') when
    -- determining the names of values in foreign languages.  This defaults to
    -- the external name of the bitspace, plus an underscore.
    --
    -- See 'bitspaceSetValuePrefix'.
  }

instance Eq Bitspace where
  (==) = (==) `on` bitspaceExtName

instance Show Bitspace where
  show e = concat ["<Bitspace ", show (bitspaceExtName e), " ", show (bitspaceType e), ">"]

instance HasExtNames Bitspace where
  getPrimaryExtName = bitspaceExtName

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
  (fromExtName extName ++ "_")

-- | Sets the prefix applied to the names of enum values' identifiers in foreign
-- languages.
--
-- See 'enumValuePrefix'.
bitspaceSetValuePrefix :: String -> Bitspace -> Bitspace
bitspaceSetValuePrefix prefix bitspace = bitspace { bitspaceValuePrefix = prefix }

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
--
-- Use this data type's 'HasReqs' instance to make the function accessible.  You
-- do not need to add requirements for parameter or return types.
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
    -- ^ Requirements for bindings to access this function.
  , fnExceptionHandlers :: ExceptionHandlers
    -- ^ Exceptions that the function might throw.
  , fnAddendum :: Addendum
    -- ^ The function's addendum.
  }

instance Show Function where
  show fn =
    concat ["<Function ", show (fnExtName fn), " ", show (fnCName fn),
            show (fnParams fn), " ", show (fnReturn fn), ">"]

instance HasExtNames Function where
  getPrimaryExtName = fnExtName

instance HasReqs Function where
  getReqs = fnReqs
  setReqs reqs fn = fn { fnReqs = reqs }

instance HandlesExceptions Function where
  getExceptionHandlers = fnExceptionHandlers
  modifyExceptionHandlers f fn = fn { fnExceptionHandlers = f $ fnExceptionHandlers fn }

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
              purity paramTypes retType mempty mempty mempty

-- | A C++ class declaration.  See 'IsClassEntity' for more information about the
-- interaction between a class's names and the names of entities within the
-- class.
--
-- Use this data type's 'HasReqs' instance to make the class accessible.  You do
-- not need to add requirements for methods' parameter or return types.
data Class = Class
  { classIdentifier :: Identifier
    -- ^ The identifier used to refer to the class.
  , classExtName :: ExtName
    -- ^ The class's external name.
  , classSuperclasses :: [Class]
    -- ^ The class's public superclasses.
  , classEntities :: [ClassEntity]
    -- ^ The class's entities.
  , classDtorIsPublic :: Bool
    -- ^ The class's methods.
  , classConversion :: ClassConversion
    -- ^ Behaviour for converting objects to and from foriegn values.
  , classReqs :: Reqs
    -- ^ Requirements for bindings to access this class.
  , classAddendum :: Addendum
    -- ^ The class's addendum.
  , classIsMonomorphicSuperclass :: Bool
    -- ^ This is true for classes passed through
    -- 'classSetMonomorphicSuperclass'.
  , classIsSubclassOfMonomorphic :: Bool
    -- ^ This is true for classes passed through
    -- 'classSetSubclassOfMonomorphic'.
  , classIsException :: Bool
    -- ^ Whether to support using the class as a C++ exception.
  , classEntityPrefix :: String
    -- ^ The prefix applied to the external names of entities (methods, etc.)
    -- within this class when determining the names of foreign languages'
    -- corresponding bindings.  This defaults to the external name of the class,
    -- plus an underscore.  Changing this allows you to potentially have
    -- entities with the same foreign name in separate modules.  This may be the
    -- empty string, in which case the foreign name will simply be the external
    -- name of the entity.
    --
    -- This does __not__ affect the things' external names themselves; external
    -- names must still be unique in an interface.  For instance, a method with
    -- external name @bar@ in a class with external name @Flab@ and prefix
    -- @Flob_@ will use the effective external name @Flab_bar@, but the
    -- generated name in say Haskell would be @Flob_bar@.
    --
    -- See 'IsClassEntity' and 'classSetEntityPrefix'.
  }

instance Eq Class where
  (==) = (==) `on` classExtName

instance Ord Class where
  compare = compare `on` classExtName

instance Show Class where
  show cls =
    concat ["<Class ", show (classExtName cls), " ", show (classIdentifier cls), ">"]

instance HasExtNames Class where
  getPrimaryExtName = classExtName

  getNestedExtNames cls = concatMap (classEntityExtNames cls) $ classEntities cls

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
          -- identifier if absent by dropping leading namespaces, and taking the
          -- last component (sans template arguments).
          -> [Class]  -- ^ Superclasses.
          -> [ClassEntity]
          -> Class
makeClass identifier maybeExtName supers entities =
  let extName = extNameOrIdentifier identifier maybeExtName
  in Class
     { classIdentifier = identifier
     , classExtName = extName
     , classSuperclasses = supers
     , classEntities = entities
     , classDtorIsPublic = True
     , classConversion = classConversionNone
     , classReqs = mempty
     , classAddendum = mempty
     , classIsMonomorphicSuperclass = False
     , classIsSubclassOfMonomorphic = False
     , classIsException = False
     , classEntityPrefix = fromExtName extName ++ "_"
     }

-- | Sets the prefix applied to foreign languages' entities generated from
-- methods, etc. within the class.
--
-- See 'IsClassEntity' and 'classEntityPrefix'.
classSetEntityPrefix :: String -> Class -> Class
classSetEntityPrefix prefix cls = cls { classEntityPrefix = prefix }

-- | Adds constructors to a class.
classAddEntities :: [ClassEntity] -> Class -> Class
classAddEntities ents cls =
  if null ents then cls else cls { classEntities = classEntities cls ++ ents }

-- | Returns all of the class's variables.
classVariables :: Class -> [ClassVariable]
classVariables = mapMaybe pickVar . classEntities
  where pickVar ent = case ent of
          CEVar v -> Just v
          CECtor _ -> Nothing
          CEMethod _ -> Nothing
          CEProp _ -> Nothing

-- | Returns all of the class's constructors.
classCtors :: Class -> [Ctor]
classCtors = mapMaybe pickCtor . classEntities
  where pickCtor ent = case ent of
          CEVar _ -> Nothing
          CECtor ctor -> Just ctor
          CEMethod _ -> Nothing
          CEProp _ -> Nothing

-- | Returns all of the class's methods, including methods generated from
-- 'Prop's.
classMethods :: Class -> [Method]
classMethods = concatMap pickMethods . classEntities
  where pickMethods ent = case ent of
          CEVar _ -> []
          CECtor _ -> []
          CEMethod m -> [m]
          CEProp (Prop ms) -> ms

-- | Marks a class's destructor as private, so that a binding for it won't be
-- generated.
classSetDtorPrivate :: Class -> Class
classSetDtorPrivate cls = cls { classDtorIsPublic = False }

-- | Explicitly marks a class as being monomorphic (i.e. not having any
-- virtual methods or destructors).  By default, Hoppy assumes that a class that
-- is derived is also polymorphic, but it can happen that this is not the case.
-- Downcasting with @dynamic_cast@ from such classes is not available.  See also
-- 'classSetSubclassOfMonomorphic'.
classSetMonomorphicSuperclass :: Class -> Class
classSetMonomorphicSuperclass cls = cls { classIsMonomorphicSuperclass = True }

-- | Marks a class as being derived from some monomorphic superclass.  This
-- prevents any downcasting to this class.  Generally it is better to use
-- 'classSetMonomorphicSuperclass' on the specific superclasses that are
-- monomorphic, but in cases where this is not possible, this function can be
-- applied to the subclass instead.
classSetSubclassOfMonomorphic :: Class -> Class
classSetSubclassOfMonomorphic cls = cls { classIsSubclassOfMonomorphic = True }

-- | Marks a class as being used as an exception.  This makes the class
-- throwable and catchable.
classMakeException :: Class -> Class
classMakeException cls = case classIsException cls of
  False -> cls { classIsException = True }
  True -> cls

-- | Separately from passing object handles between C++ and foreign languages,
-- objects can also be made to implicitly convert to native values in foreign
-- languages.  A single such type may be associated with any C++ class for each
-- foreign language.  The foreign type and the conversion process in each
-- direction are specified using this object.  Converting a C++ object to a
-- foreign value is also called decoding, and vice versa is called encoding.  A
-- class may be convertible in one direction and not the other.
--
-- To use these implicit conversions, instead of specifying an object handle
-- type such as
-- @'Foreign.Hoppy.Generator.Types.ptrT' . 'Foreign.Hoppy.Generator.Types.objT'@
-- or
-- @'Foreign.Hoppy.Generator.Types.refT' . 'Foreign.Hoppy.Generator.Types.objT'@,
-- use 'Foreign.Hoppy.Generator.Types.objT' directly.
--
-- The subfields in this object specify how to do conversions between C++ and
-- foreign languages.
data ClassConversion = ClassConversion
  { classHaskellConversion :: ClassHaskellConversion
    -- ^ Conversions to and from Haskell.

    -- NOTE!  When adding new languages here, add the language to
    -- 'classSetConversionToHeap', and 'classSetConversionToGc' as well if the
    -- language supports garbage collection.
  }

-- | Conversion behaviour for a class that is not convertible.
classConversionNone :: ClassConversion
classConversionNone = ClassConversion classHaskellConversionNone

-- | Modifies a class's 'ClassConversion' structure with a given function.
classModifyConversion :: (ClassConversion -> ClassConversion) -> Class -> Class
classModifyConversion f cls =
  let cls' = cls { classConversion = f $ classConversion cls }
      conv = classConversion cls'
      haskellConv = classHaskellConversion conv
  in case undefined of
    _ | (isJust (classHaskellConversionToCppFn haskellConv) ||
         isJust (classHaskellConversionFromCppFn haskellConv)) &&
        isNothing (classHaskellConversionType haskellConv) ->
      error $ "classModifyConversion: " ++ show cls' ++
      " was given a Haskell-to-C++ or C++-to-Haskell conversion function" ++
      " but no Haskell type.  Please provide a classHaskellConversionType."
    _ -> cls'

-- | Replaces a class's 'ClassConversion' structure.
classSetConversion :: ClassConversion -> Class -> Class
classSetConversion c = classModifyConversion $ const c

-- | Controls how conversions between C++ objects and Haskell values happen in
-- Haskell bindings.
data ClassHaskellConversion = ClassHaskellConversion
  { classHaskellConversionType :: Maybe (Haskell.Generator HsType)
    -- ^ Produces the Haskell type that represents a value of the corresponding
    -- C++ class.  This generator may add imports, but must not output code or
    -- add exports.
  , classHaskellConversionToCppFn :: Maybe (Haskell.Generator ())
    -- ^ Produces a Haskell expression that evaluates to a function that takes
    -- an value of the type that 'classHaskellConversionType' generates, and
    -- returns a non-const handle for a new C++ object in IO.  The generator
    -- must output code and may add imports, but must not add exports.
    --
    -- If this field is present, then 'classHaskellConversionType' must also be
    -- present.
  , classHaskellConversionFromCppFn :: Maybe (Haskell.Generator ())
    -- ^ Produces a Haskell expression that evaluates to a function that takes a
    -- const handle for a C++ object, and returns a value of the type that
    -- 'classHaskellConversionType' generates, in IO.  It should not delete the
    -- handle.  The generator must output code and may add imports, but must not
    -- add exports.
    --
    -- If this field is present, then 'classHaskellConversionType' must also be
    -- present.
  }

-- | Conversion behaviour for a class that is not convertible to or from
-- Haskell.
classHaskellConversionNone :: ClassHaskellConversion
classHaskellConversionNone =
  ClassHaskellConversion
  { classHaskellConversionType = Nothing
  , classHaskellConversionToCppFn = Nothing
  , classHaskellConversionFromCppFn = Nothing
  }

-- | Replaces a class's 'classHaskellConversion' with a given value.
classSetHaskellConversion :: ClassHaskellConversion -> Class -> Class
classSetHaskellConversion conv = classModifyConversion $ \c ->
  c { classHaskellConversion = conv }

-- | Things that live inside of a class, and have the class's external name
-- prepended to their own in generated code.  With an external name of @\"bar\"@
-- and a class with external name @\"foo\"@, the resulting name will be
-- @\"foo_bar\"@.
--
-- See 'classEntityPrefix' and 'classSetEntityPrefix'.
class IsClassEntity a where
  -- | Extracts the external name of the object, without the class name added.
  classEntityExtNameSuffix :: a -> ExtName

-- | Computes the external name to use in generated code, containing both the
-- class's and object's external names.  This is the concatenation of the
-- class's and entity's external names, separated by an underscore.
classEntityExtName :: IsClassEntity a => Class -> a -> ExtName
classEntityExtName cls x =
  toExtName $ fromExtName (classExtName cls) ++ "_" ++ fromExtName (classEntityExtNameSuffix x)

-- | Computes the name under which a class entity is to be exposed in foreign
-- languages.  This is the concatenation of a class's entity prefix, and the
-- external name of the entity.
classEntityForeignName :: IsClassEntity a => Class -> a -> ExtName
classEntityForeignName cls x =
  classEntityForeignName' cls $ classEntityExtNameSuffix x

-- | Computes the name under which a class entity is to be exposed in foreign
-- languages, given a class and an entity's external name.  The result is the
-- concatenation of a class's entity prefix, and the external name of the
-- entity.
classEntityForeignName' :: Class -> ExtName -> ExtName
classEntityForeignName' cls extName =
  toExtName $ classEntityPrefix cls ++ fromExtName extName

-- | A C++ entity that belongs to a class.
data ClassEntity =
    CEVar ClassVariable
  | CECtor Ctor
  | CEMethod Method
  | CEProp Prop

-- | Returns all of the names in a 'ClassEntity' within the corresponding
-- 'Class'.
classEntityExtNames :: Class -> ClassEntity -> [ExtName]
classEntityExtNames cls ent = case ent of
  CEVar v -> [classEntityExtName cls v]
  CECtor ctor -> [classEntityExtName cls ctor]
  CEMethod m -> [classEntityExtName cls m]
  CEProp (Prop methods) -> map (classEntityExtName cls) methods

-- | A C++ member variable.
data ClassVariable = ClassVariable
  { classVarCName :: String
    -- ^ The variable's C++ name.
  , classVarExtName :: ExtName
    -- ^ The variable's external name.
  , classVarType :: Type
    -- ^ The variable's type.  This may be
    -- 'Foreign.Hoppy.Generator.Types.constT' to indicate that the variable is
    -- read-only.
  , classVarStatic :: Staticness
    -- ^ Whether the variable is static (i.e. whether it exists once in the
    -- class itself and not in each instance).
  , classVarGettable :: Bool
    -- ^ Whether the variable should have an accompanying getter. Note this
    -- exists only for disabling getters on callback variables - as there is
    -- currently no functionality to pass callbacks out of c++
  }

instance Show ClassVariable where
  show v =
    concat ["<ClassVariable ",
            show $ classVarCName v, " ",
            show $ classVarExtName v, " ",
            show $ classVarStatic v, " ",
            show $ classVarType v, ">"]

instance IsClassEntity ClassVariable where
  classEntityExtNameSuffix = classVarExtName

-- | Creates a 'ClassVariable' with full generality and manual name specification.
--
-- The result is wrapped in a 'CEVar'.  For an unwrapped value, use
-- 'makeClassVariable_'.
makeClassVariable :: String -> Maybe ExtName -> Type -> Staticness -> Bool -> ClassEntity
makeClassVariable cName maybeExtName tp static gettable =
  CEVar $ makeClassVariable_ cName maybeExtName tp static gettable

-- | The unwrapped version of 'makeClassVariable'.
makeClassVariable_ :: String -> Maybe ExtName -> Type -> Staticness -> Bool -> ClassVariable
makeClassVariable_ cName maybeExtName =
  ClassVariable cName $ extNameOrString cName maybeExtName

-- | Creates a 'ClassVariable' for a nonstatic class variable for
-- @class::varName@ whose external name is @class_varName@.
--
-- The result is wrapped in a 'CEVar'.  For an unwrapped value, use
-- 'mkClassVariable_'.
mkClassVariable :: String -> Type -> ClassEntity
mkClassVariable = (CEVar .) . mkClassVariable_

-- | The unwrapped version of 'mkClassVariable'.
mkClassVariable_ :: String -> Type -> ClassVariable
mkClassVariable_ cName t = makeClassVariable_ cName Nothing t Nonstatic True

-- | Same as 'mkClassVariable', but returns a static variable instead.
--
-- The result is wrapped in a 'CEVar'.  For an unwrapped value, use
-- 'mkStaticClassVariable_'.
mkStaticClassVariable :: String -> Type -> ClassEntity
mkStaticClassVariable = (CEVar .) . mkStaticClassVariable_

-- | The unwrapped version of 'mkStaticClassVariable'.
mkStaticClassVariable_ :: String -> Type -> ClassVariable
mkStaticClassVariable_ cName t = makeClassVariable_ cName Nothing t Static True

-- | Returns the external name of the getter function for the class variable.
classVarGetterExtName :: Class -> ClassVariable -> ExtName
classVarGetterExtName cls v =
  toExtName $ fromExtName (classEntityExtName cls v) ++ "_get"

-- | Returns the foreign name of the getter function for the class variable.
classVarGetterForeignName :: Class -> ClassVariable -> ExtName
classVarGetterForeignName cls v =
  toExtName $ fromExtName (classEntityForeignName cls v) ++ "_get"

-- | Returns the external name of the setter function for the class variable.
classVarSetterExtName :: Class -> ClassVariable -> ExtName
classVarSetterExtName cls v =
  toExtName $ fromExtName (classEntityExtName cls v) ++ "_set"

-- | Returns the foreign name of the setter function for the class variable.
classVarSetterForeignName :: Class -> ClassVariable -> ExtName
classVarSetterForeignName cls v =
  toExtName $ fromExtName (classEntityForeignName cls v) ++ "_set"

-- | A C++ class constructor declaration.
data Ctor = Ctor
  { ctorExtName :: ExtName
    -- ^ The constructor's external name.
  , ctorParams :: [Type]
    -- ^ The constructor's parameter types.
  , ctorExceptionHandlers :: ExceptionHandlers
    -- ^ Exceptions that the constructor may throw.
  }

instance Show Ctor where
  show ctor = concat ["<Ctor ", show (ctorExtName ctor), " ", show (ctorParams ctor), ">"]

instance HandlesExceptions Ctor where
  getExceptionHandlers = ctorExceptionHandlers
  modifyExceptionHandlers f ctor = ctor { ctorExceptionHandlers = f $ ctorExceptionHandlers ctor }

instance IsClassEntity Ctor where
  classEntityExtNameSuffix = ctorExtName

-- | Creates a 'Ctor' with full generality.
--
-- The result is wrapped in a 'CECtor'.  For an unwrapped value, use
-- 'makeCtor_'.
makeCtor :: ExtName
         -> [Type]  -- ^ Parameter types.
         -> ClassEntity
makeCtor = (CECtor .) . makeCtor_

-- | The unwrapped version of 'makeCtor'.
makeCtor_ :: ExtName -> [Type] -> Ctor
makeCtor_ extName paramTypes = Ctor extName paramTypes mempty

-- | @mkCtor name@ creates a 'Ctor' whose external name is @className_name@.
--
-- The result is wrapped in a 'CECtor'.  For an unwrapped value, use
-- 'makeCtor_'.
mkCtor :: String
       -> [Type]  -- ^ Parameter types.
       -> ClassEntity
mkCtor = (CECtor .) . mkCtor_

-- | The unwrapped version of 'mkCtor'.
mkCtor_ :: String -> [Type] -> Ctor
mkCtor_ = makeCtor_ . toExtName

-- | Searches a class for a copy constructor, returning it if found.
classFindCopyCtor :: Class -> Maybe Ctor
classFindCopyCtor cls = case mapMaybe check $ classEntities cls of
  [ctor] -> Just ctor
  _ -> Nothing
  where check entity = case entity of
          CECtor ctor ->
            let params = map (stripConst . normalizeType) (ctorParams ctor)
            in if params == [Internal_TObj cls] ||
                  params == [Internal_TRef $ Internal_TConst $ Internal_TObj cls]
            then Just ctor
            else Nothing
          _ -> Nothing

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
  , methodExceptionHandlers :: ExceptionHandlers
    -- ^ Exceptions that the method might throw.
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

instance HandlesExceptions Method where
  getExceptionHandlers = methodExceptionHandlers

  modifyExceptionHandlers f method =
    method { methodExceptionHandlers = f $ methodExceptionHandlers method }

instance IsClassEntity Method where
  classEntityExtNameSuffix = methodExtName

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
                         deriving (Bounded, Enum, Eq, Show)

-- | Whether or not a method is const.
data Constness = Nonconst | Const
               deriving (Bounded, Enum, Eq, Show)

-- | Returns the opposite constness value.
constNegate :: Constness -> Constness
constNegate Nonconst = Const
constNegate Const = Nonconst

-- | Whether or not a method is static.
data Staticness = Nonstatic | Static
               deriving (Bounded, Enum, Eq, Show)

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
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'makeMethod_'.
makeMethod :: IsFnName String name
           => name  -- ^ The C++ name of the method.
           -> ExtName  -- ^ The external name of the method.
           -> MethodApplicability
           -> Purity
           -> [Type]  -- ^ Parameter types.
           -> Type  -- ^ Return type.
           -> ClassEntity
makeMethod = (((((CEMethod .) .) .) .) .) . makeMethod_

-- | The unwrapped version of 'makeMethod'.
makeMethod_ :: IsFnName String name
            => name
            -> ExtName
            -> MethodApplicability
            -> Purity
            -> [Type]
            -> Type
            -> Method
makeMethod_ cName extName appl purity paramTypes retType =
  Method (RealMethod $ toFnName cName) extName appl purity paramTypes retType mempty

-- | Creates a 'Method' that is in fact backed by a C++ non-member function (a
-- la 'makeFn'), but appears to be a regular method.  This is useful for
-- wrapping a method on the C++ side when its arguments aren't right for binding
-- directly.
--
-- A @this@ pointer parameter is __not__ automatically added to the parameter
-- list for non-static methods created with @makeFnMethod@.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'makeFnMethod_'.
makeFnMethod :: IsFnName Identifier name
             => name
             -> String
             -> MethodApplicability
             -> Purity
             -> [Type]
             -> Type
             -> ClassEntity
makeFnMethod = (((((CEMethod .) .) .) .) .) . makeFnMethod_

-- | The unwrapped version of 'makeFnMethod'.
makeFnMethod_ :: IsFnName Identifier name
              => name
              -> String
              -> MethodApplicability
              -> Purity
              -> [Type]
              -> Type
              -> Method
makeFnMethod_ cName foreignName appl purity paramTypes retType =
  Method (FnMethod $ toFnName cName) (toExtName foreignName)
         appl purity paramTypes retType mempty

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
  in makeMethod_ name extName appl purity paramTypes retType

-- | Creates a nonconst, nonstatic 'Method' for @class::methodName@ and whose
-- external name is @class_methodName@.  If the name is an operator, then the
-- 'operatorPreferredExtName' will be used in the external name.
--
-- For creating multiple bindings to a method, see 'mkMethod''.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'mkMethod_'.
mkMethod :: IsFnName String name
         => name  -- ^ The C++ name of the method.
         -> [Type]  -- ^ Parameter types.
         -> Type  -- ^ Return type.
         -> ClassEntity
mkMethod = ((CEMethod .) .) . mkMethod_

-- | The unwrapped version of 'mkMethod'.
mkMethod_ :: IsFnName String name
          => name
          -> [Type]
          -> Type
          -> Method
mkMethod_ name = makeMethod' name MNormal Nonpure

-- | Creates a nonconst, nonstatic 'Method' for method @class::methodName@ and
-- whose external name is @class_methodName@.  This enables multiple 'Method's
-- with different foreign names (and hence different external names) to bind to
-- the same method, e.g. to make use of optional arguments or overloading.  See
-- 'mkMethod' for a simpler form.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'mkMethod'_'.
mkMethod' :: IsFnName String name
          => name  -- ^ The C++ name of the method.
          -> String  -- ^ A foreign name for the method.
          -> [Type]  -- ^ Parameter types.
          -> Type  -- ^ Return type.
          -> ClassEntity
mkMethod' = (((CEMethod .) .) .) . mkMethod'_

-- | The unwrapped version of 'mkMethod''.
mkMethod'_ :: IsFnName String name
           => name
           -> String
           -> [Type]
           -> Type
           -> Method
mkMethod'_ cName foreignName = makeMethod'' cName foreignName MNormal Nonpure

-- | Same as 'mkMethod', but returns an 'MConst' method.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'mkConstMethod_'.
mkConstMethod :: IsFnName String name => name -> [Type] -> Type -> ClassEntity
mkConstMethod = ((CEMethod .) .) . mkConstMethod_

-- | The unwrapped version of 'mkConstMethod'.
mkConstMethod_ :: IsFnName String name => name -> [Type] -> Type -> Method
mkConstMethod_ name = makeMethod' name MConst Nonpure

-- | Same as 'mkMethod'', but returns an 'MConst' method.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'mkConstMethod'_'.
mkConstMethod' :: IsFnName String name => name -> String -> [Type] -> Type -> ClassEntity
mkConstMethod' = (((CEMethod .) .) .) . mkConstMethod'_

-- | The unwrapped version of 'mkConstMethod''.
mkConstMethod'_ :: IsFnName String name => name -> String -> [Type] -> Type -> Method
mkConstMethod'_ cName foreignName = makeMethod'' cName foreignName MConst Nonpure

-- | Same as 'mkMethod', but returns an 'MStatic' method.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'mkStaticMethod_'.
mkStaticMethod :: IsFnName String name => name -> [Type] -> Type -> ClassEntity
mkStaticMethod = ((CEMethod .) .) . mkStaticMethod_

-- | The unwrapped version of 'mkStaticMethod'.
mkStaticMethod_ :: IsFnName String name => name -> [Type] -> Type -> Method
mkStaticMethod_ name = makeMethod' name MStatic Nonpure

-- | Same as 'mkMethod'', but returns an 'MStatic' method.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'mkStaticMethod'_'.
mkStaticMethod' :: IsFnName String name => name -> String -> [Type] -> Type -> ClassEntity
mkStaticMethod' = (((CEMethod .) .) .) . mkStaticMethod'_

-- | The unwrapped version of 'mkStaticMethod''.
mkStaticMethod'_ :: IsFnName String name => name -> String -> [Type] -> Type -> Method
mkStaticMethod'_ cName foreignName = makeMethod'' cName foreignName MStatic Nonpure

-- | A \"property\" getter/setter pair.
newtype Prop = Prop [Method]

-- | Creates a getter/setter binding pair for methods:
--
-- > T foo() const
-- > void setFoo(T)
--
-- The result is wrapped in a 'CEProp'.  For an unwrapped value, use
-- 'mkProp_'.
mkProp :: String -> Type -> ClassEntity
mkProp = (CEProp .) . mkProp_

-- | The unwrapped version of 'mkProp'.
mkProp_ :: String -> Type -> Prop
mkProp_ name t =
  let c:cs = name
      setName = 's' : 'e' : 't' : toUpper c : cs
  in Prop [ mkConstMethod_ name [] t
          , mkMethod_ setName [t] Internal_TVoid
          ]

-- | Creates a getter/setter binding pair for static methods:
--
-- > static T foo() const
-- > static void setFoo(T)
mkStaticProp :: String -> Type -> ClassEntity
mkStaticProp = (CEProp .) . mkStaticProp_

-- | The unwrapped version of 'mkStaticProp'.
mkStaticProp_ :: String -> Type -> Prop
mkStaticProp_ name t =
  let c:cs = name
      setName = 's' : 'e' : 't' : toUpper c : cs
  in Prop [ mkStaticMethod_ name [] t
          , mkStaticMethod_ setName [t] Internal_TVoid
          ]

-- | Creates a getter/setter binding pair for boolean methods, where the getter
-- is prefixed with @is@:
--
-- > bool isFoo() const
-- > void setFoo(bool)
--
-- The result is wrapped in a 'CEProp'.  For an unwrapped value, use
-- 'mkBoolIsProp_'.
mkBoolIsProp :: String -> ClassEntity
mkBoolIsProp = CEProp . mkBoolIsProp_

-- | The unwrapped version of 'mkBoolIsProp'.
mkBoolIsProp_ :: String -> Prop
mkBoolIsProp_ name =
  let c:cs = name
      name' = toUpper c : cs
      isName = 'i':'s':name'
      setName = 's':'e':'t':name'
  in Prop [ mkConstMethod_ isName [] Internal_TBool
          , mkMethod_ setName [Internal_TBool] Internal_TVoid
          ]

-- | Creates a getter/setter binding pair for boolean methods, where the getter
-- is prefixed with @has@:
--
-- > bool hasFoo() const
-- > void setFoo(bool)
--
-- The result is wrapped in a 'CEProp'.  For an unwrapped value, use
-- 'mkBoolHasProp_'.
mkBoolHasProp :: String -> ClassEntity
mkBoolHasProp = CEProp . mkBoolHasProp_

-- | The unwrapped version of 'mkBoolHasProp'.
mkBoolHasProp_ :: String -> Prop
mkBoolHasProp_ name =
  let c:cs = name
      name' = toUpper c : cs
      hasName = 'h':'a':'s':name'
      setName = 's':'e':'t':name'
  in Prop [ mkConstMethod_ hasName [] Internal_TBool
          , mkMethod_ setName [Internal_TBool] Internal_TVoid
          ]

-- | A non-C++ function that can be invoked via a C++ functor or function
-- pointer.
--
-- Use this data type's 'HasReqs' instance to add extra requirements, however
-- manually adding requirements for parameter and return types is not necessary.
data Callback = Callback
  { callbackExtName :: ExtName
    -- ^ The callback's external name.
  , callbackParams :: [Type]
    -- ^ The callback's parameter types.
  , callbackReturn :: Type
    -- ^ The callback's return type.
  , callbackThrows :: Maybe Bool
    -- ^ Whether the callback supports throwing C++ exceptions from Haskell into
    -- C++ during its execution.  When absent, the value is inherited from
    -- 'moduleCallbacksThrow' and 'interfaceCallbacksThrow'.
  , callbackReqs :: Reqs
    -- ^ Extra requirements for the callback.
  , callbackAddendum :: Addendum
    -- ^ The callback's addendum.
  }

instance Eq Callback where
  (==) = (==) `on` callbackExtName

instance Show Callback where
  show cb =
    concat ["<Callback ", show (callbackExtName cb), " ", show (callbackParams cb), " ",
            show (callbackReturn cb)]

instance HasExtNames Callback where
  getPrimaryExtName = callbackExtName

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
makeCallback extName paramTypes retType =
  Callback extName paramTypes retType Nothing mempty mempty

-- | Sets whether a callback supports handling thrown C++ exceptions and passing
-- them into C++.
callbackSetThrows :: Bool -> Callback -> Callback
callbackSetThrows value cb = cb { callbackThrows = Just value }

-- | Each exception class has a unique exception ID.
newtype ExceptionId = ExceptionId
  { getExceptionId :: Int  -- ^ Internal.
  } deriving (Eq, Show)

-- | The exception ID that represents the catch-all type.
exceptionCatchAllId :: ExceptionId
exceptionCatchAllId = ExceptionId 1

-- | The lowest exception ID to be used for classes.
exceptionFirstFreeId :: Int
exceptionFirstFreeId = getExceptionId exceptionCatchAllId + 1

-- | Indicates the ability to handle a certain type of C++ exception.
data ExceptionHandler =
    CatchClass Class
    -- ^ Indicates that instances of the given class are handled (including
    -- derived types).
  | CatchAll
    -- ^ Indicates that all C++ exceptions are handled, i.e. @catch (...)@.
  deriving (Eq, Ord)

-- | Represents a list of exception handlers to be used for a body of code.
-- Order is important; a 'CatchAll' will prevent all subsequent handlers from
-- being invoked.
data ExceptionHandlers = ExceptionHandlers
  { exceptionHandlersList :: [ExceptionHandler]
    -- ^ Extracts the list of exception handlers.
  }

instance Monoid ExceptionHandlers where
  mempty = ExceptionHandlers []

  mappend e1 e2 =
    ExceptionHandlers $ exceptionHandlersList e1 ++ exceptionHandlersList e2

-- | Types that can handle exceptions.
class HandlesExceptions a where
  -- | Extracts the exception handlers for an object.
  getExceptionHandlers :: a -> ExceptionHandlers

  -- | Modifies an object's exception handlers with a given function.
  modifyExceptionHandlers :: (ExceptionHandlers -> ExceptionHandlers) -> a -> a

-- | Appends additional exception handlers to an object.
handleExceptions :: HandlesExceptions a => [ExceptionHandler] -> a -> a
handleExceptions classes =
  modifyExceptionHandlers $ mappend mempty { exceptionHandlersList = classes }

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

-- | Constructor for an import set.
makeHsImportSet :: M.Map HsImportKey HsImportSpecs -> HsImportSet
makeHsImportSet = HsImportSet

-- | Sets all of the import specifications in an import set to be
-- @{-#SOURCE#-}@ imports.
hsImportSetMakeSource :: HsImportSet -> HsImportSet
hsImportSetMakeSource (HsImportSet m) =
  HsImportSet $ M.map (\specs -> specs { hsImportSource = True }) m

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

-- | Imports "Control.Exception" qualified as @HoppyCE@.
hsImportForException :: HsImportSet
hsImportForException = hsQualifiedImport "Control.Exception" "HoppyCE"

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

-- | Imports "Data.Map" qualified as @HoppyDM@.
hsImportForMap :: HsImportSet
hsImportForMap = hsQualifiedImport "Data.Map" "HoppyDM"

-- | Imports "Prelude" qualified as @HoppyP@.
hsImportForPrelude :: HsImportSet
hsImportForPrelude = hsQualifiedImport "Prelude" "HoppyP"

-- | Imports "Foreign.Hoppy.Runtime" qualified as @HoppyFHR@.
hsImportForRuntime :: HsImportSet
hsImportForRuntime = hsQualifiedImport "Foreign.Hoppy.Runtime" "HoppyFHR"

-- | Imports "System.Posix.Types" qualified as @HoppySPT@.
hsImportForSystemPosixTypes :: HsImportSet
hsImportForSystemPosixTypes = hsQualifiedImport "System.Posix.Types" "HoppySPT"

-- | Imports "Data.Typeable" qualified as @HoppyDT@.
hsImportForTypeable :: HsImportSet
hsImportForTypeable = hsQualifiedImport "Data.Typeable" "HoppyDT"

-- | Imports "System.IO.Unsafe" qualified as @HoppySIU@.
hsImportForUnsafeIO :: HsImportSet
hsImportForUnsafeIO = hsQualifiedImport "System.IO.Unsafe" "HoppySIU"

-- | Returns an error message indicating that
-- 'Foreign.Hoppy.Generator.Types.objToHeapT' is used where data is going from a
-- foreign language into C++.
objToHeapTWrongDirectionErrorMsg :: Maybe String -> Class -> String
objToHeapTWrongDirectionErrorMsg maybeCaller cls =
  concat [maybe "" (++ ": ") maybeCaller,
          "(TObjToHeap ", show cls, ") cannot be passed into C++",
          maybe "" (const ".") maybeCaller]

-- | Returns an error message indicating that
-- 'Foreign.Hoppy.Generator.Types.objToHeapT' is used where data is going from a
-- foreign language into C++.
tToGcInvalidFormErrorMessage :: Maybe String -> Type -> String
tToGcInvalidFormErrorMessage maybeCaller typeArg =
  concat [maybe "" (++ ": ") maybeCaller,
          "(", show (Internal_TToGc typeArg), ") is an invalid form for TToGc.",
          maybe "" (const ".") maybeCaller]

-- | Returns an error message indicating that
-- 'Foreign.Hoppy.Generator.Types.toGcT' is used where data is going from a
-- foreign language into C++.
toGcTWrongDirectionErrorMsg :: Maybe String -> Type -> String
toGcTWrongDirectionErrorMsg maybeCaller typeArg =
  concat [maybe "" (++ ": ") maybeCaller,
          "(", show (Internal_TToGc typeArg), ") cannot be passed into C++",
          maybe "" (const ".") maybeCaller]
