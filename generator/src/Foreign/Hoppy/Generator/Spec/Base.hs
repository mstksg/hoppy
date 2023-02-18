-- This file is part of Hoppy.
--
-- Copyright 2015-2023 Bryan Gardiner <bog@khumba.net>
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
  ErrorMsg,
  -- * Interfaces
  Interface,
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
  interfaceCompiler,
  interfaceSetCompiler,
  interfaceSetCompiler',
  interfaceSetNoCompiler,
  interfaceValidateEnumTypes,
  interfaceSetValidateEnumTypes,
  interfaceHooks,
  interfaceModifyHooks,
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
  -- * Names
  ExtName,
  toExtName,
  extNameOrIdentifier,
  extNameOrFnIdentifier,
  extNameOrString,
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
  Identifier,
  makeIdentifier,
  identifierParts,
  IdPart,
  makeIdPart,
  idPartBase,
  idPartArgs,
  ident, ident', ident1, ident2, ident3, ident4, ident5,
  identT, identT', ident1T, ident2T, ident3T, ident4T, ident5T,
  -- * Exports
  Exportable (..),
  Export (..),
  -- * Basic types
  Type (..),
  normalizeType,
  stripConst,
  stripToGc,
  Scoped (..),
  isScoped,
  -- * Functions and parameters
  Constness (..), constNegate,
  Purity (..),
  Parameter, parameterType, onParameterType, parameterName,
  IsParameter (..), toParameters,
  np, (~:),
  -- * Conversions
  ConversionMethod (..),
  ConversionSpec (conversionSpecName, conversionSpecCpp, conversionSpecHaskell),
  makeConversionSpec,
  ConversionSpecCpp (
    ConversionSpecCpp,
    conversionSpecCppName,
    conversionSpecCppReqs,
    conversionSpecCppConversionType,
    conversionSpecCppConversionToCppExpr,
    conversionSpecCppConversionFromCppExpr
  ),
  makeConversionSpecCpp,
  ConversionSpecHaskell (
    ConversionSpecHaskell,
    conversionSpecHaskellHsType,
    conversionSpecHaskellHsArgType,
    conversionSpecHaskellCType,
    conversionSpecHaskellToCppFn,
    conversionSpecHaskellFromCppFn
  ),
  makeConversionSpecHaskell,
  -- * Exceptions
  ExceptionId (..),
  exceptionCatchAllId,
  ExceptionHandler (..),
  ExceptionHandlers (..),
  HandlesExceptions (..),
  handleExceptions,
  -- * Addenda
  Addendum (..),
  HasAddendum (..),
  addAddendumHaskell,
  -- * Enum support
  EnumInfo (..),
  EnumEntryWords,
  EnumValueMap (..),
  EnumValue (..),
  -- * Languages
  ForeignLanguage (..),
  WithForeignLanguageOverrides,
  MapWithForeignLanguageOverrides,
  -- * Haskell imports
  HsModuleName, HsImportSet, HsImportKey (..), HsImportSpecs (..), HsImportName, HsImportVal (..),
  hsWholeModuleImport, hsQualifiedImport, hsImport1, hsImport1', hsImports, hsImports',
  hsImportSetMakeSource,
  -- * Internal to Hoppy
  interfaceAllExceptionClasses,
  interfaceSharedPtr,
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
import Control.Monad.State (MonadState, StateT, execStateT, get, modify, put)
import Data.Char (isAlpha, isAlphaNum)
import Data.Function (on)
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid, mappend, mconcat, mempty)
#endif
import Data.Semigroup as Sem
import qualified Data.Set as S
import Data.Typeable (Typeable, cast)
import Foreign.Hoppy.Generator.Common
import Foreign.Hoppy.Generator.Compiler (Compiler, SomeCompiler (SomeCompiler), defaultCompiler)
import {-# SOURCE #-} Foreign.Hoppy.Generator.Hook (Hooks, defaultHooks)
import {-# SOURCE #-} qualified Foreign.Hoppy.Generator.Language.Cpp as LC
import {-# SOURCE #-} qualified Foreign.Hoppy.Generator.Language.Haskell as LH
import Foreign.Hoppy.Generator.Override (MapWithOverrides, WithOverrides)
import {-# SOURCE #-} Foreign.Hoppy.Generator.Spec.Class (Class, classExtName)
import GHC.Stack (HasCallStack)
import Language.Haskell.Syntax (HsName, HsQualType, HsType)

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
    -- 'moduleCallbacksThrow' and
    -- 'Foreign.Hoppy.Generator.Spec.Callback.callbackThrows'.
  , interfaceExceptionNamesToIds :: M.Map ExtName ExceptionId
    -- ^ Maps from external names of exception classes to their exception IDs.
  , interfaceExceptionSupportModule :: Maybe Module
    -- ^ When an interface uses C++ exceptions, then one module needs to
    -- manually be selected to contain some interface-specific runtime support.
    -- This is the selected module.
  , interfaceSharedPtr :: (Reqs, String)
    -- ^ The name of the @shared_ptr@ class to use, and the requirements to use
    -- it.  This defaults to using @std::shared_ptr@ from @\<memory\>@, but can
    -- be changed if necessary via 'interfaceSetSharedPtr'.
  , interfaceCompiler :: Maybe SomeCompiler
    -- ^ The C++ compiler for the generator itself to use when building
    -- temporary code for the interface.  This can be overridden or disabled.
    -- This defaults to 'defaultCompiler'.
    --
    -- __This is separate__ from the @./configure && make@ compilation process
    -- used by @Foreign.Hoppy.Runtime.Setup.cppMain@ to build generated C++
    -- bindings (see hoppy-runtime).  This compiler is used to evaluate enums'
    -- numeric values when the generator is called, and is not used otherwise.
    -- See 'Foreign.Hoppy.Generator.Spec.Enum.makeAutoEnum' and
    -- "Foreign.Hoppy.Generator.Hooks".
  , interfaceHooks :: Hooks
    -- ^ Hooks allowing the interface to execute code at various points during
    -- the code generator's execution.  This defaults to 'defaultHooks'.
  , interfaceValidateEnumTypes :: Bool
    -- ^ Whether to validate manually-provided enum numeric types
    -- ('Foreign.Hoppy.Generator.Spec.Enum.enumNumericType') using a compiled
    -- C++ @sizeof()@, as is done for enums that don't have an @enumNumericType@
    -- set.
    --
    -- This defaults to true, but can be set to false to discourage requiring a
    -- compiler.  See 'interfaceSetNoCompiler'.
  }

instance Show Interface where
  show iface = concat ["<Interface ", show (interfaceName iface), ">"]

instance HasExports Interface where
  lookupExport name iface =
    lookupExport name =<< M.lookup name (interfaceNamesToModules iface)

-- | Optional parameters when constructing an 'Interface' with 'interface'.
newtype InterfaceOptions = InterfaceOptions
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
        for modules $ \m ->
        let extNames = concatMap getAllExtNames $ M.elems $ moduleExports m
        in M.fromList $ zip extNames $ repeat [m]

      extNamesInMultipleModules :: [(ExtName, [Module])]
      extNamesInMultipleModules =
        M.toList $
        M.filter (\case
                     _:_:_ -> True
                     _ -> False)
        extNamesToModules

  unless (null extNamesInMultipleModules) $
    Left $ unlines $
    "Some external name(s) are exported by multiple modules:" :
    map (\(extName, modules') ->
          concat $ "- " : show extName : ": " : intersperse ", " (map show modules'))
        extNamesInMultipleModules

  let haskellModuleImportNames =
        M.fromList $
        (\a b f -> zipWith f a b) modules [(1::Int)..] $
        \m index -> (m, 'M' : show index)

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
    , interfaceCompiler = Just $ SomeCompiler defaultCompiler
    , interfaceHooks = defaultHooks
    , interfaceValidateEnumTypes = True
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
-- (i.e. if it's been marked as an exception class with
-- 'Foreign.Hoppy.Generator.Spec.Class.classMakeException').
interfaceExceptionClassId :: Interface -> Class -> Maybe ExceptionId
interfaceExceptionClassId iface cls =
  M.lookup (classExtName cls) $ interfaceExceptionNamesToIds iface

-- | Returns all of the exception classes in an interface.
interfaceAllExceptionClasses :: Interface -> [Class]
interfaceAllExceptionClasses = interfaceAllExceptionClasses' . M.elems . interfaceModules

interfaceAllExceptionClasses' :: [Module] -> [Class]
interfaceAllExceptionClasses' modules =
  flip concatMap modules $ \m ->
  catMaybes $
  map getExportExceptionClass $
  M.elems $ moduleExports m

-- | Changes 'Foreign.Hoppy.Generator.Spec.Callback.callbackThrows' for all
-- callbacks in an interface that don't have it set explicitly at the module or
-- callback level.
interfaceSetCallbacksThrow :: Bool -> Interface -> Interface
interfaceSetCallbacksThrow b iface = iface { interfaceCallbacksThrow = b }

-- | Sets an interface's exception support module, for interfaces that use
-- exceptions.
interfaceSetExceptionSupportModule :: HasCallStack => Module -> Interface -> Interface
interfaceSetExceptionSupportModule m iface = case interfaceExceptionSupportModule iface of
  Nothing -> iface { interfaceExceptionSupportModule = Just m }
  Just existingMod ->
    if m == existingMod
    then iface
    else error $ "interfaceSetExceptionSupportModule: " ++ show iface ++
         " already has exception support module " ++ show existingMod ++
         ", trying to set " ++ show m ++ "."

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

-- | Replaces the default compiler used by the interface.
--
-- @interfaceSetCompiler c = 'interfaceSetCompiler'' ('SomeCompiler' c)@
interfaceSetCompiler :: Compiler a => a -> Interface -> Interface
interfaceSetCompiler = interfaceSetCompiler' . Just . SomeCompiler

-- | Replaces the default compiler used by the interface.  When given @Nothing@,
-- the interface will not be allowed to compile any code when it generates
-- bindings.
interfaceSetCompiler' :: Maybe SomeCompiler -> Interface -> Interface
interfaceSetCompiler' compiler iface = iface { interfaceCompiler = compiler }

-- | Sets an interface to never compile C++ code during binding generation.
--
-- This sets the interface to have no compiler, and also asks the interface not
-- to do things that require a compiler, which would otherwise cause a runtime
-- failure: currently just validation of provided enum numeric types
-- (@'interfaceSetValidateEnumTypes' False@).
interfaceSetNoCompiler :: Interface -> Interface
interfaceSetNoCompiler =
  interfaceSetValidateEnumTypes False .
  interfaceSetCompiler' Nothing

-- | Controls whether the interface will validate manually specified enum types
-- ('Foreign.Hoppy.Generator.Spec.Enum.enumNumericType') by compiling a C++
-- program.
--
-- See 'interfaceValidateEnumTypes'.
interfaceSetValidateEnumTypes :: Bool -> Interface -> Interface
interfaceSetValidateEnumTypes validate iface =
  iface { interfaceValidateEnumTypes = validate }

-- | Modifies the hooks associated with an interface.
interfaceModifyHooks :: (Hooks -> Hooks) -> Interface -> Interface
interfaceModifyHooks f iface =
  iface { interfaceHooks = f $ interfaceHooks iface }

-- | An @#include@ directive in a C++ file.
newtype Include = Include
  { includeToString :: String
    -- ^ Returns the complete @#include ...@ line for an include, including
    -- trailing newline.
  } deriving (Eq, Ord, Show)

-- | Creates an @#include \<...\>@ directive.
--
-- This can be added to most types of C++ entities with 'addReqIncludes'.
includeStd :: String -> Include
includeStd path = Include $ "#include <" ++ path ++ ">\n"

-- | Creates an @#include "..."@ directive.
--
-- This can be added to most types of C++ entities with 'addReqIncludes'.
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
    -- 'interfaceCallbacksThrow'.  This maybe overridden by
    -- 'Foreign.Hoppy.Generator.Spec.Callback.callbackThrows'.
  , moduleAddendum :: Addendum
    -- ^ The module's addendum.
  }

instance Eq Module where
  (==) = (==) `on` moduleName

instance Ord Module where
  compare = compare `on` moduleName

instance Show Module where
  show m = concat ["<Module ", moduleName m, ">"]

instance HasExports Module where
  lookupExport name m = M.lookup name $ moduleExports m

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
moduleModify' :: HasCallStack => Module -> StateT Module (Either String) () -> Module
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
    then put m { moduleExports = existingExports `mappend` newExports }
    else throwError $ concat
         ["moduleAddExports: ", show m, " defines external names multiple times: ",
          show duplicateNames]

-- | Changes a module's 'moduleHaskellName' from the default.  This can only be
-- called once on a module.
moduleAddHaskellName :: (MonadError String m, MonadState Module m) => [String] -> m ()
moduleAddHaskellName name = do
  m <- get
  case moduleHaskellName m of
    Nothing -> put m { moduleHaskellName = Just name }
    Just name' ->
      throwError $ concat
      ["moduleAddHaskellName: ", show m, " already has Haskell name ",
       show name', "; trying to add name ", show name, "."]

-- | Changes 'Foreign.Hoppy.Generator.Spec.Callback.callbackThrows' for all
-- callbacks in a module that don't have it set explicitly.
moduleSetCallbacksThrow :: MonadState Module m => Maybe Bool -> m ()
moduleSetCallbacksThrow b = modify $ \m -> m { moduleCallbacksThrow = b }

-- | A set of requirements of needed to use an identifier in C++ (function,
-- type, etc.), via a set of 'Include's.  The monoid instance has 'mempty' as an
-- empty set of includes, and 'mappend' unions two include sets.
newtype Reqs = Reqs
  { reqsIncludes :: S.Set Include
    -- ^ The includes specified by a 'Reqs'.
  } deriving (Show)

instance Sem.Semigroup Reqs where
  (<>) (Reqs incl) (Reqs incl') = Reqs $ mappend incl incl'

instance Monoid Reqs where
  mempty = Reqs mempty

  mappend = (<>)

  mconcat reqs = Reqs $ mconcat $ map reqsIncludes reqs

-- | Creates a 'Reqs' that contains the given include.
reqInclude :: Include -> Reqs
reqInclude include = mempty { reqsIncludes = S.singleton include }

-- | Contains the data types for bindings to C++ entities:
-- 'Foreign.Hoppy.Generator.Spec.Function.Function',
-- 'Foreign.Hoppy.Generator.Spec.Class.Class', etc.  Use 'addReqs' or
-- 'addReqIncludes' to specify requirements for these entities, e.g. header
-- files that must be included in order to access the underlying entities that
-- are being bound.

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
  } deriving (Eq, Sem.Semigroup, Monoid, Ord)

instance Show ExtName where
  show extName = concat ["$\"", fromExtName extName, "\"$"]

-- | Creates an 'ExtName' that contains the given string, erroring if the string
-- is an invalid 'ExtName'.
toExtName :: HasCallStack => String -> ExtName
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
extNameOrIdentifier :: HasCallStack => Identifier -> Maybe ExtName -> ExtName
extNameOrIdentifier identifier = fromMaybe $ case identifierParts identifier of
  [] -> error "extNameOrIdentifier: Invalid empty identifier."
  parts -> toExtName $ idPartBase $ last parts

-- | Generates an 'ExtName' from an @'FnName' 'Identifier'@, if the given name
-- is absent.
extNameOrFnIdentifier :: HasCallStack => FnName Identifier -> Maybe ExtName -> ExtName
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
operatorPreferredExtName :: HasCallStack => Operator -> ExtName
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
operatorType :: HasCallStack => Operator -> OperatorType
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

-- | Types that contain 'Export's that can be looked up by their 'ExtName's.
class HasExports a where
  -- | Looks up the 'Export' for an 'ExtName' in the given object.
  lookupExport :: ExtName -> a -> Maybe Export

-- | A path to some C++ object, including namespaces.  An identifier consists of
-- multiple parts separated by @\"::\"@.  Each part has a name string followed
-- by an optional template argument list, where each argument gets rendered from
-- a 'Type' (non-type arguments for template metaprogramming are not supported).
--
-- The 'Monoid' instance inserts a @::@ between joined identifiers.  Usually an
-- identifier needs to contain at least one part, so 'mempty' is an invalid
-- argument to many functions in Hoppy, but it is useful as a base case for
-- appending.
newtype Identifier = Identifier
  { identifierParts :: [IdPart]
    -- ^ The separate parts of the identifier, between @::@s.
  } deriving (Eq, Monoid, Sem.Semigroup)

instance Show Identifier where
  show identifier =
    (\wordList -> concat $ "<Identifier " : wordList ++ [">"]) $
    intersperse "::" $
    map (\part -> case idPartArgs part of
            Nothing -> idPartBase part
            Just args ->
              concat $
              idPartBase part : "<" :
              intersperse ", " (map show args) ++ [">"]) $
    identifierParts identifier

-- | A single component of an 'Identifier', between @::@s.
data IdPart = IdPart
  { idPartBase :: String
    -- ^ The name within the enclosing scope.
  , idPartArgs :: Maybe [Type]
    -- ^ Template arguments, if present.
  } deriving (Eq, Show)

-- | Creates an identifier from a collection of 'IdPart's, with @::@s between.
makeIdentifier :: [IdPart] -> Identifier
makeIdentifier = Identifier

-- | Creates an object representing one component of an identifier.
makeIdPart :: String -> Maybe [Type] -> IdPart
makeIdPart = IdPart

-- | Creates a identifier of the form @a@, without any namespace operators
-- (@::@).
ident :: String -> Identifier
ident a = Identifier [IdPart a Nothing]

-- | Creates an identifier of the form @a1::a2::...::aN@.
ident' :: [String] -> Identifier
ident' = Identifier . map (\x -> IdPart { idPartBase = x, idPartArgs = Nothing })

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

-- | Instances of this typeclass are C++ entities that Hoppy can expose to
-- foreign languages: functions, classes, global variables, etc.  'Interface's
-- are largely composed of exports (grouped into modules).  Hoppy uses this
-- interface to perform code generation for each entity.
class (HasAddendum a, HasExtNames a, HasReqs a, Typeable a, Show a) => Exportable a where
  -- | Wraps an exportable object in an existential data type.
  --
  -- The default instance is just @toExport = 'Export'@, which does not need to
  -- be overridden in general.
  toExport :: a -> Export
  toExport = Export

  -- | Attempts to cast an exportable object to a specific type, pulling off
  -- 'Export' wrappers as necessary.
  --
  -- The default @castExport = 'cast'@ is fine.
  castExport :: (Typeable a, Exportable b, Typeable b) => a -> Maybe b
  castExport = cast

  -- | Generates the C++ side of the binding for an entity.
  --
  -- For an entity, Hoppy invokes this function once with 'LC.SayHeader' when
  -- generating the header file for a module, and once with 'LC.SaySource' when
  -- generating the corresponding source file.
  sayExportCpp :: LC.SayExportMode -> a -> LC.Generator ()

  -- | Generates the Haskell side of the binding for an entity.
  --
  -- For an entity, Hoppy invokes this function once with
  -- 'LH.SayExportForeignImports' when it is time to emit foreign imports, and
  -- once with 'LH.SayExportDecls' when it is time to generate Haskell binding
  -- code later in the module.  Hoppy may also call the function with
  -- 'LH.SayExportBoot', if necessary.
  --
  -- See 'LH.SayExportMode'.
  sayExportHaskell :: LH.SayExportMode -> a -> LH.Generator ()

  -- | If the export is backed by an C++ enum, then this returns known
  -- structural information about the enum.  This provides information to the
  -- \"evaluate enums\" hook so that Hoppy can determine enum values on its own.
  --
  -- By default, this returns @Nothing@.
  --
  -- See 'Hooks'.
  getExportEnumInfo :: a -> Maybe EnumInfo
  getExportEnumInfo _ = Nothing

  -- | If the export is backed by a C++ class that is marked as supporting
  -- exceptions, then this returns the class definition.
  --
  -- By default, this returns @Nothing@.
  getExportExceptionClass :: a -> Maybe Class
  getExportExceptionClass _ = Nothing

-- | Specifies some C++ declaration (function, class, etc.) to give access to.
data Export = forall a. Exportable a => Export a

instance HasAddendum Export where
  getAddendum (Export e) = getAddendum e
  setAddendum a (Export e) = Export $ setAddendum a e
  modifyAddendum f (Export e) = Export $ modifyAddendum f e

instance HasExtNames Export where
  getPrimaryExtName (Export e) = getPrimaryExtName e
  getNestedExtNames (Export e) = getNestedExtNames e

instance HasReqs Export where
  getReqs (Export e) = getReqs e
  setReqs reqs (Export e) = Export $ setReqs reqs e
  modifyReqs f (Export e) = Export $ modifyReqs f e

instance Exportable Export where
  toExport = id

  castExport (Export e) = castExport e

  sayExportCpp sayBody (Export e) = sayExportCpp sayBody e

  sayExportHaskell mode (Export e) = sayExportHaskell mode e

  getExportEnumInfo (Export e) = getExportEnumInfo e

  getExportExceptionClass (Export e) = getExportExceptionClass e

instance Show Export where
  show (Export e) = "<Export " ++ show e ++ ">"

-- | A concrete C++ type.  Use the bindings in "Foreign.Hoppy.Generator.Types"
-- for values of this type; these data constructors are subject to change
-- without notice.
data Type =
    Internal_TVoid
  | Internal_TPtr Type
  | Internal_TRef Type
  | Internal_TFn [Parameter] Type
  | Internal_TObj Class
  | Internal_TObjToHeap Class
  | Internal_TToGc Type
  | Internal_TManual ConversionSpec
  | Internal_TConst Type
  -- When changing the declarations here, be sure to update the Eq instance.
  deriving (Show)

instance Eq Type where
  Internal_TVoid == Internal_TVoid = True
  (Internal_TPtr t) == (Internal_TPtr t') = t == t'
  (Internal_TRef t) == (Internal_TRef t') = t == t'
  (Internal_TFn ps r) == (Internal_TFn ps' r') =
    (and $ zipWith ((==) `on` parameterType) ps ps') && r == r'
  (Internal_TObj cls) == (Internal_TObj cls') = cls == cls'
  (Internal_TObjToHeap cls) == (Internal_TObjToHeap cls') = cls == cls'
  (Internal_TToGc t) == (Internal_TToGc t') = t == t'
  (Internal_TManual s) == (Internal_TManual s') = s == s'
  (Internal_TConst t) == (Internal_TConst t') = t == t'
  _ == _ = False

-- | Canonicalizes a 'Type' without changing its meaning.  Multiple nested
-- 'Internal_TConst's are collapsed into a single one.
normalizeType :: Type -> Type
normalizeType t = case t of
  Internal_TVoid -> t
  Internal_TPtr t' -> Internal_TPtr $ normalizeType t'
  Internal_TRef t' -> Internal_TRef $ normalizeType t'
  Internal_TFn params retType ->
    Internal_TFn (map (onParameterType normalizeType) params) $ normalizeType retType
  Internal_TObj _ -> t
  Internal_TObjToHeap _ -> t
  Internal_TToGc _ -> t
  Internal_TManual _ -> t
  Internal_TConst (Internal_TConst t') -> normalizeType $ Internal_TConst t'
  Internal_TConst _ -> t

-- | Strips leading 'Internal_TConst's off of a type.
stripConst :: Type -> Type
stripConst t = case t of
  Internal_TConst t' -> stripConst t'
  _ -> t

-- | Strips a leading 'Internal_TToGc' off of a type.
stripToGc :: Type -> Type
stripToGc t = case t of
  Internal_TToGc t' -> t'
  _ -> t

-- | Indicates whether an entity is scoped or unscoped.
--
-- This is used to distinguish unscoped enums (@enum@) or scoped ones (@enum
-- class@ or @enum struct@).
data Scoped =
    Unscoped  -- ^ Indicates an unscoped entity (e.g. an enum).
  | Scoped  -- ^ Indicates a scoped entity (e.g. an enum).
  deriving (Eq, Ord, Show)

-- | Returns true if a 'Scoped' value is scoped, and false if it is unscoped.
isScoped :: Scoped -> Bool
isScoped Unscoped = False
isScoped Scoped = True

-- | Whether or not @const@ is applied to an entity.
data Constness = Nonconst | Const
               deriving (Bounded, Enum, Eq, Show)

-- | Returns the opposite constness value.
constNegate :: Constness -> Constness
constNegate Nonconst = Const
constNegate Const = Nonconst

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

-- | A parameter to a function, including a type and an optional name.  A name
-- can be conveniently associated with a type with the @('~:')@ operator.
--
-- Two @Parameter@s are equal if their types are equal.
data Parameter = Parameter
  { parameterType :: Type
    -- ^ The parameter's data type.
  , parameterName :: Maybe String
    -- ^ An optional variable name to describe the parameter.  This name should
    -- follow the same rules as 'ExtName' for its contents.
  } deriving (Show)

-- | Objects that can be coerced to function parameter definitions.
class Show a => IsParameter a where
  toParameter :: a -> Parameter

instance IsParameter Parameter where
  toParameter = id

instance IsParameter Type where
  toParameter t =
    Parameter
    { parameterType = t
    , parameterName = Nothing
    }

-- | Maps a function over a parameter's type.
onParameterType :: (Type -> Type) -> (Parameter -> Parameter)
onParameterType f p = p { parameterType = f $ parameterType p }

-- | An empty parameter list.  This should be used instead of a literal @[]@
-- when declaring an empty parameter list, because in the context of
-- @'IsParameter' a => [a]@, the empty list is ambiguously typed, even though it
-- doesn't matter which instance is selected.
np :: [Parameter]
np = []

-- | Converts a list of parameter-like objects to parameters.
toParameters :: IsParameter a => [a] -> [Parameter]
toParameters = map toParameter

-- | Associates a name string with a type to create a 'Parameter' that
-- can be given as a function or method parameter, instead of a raw 'Type'.  The
-- name given here will be included as documentation in the generated code.
--
-- An empty string given for the name means not to associate a name with the
-- parameter.  This is useful to leave some parameters unnamed in a parameter
-- list while naming other parameters, since the list must either contain all
-- 'Type's or all 'Parameter's.
(~:) :: IsParameter a => String -> a -> Parameter
(~:) name param =
  (toParameter param) { parameterName = if null name then Nothing else Just name }
infixr 0 ~:

-- | Defines the process for converting a value in one direction between C++ and
-- a foreign language.  The type parameter varies depending on the actual
-- conversion being defined.
data ConversionMethod c =
    ConversionUnsupported
    -- ^ The conversion is unsupported.  If part of an interface depends on
    -- performing this conversion, code generation will fail.
  | BinaryCompatible
    -- ^ The input value and its corresponding output have the same binary
    -- representation in memory, and require no explicit conversion.  Numeric
    -- types may use this conversion method.
  | CustomConversion c
    -- ^ Conversion requires a custom process as specified by the argument.
    --
    -- TODO Split into pure (let) vs nonpure (<-)?
  deriving (Show)

-- | The root data type for specifying how conversions happen between C++ and foreign
-- values.
--
-- Prefer 'makeConversionSpec' to using this data constructor directly, then
-- override the record to specify conversions for foreign languages.
--
-- The @Cpp@ component of this data structure specifies a C++ type, and
-- conversions between it and something that can be marshalled over a C FFI
-- layer, if such a conversion is possible in each direction.
--
-- Each foreign language has its own component that must be specified in order
-- for types using this specification to be usable in that language.
data ConversionSpec = ConversionSpec
  { conversionSpecName :: String
    -- ^ An identifying name, used for rendering in e.g. error messages.
  , conversionSpecCpp :: ConversionSpecCpp
    -- ^ Fundamental information about the C++ type.
  , conversionSpecHaskell :: Maybe ConversionSpecHaskell
    -- ^ A specification for how values can be used in Haskell.
  }

instance Eq ConversionSpec where
  (==) = (==) `on` conversionSpecName

instance Show ConversionSpec where
  show x = "<ConversionSpec " ++ show (conversionSpecName x) ++ ">"

-- | Creates a 'ConversionSpec' from an identifying name and a specification of
-- the C++ conversion behaviour.  By default, no foreign language conversion
-- behaviour is configured.  For Haskell, this should be done by using
-- 'makeConversionSpecHaskell' to specify behaviour, then writing that to the
-- 'conversionSpecHaskell' field of the 'ConversionSpec' returned here.
makeConversionSpec ::
     String  -- ^ 'conversionSpecName'
  -> ConversionSpecCpp  -- ^ 'conversionSpecCpp'
  -> ConversionSpec
makeConversionSpec name cppSpec =
  ConversionSpec
  { conversionSpecName = name
  , conversionSpecCpp = cppSpec
  , conversionSpecHaskell = Nothing
  }

-- | For a 'ConversionSpec', defines the C++ type and conversions to and from a
-- C FFI layer.
--
-- Prefer 'makeConversionSpecCpp' to using this data constructor directly, then
-- override the record to specify additional conversion properties.
--
-- 'conversionSpecCppName' specifies the C++ type of the conversion.  This will
-- be the type that is passed over the C FFI as well, unless
-- 'conversionSpecCppConversionType' overrides it.
-- 'conversionSpecCppConversionToCppExpr' and
-- 'conversionSpecCppConversionFromCppExpr' may define custom code generation
-- for passing values over the FFI.
data ConversionSpecCpp = ConversionSpecCpp
  { conversionSpecCppName :: String
    -- ^ The name of the C++ type.  May identify a primitive C++ type such as
    -- @\"unsigned int\"@, or a more complex type like
    -- @std::list\<std::string\>@.

  , conversionSpecCppReqs :: LC.Generator Reqs
    -- ^ Computes requirements to refer to the C++ type.  Being in the generator
    -- monad, this may use its environment, but should not emit code or 'Reqs'
    -- to the generator directly.

  , conversionSpecCppConversionType :: LC.Generator (Maybe Type)
    -- ^ Specifies the type that will be passed over the C FFI.
    --
    -- If absent (default), then the type named by 'conversionSpecCppName' is
    -- also used for marshalling to foreign languages.
    --
    -- If present, this represents a type distinct from 'conversionSpecCppName'
    -- that will be exchanged across the FFI boundary.  In this case, you may
    -- also want to define one or both of 'conversionSpecCppConversionToCppExpr'
    -- and 'conversionSpecCppConversionFromCppExpr'.
    --
    -- This is a monadic value so that it has access to the generator's
    -- environment.  The action should not add imports or emit code.

  , conversionSpecCppConversionToCppExpr ::
      Maybe (LC.Generator () -> Maybe (LC.Generator ()) -> LC.Generator ())
    -- ^ This controls behaviour for receiving a value passed into C++ over the
    -- FFI.  Specifically, this powers the @ConversionSpec@ being used as
    -- 'Foreign.Hoppy.Generator.Spec.Function.Function' arguments and
    -- 'Foreign.Hoppy.Generator.Spec.Callback.Callback' return values.
    --
    -- When absent (default), generated code assumes that it can implicitly
    -- convert a value passed over the FFI from the C FFI type (see
    -- 'conversionSpecCppConversionType') to the C++ type
    -- (i.e. 'conversionSpecCppName').  When the former is absent, this is
    -- always fine.  "Implicitly" here means in the sense of a C++ implicit
    -- conversion, not necessarily binary compatibility.
    --
    -- When present, this provides custom conversion behaviour for receiving a
    -- value passed into C++ over the FFI.  The function should generate C++
    -- code to convert a value from the type passed over the C FFI into the
    -- actual C++ type.
    --
    -- This is a function of the form:
    --
    -- > \emitFromExpr maybeEmitToVar -> ...
    --
    -- If the function's second argument is present, then the function should
    -- emit a variable declaration for that name, created from the expression
    -- emitted by the first argument.
    --
    -- If the function's second argument is absent, then the function should
    -- emit an expression that converts the expression emitted by the first
    -- argument into the appropriate type.
    --
    -- In both cases, the first generator argument should only be evaluated once
    -- by the resulting C++ expression; it is not guaranteed to be pure.

  , conversionSpecCppConversionFromCppExpr ::
      Maybe (LC.Generator () -> Maybe (LC.Generator ()) -> LC.Generator ())
    -- ^ This is the opposite of 'conversionSpecCppConversionToCppExpr'.  This
    -- being present enables custom conversion behaviour for passing a value
    -- /out of/ C++ through the FFI.  This powers the @ConversionSpec@ being
    -- used as 'Foreign.Hoppy.Generator.Spec.Function.Function' return values
    -- and 'Foreign.Hoppy.Generator.Spec.Callback.Callback' arguments.
    --
    -- Arguments are the same as with 'conversionSpecCppConversionToCppExpr'.
  }

-- | Builds a 'ConversionSpecCpp' with a C++ type, with no conversions defined.
makeConversionSpecCpp :: String -> LC.Generator Reqs -> ConversionSpecCpp
makeConversionSpecCpp cppName cppReqs =
  ConversionSpecCpp
  { conversionSpecCppName = cppName
  , conversionSpecCppReqs = cppReqs
  , conversionSpecCppConversionType = return Nothing
  , conversionSpecCppConversionToCppExpr = Nothing
  , conversionSpecCppConversionFromCppExpr = Nothing
  }

-- | Controls how conversions between C++ values and Haskell values happen in
-- Haskell bindings.
--
-- Prefer 'makeConversionSpecHaskell' to using this data constructor directly.
data ConversionSpecHaskell = ConversionSpecHaskell
  { conversionSpecHaskellHsType :: LH.Generator HsType
    -- ^ The type exposed to users of the Haskell side of a binding.  Functions
    -- that take one of these values as an argument will expect this type, and
    -- functions returning one of these values will return this type.
    --
    -- This type is wrapped in a generator in order to be able to specify any
    -- necessary imports.  This generator should not generate code or add
    -- exports.

  , conversionSpecHaskellHsArgType :: Maybe (HsName -> LH.Generator HsQualType)
    -- ^ If present, then for bindings for C++ functions that expect one of
    -- these values as an argument, rather than taking a fixed concrete type
    -- ('conversionSpecHaskellHsType'), this qualified type will be used
    -- instead.  The 'HsName' parameter receives a unique name from the
    -- generator that can be used with 'Language.Haskell.Syntax.HsTyVar' like
    -- so:
    --
    -- > \name -> return $ HsQualType [...constraints...] (HsTyVar name)
    --
    -- 'conversionSpecHaskellHsType' should satisfy this constraint, when
    -- present.
    --
    -- This type is wrapped in a generator in order to be able to specify any
    -- necessary imports.  This generator should not generate code or add
    -- exports.

  , conversionSpecHaskellCType :: Maybe (LH.Generator HsType)
    -- ^ If present, then rather than passing a value of native Haskell type
    -- ('conversionSpecHaskellHsType') directly over the FFI, this is an
    -- intermediate type that will be passed instead.  This is needed any time
    -- that the former type isn't a simple type that the FFI supports.
    --
    -- 'conversionSpecHaskellToCppFn' and 'conversionSpecHaskellFromCppFn'
    -- marshal values into and out of this type, respsectively.
    --
    -- This type is wrapped in a generator in order to be able to specify any
    -- necessary imports.  This generator should not generate code or add
    -- exports.

  , conversionSpecHaskellToCppFn :: ConversionMethod (LH.Generator ())
    -- ^ This defines how a Haskell value is passed to C++.  If this is
    -- 'CustomConversion', then 'conversionSpecHaskellCType' must be present,
    -- and the generator should output a function that takes a value of type
    -- 'conversionSpecHaskellHsType' and return a value of
    -- 'conversionSpecHaskellCType'.
    --
    -- If 'conversionSpecHaskellHsArgType' is present, then the function should
    -- be able to accept that more general type instead.  This is used for
    -- bindings that call into C++ functions.  This function is still
    -- specialized to 'conversionSpecHaskellHsType' when generating code for
    -- callback return values.
    --
    -- The generator should output code and may add imports, but should not add
    -- exports.

  , conversionSpecHaskellFromCppFn :: ConversionMethod (LH.Generator ())
    -- ^ This defines how a Haskell value is passed from C++.  If this is
    -- 'CustomConversion', then 'conversionSpecHaskellCType' must be present,
    -- and the generator should output a function that takes a value of type
    -- 'conversionSpecHaskellCType' and return a value of
    -- 'conversionSpecHaskellHsType'.
    --
    -- The generator should output code and may add imports, but should not add
    -- exports.
  }

-- | Builds a 'ConversionSpecHaskell' with the mandatory parameters given.
makeConversionSpecHaskell ::
  LH.Generator HsType  -- ^ 'conversionSpecHaskellHsType'
  -> Maybe (LH.Generator HsType)  -- ^ 'conversionSpecHaskellCType'
  -> ConversionMethod (LH.Generator ())  -- ^ 'conversionSpecHaskellToCppFn'
  -> ConversionMethod (LH.Generator ())  -- ^ 'conversionSpecHaskellFromCppFn'
  -> ConversionSpecHaskell
makeConversionSpecHaskell hsType cType toCppFn fromCppFn =
  ConversionSpecHaskell
  { conversionSpecHaskellHsType = hsType
  , conversionSpecHaskellHsArgType = Nothing
  , conversionSpecHaskellCType = cType
  , conversionSpecHaskellToCppFn = toCppFn
  , conversionSpecHaskellFromCppFn = fromCppFn
  }

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
newtype ExceptionHandlers = ExceptionHandlers
  { exceptionHandlersList :: [ExceptionHandler]
    -- ^ Extracts the list of exception handlers.
  }

instance Sem.Semigroup ExceptionHandlers where
  (<>) e1 e2 =
    ExceptionHandlers $ exceptionHandlersList e1 ++ exceptionHandlersList e2

instance Monoid ExceptionHandlers where
  mempty = ExceptionHandlers []

  mappend = (<>)

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
newtype Addendum = Addendum
  { addendumHaskell :: LH.Generator ()
    -- ^ Code to be output into the Haskell binding.  May also add imports and
    -- exports.
  }

instance Sem.Semigroup Addendum where
  (<>) (Addendum a) (Addendum b) = Addendum $ a >> b

instance Monoid Addendum where
  mempty = Addendum $ return ()
  mappend = (<>)

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
addAddendumHaskell :: HasAddendum a => LH.Generator () -> a -> a
addAddendumHaskell gen = modifyAddendum $ \addendum ->
  addendum `mappend` mempty { addendumHaskell = gen }

-- | Structural information about a C++ enum.  This is used when Hoppy is
-- evaluating enum data, see 'getExportEnumInfo'.
--
-- See 'Foreign.Hoppy.Generator.Spec.Enum.CppEnum'.
data EnumInfo = EnumInfo
  { enumInfoExtName :: ExtName
    -- ^ The external name of the enum.
  , enumInfoIdentifier :: Identifier
    -- ^ The enum's identifier.
  , enumInfoNumericType :: Maybe Type
    -- ^ The enum's numeric type, if explicitly known to the bindings.  This
    -- does not need to be provided.  If absent, then Hoppy will calculate the
    -- enum's numeric type on its own, using a C++ compiler.  If this is present
    -- however, Hoppy will use it, and additionally validate it against what the
    -- C++ compiler thinks, if validation is enabled (see
    -- 'interfaceValidateEnumTypes').
  , enumInfoReqs :: Reqs
    -- ^ Requirements for accessing the enum.
  , enumInfoScoped :: Scoped
    -- ^ Whether the enum is scoped or unscoped.
  , enumInfoValues :: EnumValueMap
    -- ^ The entries in the enum.
  }

-- | A list of words that comprise the name of an enum entry.  Each string in
-- this list is treated as a distinct word for the purpose of performing case
-- conversion to create identifiers in foreign languages.  These values are most
-- easily created from a C++ identifier using
-- 'Foreign.Hoppy.Generator.Util.splitIntoWords'.
type EnumEntryWords = [String]

-- | Describes the entries in a C++ enum.
--
-- Equality is defined as having the same 'enumValueMapValues'.
data EnumValueMap = EnumValueMap
  { enumValueMapNames :: [EnumEntryWords]
    -- ^ The names of all entries in the enum being generated, in the order
    -- specified by the enum definition.  These are the strings used to name
    -- generated bindings.  Each name is broken up into words.  How the words
    -- and get combined to make a name in a particular foreign language depends
    -- on the language.
  , enumValueMapForeignNames :: MapWithForeignLanguageOverrides EnumEntryWords EnumEntryWords
    -- ^ Per-language renames of enum value entries.
  , enumValueMapValues :: M.Map EnumEntryWords EnumValue
    -- ^ A map specifying for each entry in 'enumValueMapNames', how to
    -- determine the entry's numeric value.
  }

instance Eq EnumValueMap where
  (==) = (==) `on` enumValueMapValues

instance Show EnumValueMap where
  show x = "<EnumValueMap values=" ++ show (enumValueMapValues x) ++ ">"

-- | Describes the value of an entry in a C++ enum.  A numeric value may either
-- be provided manually, or if omitted, Hoppy can determine it automatically.
data EnumValue =
    EnumValueManual Integer
    -- ^ A manually specified numeric enum value.
  | EnumValueAuto Identifier
    -- ^ A numeric enum value that will be determined when the generator is run,
    -- by means of compiling a C++ program.
  deriving (Eq, Show)

-- | Languages that Hoppy supports binding to.  Currently this is only Haskell.
data ForeignLanguage =
  Haskell  -- ^ The Haskell language.
  deriving (Eq, Ord, Show)

-- | A value that may be overridden based on a 'ForeignLanguage'.
type WithForeignLanguageOverrides = WithOverrides ForeignLanguage

-- | A map whose values may be overridden based on a 'ForeignLanguage'.
type MapWithForeignLanguageOverrides = MapWithOverrides ForeignLanguage

-- | A collection of imports for a Haskell module.  This is a monoid: import
-- Statements are merged to give the union of imported bindings.
--
-- This structure supports two specific types of imports:
--     - @import Foo (...)@
--     - @import qualified Foo as Bar@
-- Imports with @as@ but without @qualified@, and @qualified@ imports with a
-- spec list, are not supported.  This satisfies the needs of the code
-- generator, and keeps the merging logic simple.
data HsImportSet = HsImportSet
  { getHsImportSet :: M.Map HsImportKey HsImportSpecs
    -- ^ Returns the import set's internal map from module names to imported
    -- bindings.
  } deriving (Show)

-- TODO Make HsImportSet back into a newtype when it doesn't involve listing out
-- its contents recursively in Base.hs-boot.

instance Sem.Semigroup HsImportSet where
  (<>) (HsImportSet m) (HsImportSet m') =
    HsImportSet $ M.unionWith mergeImportSpecs m m'

instance Monoid HsImportSet where
  mempty = HsImportSet M.empty

  mappend = (<>)

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
          (HsImportValSome x, HsImportValSome x') -> HsImportValSome $ x ++ x'
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
hsWholeModuleImport modName =
  HsImportSet $ M.singleton (HsImportKey modName Nothing) $
  HsImportSpecs Nothing False

-- | A qualified import of a Haskell module.
hsQualifiedImport :: HsModuleName -> HsModuleName -> HsImportSet
hsQualifiedImport modName qualifiedName =
  HsImportSet $ M.singleton (HsImportKey modName $ Just qualifiedName) $
  HsImportSpecs Nothing False

-- | An import of a single name from a Haskell module.
hsImport1 :: HsModuleName -> HsImportName -> HsImportSet
hsImport1 modName valueName = hsImport1' modName valueName HsImportVal

-- | A detailed import of a single name from a Haskell module.
hsImport1' :: HsModuleName -> HsImportName -> HsImportVal -> HsImportSet
hsImport1' modName valueName valueType =
  HsImportSet $ M.singleton (HsImportKey modName Nothing) $
  HsImportSpecs (Just $ M.singleton valueName valueType) False

-- | An import of multiple names from a Haskell module.
hsImports :: HsModuleName -> [HsImportName] -> HsImportSet
hsImports modName names =
  hsImports' modName $ map (\name -> (name, HsImportVal)) names

-- | A detailed import of multiple names from a Haskell module.
hsImports' :: HsModuleName -> [(HsImportName, HsImportVal)] -> HsImportSet
hsImports' modName values =
  HsImportSet $ M.singleton (HsImportKey modName Nothing) $
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
