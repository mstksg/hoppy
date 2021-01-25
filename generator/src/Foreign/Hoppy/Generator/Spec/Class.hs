-- This file is part of Hoppy.
--
-- Copyright 2015-2021 Bryan Gardiner <bog@khumba.net>
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

-- | Interface for defining bindings to C++ classes.
module Foreign.Hoppy.Generator.Spec.Class (
  -- * Data type
  Class,
  -- * Construction
  makeClass,
  -- * Properties
  -- ** Common
  classExtName,
  classIdentifier,
  classReqs,
  classAddendum,
  -- ** Class hierarchy
  classSuperclasses,
  classIsMonomorphicSuperclass, classSetMonomorphicSuperclass,
  classIsSubclassOfMonomorphic, classSetSubclassOfMonomorphic,
  -- ** Entities
  classEntities, classAddEntities, classVariables, classCtors, classMethods,
  classEntityPrefix, classSetEntityPrefix,
  classDtorIsPublic, classSetDtorPrivate,
  classConversion,
  classIsException, classMakeException,
  -- * Entity types
  ClassEntity (..), IsClassEntity (..),
  classEntityExtName, classEntityExtNames,
  classEntityForeignName, classEntityForeignName',
  -- ** Class variables
  ClassVariable,
  -- *** Construction
  makeClassVariable, makeClassVariable_,
  mkClassVariable, mkClassVariable_,
  mkStaticClassVariable,
  mkStaticClassVariable_,
  -- ** Constructors
  Ctor,
  -- *** Construction
  makeCtor, makeCtor_,
  mkCtor, mkCtor_,
  -- *** Properties
  ctorExtName,
  ctorParams,
  ctorExceptionHandlers,
  -- ** Methods (member functions)
  Method, MethodApplicability (..), Staticness (..), MethodImpl (..),
  -- *** Construction
  makeMethod, makeMethod_,
  makeFnMethod, makeFnMethod_,
  mkMethod, mkMethod_, mkMethod', mkMethod'_,
  mkConstMethod, mkConstMethod_, mkConstMethod', mkConstMethod'_,
  mkStaticMethod, mkStaticMethod_, mkStaticMethod', mkStaticMethod'_,
  -- *** Properties
  methodExtName, methodImpl, methodApplicability, methodConst, methodStatic, methodPurity,
  methodParams, methodReturn, methodExceptionHandlers,
  -- ** Class properties (getter/setter pairs)
  Prop,
  -- ** Construction
  mkProp, mkProp_,
  mkStaticProp, mkStaticProp_,
  mkBoolIsProp, mkBoolIsProp_,
  mkBoolHasProp, mkBoolHasProp_,
  -- * Conversions
  ClassConversion (..), classConversionNone, classModifyConversion, classSetConversion,
  ClassHaskellConversion (..), classSetHaskellConversion,
  -- * Haskell generator
  -- ** Names
  toHsValueClassName, toHsValueClassName',
  toHsWithValuePtrName, toHsWithValuePtrName',
  toHsPtrClassName, toHsPtrClassName',
  toHsCastMethodName, toHsCastMethodName',
  toHsDownCastClassName, toHsDownCastClassName',
  toHsDownCastMethodName, toHsDownCastMethodName',
  toHsCastPrimitiveName, toHsCastPrimitiveName',
  toHsConstCastFnName, toHsConstCastFnName',
  toHsDataTypeName, toHsDataTypeName',
  toHsDataCtorName, toHsDataCtorName',
  toHsClassDeleteFnName',
  toHsClassDeleteFnPtrName',
  toHsCtorName, toHsCtorName',
  toHsMethodName, toHsMethodName',
  toHsClassEntityName, toHsClassEntityName',
  -- * Internal
  classFindCopyCtor,
  sayCppExportVar,
  sayHsExportVar,
  ) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except (throwError)
import Data.Char (toUpper)
import Data.Function (on)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.List (intersperse)
import Foreign.Hoppy.Generator.Common (fromMaybeM, lowerFirst)
import {-# SOURCE #-} qualified Foreign.Hoppy.Generator.Language.Cpp as LC
import {-# SOURCE #-} qualified Foreign.Hoppy.Generator.Language.Haskell as LH
import Foreign.Hoppy.Generator.Spec.Base
import qualified Foreign.Hoppy.Generator.Spec.Function as Function
import Foreign.Hoppy.Generator.Types (boolT, constT, fnT, objT, ptrT, refT, voidT)
import GHC.Stack (HasCallStack)
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon, HsTyFun, HsTyVar),
  )

-- | A C++ class declaration.  See 'IsClassEntity' for more information about the
-- interaction between a class's names and the names of entities within the
-- class.
--
-- Use this data type's 'HasReqs' instance to make the class accessible.  You do
-- not need to add requirements for methods' parameter or return types.
data Class = Class
  { classExtName :: ExtName
    -- ^ The class's external name.
  , classIdentifier :: Identifier
    -- ^ The identifier used to refer to the class.
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

instance Exportable Class where
  sayExportCpp = sayCppExport

  sayExportHaskell = sayHsExport

  getExportExceptionClass cls =
    if classIsException cls
    then Just cls
    else Nothing

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
classModifyConversion :: HasCallStack => (ClassConversion -> ClassConversion) -> Class -> Class
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
  { classHaskellConversionType :: Maybe (LH.Generator HsType)
    -- ^ Produces the Haskell type that represents a value of the corresponding
    -- C++ class.  This generator may add imports, but must not output code or
    -- add exports.
  , classHaskellConversionToCppFn :: Maybe (LH.Generator ())
    -- ^ Produces a Haskell expression that evaluates to a function that takes
    -- an value of the type that 'classHaskellConversionType' generates, and
    -- returns a non-const handle for a new C++ object in IO.  The generator
    -- must output code and may add imports, but must not add exports.
    --
    -- If this field is present, then 'classHaskellConversionType' must also be
    -- present.
  , classHaskellConversionFromCppFn :: Maybe (LH.Generator ())
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
  { classVarExtName :: ExtName
    -- ^ The variable's external name.
  , classVarCName :: String
    -- ^ The variable's C++ name.
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
            show $ classVarExtName v, " ",
            show $ classVarCName v, " ",
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
  ClassVariable (extNameOrString cName maybeExtName) cName

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
  , ctorParams :: [Parameter]
    -- ^ The constructor's parameters.
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
makeCtor :: IsParameter p => ExtName -> [p] -> ClassEntity
makeCtor = (CECtor .) . makeCtor_

-- | The unwrapped version of 'makeCtor'.
makeCtor_ :: IsParameter p => ExtName -> [p] -> Ctor
makeCtor_ extName params = Ctor extName (map toParameter params) mempty

-- | @mkCtor name@ creates a 'Ctor' whose external name is @className_name@.
--
-- The result is wrapped in a 'CECtor'.  For an unwrapped value, use
-- 'makeCtor_'.
mkCtor :: IsParameter p => String -> [p] -> ClassEntity
mkCtor = (CECtor .) . mkCtor_

-- | The unwrapped version of 'mkCtor'.
mkCtor_ :: IsParameter p => String -> [p] -> Ctor
mkCtor_ extName params = makeCtor_ (toExtName extName) (map toParameter params)

-- | Searches a class for a copy constructor, returning it if found.
classFindCopyCtor :: Class -> Maybe Ctor
classFindCopyCtor cls = case mapMaybe check $ classCtors cls of
  [ctor] -> Just ctor
  _ -> Nothing
  where check ctor =
          let paramTypes = map (stripConst . normalizeType . parameterType) $ ctorParams ctor
          in if paramTypes == [Internal_TObj cls] ||
                paramTypes == [Internal_TRef $ Internal_TConst $ Internal_TObj cls]
          then Just ctor
          else Nothing

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
  , methodParams :: [Parameter]
    -- ^ The method's parameters.
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
    -- with another function, this is preferrable to just using a
    -- 'Foreign.Hoppy.Generator.Spec.Function.Function' binding because a method
    -- will still appear to be part of the class in foreign bindings.
  deriving (Eq, Show)

-- | How a method is associated to its class.  A method may be static, const, or
-- neither (a regular method).
data MethodApplicability = MNormal | MStatic | MConst
                         deriving (Bounded, Enum, Eq, Show)

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
makeMethod :: (IsFnName String name, IsParameter p)
           => name  -- ^ The C++ name of the method.
           -> ExtName  -- ^ The external name of the method.
           -> MethodApplicability
           -> Purity
           -> [p]  -- ^ Parameter types.
           -> Type  -- ^ Return type.
           -> ClassEntity
makeMethod = (((((CEMethod .) .) .) .) .) . makeMethod_

-- | The unwrapped version of 'makeMethod'.
makeMethod_ :: (IsFnName String name, IsParameter p)
            => name
            -> ExtName
            -> MethodApplicability
            -> Purity
            -> [p]
            -> Type
            -> Method
makeMethod_ cName extName appl purity paramTypes retType =
  Method (RealMethod $ toFnName cName) extName appl purity
         (toParameters paramTypes) retType mempty

-- | Creates a 'Method' that is in fact backed by a C++ non-member function (a
-- la 'Foreign.Hoppy.Generator.Spec.Function.makeFn'), but appears to be a
-- regular method.  This is useful for wrapping a method on the C++ side when
-- its arguments aren't right for binding directly.
--
-- A @this@ pointer parameter is __not__ automatically added to the parameter
-- list for non-static methods created with @makeFnMethod@.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'makeFnMethod_'.
makeFnMethod :: (IsFnName Identifier name, IsParameter p)
             => name
             -> String
             -> MethodApplicability
             -> Purity
             -> [p]
             -> Type
             -> ClassEntity
makeFnMethod = (((((CEMethod .) .) .) .) .) . makeFnMethod_

-- | The unwrapped version of 'makeFnMethod'.
makeFnMethod_ :: (IsFnName Identifier name, IsParameter p)
              => name
              -> String
              -> MethodApplicability
              -> Purity
              -> [p]
              -> Type
              -> Method
makeFnMethod_ cName foreignName appl purity paramTypes retType =
  Method (FnMethod $ toFnName cName) (toExtName foreignName)
         appl purity (toParameters paramTypes) retType mempty

-- | This function is internal.
--
-- Creates a method similar to 'makeMethod', but with automatic naming.  The
-- method's external name will be @className ++ \"_\" ++ cppMethodName@.  If the
-- method name is a 'FnOp' then the 'operatorPreferredExtName' will be appeneded
-- to the class name.
--
-- For creating multiple bindings to a method, see @makeMethod''@.
makeMethod' :: (IsFnName String name, IsParameter p)
            => name  -- ^ The C++ name of the method.
            -> MethodApplicability
            -> Purity
            -> [p]  -- ^ Parameter types.
            -> Type  -- ^ Return type.
            -> Method
makeMethod' name = makeMethod''' (toFnName name) Nothing

-- | This function is internal.
--
-- Creates a method similar to @makeMethod'@, but with an custom string that
-- will be appended to the class name to form the method's external name.  This
-- is useful for making multiple bindings to a method, e.g. for overloading and
-- optional arguments.
makeMethod'' :: (IsFnName String name, IsParameter p)
             => name  -- ^ The C++ name of the method.
             -> String  -- ^ A foreign name for the method.
             -> MethodApplicability
             -> Purity
             -> [p]  -- ^ Parameter types.
             -> Type  -- ^ Return type.
             -> Method
makeMethod'' name foreignName = makeMethod''' (toFnName name) $ Just foreignName

-- | The implementation of @makeMethod'@ and @makeMethod''@.
makeMethod''' :: (HasCallStack, IsParameter p)
              => FnName String  -- ^ The C++ name of the method.
              -> Maybe String  -- ^ A foreign name for the method.
              -> MethodApplicability
              -> Purity
              -> [p]  -- ^ Parameter types.
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
  in makeMethod_ name extName appl purity (toParameters paramTypes) retType

-- | Creates a nonconst, nonstatic 'Method' for @class::methodName@ and whose
-- external name is @class_methodName@.  If the name is an operator, then the
-- 'operatorPreferredExtName' will be used in the external name.
--
-- For creating multiple bindings to a method, see 'mkMethod''.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'mkMethod_'.
mkMethod :: (IsFnName String name, IsParameter p)
         => name  -- ^ The C++ name of the method.
         -> [p]  -- ^ Parameter types.
         -> Type  -- ^ Return type.
         -> ClassEntity
mkMethod = ((CEMethod .) .) . mkMethod_

-- | The unwrapped version of 'mkMethod'.
mkMethod_ :: (IsFnName String name, IsParameter p)
          => name
          -> [p]
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
mkMethod' :: (IsFnName String name, IsParameter p)
          => name  -- ^ The C++ name of the method.
          -> String  -- ^ A foreign name for the method.
          -> [p]  -- ^ Parameter types.
          -> Type  -- ^ Return type.
          -> ClassEntity
mkMethod' = (((CEMethod .) .) .) . mkMethod'_

-- | The unwrapped version of 'mkMethod''.
mkMethod'_ :: (IsFnName String name, IsParameter p)
           => name
           -> String
           -> [p]
           -> Type
           -> Method
mkMethod'_ cName foreignName = makeMethod'' cName foreignName MNormal Nonpure

-- | Same as 'mkMethod', but returns an 'MConst' method.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'mkConstMethod_'.
mkConstMethod :: (IsFnName String name, IsParameter p)
              => name
              -> [p]
              -> Type
              -> ClassEntity
mkConstMethod = ((CEMethod .) .) . mkConstMethod_

-- | The unwrapped version of 'mkConstMethod'.
mkConstMethod_ :: (IsFnName String name, IsParameter p)
               => name
               -> [p]
               -> Type
               -> Method
mkConstMethod_ name = makeMethod' name MConst Nonpure

-- | Same as 'mkMethod'', but returns an 'MConst' method.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'mkConstMethod'_'.
mkConstMethod' :: (IsFnName String name, IsParameter p)
               => name
               -> String
               -> [p]
               -> Type
               -> ClassEntity
mkConstMethod' = (((CEMethod .) .) .) . mkConstMethod'_

-- | The unwrapped version of 'mkConstMethod''.
mkConstMethod'_ :: (IsFnName String name, IsParameter p)
                => name
                -> String
                -> [p]
                -> Type
                -> Method
mkConstMethod'_ cName foreignName = makeMethod'' cName foreignName MConst Nonpure

-- | Same as 'mkMethod', but returns an 'MStatic' method.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'mkStaticMethod_'.
mkStaticMethod :: (IsFnName String name, IsParameter p)
               => name
               -> [p]
               -> Type
               -> ClassEntity
mkStaticMethod = ((CEMethod .) .) . mkStaticMethod_

-- | The unwrapped version of 'mkStaticMethod'.
mkStaticMethod_ :: (IsFnName String name, IsParameter p)
                => name
                -> [p]
                -> Type
                -> Method
mkStaticMethod_ name = makeMethod' name MStatic Nonpure

-- | Same as 'mkMethod'', but returns an 'MStatic' method.
--
-- The result is wrapped in a 'CEMethod'.  For an unwrapped value, use
-- 'mkStaticMethod'_'.
mkStaticMethod' :: (IsFnName String name, IsParameter p)
                => name
                -> String
                -> [p]
                -> Type
                -> ClassEntity
mkStaticMethod' = (((CEMethod .) .) .) . mkStaticMethod'_

-- | The unwrapped version of 'mkStaticMethod''.
mkStaticMethod'_ :: (IsFnName String name, IsParameter p)
                 => name
                 -> String
                 -> [p]
                 -> Type
                 -> Method
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
  in Prop [ mkConstMethod_ name np t
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
  in Prop [ mkStaticMethod_ name np t
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
  in Prop [ mkConstMethod_ isName np boolT
          , mkMethod_ setName [boolT] voidT
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
  in Prop [ mkConstMethod_ hasName np boolT
          , mkMethod_ setName [boolT] voidT
          ]

sayCppExport :: LC.SayExportMode -> Class -> LC.Generator ()
sayCppExport mode cls = case mode of
  LC.SayHeader -> return ()
  LC.SaySource -> do
    let clsPtr = ptrT $ objT cls
        constClsPtr = ptrT $ constT $ objT cls
    -- TODO Is this redundant for a completely empty class?  (No ctors or methods, private dtor.)
    LC.addReqsM $ classReqs cls  -- This is needed at least for the delete function.

    -- Export each of the class's constructors.
    forM_ (classCtors cls) $ \ctor ->
      Function.sayCppExportFn
        (classEntityExtName cls ctor)
        (Function.CallFn $ LC.say "new" >> LC.sayIdentifier (classIdentifier cls))
        Nothing
        (ctorParams ctor)
        clsPtr
        (ctorExceptionHandlers ctor)
        True  -- Render the body.

    -- Export a delete function for the class.
    when (classDtorIsPublic cls) $
      LC.sayFunction (cppDeleteFnName cls)
                     ["self"]
                     (fnT [constClsPtr] voidT) $
        Just $ LC.say "delete self;\n"

    -- Export each of the class's variables.
    forM_ (classVariables cls) $ sayCppExportClassVar cls

    -- Export each of the class's methods.
    forM_ (classMethods cls) $ \method -> do
      let static = case methodStatic method of
            Static -> True
            Nonstatic -> False
          thisType = case methodConst method of
            Const -> constClsPtr
            Nonconst -> clsPtr
          nonMemberCall = static || case methodImpl method of
            RealMethod {} -> False
            FnMethod {} -> True
      Function.sayCppExportFn
        (classEntityExtName cls method)
        (case methodImpl method of
           RealMethod name -> case name of
             FnName cName -> Function.CallFn $ do
               when static $ do
                 LC.sayIdentifier (classIdentifier cls)
                 LC.say "::"
               LC.say cName
             FnOp op -> Function.CallOp op
           FnMethod name -> case name of
             FnName cName -> Function.CallFn $ LC.sayIdentifier cName
             FnOp op -> Function.CallOp op)
        (if nonMemberCall then Nothing else Just thisType)
        (methodParams method)
        (methodReturn method)
        (methodExceptionHandlers method)
        True  -- Render the body.

    -- Export upcast functions for the class to its direct superclasses.
    forM_ (classSuperclasses cls) $ genUpcastFns cls
    -- Export downcast functions from the class's direct and indirect
    -- superclasses to it.
    unless (classIsSubclassOfMonomorphic cls) $
      forM_ (classSuperclasses cls) $ genDowncastFns cls

  where genUpcastFns :: Class -> Class -> LC.Generator ()
        genUpcastFns cls' ancestorCls = do
          LC.sayFunction (cppCastFnName cls' ancestorCls)
                         ["self"]
                         (fnT [ptrT $ constT $ objT cls'] $ ptrT $ constT $ objT ancestorCls)
                         (Just $ LC.say "return self;\n")
          forM_ (classSuperclasses ancestorCls) $ genUpcastFns cls'

        genDowncastFns :: Class -> Class -> LC.Generator ()
        genDowncastFns cls' ancestorCls = unless (classIsMonomorphicSuperclass ancestorCls) $ do
          let clsPtr = ptrT $ constT $ objT cls'
              ancestorPtr = ptrT $ constT $ objT ancestorCls
          LC.sayFunction (cppCastFnName ancestorCls cls')
                         ["self"]
                         (fnT [ancestorPtr] clsPtr) $ Just $ do
            LC.say "return dynamic_cast<"
            LC.sayType Nothing clsPtr
            LC.say ">(self);\n"
          forM_ (classSuperclasses ancestorCls) $ genDowncastFns cls'

sayCppExportClassVar :: Class -> ClassVariable -> LC.Generator ()
sayCppExportClassVar cls v =
  sayCppExportVar (classVarType v)
                  (case classVarStatic v of
                     Nonstatic -> Just (ptrT $ constT $ objT cls, ptrT $ objT cls)
                     Static -> Nothing)
                  (classVarGettable v)
                  (classVarGetterExtName cls v)
                  (classVarSetterExtName cls v)
                  (case classVarStatic v of
                     Nonstatic -> LC.say $ classVarCName v
                     Static -> do LC.sayIdentifier $ classIdentifier cls
                                  LC.says ["::", classVarCName v])

makeClassCppName :: String -> Class -> String
makeClassCppName prefix cls = LC.makeCppName [prefix, fromExtName $ classExtName cls]

-- | \"gendel\" is the prefix used for wrappers for @delete@ calls.
cppDeleteFnPrefix :: String
cppDeleteFnPrefix = "gendel"

-- | Returns the C++ binding function name of the wrapper for the delete method
-- for a class.
cppDeleteFnName :: Class -> String
cppDeleteFnName = makeClassCppName cppDeleteFnPrefix

-- | @cppCastFnName fromCls toCls@ returns the name of the generated C++
-- function that casts a pointer from @fromCls@ to @toCls@.
cppCastFnName :: Class -> Class -> String
cppCastFnName from to =
  concat [ "gencast__"
         , fromExtName $ classExtName from
         , "__"
         , fromExtName $ classExtName to
         ]

sayHsExport :: LH.SayExportMode -> Class -> LH.Generator ()
sayHsExport mode cls = LH.withErrorContext ("generating class " ++ show (classExtName cls)) $ do
  case mode of
    LH.SayExportForeignImports -> do
      sayHsExportClassVars mode cls
      sayHsExportClassCtors mode cls

      forM_ (classMethods cls) $ \method ->
        (Function.sayHsExportFn mode <$> classEntityExtName cls <*> classEntityForeignName cls <*>
         methodPurity <*> pure (getMethodEffectiveParams cls method) <*>
         methodReturn <*> methodExceptionHandlers)
        method

    LH.SayExportDecls -> do
      sayHsExportClassClass True cls Const
      sayHsExportClassClass True cls Nonconst

      sayHsExportClassStaticMethods cls

      -- Create a newtype for referencing foreign objects with pointers.  The
      -- newtype is not used with encodings of value objects.
      sayHsExportClassDataType True cls Const
      sayHsExportClassDataType True cls Nonconst

      sayHsExportClassExceptionSupport True cls

      sayHsExportClassVars mode cls
      sayHsExportClassCtors mode cls

    LH.SayExportBoot -> do
      sayHsExportClassClass False cls Const
      sayHsExportClassClass False cls Nonconst

      sayHsExportClassDataType False cls Const
      sayHsExportClassDataType False cls Nonconst

      sayHsExportClassExceptionSupport False cls

      sayHsExportClassVars mode cls

  sayHsExportClassCastPrimitives mode cls
  sayHsExportClassSpecialFns mode cls

sayHsExportClassClass :: Bool -> Class -> Constness -> LH.Generator ()
sayHsExportClassClass doDecls cls cst = LH.withErrorContext "generating Haskell typeclass" $ do
  hsTypeName <- toHsDataTypeName cst cls
  hsValueClassName <- toHsValueClassName cls
  hsWithValuePtrName <- toHsWithValuePtrName cls
  hsPtrClassName <- toHsPtrClassName cst cls
  hsCastMethodName <- toHsCastMethodName cst cls
  let supers = classSuperclasses cls

  hsSupers <-
    (\x -> if null x
           then do LH.addImports hsImportForRuntime
                   return ["HoppyFHR.CppPtr"]
           else return x) =<<
    case cst of
      Const -> mapM (toHsPtrClassName Const) supers
      Nonconst ->
        (:) <$> toHsPtrClassName Const cls <*> mapM (toHsPtrClassName Nonconst) supers

  -- Print the value class definition.  There is only one of these, and it is
  -- spiritually closer to the const version of the pointers for this class, so
  -- we emit for the const case only.
  when (cst == Const) $ do
    LH.addImports hsImportForPrelude
    LH.addExport' hsValueClassName
    LH.ln
    LH.saysLn ["class ", hsValueClassName, " a where"]
    LH.indent $
      LH.saysLn [hsWithValuePtrName, " :: a -> (", hsTypeName, " -> HoppyP.IO b) -> HoppyP.IO b"]

    -- Generate instances for all pointer subtypes.
    LH.ln
    LH.saysLn ["instance {-# OVERLAPPABLE #-} ", hsPtrClassName, " a => ", hsValueClassName, " a",
               if doDecls then " where" else ""]
    when doDecls $ do
      LH.addImports $ mconcat [hsImports "Prelude" ["($)", "(.)"],
                               hsImportForPrelude]
      LH.indent $ LH.saysLn [hsWithValuePtrName, " = HoppyP.flip ($) . ", hsCastMethodName]

    -- When the class is encodable to a native Haskell type, also print an
    -- instance for it.
    let conv = LH.getClassHaskellConversion cls
    case (classHaskellConversionType conv,
          classHaskellConversionToCppFn conv) of
      (Just hsTypeGen, Just _) -> do
        hsType <- hsTypeGen
        LH.ln
        LH.saysLn ["instance {-# OVERLAPPING #-} ", hsValueClassName,
                   " (", LH.prettyPrint hsType, ")", if doDecls then " where" else ""]
        when doDecls $ do
          LH.addImports hsImportForRuntime
          LH.indent $ LH.saysLn [hsWithValuePtrName, " = HoppyFHR.withCppObj"]
      _ -> return ()

  -- Print the pointer class definition.
  LH.addExport' hsPtrClassName
  LH.ln
  LH.saysLn $
    "class (" :
    intersperse ", " (map (++ " this") hsSupers) ++
    [") => ", hsPtrClassName, " this where"]
  LH.indent $ LH.saysLn [hsCastMethodName, " :: this -> ", hsTypeName]

  -- Print the non-static methods.
  when doDecls $ do
    let methods = filter ((cst ==) . methodConst) $ classMethods cls
    forM_ methods $ \method ->
      when (methodStatic method == Nonstatic) $
      (Function.sayHsExportFn LH.SayExportDecls <$>
       classEntityExtName cls <*> classEntityForeignName cls <*>
       methodPurity <*> pure (getMethodEffectiveParams cls method) <*>
       methodReturn <*> methodExceptionHandlers) method

sayHsExportClassStaticMethods :: Class -> LH.Generator ()
sayHsExportClassStaticMethods cls =
  forM_ (classMethods cls) $ \method ->
    when (methodStatic method == Static) $
    (Function.sayHsExportFn LH.SayExportDecls <$>
     classEntityExtName cls <*> classEntityForeignName cls <*>
     methodPurity <*> methodParams <*> methodReturn <*> methodExceptionHandlers) method

sayHsExportClassDataType :: Bool -> Class -> Constness -> LH.Generator ()
sayHsExportClassDataType doDecls cls cst = LH.withErrorContext "generating Haskell data types" $ do
  hsTypeName <- toHsDataTypeName cst cls
  hsCtor <- toHsDataCtorName LH.Unmanaged cst cls
  hsCtorGc <- toHsDataCtorName LH.Managed cst cls
  constCastFnName <- toHsConstCastFnName cst cls

  LH.addImports $ mconcat [hsImportForForeign, hsImportForPrelude, hsImportForRuntime]
  -- Unfortunately, we must export the data constructor, so that GHC can marshal
  -- it in foreign calls in other modules.
  LH.addExport' hsTypeName
  LH.ln
  LH.saysLn ["data ", hsTypeName, " ="]
  LH.indent $ do
    LH.saysLn ["  ", hsCtor, " (HoppyF.Ptr ", hsTypeName, ")"]
    LH.saysLn ["| ", hsCtorGc, " (HoppyF.ForeignPtr ()) (HoppyF.Ptr ", hsTypeName, ")"]
  when doDecls $ do
    LH.addImports $ hsImport1 "Prelude" "(==)"
    LH.indent $ LH.sayLn "deriving (HoppyP.Show)"
    LH.ln
    LH.saysLn ["instance HoppyP.Eq ", hsTypeName, " where"]
    LH.indent $ LH.saysLn ["x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y"]
    LH.ln
    LH.saysLn ["instance HoppyP.Ord ", hsTypeName, " where"]
    LH.indent $ LH.saysLn ["compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)"]

  -- Generate const_cast functions:
  --   castFooToConst :: Foo -> FooConst
  --   castFooToNonconst :: FooConst -> Foo
  hsTypeNameOppConst <- toHsDataTypeName (constNegate cst) cls
  LH.ln
  LH.addExport constCastFnName
  LH.saysLn [constCastFnName, " :: ", hsTypeNameOppConst, " -> ", hsTypeName]
  when doDecls $ do
    LH.addImports $ hsImport1 "Prelude" "($)"
    hsCtorOppConst <- toHsDataCtorName LH.Unmanaged (constNegate cst) cls
    hsCtorGcOppConst <- toHsDataCtorName LH.Managed (constNegate cst) cls
    LH.saysLn [constCastFnName, " (", hsCtorOppConst,
               " ptr') = ", hsCtor, " $ HoppyF.castPtr ptr'"]
    LH.saysLn [constCastFnName, " (", hsCtorGcOppConst,
               " fptr' ptr') = ", hsCtorGc, " fptr' $ HoppyF.castPtr ptr'"]

  -- Generate an instance of CppPtr.
  LH.ln
  if doDecls
    then do LH.addImports $ hsImport1 "Prelude" "($)"
            LH.saysLn ["instance HoppyFHR.CppPtr ", hsTypeName, " where"]
            LH.indent $ do
              LH.saysLn ["nullptr = ", hsCtor, " HoppyF.nullPtr"]
              LH.ln
              LH.saysLn ["withCppPtr (", hsCtor, " ptr') f' = f' ptr'"]
              LH.saysLn ["withCppPtr (", hsCtorGc,
                         " fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \\_ -> f' ptr'"]
              LH.ln
              LH.saysLn ["toPtr (", hsCtor, " ptr') = ptr'"]
              LH.saysLn ["toPtr (", hsCtorGc, " _ ptr') = ptr'"]
              LH.ln
              LH.saysLn ["touchCppPtr (", hsCtor, " _) = HoppyP.return ()"]
              LH.saysLn ["touchCppPtr (", hsCtorGc, " fptr' _) = HoppyF.touchForeignPtr fptr'"]

            when (classDtorIsPublic cls) $ do
              LH.addImports $ hsImport1 "Prelude" "(==)"
              LH.ln
              LH.saysLn ["instance HoppyFHR.Deletable ", hsTypeName, " where"]
              LH.indent $ do
                -- Note, similar "delete" and "toGc" functions are generated for exception
                -- classes' ExceptionClassInfo structures.
                case cst of
                  Const ->
                    LH.saysLn ["delete (", hsCtor, " ptr') = ", toHsClassDeleteFnName' cls, " ptr'"]
                  Nonconst -> do
                    constTypeName <- toHsDataTypeName Const cls
                    LH.saysLn ["delete (",hsCtor, " ptr') = ", toHsClassDeleteFnName' cls,
                               " $ (HoppyF.castPtr ptr' :: HoppyF.Ptr ", constTypeName, ")"]
                LH.saysLn ["delete (", hsCtorGc,
                           " _ _) = HoppyP.fail $ HoppyP.concat ",
                           "[\"Deletable.delete: Asked to delete a GC-managed \", ",
                           show hsTypeName, ", \" object.\"]"]
                LH.ln
                LH.saysLn ["toGc this'@(", hsCtor, " ptr') = ",
                           -- No sense in creating a ForeignPtr for a null pointer.
                           "if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap ",
                           "(HoppyP.flip ", hsCtorGc, " ptr') $ ",
                           "HoppyF.newForeignPtr ",
                           -- The foreign delete function takes a const pointer; we cast it to
                           -- take a Ptr () to match up with the ForeignPtr () we're creating,
                           -- assuming that data pointers have the same representation.
                           "(HoppyF.castFunPtr ", toHsClassDeleteFnPtrName' cls,
                           " :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) ",
                           "(HoppyF.castPtr ptr' :: HoppyF.Ptr ())"]
                LH.saysLn ["toGc this'@(", hsCtorGc, " {}) = HoppyP.return this'"]

            forM_ (classFindCopyCtor cls) $ \copyCtor -> do
              copyCtorName <- toHsCtorName cls copyCtor
              LH.ln
              LH.saysLn ["instance HoppyFHR.Copyable ", hsTypeName, " ",
                         case cst of
                           Nonconst -> hsTypeName
                           Const -> hsTypeNameOppConst,
                         " where copy = ", copyCtorName]

    else do LH.saysLn ["instance HoppyFHR.CppPtr ", hsTypeName]

            when (classDtorIsPublic cls) $
              LH.saysLn ["instance HoppyFHR.Deletable ", hsTypeName]

            forM_ (classFindCopyCtor cls) $ \_ ->
              LH.saysLn ["instance HoppyFHR.Copyable ", hsTypeName, " ",
                         case cst of
                           Nonconst -> hsTypeName
                           Const -> hsTypeNameOppConst]

  -- Generate instances for all superclasses' typeclasses.
  genInstances hsTypeName [] cls

  where genInstances :: String -> [Class] -> Class -> LH.Generator ()
        genInstances hsTypeName path ancestorCls = do
          -- In this example Bar inherits from Foo.  We are generating instances
          -- either for BarConst or Bar, depending on 'cst'.
          --
          -- BarConst's instances:
          --   instance FooConstPtr BarConst where
          --     toFooConst (BarConst ptr') = FooConst $ castBarToFoo ptr'
          --     toFooConst (BarConstGc fptr' ptr') = FooConstGc fptr' $ castBarToFoo ptr'
          --
          --   instance BarConstPtr BarConst where
          --     toFooConst = id
          --
          -- Bar's instances:
          --   instance FooConstPtr Bar
          --     toFooConst (Bar ptr') =
          --       FooConst $ castBarToFoo $ castBarToConst ptr'
          --     toFooConst (BarGc fptr' ptr') =
          --       FooConstGc fptr' $ castBarToFoo $ castBarToConst ptr'
          --
          --   instance FooPtr Bar
          --     toFoo (Bar ptr') =
          --       Foo $ castFooToNonconst $ castBarToFoo $ castBarToConst ptr'
          --     toFoo (BarGc fptr' ptr') =
          --       FooGc fptr' $ castFooToNonconst $ castBarToFoo $ castBarToConst ptr'
          --
          --   instance BarConstPtr Bar
          --     toBarConst (Bar ptr') = Bar $ castBarToConst ptr'
          --     toBarConst (BarGc fptr' ptr') = BarGc fptr' $ castBarToConst ptr'
          --
          --   instance BarPtr Bar
          --     toBar = id
          --
          -- In all cases, we unwrap the pointer, maybe add const, maybe do an
          -- upcast, maybe remove const, then rewrap the pointer.  The identity
          -- cases are where we just unwrap and wrap again.

          forM_ (case cst of
                   Const -> [Const]
                   Nonconst -> [Const, Nonconst]) $ \ancestorCst -> do
            LH.ln
            ancestorPtrClassName <- toHsPtrClassName ancestorCst ancestorCls
            LH.saysLn ["instance ", ancestorPtrClassName, " ", hsTypeName,
                       if doDecls then " where" else ""]
            when doDecls $ LH.indent $ do
              -- Unqualified, for Haskell instance methods.
              let castMethodName = toHsCastMethodName' ancestorCst ancestorCls
              if null path && cst == ancestorCst
                then do LH.addImports hsImportForPrelude
                        LH.saysLn [castMethodName, " = HoppyP.id"]
                else do let addConst = cst == Nonconst
                            removeConst = ancestorCst == Nonconst
                        when (addConst || removeConst) $
                          LH.addImports hsImportForForeign
                        forM_ ([minBound..] :: [LH.Managed]) $ \managed -> do
                          ancestorCtor <- case managed of
                            LH.Unmanaged -> (\x -> [x]) <$>
                                            toHsDataCtorName LH.Unmanaged ancestorCst ancestorCls
                            LH.Managed -> (\x -> [x, " fptr'"]) <$>
                                          toHsDataCtorName LH.Managed ancestorCst ancestorCls
                          ptrPattern <- case managed of
                            LH.Unmanaged -> (\x -> [x, " ptr'"]) <$>
                                            toHsDataCtorName LH.Unmanaged cst cls
                            LH.Managed -> (\x -> [x, " fptr' ptr'"]) <$>
                                          toHsDataCtorName LH.Managed cst cls
                          LH.saysLn . concat =<< sequence
                            [ return $
                              [castMethodName, " ("] ++ ptrPattern ++ [") = "] ++ ancestorCtor
                            , if removeConst
                              then do ancestorConstType <- toHsDataTypeName Const ancestorCls
                                      ancestorNonconstType <- toHsDataTypeName Nonconst ancestorCls
                                      return [" $ (HoppyF.castPtr :: HoppyF.Ptr ",
                                              ancestorConstType, " -> HoppyF.Ptr ",
                                              ancestorNonconstType, ")"]
                              else return []
                            , if not $ null path
                              then do LH.addImports $ hsImport1 "Prelude" "($)"
                                      castPrimitiveName <- toHsCastPrimitiveName cls cls ancestorCls
                                      return [" $ ", castPrimitiveName]
                              else return []
                            , if addConst
                              then do LH.addImports $ hsImport1 "Prelude" "($)"
                                      nonconstTypeName <- toHsDataTypeName Nonconst cls
                                      constTypeName <- toHsDataTypeName Const cls
                                      return [" $ (HoppyF.castPtr :: HoppyF.Ptr ",
                                              nonconstTypeName, " -> HoppyF.Ptr ",
                                              constTypeName, ")"]
                              else return []
                            , return [" ptr'"]
                            ]

          forM_ (classSuperclasses ancestorCls) $
            genInstances hsTypeName $
            ancestorCls : path

sayHsExportClassVars :: LH.SayExportMode -> Class -> LH.Generator ()
sayHsExportClassVars mode cls =
  forM_ (classVariables cls) $ sayHsExportClassVar mode cls

sayHsExportClassVar :: LH.SayExportMode -> Class -> ClassVariable -> LH.Generator ()
sayHsExportClassVar mode cls v =
  LH.withErrorContext ("generating variable " ++ show (classVarExtName v)) $
  sayHsExportVar mode
                 (classVarType v)
                 (case classVarStatic v of
                    Nonstatic -> Just cls
                    Static -> Nothing)
                 (classVarGettable v)
                 (classVarGetterExtName cls v)
                 (classVarGetterForeignName cls v)
                 (classVarSetterExtName cls v)
                 (classVarSetterForeignName cls v)

sayHsExportClassCtors :: LH.SayExportMode -> Class -> LH.Generator ()
sayHsExportClassCtors mode cls =
  LH.withErrorContext "generating constructors" $
  forM_ (classCtors cls) $ \ctor ->
  (Function.sayHsExportFn mode <$>
   classEntityExtName cls <*> classEntityForeignName cls <*>
   pure Nonpure <*> ctorParams <*> pure (ptrT $ objT cls) <*>
   ctorExceptionHandlers) ctor

sayHsExportClassSpecialFns :: LH.SayExportMode -> Class -> LH.Generator ()
sayHsExportClassSpecialFns mode cls = do
  typeName <- toHsDataTypeName Nonconst cls
  typeNameConst <- toHsDataTypeName Const cls

  -- Say the delete function.
  LH.withErrorContext "generating delete bindings" $
    case mode of
      LH.SayExportForeignImports -> when (classDtorIsPublic cls) $ do
        LH.addImports $ mconcat [hsImportForForeign, hsImportForPrelude]
        LH.saysLn ["foreign import ccall \"", cppDeleteFnName cls, "\" ",
                   toHsClassDeleteFnName' cls, " :: HoppyF.Ptr ",
                   typeNameConst, " -> HoppyP.IO ()"]
        LH.saysLn ["foreign import ccall \"&", cppDeleteFnName cls, "\" ",
                   toHsClassDeleteFnPtrName' cls, " :: HoppyF.FunPtr (HoppyF.Ptr ",
                   typeNameConst, " -> HoppyP.IO ())"]
      -- The user interface to this is the generic 'delete' function, rendered
      -- elsewhere.
      LH.SayExportDecls -> return ()
      LH.SayExportBoot -> return ()

  LH.withErrorContext "generating pointer Assignable instance" $
    case mode of
      LH.SayExportForeignImports -> return ()
      LH.SayExportDecls -> do
        LH.addImports $ mconcat [hsImport1 "Prelude" "($)",
                                 hsImportForForeign,
                                 hsImportForRuntime]
        LH.ln
        LH.saysLn ["instance HoppyFHR.Assignable (HoppyF.Ptr (HoppyF.Ptr ", typeName, ")) ",
                   typeName, " where"]
        LH.indent $ LH.sayLn "assign ptr' value' = HoppyF.poke ptr' $ HoppyFHR.toPtr value'"
      LH.SayExportBoot -> return ()

  -- If the class has an assignment operator that takes its own type, then
  -- generate an instance of Assignable.
  LH.withErrorContext "generating Assignable instance" $ do
    let assignmentMethods = flip filter (classMethods cls) $ \m ->
          let paramTypes = map parameterType $ methodParams m
          in methodApplicability m == MNormal &&
             (paramTypes == [objT cls] || paramTypes == [refT $ constT $ objT cls]) &&
             (case methodImpl m of
               RealMethod name -> name == FnOp OpAssign
               FnMethod name -> name == FnOp OpAssign)
        withAssignmentMethod f = case assignmentMethods of
          [] -> return ()
          [m] -> f m
          _ ->
            throwError $ concat
            ["Can't determine an Assignable instance to generator for ", show cls,
            " because it has multiple assignment operators ", show assignmentMethods]
    when (mode == LH.SayExportDecls) $ withAssignmentMethod $ \m -> do
      LH.addImports $ mconcat [hsImport1 "Prelude" "(>>)", hsImportForPrelude]
      valueClassName <- toHsValueClassName cls
      assignmentMethodName <- toHsMethodName cls m
      LH.ln
      LH.saysLn ["instance ", valueClassName, " a => HoppyFHR.Assignable ", typeName, " a where"]
      LH.indent $
        LH.saysLn ["assign x' y' = ", assignmentMethodName, " x' y' >> HoppyP.return ()"]

  -- A pointer to an object pointer is decodable to an object pointer by peeking
  -- at the value, so generate a Decodable instance.  You are now a two-star
  -- programmer.  There is a generic @Ptr (Ptr a)@ to @Ptr a@ instance which
  -- handles deeper levels.
  LH.withErrorContext "generating pointer Decodable instance" $ do
    case mode of
      LH.SayExportForeignImports -> return ()

      LH.SayExportDecls -> do
        LH.addImports $ mconcat [hsImport1 "Prelude" "(.)",
                                 hsImportForForeign,
                                 hsImportForPrelude,
                                 hsImportForRuntime]
        LH.ln
        LH.saysLn ["instance HoppyFHR.Decodable (HoppyF.Ptr (HoppyF.Ptr ",
                   typeName, ")) ", typeName, " where"]
        LH.indent $ do
          ctorName <- toHsDataCtorName LH.Unmanaged Nonconst cls
          LH.saysLn ["decode = HoppyP.fmap ", ctorName, " . HoppyF.peek"]

      LH.SayExportBoot -> do
        LH.addImports $ mconcat [hsImportForForeign, hsImportForRuntime]
        LH.ln
        -- TODO Encodable.
        LH.saysLn ["instance HoppyFHR.Decodable (HoppyF.Ptr (HoppyF.Ptr ", typeName, ")) ",
                   typeName]

  -- Say Encodable and Decodable instances, if the class is encodable and
  -- decodable.
  LH.withErrorContext "generating Encodable/Decodable instances" $ do
    let conv = LH.getClassHaskellConversion cls
    forM_ (classHaskellConversionType conv) $ \hsTypeGen -> do
      let hsTypeStrGen = hsTypeGen >>= \hsType -> return $ "(" ++ LH.prettyPrint hsType ++ ")"

      case mode of
        LH.SayExportForeignImports -> return ()

        LH.SayExportDecls -> do
          -- Say the Encodable instances.
          forM_ (classHaskellConversionToCppFn conv) $ \toCppFnGen -> do
            hsTypeStr <- hsTypeStrGen
            LH.addImports $ mconcat [hsImportForPrelude, hsImportForRuntime]
            castMethodName <- toHsCastMethodName Const cls

            LH.ln
            LH.saysLn ["instance HoppyFHR.Encodable ", typeName, " ", hsTypeStr, " where"]
            LH.indent $ do
              LH.sayLn "encode ="
              LH.indent toCppFnGen
            LH.ln
            LH.saysLn ["instance HoppyFHR.Encodable ", typeNameConst, " ", hsTypeStr, " where"]
            LH.indent $
              LH.saysLn ["encode = HoppyP.fmap (", castMethodName,
                         ") . HoppyFHR.encodeAs (HoppyP.undefined :: ", typeName, ")"]

          -- Say the Decodable instances.
          forM_ (classHaskellConversionFromCppFn conv) $ \fromCppFnGen -> do
            hsTypeStr <- hsTypeStrGen
            LH.addImports hsImportForRuntime
            castMethodName <- toHsCastMethodName Const cls

            LH.ln
            LH.saysLn ["instance HoppyFHR.Decodable ", typeName, " ", hsTypeStr, " where"]
            LH.indent $
              LH.saysLn ["decode = HoppyFHR.decode . ", castMethodName]
            LH.ln
            LH.saysLn ["instance HoppyFHR.Decodable ", typeNameConst, " ", hsTypeStr, " where"]
            LH.indent $ do
              LH.sayLn "decode ="
              LH.indent fromCppFnGen

        LH.SayExportBoot -> do
          -- Say the Encodable instances.
          forM_ (classHaskellConversionToCppFn conv) $ \_ -> do
            hsTypeStr <- hsTypeStrGen
            LH.addImports hsImportForRuntime
            LH.ln
            LH.saysLn ["instance HoppyFHR.Encodable ", typeName, " (", hsTypeStr, ")"]
            LH.saysLn ["instance HoppyFHR.Encodable ", typeNameConst, " (", hsTypeStr, ")"]

          -- Say the Decodable instances.
          forM_ (classHaskellConversionFromCppFn conv) $ \_ -> do
            hsTypeStr <- hsTypeStrGen
            LH.addImports hsImportForRuntime
            LH.ln
            LH.saysLn ["instance HoppyFHR.Decodable ", typeName, " (", hsTypeStr, ")"]
            LH.saysLn ["instance HoppyFHR.Decodable ", typeNameConst, " (", hsTypeStr, ")"]

-- | Generates a non-const @CppException@ instance if the class is an exception
-- class.
sayHsExportClassExceptionSupport :: Bool -> Class -> LH.Generator ()
sayHsExportClassExceptionSupport doDecls cls =
  when (classIsException cls) $
  LH.withErrorContext "generating exception support" $ do
  typeName <- toHsDataTypeName Nonconst cls
  typeNameConst <- toHsDataTypeName Const cls

  -- Generate a non-const CppException instance.
  exceptionId <- getHsClassExceptionId cls
  LH.addImports hsImportForRuntime
  LH.ln
  LH.saysLn ["instance HoppyFHR.CppException ", typeName,
             if doDecls then " where" else ""]
  when doDecls $ LH.indent $ do
    ctorName <- toHsDataCtorName LH.Unmanaged Nonconst cls
    ctorGcName <- toHsDataCtorName LH.Managed Nonconst cls
    LH.addImports $ mconcat [hsImports "Prelude" ["($)", "(.)", "(=<<)"],
                             hsImportForForeign,
                             hsImportForMap,
                             hsImportForPrelude]
    LH.sayLn "cppExceptionInfo _ ="
    LH.indent $ do
      LH.saysLn ["HoppyFHR.ExceptionClassInfo (HoppyFHR.ExceptionId ",
                 show $ getExceptionId exceptionId, ") ", show typeName,
                 " upcasts' delete' copy' toGc'"]

      -- Note, similar "delete" and "toGc" functions are generated for the class's
      -- Deletable instance.
      LH.saysLn ["where delete' ptr' = ", toHsClassDeleteFnName' cls,
                 " (HoppyF.castPtr ptr' :: HoppyF.Ptr ", typeNameConst, ")"]

      LH.indentSpaces 6 $ do
        LH.ln
        LH.saysLn ["copy' = HoppyP.fmap (HoppyF.castPtr . HoppyFHR.toPtr) . HoppyFHR.copy . ",
                   ctorName, " . HoppyF.castPtr"]

        LH.ln
        LH.saysLn ["toGc' ptr' = HoppyF.newForeignPtr ",
                   -- The foreign delete function takes a const pointer; we cast it to
                   -- take a Ptr () to match up with the ForeignPtr () we're creating,
                   -- assuming that data pointers have the same representation.
                   "(HoppyF.castFunPtr ", toHsClassDeleteFnPtrName' cls,
                   " :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) ",
                   "ptr'"]

        LH.sayLn "upcasts' = HoppyDM.fromList"
        LH.indent $ case classSuperclasses cls of
          [] -> LH.sayLn "[]"
          _ -> do
            let genCast :: Bool -> [Class] -> Class -> LH.Generator ()
                genCast first path ancestorCls =
                  when (classIsException ancestorCls) $ do
                    let path' = ancestorCls : path
                    ancestorId <- getHsClassExceptionId ancestorCls
                    ancestorCastChain <- forM (zip path' $ drop 1 path') $ \(to, from) ->
                      -- We're upcasting, so 'from' is the subclass.
                      toHsCastPrimitiveName from from to
                    LH.saysLn $ concat [ [if first then "[" else ",",
                                          " ( HoppyFHR.ExceptionId ",
                                          show $ getExceptionId ancestorId,
                                          ", \\(e' :: HoppyF.Ptr ()) -> "]
                                       , intersperse " $ " $
                                           "HoppyF.castPtr" :
                                           ancestorCastChain ++
                                           ["HoppyF.castPtr e' :: HoppyF.Ptr ()"]
                                       , [")"]
                                       ]
                    forM_ (classSuperclasses ancestorCls) $ genCast False path'

            forM_ (zip (classSuperclasses cls) (True : repeat False)) $
              \(ancestorCls, first) -> genCast first [cls] ancestorCls
            LH.sayLn "]"

    LH.ln
    LH.saysLn ["cppExceptionBuild fptr' ptr' = ", ctorGcName,
               " fptr' (HoppyF.castPtr ptr' :: HoppyF.Ptr ", typeName, ")"]
    LH.ln
    LH.saysLn ["cppExceptionBuildToGc ptr' = HoppyFHR.toGc $ ", ctorName,
               " (HoppyF.castPtr ptr' :: HoppyF.Ptr ", typeName, ")"]

  -- Generate a const CppException instance that piggybacks off of the
  -- non-const implementation.
  LH.ln
  LH.saysLn ["instance HoppyFHR.CppException ", typeNameConst,
             if doDecls then " where" else ""]
  when doDecls $ LH.indent $ do
    LH.addImports $ mconcat [hsImport1 "Prelude" "(.)",
                             hsImportForPrelude]
    constCastFnName <- toHsConstCastFnName Const cls
    LH.saysLn ["cppExceptionInfo _ = HoppyFHR.cppExceptionInfo (HoppyP.undefined :: ",
               typeName, ")"]
    LH.saysLn ["cppExceptionBuild = (", constCastFnName,
               " .) . HoppyFHR.cppExceptionBuild"]
    LH.saysLn ["cppExceptionBuildToGc = HoppyP.fmap ", constCastFnName,
               " . HoppyFHR.cppExceptionBuildToGc"]

  -- Generate a non-const CppThrowable instance.
  LH.ln
  LH.saysLn ["instance HoppyFHR.CppThrowable ", typeName,
             if doDecls then " where" else ""]
  when doDecls $ LH.indent $ do
    ctorName <- toHsDataCtorName LH.Unmanaged Nonconst cls
    ctorGcName <- toHsDataCtorName LH.Managed Nonconst cls
    LH.addImports $ mconcat [hsImportForForeign,
                             hsImportForPrelude]
    LH.saysLn ["toSomeCppException this'@(", ctorName, " ptr') = ",
               "HoppyFHR.SomeCppException (HoppyFHR.cppExceptionInfo this') HoppyP.Nothing ",
               "(HoppyF.castPtr ptr')"]
    LH.saysLn ["toSomeCppException this'@(", ctorGcName, " fptr' ptr') = ",
               "HoppyFHR.SomeCppException (HoppyFHR.cppExceptionInfo this') (HoppyP.Just fptr') ",
               "(HoppyF.castPtr ptr')"]

sayHsExportClassCastPrimitives :: LH.SayExportMode -> Class -> LH.Generator ()
sayHsExportClassCastPrimitives mode cls = LH.withErrorContext "generating cast primitives" $ do
  clsType <- toHsDataTypeName Const cls
  case mode of
    LH.SayExportForeignImports ->
      forAncestors cls $ \super -> do
        hsCastFnName <- toHsCastPrimitiveName cls cls super
        hsDownCastFnName <- toHsCastPrimitiveName cls super cls
        superType <- toHsDataTypeName Const super
        LH.addImports hsImportForForeign
        LH.addExport hsCastFnName
        LH.saysLn [ "foreign import ccall \"", cppCastFnName cls super
                  , "\" ", hsCastFnName, " :: HoppyF.Ptr ", clsType, " -> HoppyF.Ptr ", superType
                  ]
        unless (classIsSubclassOfMonomorphic cls || classIsMonomorphicSuperclass super) $ do
          LH.addExport hsDownCastFnName
          LH.saysLn [ "foreign import ccall \"", cppCastFnName super cls
                    , "\" ", hsDownCastFnName, " :: HoppyF.Ptr ", superType, " -> HoppyF.Ptr "
                    , clsType
                    ]
        return True

    LH.SayExportDecls ->
      -- Generate a downcast typeclass and instances for all ancestor classes
      -- for the current constness.  These don't need to be in the boot file,
      -- since they're not used by other generated bindings.
      unless (classIsSubclassOfMonomorphic cls) $
      forM_ [minBound..] $ \cst -> do
        downCastClassName <- toHsDownCastClassName cst cls
        downCastMethodName <- toHsDownCastMethodName cst cls
        typeName <- toHsDataTypeName cst cls
        LH.addExport' downCastClassName
        LH.ln
        LH.saysLn ["class ", downCastClassName, " a where"]
        LH.indent $ LH.saysLn [downCastMethodName, " :: ",
                            LH.prettyPrint $ HsTyFun (HsTyVar $ HsIdent "a") $
                            HsTyCon $ UnQual $ HsIdent typeName]
        LH.ln
        forAncestors cls $ \super -> case classIsMonomorphicSuperclass super of
          True -> return False
          False -> do
            superTypeName <- toHsDataTypeName cst super
            primitiveCastFn <- toHsCastPrimitiveName cls super cls
            LH.saysLn ["instance ", downCastClassName, " ", superTypeName, " where"]

            -- If Foo is a superclass of Bar:
            --
            -- instance BarSuper Foo where
            --   downToBar castFooToNonconst . downcast' . castFooToConst
            --     where downcast' (FooConst ptr') = BarConst $ castFooToBar ptr'
            --           downcast' (FooConstGc fptr' ptr') = BarConstGc fptr' $ castFooToBar ptr'
            --
            -- instance BarSuperConst FooConst where
            --   downToBarConst = downcast'
            --     where downcast' (FooConst ptr') = BarConst $ castFooToBar ptr'
            --           downcast' (FooConstGc fptr' ptr') = BarConstGc fptr' $ castFooToBar ptr'

            LH.indent $ do
              case cst of
                Const -> LH.saysLn [downCastMethodName, " = cast'"]
                Nonconst -> do
                  LH.addImports $ hsImport1 "Prelude" "(.)"
                  castClsToNonconst <- toHsConstCastFnName Nonconst cls
                  castSuperToConst <- toHsConstCastFnName Const super
                  LH.saysLn [downCastMethodName, " = ", castClsToNonconst, " . cast' . ",
                             castSuperToConst]
              LH.indent $ do
                LH.sayLn "where"
                LH.indent $ do
                  clsCtorName <- toHsDataCtorName LH.Unmanaged Const cls
                  clsCtorGcName <- toHsDataCtorName LH.Managed Const cls
                  superCtorName <- toHsDataCtorName LH.Unmanaged Const super
                  superCtorGcName <- toHsDataCtorName LH.Managed Const super
                  LH.saysLn ["cast' (", superCtorName, " ptr') = ",
                             clsCtorName, " $ ", primitiveCastFn, " ptr'"]
                  LH.saysLn ["cast' (", superCtorGcName, " fptr' ptr') = ",
                             clsCtorGcName , " fptr' $ ", primitiveCastFn, " ptr'"]
            return True

    LH.SayExportBoot -> do
      forAncestors cls $ \super -> do
        hsCastFnName <- toHsCastPrimitiveName cls cls super
        superType <- toHsDataTypeName Const super
        LH.addImports hsImportForForeign
        LH.addExport hsCastFnName
        LH.saysLn [hsCastFnName, " :: HoppyF.Ptr ", clsType, " -> HoppyF.Ptr ", superType]
        return True

  where forAncestors :: Class -> (Class -> LH.Generator Bool) -> LH.Generator ()
        forAncestors cls' f = forM_ (classSuperclasses cls') $ \super -> do
          recur <- f super
          when recur $ forAncestors super f

getMethodEffectiveParams :: Class -> Method -> [Parameter]
getMethodEffectiveParams cls method =
  (case methodImpl method of
     RealMethod {} -> case methodApplicability method of
       MNormal -> (("this" ~: ptrT $ objT cls) :)
       MConst -> (("this" ~: ptrT $ constT $ objT cls) :)
       MStatic -> id
     FnMethod {} -> id) $
  methodParams method

getHsClassExceptionId :: Class -> LH.Generator ExceptionId
getHsClassExceptionId cls = do
  iface <- LH.askInterface
  fromMaybeM (throwError $ concat
              ["Internal error, exception class ", show cls, " doesn't have an exception ID"]) $
    interfaceExceptionClassId iface cls

-- | The name for the typeclass of types that can be represented as values of
-- the given C++ class.
toHsValueClassName :: Class -> LH.Generator String
toHsValueClassName cls =
  LH.inFunction "toHsValueClassName" $
  LH.addExtNameModule (classExtName cls) $ toHsValueClassName' cls

-- | Pure version of 'toHsValueClassName' that doesn't create a qualified name.
toHsValueClassName' :: Class -> String
toHsValueClassName' cls = toHsDataTypeName' Nonconst cls ++ "Value"

-- | The name of the method within the 'toHsValueClassName' typeclass for
-- accessing an object of the type as a pointer.
toHsWithValuePtrName :: Class -> LH.Generator String
toHsWithValuePtrName cls =
  LH.inFunction "toHsWithValuePtrName" $
  LH.addExtNameModule (classExtName cls) $ toHsWithValuePtrName' cls

-- | Pure version of 'toHsWithValuePtrName' that doesn't create a qualified name.
toHsWithValuePtrName' :: Class -> String
toHsWithValuePtrName' cls = concat ["with", toHsDataTypeName' Nonconst cls, "Ptr"]

-- | The name for the typeclass of types that are (possibly const) pointers to
-- objects of the given C++ class, or subclasses.
toHsPtrClassName :: Constness -> Class -> LH.Generator String
toHsPtrClassName cst cls =
  LH.inFunction "toHsPtrClassName" $
  LH.addExtNameModule (classExtName cls) $ toHsPtrClassName' cst cls

-- | Pure version of 'toHsPtrClassName' that doesn't create a qualified name.
toHsPtrClassName' :: Constness -> Class -> String
toHsPtrClassName' cst cls = toHsDataTypeName' cst cls ++ "Ptr"

-- | The name of the function that upcasts pointers to the specific class type
-- and constness.
toHsCastMethodName :: Constness -> Class -> LH.Generator String
toHsCastMethodName cst cls =
  LH.inFunction "toHsCastMethodName" $
  LH.addExtNameModule (classExtName cls) $ toHsCastMethodName' cst cls

-- | Pure version of 'toHsCastMethodName' that doesn't create a qualified name.
toHsCastMethodName' :: Constness -> Class -> String
toHsCastMethodName' cst cls = "to" ++ toHsDataTypeName' cst cls

-- | The name of the typeclass that provides a method to downcast to a specific
-- class type.  See 'toHsDownCastMethodName'.
toHsDownCastClassName :: Constness -> Class -> LH.Generator String
toHsDownCastClassName cst cls =
  LH.inFunction "toHsDownCastClassName" $
  LH.addExtNameModule (classExtName cls) $ toHsDownCastClassName' cst cls

-- | Pure version of 'toHsDownCastClassName' that doesn't create a qualified
-- name.
toHsDownCastClassName' :: Constness -> Class -> String
toHsDownCastClassName' cst cls =
  concat [toHsDataTypeName' Nonconst cls,
          "Super",
          case cst of
            Const -> "Const"
            Nonconst -> ""]

-- | The name of the function that downcasts pointers to the specific class type
-- and constness.
toHsDownCastMethodName :: Constness -> Class -> LH.Generator String
toHsDownCastMethodName cst cls =
  LH.inFunction "toHsDownCastMethodName" $
  LH.addExtNameModule (classExtName cls) $ toHsDownCastMethodName' cst cls

-- | Pure version of 'toHsDownCastMethodName' that doesn't create a qualified
-- name.
toHsDownCastMethodName' :: Constness -> Class -> String
toHsDownCastMethodName' cst cls = "downTo" ++ toHsDataTypeName' cst cls

-- | The import name for the foreign function that casts between two specific
-- pointer types.  Used for upcasting and downcasting.
--
-- We need to know which module the cast function resides in, and while we could
-- look this up, the caller always knows, so we just have them pass it in.
toHsCastPrimitiveName :: Class -> Class -> Class -> LH.Generator String
toHsCastPrimitiveName descendentClass from to =
  LH.inFunction "toHsCastPrimitiveName" $
  LH.addExtNameModule (classExtName descendentClass) $ toHsCastPrimitiveName' from to

-- | Pure version of 'toHsCastPrimitiveName' that doesn't create a qualified
-- name.
toHsCastPrimitiveName' :: Class -> Class -> String
toHsCastPrimitiveName' from to =
  concat ["cast", toHsDataTypeName' Nonconst from, "To", toHsDataTypeName' Nonconst to]

-- | The name of one of the functions that add/remove const to/from a class's
-- pointer type.  Given 'Const', it will return the function that adds const,
-- and given 'Nonconst', it will return the function that removes const.
toHsConstCastFnName :: Constness -> Class -> LH.Generator String
toHsConstCastFnName cst cls =
  LH.inFunction "toHsConstCastFnName" $
  LH.addExtNameModule (classExtName cls) $ toHsConstCastFnName' cst cls

-- | Pure version of 'toHsConstCastFnName' that doesn't create a qualified name.
toHsConstCastFnName' :: Constness -> Class -> String
toHsConstCastFnName' cst cls =
  concat ["cast", toHsDataTypeName' Nonconst cls,
          case cst of
            Const -> "ToConst"
            Nonconst -> "ToNonconst"]

-- | The name of the data type that represents a pointer to an object of the
-- given class and constness.
toHsDataTypeName :: Constness -> Class -> LH.Generator String
toHsDataTypeName cst cls =
  LH.inFunction "toHsDataTypeName" $
  LH.addExtNameModule (classExtName cls) $ toHsDataTypeName' cst cls

-- | Pure version of 'toHsDataTypeName' that doesn't create a qualified name.
toHsDataTypeName' :: Constness -> Class -> String
toHsDataTypeName' cst cls = LH.toHsTypeName' cst $ classExtName cls

-- | The name of a data constructor for one of the object pointer types.
toHsDataCtorName :: LH.Managed -> Constness -> Class -> LH.Generator String
toHsDataCtorName m cst cls =
  LH.inFunction "toHsDataCtorName" $
  LH.addExtNameModule (classExtName cls) $ toHsDataCtorName' m cst cls

-- | Pure version of 'toHsDataCtorName' that doesn't create a qualified name.
toHsDataCtorName' :: LH.Managed -> Constness -> Class -> String
toHsDataCtorName' m cst cls = case m of
  LH.Unmanaged -> base
  LH.Managed -> base ++ "Gc"
  where base = toHsDataTypeName' cst cls

-- | The name of the foreign function import wrapping @delete@ for the given
-- class type.  This is in internal to the binding; normal users should use
-- 'Foreign.Hoppy.Runtime.delete'.
--
-- This is internal to a generated Haskell module, so it does not have a public
-- (qualified) form.
toHsClassDeleteFnName' :: Class -> String
toHsClassDeleteFnName' cls = 'd':'e':'l':'e':'t':'e':'\'':toHsDataTypeName' Nonconst cls

-- | The name of the foreign import that imports the same function as
-- 'toHsClassDeleteFnName'', but as a 'Foreign.Ptr.FunPtr' rather than an actual
-- function.
--
-- This is internal to a generated Haskell module, so it does not have a public
-- (qualified) form.
toHsClassDeleteFnPtrName' :: Class -> String
toHsClassDeleteFnPtrName' cls =
  'd':'e':'l':'e':'t':'e':'P':'t':'r':'\'':toHsDataTypeName' Nonconst cls

-- | Returns the name of the Haskell function that invokes the given
-- constructor.
toHsCtorName :: Class -> Ctor -> LH.Generator String
toHsCtorName cls ctor =
  LH.inFunction "toHsCtorName" $
  toHsClassEntityName cls $ fromExtName $ ctorExtName ctor

-- | Pure version of 'toHsCtorName' that doesn't create a qualified name.
toHsCtorName' :: Class -> Ctor -> String
toHsCtorName' cls ctor =
  toHsClassEntityName' cls $ fromExtName $ ctorExtName ctor

-- | Returns the name of the Haskell function that invokes the given method.
toHsMethodName :: Class -> Method -> LH.Generator String
toHsMethodName cls method =
  LH.inFunction "toHsMethodName" $
  toHsClassEntityName cls $ fromExtName $ methodExtName method

-- | Pure version of 'toHsMethodName' that doesn't create a qualified name.
toHsMethodName' :: Class -> Method -> String
toHsMethodName' cls method =
  toHsClassEntityName' cls $ fromExtName $ methodExtName method

-- | Returns the name of the Haskell function for an entity in a class.
toHsClassEntityName :: IsFnName String name => Class -> name -> LH.Generator String
toHsClassEntityName cls name =
  LH.addExtNameModule (classExtName cls) $ toHsClassEntityName' cls name

-- | Pure version of 'toHsClassEntityName' that doesn't create a qualified name.
toHsClassEntityName' :: IsFnName String name => Class -> name -> String
toHsClassEntityName' cls name =
  lowerFirst $ fromExtName $
  classEntityForeignName' cls $
  case toFnName name of
    FnName name' -> toExtName name'
    FnOp op -> operatorPreferredExtName op

-- | Generates C++ gateway functions (via 'Function.sayCppExportFn') for getting
-- and setting a variable (possibly a class variable).
sayCppExportVar ::
     Type  -- ^ The type that the variable holds.
  -> Maybe (Type, Type)
     -- ^ @Nothing@ if the variable is not a class variable.  If it is, then the
     -- first type is the generated getter's argument type for the object, and
     -- the second is the generated setter's argument type.  For a class @cls@,
     -- this can be:
     --
     -- > Just ('ptrT' $ 'constT' $ 'objT' cls, 'ptrT' $ 'objT' cls)
  -> Bool
     -- ^ Whether to generate a getter.  Passing false here is useful when a
     -- variable's type can't be sensibly converted to a foreign language's
     -- value.
  -> ExtName
     -- ^ An external name from which to generate a getter function name.
  -> ExtName
     -- ^ An external name from which to generate a setter function name.
  -> LC.Generator ()  -- ^ A C++ generator that emits the variable name.
  -> LC.Generator ()
sayCppExportVar t maybeThisTypes gettable getterName setterName sayVarName = do
  let (isConst, deconstType) = case t of
        Internal_TConst t' -> (True, t')
        t' -> (False, t')

  -- Say a getter function.
  when gettable $
    Function.sayCppExportFn getterName
                            (Function.VarRead sayVarName)
                            (fmap fst maybeThisTypes)
                            []
                            deconstType
                            mempty
                            True

  -- Say a setter function.
  unless isConst $
    Function.sayCppExportFn setterName
                            (Function.VarWrite sayVarName)
                            (fmap snd maybeThisTypes)
                            [toParameter $ deconstType]
                            voidT
                            mempty
                            True

-- | Generates Haskell gateway functions (via 'Function.sayHsExportFn') for
-- getting and setting a variable (possibly a class variable).
sayHsExportVar ::
     LH.SayExportMode  -- ^ The phase of code generation.
  -> Type  -- ^ The type that the variable holds.
  -> Maybe Class
     -- ^ The type of the class holding the variable, if generating code for a
     -- class variable.
  -> Bool
     -- ^ Whether to generate a getter.  Passing false here is useful when a
     -- variable's type can't be sensibly converted to a foreign language's
     -- value.
  -> ExtName
     -- ^ An external name for the getter.
  -> ExtName
     -- ^ A foreign external name for the getter.  See 'Function.sayHsExportFn'.
  -> ExtName
     -- ^ An external name for the setter.
  -> ExtName
     -- ^ A foreign external name for the setter.  See 'Function.sayHsExportFn'.
  -> LH.Generator ()
sayHsExportVar mode
               t
               classIfNonstatic
               gettable
               getterExtName
               getterForeignName
               setterExtName
               setterForeignName = do
  let (isConst, deconstType) = case t of
        Internal_TConst t' -> (True, t')
        t' -> (False, t')

  when gettable $
    Function.sayHsExportFn
    mode
    getterExtName
    getterForeignName
    Nonpure
    (maybe [] (\cls -> [toParameter $ ptrT $ constT $ objT cls]) classIfNonstatic)
    deconstType
    mempty

  unless isConst $
    Function.sayHsExportFn
    mode
    setterExtName
    setterForeignName
    Nonpure
    (maybe [toParameter deconstType]
           (\cls -> [toParameter $ ptrT $ objT cls, toParameter deconstType])
           classIfNonstatic)
    voidT
    mempty
