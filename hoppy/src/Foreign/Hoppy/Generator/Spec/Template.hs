-- | Binding declarations for C++ templates.
--
-- These templates cannot be exported directly; instead they must be
-- instantiated, and the resulting objects can be exported.
module Foreign.Hoppy.Generator.Spec.Template (
  -- * Function templates
  FnTemplate, makeFnTemplate, instantiateFnTemplate, instantiateFnTemplate',
  fnTemplateIdentifier, fnTemplateExtNamePrefix, fnTemplateVars, fnTemplatePurity, fnTemplateParams,
  fnTemplateReturn, fnTemplateUseReqs,
  -- * Class templates
  ClassTemplate, ClassTemplateSuper (..),
  ClassTemplateConversionsGen, ClassTemplateConversionsEnv, askTypeArgs, askMethodPrefix,
  makeClassTemplate, instantiateClassTemplate, instantiateClassTemplate',
  addClassTemplateFeatures, addClassTemplateConversions,
  classTemplateIdentifier, classTemplateExtNamePrefix, classTemplateVars, classTemplateSuperclasses,
  classTemplateCtors, classTemplateMethods, classTemplateUseReqs,
  -- ** Internal to Hoppy
  ClassInstantiationInfo,
  ) where

import Control.Monad (forM, unless, when)
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.Trans (lift)
import Data.Function (on)
import Foreign.Hoppy.Common
import Foreign.Hoppy.Common.Consume
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Language.Haskell.General (toHsFnName)
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature,
  classAddFeatures,
  )

-- | A C++ function template declaration.
data FnTemplate = FnTemplate
  { fnTemplateIdentifier :: Identifier
    -- ^ The C++ name of the function template.
  , fnTemplateExtNamePrefix :: String
    -- ^ The template's external name.  When instantiated, a provided string
    -- will be appended to this name.
  , fnTemplateVars :: [String]
    -- ^ Template parameter names.  Within the template, these can be referenced
    -- with 'TVar'.
  , fnTemplatePurity :: Purity
    -- ^ Whether the function is pure.
  , fnTemplateParams :: [Type]
    -- ^ The function's parameter types.
  , fnTemplateReturn :: Type
    -- ^ The function's return type.
  , fnTemplateUseReqs :: Reqs
    -- ^ Requirements for a binding to use the template, ignoring requirements
    -- for template arguments (which will be handled at instantiation time).
  }

instance Show FnTemplate where
  show tmpl =
    concat ["<FnTemplate ", show (fnTemplateIdentifier tmpl), ">"]

instance HasUseReqs FnTemplate where
  getUseReqs = fnTemplateUseReqs
  setUseReqs reqs fnTemplate = fnTemplate { fnTemplateUseReqs = reqs }

-- | Creates a function template.
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

-- | Instantiates a function template, giving a function.  Returns an error
-- message if the wrong number of type arguments is given.
instantiateFnTemplate
  :: FnTemplate  -- ^ The template to instantiate.
  -> String
  -- ^ A suffix to append to the prefix given to 'makeFnTemplate' to form a
  -- complete 'ExtName' for the function.
  -> [Type]  -- ^ Types to substitute for the type variables in the template.
  -> Reqs  -- ^ Requirements for the type variables.
  -> Either ErrorMsg Function
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

-- | Instantiates a function template, calling 'error' on failure.
instantiateFnTemplate' :: FnTemplate -> String -> [Type] -> Reqs -> Function
instantiateFnTemplate' tmpl extNameSuffix typeArgs typeArgUseReqs =
  either (\e -> error $ concat
                ["instantiateFnTemplate': Failed to instantiate ", show tmpl, ": ", e])
         id $
  instantiateFnTemplate tmpl extNameSuffix typeArgs typeArgUseReqs

-- | A C++ class template declaration.
data ClassTemplate = ClassTemplate
  { classTemplateIdentifier :: Identifier
    -- ^ The C++ name of the class template.
  , classTemplateExtNamePrefix :: String
    -- ^ The template's external name.  When instantiated, a provided string
    -- will be appended to this name.
  , classTemplateVars :: [String]
    -- ^ Template parameter names.  Within the template, these can be referenced
    -- with 'TVar'.
  , classTemplateSuperclasses :: [ClassTemplateSuper]
    -- ^ Template public superclasses.
  , classTemplateCtors :: [Ctor]
    -- ^ Template constructors.
  , classTemplateMethods :: [Method]
    -- ^ Template methods.
  , classTemplateFeatures :: [ClassFeature]
    -- ^ Template class features.
  , classTemplateConversions :: Maybe ClassTemplateConversionsGen
    -- ^ Conversions for instantiated classes.
  , classTemplateUseReqs :: Reqs
    -- ^ Requirements to use the template, ignoring requirements of type
    -- arguments when instantiating the template.
  }

instance Eq ClassTemplate where
  (==) = (==) `on` classTemplateIdentifier

instance Show ClassTemplate where
  show tmpl =
    concat ["<ClassTemplate ", show (classTemplateIdentifier tmpl), ">"]

instance HasUseReqs ClassTemplate where
  getUseReqs = classTemplateUseReqs
  setUseReqs reqs classTemplate = classTemplate { classTemplateUseReqs = reqs }

-- | A superclass of a class template.
data ClassTemplateSuper =
  ClassTemplateSuperClass Class
  -- ^ A non-templated superclass for a templated class.
  | ClassTemplateSuperTemplate ClassTemplate [Type]
    -- ^ A templated superclass for a templated class.  A type argument should
    -- be given for each type parameter of the super template.

-- | A monad for generating 'ClassConversions' for a class template
-- instantiation.
type ClassTemplateConversionsGen = Reader ClassTemplateConversionsEnv ClassConversions

-- | Readable environment during class template instantiation.
data ClassTemplateConversionsEnv = ClassTemplateConversionsEnv
  { classTemplateConversionsEnvTypeArgs :: [Type]
  , classTemplateConversionsEnvMethodPrefix :: String
  }

makeClassTemplateConversionsEnv :: Class -> [Type] -> ClassTemplateConversionsEnv
makeClassTemplateConversionsEnv cls typeArgs =
  ClassTemplateConversionsEnv
  { classTemplateConversionsEnvTypeArgs = typeArgs
  , classTemplateConversionsEnvMethodPrefix = toHsFnName (classExtName cls) ++ "_"
  }

-- | Extracts the type arguments given to the current class template
-- instantiation.
askTypeArgs :: Reader ClassTemplateConversionsEnv [Type]
askTypeArgs = asks classTemplateConversionsEnvTypeArgs

-- | Extracts the method prefix for the current class template instantiation.
-- This is the @className_@ prefix on external names for things within the
-- class, everything up to the part of the name specified by methods
-- (constructors, etc.) themselves.
askMethodPrefix :: Reader ClassTemplateConversionsEnv String
askMethodPrefix = asks classTemplateConversionsEnvMethodPrefix

-- | Creates a class template.
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
                vars supers ctors methods [] Nothing mempty

-- | Adds 'ClassFeature's to classes instantiated from the given template.
addClassTemplateFeatures :: [ClassFeature] -> ClassTemplate -> ClassTemplate
addClassTemplateFeatures features tmpl =
  tmpl { classTemplateFeatures = features ++ classTemplateFeatures tmpl }

-- | Adds conversions to classes instantiated from the given template.
addClassTemplateConversions :: ClassTemplateConversionsGen -> ClassTemplate -> ClassTemplate
addClassTemplateConversions convs tmpl = case classTemplateConversions tmpl of
  Nothing -> tmpl { classTemplateConversions = Just convs }
  Just _ ->
    error $ concat
    ["addClassTemplateEncoding: ", show tmpl, " already has conversions, trying to add again."]

-- | Instantiates a class template, giving a class.  Returns an error message if
-- the instantiation fails, e.g. because the wrong number of type arguments is given.
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
      subst :: HasTVars a => a -> a
      subst = substTVars $ zip vars typeArgs
      result =
        (case classTemplateConversions tmpl of
            Nothing -> id
            Just convs -> classModifyConversions $ const $
                          runReader convs $ makeClassTemplateConversionsEnv result typeArgs) $
        addClassInstantiationInfo tmpl typeArgs $
        addUseReqs (classTemplateUseReqs tmpl `mappend` typeArgUseReqs) $
        classAddFeatures (map subst $ classTemplateFeatures tmpl) $
        subst $
        makeClass ident
                  (Just $ toExtName clsExtName)
                  concreteSupers
                  (classTemplateCtors tmpl)
                  (classTemplateMethods tmpl)

  return result

-- | Instantiates a class template, calling 'error' on failure.
instantiateClassTemplate' :: ClassTemplate -> String -> [Type] -> [Class] -> Reqs -> Class
instantiateClassTemplate' tmpl extNameSuffix typeArgs instantiatedSupers typeArgUseReqs =
  either (error $ concat ["instantiateClassTemplate': Couldn't instantiate ", show tmpl,
                          " with arguments ", show typeArgs, "."])
         id $
  instantiateClassTemplate tmpl extNameSuffix typeArgs instantiatedSupers typeArgUseReqs

-- | Information about the template from which a class was instantiated.
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
