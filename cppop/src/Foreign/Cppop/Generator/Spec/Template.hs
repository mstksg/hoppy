module Foreign.Cppop.Generator.Spec.Template (
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
  -- ** Internal to Cppop
  ClassInstantiationInfo,
  ) where

import Control.Monad (forM, unless, when)
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.Trans (lift)
import Data.Function (on)
import Foreign.Cppop.Common
import Foreign.Cppop.Common.Consume
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Spec.ClassFeature (
  ClassFeature,
  classAddFeatures,
  )

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

data ClassTemplate = ClassTemplate
  { classTemplateIdentifier :: Identifier
  , classTemplateExtNamePrefix :: String
  , classTemplateVars :: [String]
  , classTemplateSuperclasses :: [ClassTemplateSuper]
  , classTemplateCtors :: [Ctor]
  , classTemplateMethods :: [Method]
  , classTemplateFeatures :: [ClassFeature]
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
                vars supers ctors methods [] Nothing mempty

addClassTemplateFeatures :: [ClassFeature] -> ClassTemplate -> ClassTemplate
addClassTemplateFeatures features tmpl =
  tmpl { classTemplateFeatures = features ++ classTemplateFeatures tmpl }

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

instantiateClassTemplate' :: ClassTemplate -> String -> [Type] -> [Class] -> Reqs -> Class
instantiateClassTemplate' tmpl extNameSuffix typeArgs instantiatedSupers typeArgUseReqs =
  either (error $ concat ["instantiateClassTemplate': Couldn't instantiate ", show tmpl,
                          " with arguments ", show typeArgs, "."])
         id $
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
