-- | Bindings for common class operations, such as copy construction.
module Foreign.Cppop.Generator.Spec.ClassFeature (
  ClassFeature (..),
  classAddFeatures,
  ) where

import Foreign.Cppop.Generator.Spec

-- | Sets of functionality that can be stamped onto a class with
-- 'classAddFeatures'.
data ClassFeature =
    Assignable
    -- ^ Provides the assignment operator, @Foo& Foo::operator=(const Foo&)@.
  | Comparable
    -- ^ Provides operators @<@, @<=@, @>@, @>=@, for example @bool
    -- Foo::operator<(const Foo&)@.  This feature does not automatically include
    -- 'Equatable'.
  | Copyable
    -- ^ Provides copy construction, @Foo::Foo(const Foo&)@.
  | Equatable
    -- ^ Provides @operator==@ and @operator!=@, for example @bool
    -- Foo::operator==(const Foo&)@.

featureContents :: ClassFeature -> Class -> ([Ctor], [Method])
featureContents feature cls = case feature of
  Assignable -> assignableContents cls
  Comparable -> comparableContents cls
  Copyable -> copyableContents cls
  Equatable -> equatableContents cls

assignableContents :: Class -> ([Ctor], [Method])
assignableContents cls =
  ([],
   [ mkMethod OpAssign [TRef $ TConst $ TObj cls] $ TRef $ TObj cls
   ])

comparableContents :: Class -> ([Ctor], [Method])
comparableContents cls =
  ([],
   [ mkConstMethod OpLt [TRef $ TConst $ TObj cls] TBool
   , mkConstMethod OpLe [TRef $ TConst $ TObj cls] TBool
   , mkConstMethod OpGt [TRef $ TConst $ TObj cls] TBool
   , mkConstMethod OpGe [TRef $ TConst $ TObj cls] TBool
   ])

copyableContents :: Class -> ([Ctor], [Method])
copyableContents cls =
  ([ mkCtor "copy" [TRef $ TConst $ TObj cls] ], [])

equatableContents :: Class -> ([Ctor], [Method])
equatableContents cls =
  ([],
   [ mkConstMethod OpEq [TRef $ TConst $ TObj cls] TBool
   , mkConstMethod OpNe [TRef $ TConst $ TObj cls] TBool
   ])

-- | Adds the contents of a feature to a class.  Does not check for overlap with
-- existing class contents.
classAddFeatures :: [ClassFeature] -> Class -> Class
classAddFeatures features cls =
  foldr (\feature cls' ->
          let (ctors, methods) = featureContents feature cls'
          in classAddCtors ctors $
             classAddMethods methods cls')
        cls
        features

classAddCtors :: [Ctor] -> Class -> Class
classAddCtors ctors cls =
  if null ctors then cls else cls { classCtors = classCtors cls ++ ctors }

classAddMethods :: [Method] -> Class -> Class
classAddMethods methods cls =
  if null methods then cls else cls { classMethods = classMethods cls ++ methods }
