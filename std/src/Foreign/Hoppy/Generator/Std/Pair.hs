-- This file is part of Hoppy.
--
-- Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE CPP #-}

-- | Bindings for @std::pair@.
module Foreign.Hoppy.Generator.Std.Pair (
  Options (..),
  defaultOptions,
  Contents (..),
  instantiate,
  instantiate',
  toExports,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Std.Internal (includeHelper)
import Foreign.Hoppy.Generator.Types
import Foreign.Hoppy.Generator.Version (CppVersion (Cpp2011), activeCppVersion, collect, just, test)

-- | Options for instantiating @pair@.
data Options = Options
  { optPairClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @std::pair@ class.  Pairs are always
    -- 'Assignable' and 'Copyable'.
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options []

-- | A set of instantiated pair classes.
data Contents = Contents
  { c_pair :: Class  -- ^ @std::pair\<A, B>@
  }

-- | @instantiate className a b reqs@ creates a set of bindings for an
-- instantiation of @std::pair\<a, b\>@.  In the result, the 'c_pair' class has
-- an external name of @className@.
instantiate :: String -> Type -> Type -> Reqs -> Contents
instantiate pairName a b reqs = instantiate' pairName a b reqs defaultOptions

-- | 'instantiate' with additional options.
instantiate' :: String -> Type -> Type -> Reqs -> Options -> Contents
instantiate' pairName a b userReqs opts =
  let reqs = mconcat
             [ userReqs
             , reqInclude $ includeHelper "utility.hpp"
             , reqInclude $ includeStd "utility"
             ]

      pair =
        addReqs reqs $
        classAddFeatures (Assignable : Copyable : optPairClassFeatures opts) $
        makeClass (ident1T "std" "pair" [a, b]) (Just $ toExtName pairName) [] $
        collect
        [ just $ mkCtor "new" []
        , just $ mkCtor "newWith" [a, b]
        , just $ makeFnMethod (ident2 "hoppy" "utility" "pairFirst") "first" MNormal Nonpure
          [refT $ objT pair] $ refT a
        , just $ makeFnMethod (ident2 "hoppy" "utility" "pairFirst") "firstConst" MConst Nonpure
          [refT $ objT pair] $ refT $ constT a
        , just $ makeFnMethod (ident2 "hoppy" "utility" "pairSecond") "second" MNormal Nonpure
          [refT $ objT pair] $ refT b
        , just $ makeFnMethod (ident2 "hoppy" "utility" "pairSecond") "secondConst" MConst Nonpure
          [refT $ objT pair] $ refT $ constT b
        , test (activeCppVersion >= Cpp2011) $ mkMethod "swap" [refT $ objT pair] voidT
        ]

  in Contents
     { c_pair = pair
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [Export]
toExports m = map (ExportClass . ($ m)) [c_pair]
