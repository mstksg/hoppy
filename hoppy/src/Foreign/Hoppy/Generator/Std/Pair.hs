-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | Bindings for @std::pair@.
module Foreign.Hoppy.Generator.Std.Pair (
  Options (..),
  defaultOptions,
  Contents (..),
  instantiate,
  instantiate',
  toExports,
  ) where

import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
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

-- | @instantiate className a b@ creates a set of bindings for an instantiation
-- of @std::pair\<a, b\>@.  In the result, the 'c_pair' class has an external
-- name of @className@.
instantiate :: String -> Type -> Type -> Contents
instantiate pairName a b = instantiate' pairName a b defaultOptions

-- | 'instantiate' with additional options.
instantiate' :: String -> Type -> Type -> Options -> Contents
instantiate' pairName a b opts =
  let reqs = mconcat
             [ reqInclude $ includeStd "hoppy/utility.hpp"
             , reqInclude $ includeStd "utility"
             ]

      pair =
        addUseReqs reqs $
        classAddFeatures (Assignable : Copyable : optPairClassFeatures opts) $
        makeClass (ident1T "std" "pair" [a, b]) (Just $ toExtName pairName) []
        [ mkCtor "new" []
        , mkCtor "newWith" [a, b]
        ] $
        collect
        [ just $ makeFnMethod (ident2 "hoppy" "utility" "pairFirst") "first" MNormal Nonpure
          [TRef $ TObj pair] $ TRef a
        , just $ makeFnMethod (ident2 "hoppy" "utility" "pairFirst") "firstConst" MConst Nonpure
          [TRef $ TObj pair] $ TRef $ TConst a
        , just $ makeFnMethod (ident2 "hoppy" "utility" "pairSecond") "second" MNormal Nonpure
          [TRef $ TObj pair] $ TRef b
        , just $ makeFnMethod (ident2 "hoppy" "utility" "pairSecond") "secondConst" MConst Nonpure
          [TRef $ TObj pair] $ TRef $ TConst b
        , test (activeCppVersion >= Cpp2011) $ mkMethod "swap" [TRef $ TObj pair] TVoid
        ]

  in Contents
     { c_pair = pair
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [Export]
toExports m = map (ExportClass . ($ m)) [c_pair]
