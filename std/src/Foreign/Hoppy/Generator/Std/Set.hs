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

-- | Bindings for @std::set@.
module Foreign.Hoppy.Generator.Std.Set (
  Options (..),
  defaultOptions,
  Contents (..),
  instantiate,
  instantiate',
  toExports,
  ) where

import Control.Monad (forM, when)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import Foreign.Hoppy.Generator.Language.Haskell (
  HsTypeSide (HsHsSide),
  addImports,
  cppTypeToHsTypeAndUse,
  indent,
  ln,
  prettyPrint,
  sayLn,
  saysLn,
  toHsCastMethodName,
  toHsDataTypeName,
  toHsClassEntityName,
  )
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Std (ValueConversion (ConvertPtr, ConvertValue))
import Foreign.Hoppy.Generator.Std.Internal (includeHelper)
import Foreign.Hoppy.Generator.Std.Iterator
import Foreign.Hoppy.Generator.Types

-- | Options for instantiating the set classes.
data Options = Options
  { optSetClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @std::set@ class.  Sets are always
    -- 'Assignable', 'Comparable', and 'Copyable', but you may want to add
    -- 'Foreign.Hoppy.Generator.Spec.ClassFeature.Equatable' if your value type
    -- supports those.
  , optValueConversion :: Maybe ValueConversion
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options [] Nothing

-- | A set of instantiated set classes.
data Contents = Contents
  { c_set :: Class  -- ^ @std::set\<T>@
  , c_iterator :: Class  -- ^ @std::set\<T>::iterator@
  , c_constIterator :: Class  -- ^ @std::set\<T>::const_iterator@
  }

-- | @instantiate className t tReqs@ creates a set of bindings for an
-- instantiation of @std::set@ and associated types (e.g. iterators).  In the
-- result, the 'c_set' class has an external name of @className@, and the
-- iterator class is further suffixed with @\"Iterator\"@.
instantiate :: String -> Type -> Reqs -> Contents
instantiate setName t tReqs = instantiate' setName t tReqs defaultOptions

-- | 'instantiate' with additional options.
instantiate' :: String -> Type -> Reqs -> Options -> Contents
instantiate' setName t tReqs opts =
  let reqs = mconcat
             [ tReqs
             , reqInclude $ includeHelper "set.hpp"
             , reqInclude $ includeStd "set"
             ]
      iteratorName = setName ++ "Iterator"

      set =
        (case optValueConversion opts of
           Nothing -> id
           Just conversion -> addAddendumHaskell $ makeAddendum conversion) $
        addReqs reqs $
        classAddFeatures (Assignable : Comparable : Copyable : optSetClassFeatures opts) $
        makeClass (ident1T "std" "set" [t]) (Just $ toExtName setName) []
        [ mkCtor "new" []
        , mkMethod' "begin" "begin" [] $ toGcT $ objT iterator
        , mkConstMethod' "begin" "beginConst" [] $ toGcT $ objT constIterator
        , mkMethod "clear" [] voidT
        , mkConstMethod "count" [t] sizeT
          -- TODO count
        , mkConstMethod "empty" [] boolT
        , mkMethod' "end" "end" [] $ toGcT $ objT iterator
        , mkConstMethod' "end" "endConst" [] $ toGcT $ objT constIterator
          -- equalRange: find is good enough.
        , mkMethod' "erase" "erase" [objT iterator] voidT
        , mkMethod' "erase" "eraseRange" [objT iterator, objT iterator] voidT
        , mkMethod "find" [t] $ toGcT $ objT iterator
          -- TODO Replace these with a single version that returns a (toGcT std::pair).
        , makeFnMethod (ident2 "hoppy" "set" "insert") "insert"
          MNormal Nonpure [refT $ objT set, t] boolT
        , makeFnMethod (ident2 "hoppy" "set" "insertAndGetIterator") "insertAndGetIterator"
          MNormal Nonpure [refT $ objT set, t] $ toGcT $ objT iterator
          -- lower_bound: find is good enough.
        , mkConstMethod' "max_size" "maxSize" [] sizeT
        , mkConstMethod "size" [] sizeT
        , mkMethod "swap" [refT $ objT set] voidT
          -- upper_bound: find is good enough.
        ]

      -- Set iterators are always constant, because modifying elements in place
      -- will break the internal order of the set.  That said, 'iterator' and
      -- 'const_iterator' aren't guaranteed to be the same type; only that
      -- 'iterator' is convertible to 'const_iterator'.
      iterator =
        addReqs reqs $
        makeBidirectionalIterator Constant (Just t) $
        makeClass (identT' [("std", Nothing), ("set", Just [t]), ("iterator", Nothing)])
        (Just $ toExtName iteratorName) [] []

      constIterator =
        addReqs reqs $
        makeBidirectionalIterator Constant (Just t) $
        makeClass (identT' [("std", Nothing), ("set", Just [t]), ("const_iterator", Nothing)])
        (Just $ toExtName iteratorName) [] []

      -- The addendum for the set class contains HasContents and FromContents
      -- instances.
      makeAddendum conversion = do
        addImports $ mconcat [hsImport1 "Prelude" "($)",
                              hsImportForPrelude,
                              hsImportForRuntime]
        when (conversion == ConvertValue) $
          addImports $ mconcat [hsImport1 "Prelude" "(=<<)"]

        hsDataNameConst <- toHsDataTypeName Const set
        hsDataName <- toHsDataTypeName Nonconst set
        [hsValueTypeConst, hsValueType] <- forM [Const, Nonconst] $ \cst ->
            cppTypeToHsTypeAndUse HsHsSide $
            (case conversion of
               ConvertPtr -> ptrT
               ConvertValue -> id) $
            case cst of
              Const -> constT t
              Nonconst -> t

        setConstCast <- toHsCastMethodName Const set
        setEmpty <- toHsClassEntityName set "empty"
        setBeginConst <- toHsClassEntityName set "beginConst"
        setEndConst <- toHsClassEntityName set "endConst"
        iterEq <- toHsClassEntityName iterator OpEq
        iterGetConst <- toHsClassEntityName iterator "getConst"
        iterPrev <- toHsClassEntityName iterator "prev"

        -- Generate const and nonconst HasContents instances.
        ln
        saysLn ["instance HoppyFHR.HasContents ", hsDataNameConst,
                " (", prettyPrint hsValueTypeConst, ") where"]
        indent $ do
          sayLn "toContents this' = do"
          indent $ do
            saysLn ["empty' <- ", setEmpty, " this'"]
            sayLn "if empty' then HoppyP.return [] else do"
            indent $ do
              saysLn ["begin' <- ", setBeginConst, " this'"]
              saysLn ["iter' <- ", setEndConst, " this'"]
              sayLn "go' iter' begin' []"
            sayLn "where"
            indent $ do
              sayLn "go' iter' begin' acc' = do"
              indent $ do
                saysLn ["stop' <- ", iterEq, " iter' begin'"]
                sayLn "if stop' then HoppyP.return acc' else do"
                indent $ do
                  saysLn ["_ <- ", iterPrev, " iter'"]
                  saysLn ["value' <- ",
                          case conversion of
                            ConvertPtr -> ""
                            ConvertValue -> "HoppyFHR.decode =<< ",
                          iterGetConst, " iter'"]
                  sayLn "go' iter' begin' $ value':acc'"
        ln
        saysLn ["instance HoppyFHR.HasContents ", hsDataName,
                " (", prettyPrint hsValueTypeConst, ") where"]
        indent $
          saysLn ["toContents = HoppyFHR.toContents . ", setConstCast]

        -- Only generate a nonconst FromContents instance.
        ln
        saysLn ["instance HoppyFHR.FromContents ", hsDataName,
                " (", prettyPrint hsValueType, ") where"]
        indent $ do
          sayLn "fromContents values' = do"
          indent $ do
            setNew <- toHsClassEntityName set "new"
            setInsert <- toHsClassEntityName set "insert"
            saysLn ["set' <- ", setNew]
            saysLn ["HoppyP.mapM_ (", setInsert, " set') values'"]
            sayLn "HoppyP.return set'"

  in Contents
     { c_set = set
     , c_iterator = iterator
     , c_constIterator = constIterator
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [Export]
toExports m = map (ExportClass . ($ m)) [c_set, c_iterator, c_constIterator]
