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

-- | Bindings for @std::map@.
module Foreign.Hoppy.Generator.Std.Map (
  Options (..),
  defaultOptions,
  Contents (..),
  instantiate,
  instantiate',
  toExports,
  ) where

import Control.Monad (forM_, when)
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
  toHsDataTypeName,
  toHsClassEntityName,
  )
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Std (ValueConversion (ConvertPtr, ConvertValue))
import Foreign.Hoppy.Generator.Std.Internal (includeHelper)
import Foreign.Hoppy.Generator.Std.Iterator
import Foreign.Hoppy.Generator.Types

-- | Options for instantiating the map classes.
data Options = Options
  { optMapClassFeatures :: [ClassFeature]
    -- ^ Additional features to add to the @std::map@ class.  Maps are always
    -- 'Assignable' and 'Copyable'.
  , optKeyConversion :: Maybe ValueConversion
    -- ^ How to convert values of the key type.
  , optValueConversion :: Maybe ValueConversion
    -- ^ How to convert values of the value type.
  }

-- | The default options have no additional 'ClassFeature's.
defaultOptions :: Options
defaultOptions = Options [] Nothing Nothing

-- | A set of instantiated map classes.
data Contents = Contents
  { c_map :: Class  -- ^ @std::map\<K, V>@
  , c_iterator :: Class  -- ^ @std::map\<K, V>::iterator@
  , c_constIterator :: Class  -- ^ @std::map\<K, V>::const_iterator@
  }

-- | @instantiate className k v reqs@ creates a set of bindings for an
-- instantiation of @std::map\<k, v\>@ and associated types (e.g. iterators).
-- In the result, the 'c_map' class has an external name of @className@, and the
-- iterator classes are further suffixed with @\"Iterator\"@ and
-- @\"ConstIterator\"@ respectively.
instantiate :: String -> Type -> Type -> Reqs -> Contents
instantiate mapName k v reqs = instantiate' mapName k v reqs defaultOptions

-- | 'instantiate' with additional options.
instantiate' :: String -> Type -> Type -> Reqs -> Options -> Contents
instantiate' mapName k v userReqs opts =
  let extName = toExtName mapName
      reqs = mconcat
             [ userReqs
             , reqInclude $ includeHelper "map.hpp"
             , reqInclude $ includeStd "map"
             ]
      iteratorName = mapName ++ "Iterator"
      constIteratorName = mapName ++ "ConstIterator"

      getIteratorKeyIdent = ident2T "hoppy" "map" "getIteratorKey" [k, v]
      getIteratorValueIdent = ident2T "hoppy" "map" "getIteratorValue" [k, v]

      map =
        (case (optKeyConversion opts, optValueConversion opts) of
           (Nothing, Nothing) -> id
           (Just keyConv, Just valueConv) -> addAddendumHaskell $ makeAddendum keyConv valueConv
           (maybeKeyConv, maybeValueConv) ->
             error $ concat
             ["Error instantiating std::map<", show k, ", ", show v, "> (external name ",
              show extName, "), key and value conversions must either both be specified or ",
              "absent; they are, repectively, ", show maybeKeyConv, " and ", show maybeValueConv,
              "."]) $
        addReqs reqs $
        classAddFeatures (Assignable : Copyable : optMapClassFeatures opts) $
        makeClass (ident1T "std" "map" [k, v]) (Just extName) []
        [ mkCtor "new" []
        , mkMethod' "at" "at" [k] $ refT v
        , mkConstMethod' "at" "atConst" [k] $ refT $ constT v
        , mkMethod' "begin" "begin" [] $ toGcT $ objT iterator
        , mkConstMethod' "begin" "beginConst" [] $ toGcT $ objT constIterator
        , mkMethod "clear" [] voidT
        , mkConstMethod "count" [k] sizeT
        , mkConstMethod "empty" [] boolT
        , mkMethod' "end" "end" [] $ toGcT $ objT iterator
        , mkConstMethod' "end" "endConst" [] $ toGcT $ objT constIterator
          -- equal_range: find is good enough.
        , mkMethod' "erase" "erase" [objT iterator] voidT
        , mkMethod' "erase" "eraseKey" [k] sizeT
        , mkMethod' "erase" "eraseRange" [objT iterator, objT iterator] voidT
        , mkMethod' "find" "find" [k] $ toGcT $ objT iterator
        , mkConstMethod' "find" "findConst" [k] $ toGcT $ objT constIterator
          -- TODO insert
          -- lower_bound: find is good enough.
        , mkConstMethod' "max_size" "maxSize" [] sizeT
        , mkConstMethod "size" [] sizeT
        , mkMethod "swap" [refT $ objT map] voidT
          -- upper_bound: find is good enough.
        , mkMethod OpArray [k] $ refT v
        ]

      iterator =
        addReqs reqs $
        makeBidirectionalIterator Mutable Nothing $
        makeClass (identT' [("std", Nothing),
                            ("map", Just [k, v]),
                            ("iterator", Nothing)])
        (Just $ toExtName iteratorName) []
        [ makeFnMethod getIteratorKeyIdent "getKey" MConst Nonpure
          [objT iterator] $ refT $ constT k
        , makeFnMethod getIteratorValueIdent "getValue" MNormal Nonpure
          [refT $ objT iterator] $ refT v
        , makeFnMethod getIteratorValueIdent "getValueConst" MConst Nonpure
          [objT iterator] $ refT $ constT v
        ]

      constIterator =
        addReqs reqs $
        makeBidirectionalIterator Constant Nothing $
        makeClass (identT' [("std", Nothing),
                            ("map", Just [k, v]),
                            ("const_iterator", Nothing)])
        (Just $ toExtName constIteratorName)
        []
        [ mkCtor "newFromConst" [objT iterator]
        , makeFnMethod (ident2 "hoppy" "iterator" "deconst") "deconst" MConst Nonpure
          [objT constIterator, refT $ objT map] $ toGcT $ objT iterator
        , makeFnMethod getIteratorKeyIdent "getKey" MConst Nonpure
          [objT constIterator] $ refT $ constT k
        , makeFnMethod getIteratorValueIdent "getValueConst" MConst Nonpure
          [objT constIterator] $ refT $ constT v
        ]

      -- The addendum for the map class contains HasContents and FromContents
      -- instances with that work with key-value pairs.
      makeAddendum keyConv valueConv = do
        addImports $ mconcat [hsImports "Prelude" ["($)", "(=<<)"],
                              hsImportForPrelude,
                              hsImportForRuntime]

        forM_ [Const, Nonconst] $ \cst -> do
          hsDataTypeName <- toHsDataTypeName cst map

          keyHsType <-
            cppTypeToHsTypeAndUse HsHsSide $
            (case keyConv of
               ConvertPtr -> ptrT
               ConvertValue -> id) $
            constT k

          valueHsType <-
            cppTypeToHsTypeAndUse HsHsSide $
            (case valueConv of
               ConvertPtr -> ptrT
               ConvertValue -> id) $
            case cst of
              Const -> constT v
              Nonconst -> v

          -- Generate const and nonconst HasContents instances.
          ln
          saysLn ["instance HoppyFHR.HasContents ", hsDataTypeName,
                  " ((", prettyPrint keyHsType, "), (", prettyPrint valueHsType, ")) where"]
          indent $ do
            sayLn "toContents this' = do"
            indent $ do
              mapEmpty <- toHsClassEntityName map "empty"
              mapBegin <- toHsClassEntityName map $ case cst of
                Const -> "beginConst"
                Nonconst -> "begin"
              mapEnd <- toHsClassEntityName map $ case cst of
                Const -> "endConst"
                Nonconst -> "end"
              let iter = case cst of
                    Const -> constIterator
                    Nonconst -> iterator
              iterEq <- toHsClassEntityName iter OpEq
              iterGetKey <- toHsClassEntityName iter "getKey"
              iterGetValue <- toHsClassEntityName iter $ case cst of
                Const -> "getValueConst"
                Nonconst -> "getValue"
              iterPrev <- toHsClassEntityName iter "prev"

              saysLn ["empty' <- ", mapEmpty, " this'"]
              sayLn "if empty' then HoppyP.return [] else do"
              indent $ do
                saysLn ["begin' <- ", mapBegin, " this'"]
                saysLn ["iter' <- ", mapEnd, " this'"]
                sayLn "go' iter' begin' []"
              sayLn "where"
              indent $ do
                sayLn "go' iter' begin' acc' = do"
                indent $ do
                  saysLn ["stop' <- ", iterEq, " iter' begin'"]
                  sayLn "if stop' then HoppyP.return acc' else do"
                  indent $ do
                    saysLn ["_ <- ", iterPrev, " iter'"]
                    saysLn ["key' <- ",
                            case keyConv of
                              ConvertPtr -> ""
                              ConvertValue -> "HoppyFHR.decode =<< ",
                            iterGetKey, " iter'"]
                    saysLn ["value' <- ",
                            case valueConv of
                              ConvertPtr -> ""
                              ConvertValue -> "HoppyFHR.decode =<< ",
                            iterGetValue, " iter'"]
                    sayLn "go' iter' begin' $ (key', value'):acc'"

          -- Only generate a nonconst FromContents instance.
          when (cst == Nonconst) $ do
            ln
            saysLn ["instance HoppyFHR.FromContents ", hsDataTypeName,
                    " ((", prettyPrint keyHsType, "), (", prettyPrint valueHsType, ")) where"]
            indent $ do
              sayLn "fromContents values' = do"
              indent $ do
                mapNew <- toHsClassEntityName map "new"
                mapAt <- toHsClassEntityName map "at"
                saysLn ["map' <- ", mapNew]
                saysLn ["HoppyP.mapM_ (\\(k, v) -> HoppyP.flip HoppyFHR.assign v =<< ",
                        mapAt, " map' k) values'"]
                sayLn "HoppyP.return map'"

  in Contents
     { c_map = map
     , c_iterator = iterator
     , c_constIterator = constIterator
     }

-- | Converts an instantiation into a list of exports to be included in a
-- module.
toExports :: Contents -> [Export]
toExports m = map (ExportClass . ($ m)) [c_map, c_iterator, c_constIterator]
