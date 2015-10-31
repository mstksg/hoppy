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
import Foreign.Hoppy.Generator.Language.Haskell.General (
  HsTypeSide (HsHsSide),
  addImports,
  cppTypeToHsTypeAndUse,
  indent,
  ln,
  prettyPrint,
  sayLn,
  saysLn,
  toHsDataTypeName,
  toHsMethodName',
  )
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, BidirectionalIterator, Copyable),
  IteratorMutability (Constant, Mutable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Std (ValueConversion (ConvertPtr, ConvertValue))

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
             , reqInclude $ includeStd "hoppy/map.hpp"
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
        addUseReqs reqs $
        classAddFeatures (Assignable : Copyable : optMapClassFeatures opts) $
        makeClass (ident1T "std" "map" [k, v]) (Just extName) []
        [ mkCtor "new" []
        ]
        [ mkMethod' "at" "at" [k] $ TRef v
        , mkConstMethod' "at" "atConst" [k] $ TRef $ TConst v
        , mkMethod' "begin" "begin" [] $ TObjToHeap iterator
        , mkConstMethod' "begin" "beginConst" [] $ TObjToHeap constIterator
        , mkMethod "clear" [] TVoid
        , mkConstMethod "count" [k] TSize
        , mkConstMethod "empty" [] TBool
        , mkMethod' "end" "end" [] $ TObjToHeap iterator
        , mkConstMethod' "end" "endConst" [] $ TObjToHeap constIterator
          -- equal_range: find is good enough.
        , mkMethod' "erase" "erase" [TObj iterator] TVoid
        , mkMethod' "erase" "eraseKey" [k] TSize
        , mkMethod' "erase" "eraseRange" [TObj iterator, TObj iterator] TVoid
        , mkMethod' "find" "find" [k] $ TObjToHeap iterator
        , mkConstMethod' "find" "findConst" [k] $ TObjToHeap constIterator
          -- TODO insert
          -- lower_bound: find is good enough.
        , mkConstMethod' "max_size" "maxSize" [] TSize
        , mkConstMethod "size" [] TSize
        , mkMethod "swap" [TRef $ TObj map] TVoid
          -- upper_bound: find is good enough.
        , mkMethod OpArray [k] $ TRef v
        ]

      iterator =
        addUseReqs reqs $
        classAddFeatures [BidirectionalIterator Mutable Nothing] $
        makeClass (identT' [("std", Nothing),
                            ("map", Just [k, v]),
                            ("iterator", Nothing)])
        (Just $ toExtName iteratorName) [] []
        [ makeFnMethod getIteratorKeyIdent "getKey" MConst Nonpure
          [TObj iterator] $ TRef $ TConst k
        , makeFnMethod getIteratorValueIdent "getValue" MNormal Nonpure
          [TRef $ TObj iterator] $ TRef v
        , makeFnMethod getIteratorValueIdent "getValueConst" MConst Nonpure
          [TObj iterator] $ TRef $ TConst v
        ]

      constIterator =
        addUseReqs reqs $
        classAddFeatures [BidirectionalIterator Constant Nothing] $
        makeClass (identT' [("std", Nothing),
                            ("map", Just [k, v]),
                            ("const_iterator", Nothing)])
        (Just $ toExtName constIteratorName)
        []
        [ mkCtor "newFromConst" [TObj iterator]
        ]
        [ makeFnMethod (ident2 "hoppy" "iterator" "deconst") "deconst" MConst Nonpure
          [TObj constIterator, TRef $ TObj map] $ TObjToHeap iterator
        , makeFnMethod getIteratorKeyIdent "getKey" MConst Nonpure
          [TObj constIterator] $ TRef $ TConst k
        , makeFnMethod getIteratorValueIdent "getValueConst" MConst Nonpure
          [TObj constIterator] $ TRef $ TConst v
        ]

      -- The addendum for the map class contains HasContents and FromContents
      -- instances with that work with key-value pairs.
      makeAddendum keyConv valueConv = do
        addImports $ mconcat [hsImports "Prelude" ["($)", "(=<<)"],
                              hsImportForPrelude,
                              hsImportForSupport]

        forM_ [Const, Nonconst] $ \cst -> do
          let hsDataTypeName = toHsDataTypeName cst map

          keyHsType <-
            cppTypeToHsTypeAndUse HsHsSide $
            (case keyConv of
               ConvertPtr -> TPtr
               ConvertValue -> id) $
            TConst k

          valueHsType <-
            cppTypeToHsTypeAndUse HsHsSide $
            (case valueConv of
               ConvertPtr -> TPtr
               ConvertValue -> id) $
            case cst of
              Const -> TConst v
              Nonconst -> v

          -- Generate const and nonconst HasContents instances.
          ln
          saysLn ["instance HoppyFHRS.HasContents ", hsDataTypeName,
                  " ((", prettyPrint keyHsType, "), (", prettyPrint valueHsType, ")) where"]
          indent $ do
            sayLn "toContents this' = do"
            indent $ do
              let mapBegin = case cst of
                    Const -> "beginConst"
                    Nonconst -> "begin"
                  mapEnd = case cst of
                    Const -> "endConst"
                    Nonconst -> "end"
                  iter = case cst of
                    Const -> constIterator
                    Nonconst -> iterator
                  iterGetValue = case cst of
                    Const -> "getValueConst"
                    Nonconst -> "getValue"
              saysLn ["empty' <- ", toHsMethodName' map "empty", " this'"]
              sayLn "if empty' then HoppyP.return [] else"
              indent $ do
                saysLn ["HoppyFHRS.withScopedPtr (", toHsMethodName' map mapBegin,
                        " this') $ \\begin' ->"]
                saysLn ["HoppyFHRS.withScopedPtr (", toHsMethodName' map mapEnd,
                        " this') $ \\iter' ->"]
                sayLn "go' iter' begin' []"
              sayLn "where"
              indent $ do
                sayLn "go' iter' begin' acc' = do"
                indent $ do
                  saysLn ["stop' <- ", toHsMethodName' iter OpEq, " iter' begin'"]
                  sayLn "if stop' then HoppyP.return acc' else do"
                  indent $ do
                    saysLn ["_ <- ", toHsMethodName' iter OpDecPre, " iter'"]
                    saysLn ["key' <- ",
                            case keyConv of
                              ConvertPtr -> ""
                              ConvertValue -> "HoppyFHRS.decode =<< ",
                            toHsMethodName' iter "getKey", " iter'"]
                    saysLn ["value' <- ",
                            case valueConv of
                              ConvertPtr -> ""
                              ConvertValue -> "HoppyFHRS.decode =<< ",
                            toHsMethodName' iter iterGetValue, " iter'"]
                    sayLn "go' iter' begin' $ (key', value'):acc'"

          -- Only generate a nonconst FromContents instance.
          when (cst == Nonconst) $ do
            ln
            saysLn ["instance HoppyFHRS.FromContents ", hsDataTypeName,
                    " ((", prettyPrint keyHsType, "), (", prettyPrint valueHsType, ")) where"]
            indent $ do
              sayLn "fromContents values' = do"
              indent $ do
                saysLn ["map' <- ", toHsMethodName' map "new"]
                saysLn ["HoppyP.mapM_ (\\(k, v) -> HoppyP.flip HoppyFHRS.assign v =<< ",
                        toHsMethodName' map "at", " map' k) values'"]
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
