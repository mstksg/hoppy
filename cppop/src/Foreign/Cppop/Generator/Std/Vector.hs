module Foreign.Cppop.Generator.Std.Vector (
  VectorContents (..),
  instantiate,
  toExports,
  ) where

import Foreign.Cppop.Common (fromEitherM)
import Foreign.Cppop.Generator.Language.Haskell.General (
  HsTypeSide (HsCSide, HsHsSide),
  abort,
  addImports,
  cppTypeToHsTypeAndUse,
  indent,
  prettyPrint,
  sayLn,
  saysLn,
  )
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Spec.ClassFeature (
  ClassFeature (RandomIterator),
  IteratorMutability (Constant, Mutable),
  classAddFeatures,
  )
import Foreign.Cppop.Generator.Spec.Template
import Language.Haskell.Syntax (
  HsQName (Special),
  HsSpecialCon (HsListCon),
  HsType (HsTyApp, HsTyCon),
  )

data VectorContents = VectorContents
  { c_vector :: Class  -- ^ @std::vector<T>@
  , c_iterator :: Class  -- ^ @std::vector<T>::iterator@
  , c_constIterator :: Class  -- ^ @std::vector<T>::const_iterator@
  }

instantiate :: String -> Type -> VectorContents
instantiate classSuffix t =
  let reqs = reqInclude $ includeStd "vector"
      vectorName = "vector" ++ classSuffix
      iteratorName = vectorName ++ "Iterator"
      constIteratorName = vectorName ++ "ConstIterator"

      vector =
        addUseReqs reqs $
        classModifyConversions
        (\c -> c { classHaskellConversion =
                   Just $ ClassHaskellConversion
                   { classHaskellConversionType = do
                     hsHsType <-
                       fromEitherM
                       (\e -> error $ concat
                              ["Instantiation of std::vector<", show t,
                               ">: Type argument is not convertable to a Haskell Haskell-side type: ", e]) =<<
                       cppTypeToHsTypeAndUse HsHsSide t
                     return $ HsTyApp (HsTyCon $ Special HsListCon) hsHsType
                   , classHaskellConversionToCppFn = do
                     addImports $ mconcat [hsImport1 "Control.Monad" "(<=<)", hsImportForPrelude, hsImportForSupport]
                     hsCType <-
                       fromEitherM
                       (\e -> abort $ concat
                              ["Instantiation of std::vector<", show t,
                               ">: Type argument is not convertable to a Haskell C-side type: ", e]) =<<
                       cppTypeToHsTypeAndUse HsCSide t
                     sayLn "\\xs -> do"
                     indent $ do
                       saysLn ["l <- ", vectorName, "_new"]
                       saysLn [vectorName, "_reserve l (CppopFCRS.coerceIntegral (CppopP.length xs))"]
                       saysLn ["CppopP.mapM_ (", vectorName,
                               "_pushBack l <=< CppopFCRS.encodeAs (CppopP.undefined :: ",
                               prettyPrint hsCType, ")) xs"]
                       sayLn "CppopP.return l"
                   , classHaskellConversionFromCppFn = do
                     addImports hsImportForPrelude
                     sayLn "\\l -> do"
                     indent $ do
                       saysLn ["len <- ", vectorName, "_size l"]
                       saysLn ["CppopP.mapM (", vectorName, "_get l) [0..len CppopP.- 1]"]
                   }
                 }) $
        makeClass (ident1T "std" "vector" [t]) (Just $ toExtName vectorName) []
        [ mkCtor "new" [] ]
        [ mkConstMethod "back" [] t
        , mkMethod "begin" [] $ TObjToHeap iterator
        , mkConstMethod "capacity" [] TSize
        , mkConstMethod "empty" [] TBool
        , mkMethod "end" [] $ TObjToHeap iterator
        , mkMethod' "push_back" "pushBack" [t] TVoid
        , mkMethod "reserve" [TSize] TVoid
        , mkConstMethod "size" [] TSize
        , mkMethod' OpArray "at" [TSize] $ TRef t
        , mkConstMethod' OpArray "get" [TSize] t
        ]

      iterator =
        addUseReqs reqs $
        classAddFeatures [RandomIterator Mutable t TPtrdiff] $
        makeClass (identT' [("std", Nothing), ("vector", Just [t]), ("iterator", Nothing)])
        (Just $ toExtName iteratorName) [] [] []

      constIterator =
        addUseReqs reqs $
        classAddFeatures [RandomIterator Constant t TPtrdiff] $
        makeClass (identT' [("std", Nothing), ("vector", Just [t]), ("const_iterator", Nothing)])
        (Just $ toExtName constIteratorName) [] [] []

  in VectorContents
     { c_vector = vector
     , c_iterator = iterator
     , c_constIterator = constIterator
     }

toExports :: VectorContents -> [Export]
toExports m = map (ExportClass . ($ m)) [c_vector, c_iterator, c_constIterator]
