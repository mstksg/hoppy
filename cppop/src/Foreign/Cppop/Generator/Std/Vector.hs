module Foreign.Cppop.Generator.Std.Vector (
  tc_vector,
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
import Language.Haskell.Syntax (
  HsQName (Special),
  HsSpecialCon (HsListCon),
  HsType (HsTyApp, HsTyCon),
  )

-- | @std::vector<T>@
tc_vector :: ClassTemplate
tc_vector =
  addUseReqs (reqInclude $ includeStd "vector") $
  addClassTemplateConversions
  (do typeArgs <- askTypeArgs
      mp <- askMethodPrefix
      let [t] = typeArgs
      return ClassConversions
        { classHaskellConversion =
          Just $ ClassHaskellConversion
          { classHaskellConversionType = do
            hsHsType <-
              fromEitherM
              (\e -> error $ concat
                     ["Instantiation of std::vector<", show t,
                      ">: Type argument is not convertable to a Haskell Haskell-side type: ", e]) =<<
              cppTypeToHsTypeAndUse HsHsSide t
            return $ HsTyApp (HsTyCon $ Special $ HsListCon) hsHsType
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
              saysLn ["l <- ", mp, "new"]
              saysLn [mp, "reserve l (CppopFCRS.coerceIntegral (CppopP.length xs))"]
              saysLn ["CppopP.mapM_ (", mp,
                      "pushBack l <=< CppopFCRS.encodeAs (CppopP.undefined :: ",
                      prettyPrint hsCType, ")) xs"]
              sayLn "CppopP.return l"
          , classHaskellConversionFromCppFn = do
            addImports hsImportForPrelude
            sayLn "\\l -> do"
            indent $ do
              saysLn ["len <- ", mp, "size l"]
              saysLn ["CppopP.mapM (", mp, "get l) [0..len CppopP.- 1]"]
          }
        }) $
  makeClassTemplate (ident1T "std" "vector" [TVar "T"]) Nothing ["T"] []
  [ mkCtor "new" [] ]
  [ mkConstMethod "back" [] $ TVar "T"
  , mkConstMethod "capacity" [] TSize
  , mkMethod' "push_back" "pushBack" [TVar "T"] TVoid
  , mkMethod "reserve" [TSize] TVoid
  , mkConstMethod "size" [] TSize
  , mkMethod' OpArray "at" [TSize] $ TRef $ TVar "T"
  , mkConstMethod' OpArray "get" [TSize] $ TVar "T"
  ]
