module Foreign.Cppop.Generator.Std.Vector (
  tc_vector,
  c_vector_int,
  c_vector_string,
  ) where

import Data.Monoid (mconcat, mempty)
import Foreign.Cppop.Generator.Language.Haskell.General (
  HsTypeSide (HsHsSide),
  cppTypeToHsType,
  )
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std.String (c_string)
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
          hsType =
            either (\e -> error $ concat ["Instantiation of std::vector<", show t,
                                          ">: Type argument is not convertable: ", e])
                   id $
            cppTypeToHsType HsHsSide t
      return ClassConversions
        { classHaskellConversion =
          Just $ ClassHaskellConversion
          { classHaskellConversionType = HsTyApp (HsTyCon $ Special $ HsListCon) hsType
          , classHaskellConversionTypeImports = mempty
          , classHaskellConversionToCppFn =
            unlines $ map concat
            [ ["\\xs -> do"]
            , ["  l <- ", mp, "new"]
            , ["  ", mp, "reserve l (CppopFCRS.coerceIntegral (CppopP.length xs))"]
            , ["  CppopP.mapM_ (", mp, "pushBack l <=< CppopFCRS.encode) xs"]
            , ["  CppopP.return l"]
            ]
          , classHaskellConversionToCppImports =
            mconcat [hsImport1 "Control.Monad" "(<=<)", hsImportForPrelude, hsImportForSupport]
          , classHaskellConversionFromCppFn =
            unlines $ map concat
            [ ["\\l -> do"]
            , ["  len <- ", mp, "size l"]
            , ["  CppopP.mapM (", mp, "get l) [0..len CppopP.- 1]"]
            ]
          , classHaskellConversionFromCppImports = hsImportForPrelude
          }
        }) $
  makeClassTemplate (ident1T "std" "vector" [TVar "T"]) Nothing ["T"] []
  [ makeCtor (toExtName "new") [] ]
  [ makeMethod "back" (toExtName "back") MConst Nonpure [] $ TVar "T"
  , makeMethod "capacity" (toExtName "capacity") MConst Nonpure [] TSize
  , makeMethod "push_back" (toExtName "pushBack") MNormal Nonpure [TVar "T"] TVoid
  , makeMethod "reserve" (toExtName "reserve") MNormal Nonpure [TSize] TVoid
  , makeMethod "size" (toExtName "size") MConst Nonpure [] TSize
  , makeMethod OpArray (toExtName "at") MNormal Nonpure [TSize] $ TRef $ TVar "T"
  , makeMethod OpArray (toExtName "get") MConst Nonpure [TSize] $ TVar "T"
  ]

-- TODO Remove these instances, they're just for testing.
c_vector_int :: Class
c_vector_int = instantiateClassTemplate' tc_vector "Int" [TInt] [] mempty

c_vector_string :: Class
c_vector_string = instantiateClassTemplate' tc_vector "String" [TObj c_string] [] mempty
