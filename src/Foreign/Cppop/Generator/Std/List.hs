module Foreign.Cppop.Generator.Std.List (
  tc_list,
  c_list_int,
  c_list_string,
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

-- | @std::list<T>@
tc_list :: ClassTemplate
tc_list =
  addUseReqs (reqInclude $ includeStd "list") $
  addClassTemplateConversions
  (do typeArgs <- askTypeArgs
      mp <- askMethodPrefix
      let [t] = typeArgs
          hsType =
            either (\e -> error $ concat ["Instantiation of std::list<", show t,
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
            , ["  CppopP.mapM_ (", mp, "pushBack <=< CppopFCRS.encode) xs"]
            , ["  CppopP.return l"]
            ]
          , classHaskellConversionToCppImports =
            mconcat [hsImportForPrelude, hsImportForSupport]
          , classHaskellConversionFromCppFn =
            -- TODO Quadratic implementation, redo.
            unlines $ map concat
            [ ["\\l -> do"]
            , ["  len <- ", mp, "size l"]
            , ["  CppopP.mapM (", mp, "get l) [0..len CppopP.- 1]"]
            ]
          , classHaskellConversionFromCppImports = hsImportForPrelude
          }
        }) $
  makeClassTemplate (ident1T "std" "list" [TVar "T"]) Nothing ["T"] []
  [ makeCtor (toExtName "new") [] ]
  [ makeMethod "back" (toExtName "back") MConst Nonpure [] $ TVar "T"
  , makeMethod "push_back" (toExtName "pushBack") MNormal Nonpure [TVar "T"] TVoid
  , makeMethod "size" (toExtName "size") MConst Nonpure [] TSize
  ]

-- TODO Remove these instances, they're just for testing.
c_list_int :: Class
c_list_int = instantiateClassTemplate' tc_list "Int" [TInt] [] mempty

c_list_string :: Class
c_list_string = instantiateClassTemplate' tc_list "String" [TObj c_string] [] mempty
