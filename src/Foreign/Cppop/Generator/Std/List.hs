module Foreign.Cppop.Generator.Std.List (
  tc_list,
  ) where

import Foreign.Cppop.Generator.Spec

-- | @std::list<T>@
tc_list :: ClassTemplate
tc_list =
  addUseReqs (reqInclude $ includeStd "list") $
  makeClassTemplate (ident1T "std" "list" [TVar "T"]) Nothing ["T"] []
  [ makeCtor (toExtName "new") [] ]
  [ makeMethod "back" (toExtName "back") MConst Nonpure [] $ TVar "T"
  , makeMethod "push_back" (toExtName "pushBack") MNormal Nonpure [TVar "T"] TVoid
  , makeMethod "size" (toExtName "size") MConst Nonpure [] TSize
  ]
