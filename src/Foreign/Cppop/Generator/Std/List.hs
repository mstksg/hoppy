module Foreign.Cppop.Generator.Std.List (c_list_int) where

import Data.Monoid (mempty)
import Foreign.Cppop.Generator.Spec

tc_list :: ClassTemplate
tc_list =
  addUseReqs (reqInclude $ includeStd "list") $
  makeClassTemplate (ident1T "std" "list" [TVar "T"]) Nothing ["T"] []
  [ makeCtor (toExtName "new") [] ]
  [ makeMethod "back" (toExtName "back") MConst Nonpure [] $ TVar "T"
  , makeMethod "push_back" (toExtName "pushBack") MNormal Nonpure [TVar "T"] TVoid
  ]

-- TODO Remove this instance, it's just for testing.
c_list_int :: Class
c_list_int = instantiateClassTemplate' tc_list "Int" [TInt] [] mempty
