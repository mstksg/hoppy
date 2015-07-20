module Foreign.Cppop.Generator.Std.List (
  tc_list,
  c_list_int,
  c_list_string,
  ) where

import Data.Monoid (mempty)
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std.String (c_string)

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

-- TODO Remove these instances, they're just for testing.
c_list_int :: Class
c_list_int = instantiateClassTemplate' tc_list "Int" [TInt] [] mempty

c_list_string :: Class
c_list_string = instantiateClassTemplate' tc_list "String" [TObj c_string] [] mempty
