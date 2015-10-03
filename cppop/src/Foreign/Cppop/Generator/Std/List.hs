-- | Bindings for @std::list@.
module Foreign.Cppop.Generator.Std.List (
  tc_list,
  ) where

import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Spec.Template

-- | @std::list<T>@
tc_list :: ClassTemplate
tc_list =
  addUseReqs (reqInclude $ includeStd "list") $
  makeClassTemplate (ident1T "std" "list" [TVar "T"]) Nothing ["T"] []
  [ mkCtor "new" [] ]
  [ mkConstMethod "back" [] $ TVar "T"
  , mkMethod "pushBack" [TVar "T"] TVoid
  , mkConstMethod "size" [] TSize
  ]
