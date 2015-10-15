-- | Bindings for @std::list@.
module Foreign.Hoppy.Generator.Std.List (
  tc_list,
  ) where

import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.Template

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
