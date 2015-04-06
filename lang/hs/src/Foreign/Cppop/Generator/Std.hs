module Foreign.Cppop.Generator.Std (
  cls_cppop__Callback,
  cls_std__string,
  ) where

import Foreign.Cppop.Generator.Spec
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

cls_cppop__Callback :: Class
cls_cppop__Callback =
  classModifyEncoding
  (\c -> c { classCppDecoder = Just $ ident1 "cppop" "decodeCallback"
           , classCppEncoder = Just $ ident1 "cppop" "encodeCallback"
           , classHaskellType = Just $ ForeignType
                                { coderType = HsTyCon $ UnQual $ HsIdent "FCRC.CallbackId"
                                , coderDecoder = "(hget :: DBG.Get FCRC.CallbackId)"
                                , coderEncoder = "(hput :: FCRC.CallbackId -> DBP.Put)"
                                }
           }) $
  makeClass (ident1 "cppop" "Callback") [] [] []

cls_std__string :: Class
cls_std__string =
  classModifyEncoding
  (\c -> c { classCppDecoder = Just $ ident1 "cppop" "decodeStdString"
           , classCppEncoder = Just $ ident1 "cppop" "encodeStdString"
           , classHaskellType = Just $ ForeignType
                                { coderType = HsTyCon $ UnQual $ HsIdent "P.String"
                                , coderDecoder = "(hget :: DBG.Get P.String)"
                                , coderEncoder = "(hput :: P.String -> DBP.Put)"
                                }
           }) $
  makeClass (ident1 "std" "string")
  []
  []
  [ Method "size" (toExtName "string_size") MConst Nonpure [] TSize ]
