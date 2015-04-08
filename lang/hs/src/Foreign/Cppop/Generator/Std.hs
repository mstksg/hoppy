module Foreign.Cppop.Generator.Std (
  cls_std__string,
  ) where

import Foreign.Cppop.Generator.Spec
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

cls_std__string :: Class
cls_std__string =
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TChar
           , classCppDecoder = Just $ CppCoderFn $ ident1 "std" "string"
           , classCppEncoder = Just $ CppCoderExpr [Just "strdup(", Nothing, Just ".c_str())"]
           , classHaskellType = Just $ HaskellEncoding
                                { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "P.String"
                                , haskellEncodingCType = HsTyCon $ UnQual $ HsIdent "FC.CString"
                                , haskellEncodingDecoder = "FCRS.decodeAndFreeCString"
                                , haskellEncodingEncoder = HaskellEncoderWith "FC.withCString"
                                }
           }) $
  makeClass (ident1 "std" "string") (Just $ toExtName "StdString")
  []
  []
  [ Method "size" (toExtName "string_size") MConst Nonpure [] TSize ]
