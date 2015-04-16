module Foreign.Cppop.Generator.Std (
  c_std__string,
  ) where

import Foreign.Cppop.Generator.Spec
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

c_std__string :: Class
c_std__string =
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TChar
           , classCppDecoder = Just $ CppCoderFn $ ident1 "std" "string"
           , classCppDecodeThenFree = True
           , classCppEncoder = Just $ CppCoderExpr [Just "strdup(", Nothing, Just ".c_str())"]
           , classHaskellType = Just $ HaskellEncoding
                                { haskellEncodingType = HsTyCon $ UnQual $ HsIdent "P.String"
                                , haskellEncodingCType = HsTyCon $ UnQual $ HsIdent "FC.CString"
                                , haskellEncodingDecoder = "FCRS.decodeAndFreeCString"
                                , haskellEncodingEncoder = "FC.newCString"
                                }
           }) $
  makeClass (ident1 "std" "string") (Just $ toExtName "StdString")
  []
  []
  [ makeMethod "size" (toExtName "string_size") MConst Nonpure [] TSize ]
