-- This file is part of Hoppy.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | Bindings for @std::string@.
module Foreign.Hoppy.Generator.Std.String (c_string) where

import Foreign.Hoppy.Generator.Language.Haskell.General (addImports, sayLn)
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Spec.ClassFeature
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

-- | @std::string@
c_string :: Class
c_string =
  addReqIncludes [includeStd "string"] $
  classAddFeatures [Assignable, Comparable, Copyable, Equatable] $
  classModifyConversions
  (\c -> c { classHaskellConversion =
             Just ClassHaskellConversion
             { classHaskellConversionType = do
               addImports hsImportForPrelude
               return $ HsTyCon $ UnQual $ HsIdent "HoppyP.String"
             , classHaskellConversionToCppFn = do
               addImports $ mconcat [hsImportForPrelude, hsImportForForeignC]
               sayLn "HoppyP.flip HoppyFC.withCString stdString_newFromCString"
             , classHaskellConversionFromCppFn = do
               addImports $ mconcat [hsImport1 "Control.Monad" "(<=<)", hsImportForForeignC]
               sayLn "HoppyFC.peekCString <=< stdString_c_str"
             }
           }) $
  makeClass (ident1 "std" "string") (Just $ toExtName "StdString")
  []
  [ mkCtor "newFromCString" [TPtr $ TConst TChar]
  ]
  [ mkConstMethod' "at" "at" [TInt] $ TRef TChar
  , mkConstMethod' "at" "get" [TInt] TChar
  , mkConstMethod "c_str" [] $ TPtr $ TConst TChar
  , mkConstMethod "size" [] TSize
  ]
