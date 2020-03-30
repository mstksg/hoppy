-- This file is part of Hoppy.
--
-- Copyright 2015-2020 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}

-- | Internal portion of the C++ code generator.
module Foreign.Hoppy.Generator.Language.Cpp.Internal (
  Generation,
  generate,
  generatedFiles,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, tell)
import Control.Monad.Trans (lift)
import Data.Foldable (forM_)
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$))
#endif
import qualified Data.Map as M
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mappend, mconcat, mempty)
#endif
import Foreign.Hoppy.Generator.Common
import Foreign.Hoppy.Generator.Language.Cpp
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Types

-- | The in-memory result of generating C++ code for an interface.
newtype Generation = Generation
  { generatedFiles :: M.Map FilePath String
    -- ^ A map from paths of generated files to the contents of those files.
    -- The file paths are relative paths below the C++ generation root.
  }

-- | Runs the C++ code generator against an interface.
generate :: Interface -> Either ErrorMsg Generation
generate iface =
  fmap (Generation . M.fromList) $
  execWriterT $
  forM_ (M.elems $ interfaceModules iface) $ \m -> do
    let headerGuard = concat ["HOPPY_MODULE_", interfaceName iface, "_", moduleName m]
    header <- lift $ execGenerator iface m (Just headerGuard) sayModuleHeader
    tell [(moduleHppPath m, header)]
    source <- lift $ execGenerator iface m Nothing sayModuleSource
    tell [(moduleCppPath m, source)]

sayModuleHeader :: Generator ()
sayModuleHeader = do
  m <- askModule
  addReqsM $ moduleReqs m
  mapM_ (sayExportCpp SayHeader) $ M.elems $ moduleExports m

  iface <- askInterface
  when (interfaceExceptionSupportModule iface == Just m) $
    sayExceptionSupport False

sayModuleSource :: Generator ()
sayModuleSource = do
  m <- askModule
  addInclude $ includeLocal $ moduleHppPath m
  mapM_ (sayExportCpp SaySource) $ M.elems $ moduleExports m

  iface <- askInterface
  when (interfaceExceptionSupportModule iface == Just m) $
    sayExceptionSupport True

-- | Outputs interface-wide code needed to support exceptions.  Currently, this
-- comprises the function for rethrowing in C++ an exception transferred from
-- a foreign language.
sayExceptionSupport :: Bool -> Generator ()
sayExceptionSupport sayBody =
  sayFunction exceptionRethrowFnName
              ["excId", "voidPtr"]
              (fnT [intT, ptrT voidT] voidT) $
  if not sayBody
  then Nothing
  else Just $ do
    iface <- askInterface
    let excClasses = interfaceAllExceptionClasses iface

    says ["switch (excId) {\n"]

    forM_ excClasses $ \cls -> do
      excId <- fmap getExceptionId $
               fromMaybeM (abort $ "sayExceptionSupport: Internal error, " ++ show cls ++
                           "should have an exception ID, but doesn't.") $
               interfaceExceptionClassId iface cls
      says ["case ", show excId, ": {\n"]
      sayVar "excPtr" Nothing (ptrT $ objT cls) >> say " = reinterpret_cast<" >>
        sayType Nothing (ptrT $ objT cls) >> says [">(voidPtr);\n"]
      sayVar "exc" Nothing (objT cls) >> say " = *excPtr;\n"
      say "delete excPtr;\n"
      say "throw exc;\n"
      say "}\n"

    say "}\n"
    says ["throw \"Internal Hoppy error, ", exceptionRethrowFnName,
          " got an unknown exception ID.\";\n"]
