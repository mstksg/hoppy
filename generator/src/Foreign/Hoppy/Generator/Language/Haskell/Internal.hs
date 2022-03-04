-- This file is part of Hoppy.
--
-- Copyright 2015-2022 Bryan Gardiner <bog@khumba.net>
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

-- | Internal portion of the Haskell code generator.
module Foreign.Hoppy.Generator.Language.Haskell.Internal (
  Generation,
  generate,
  generatedFiles,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif
import Control.Arrow ((&&&))
import Control.Monad (forM, when)
#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except (throwError)
#else
import Control.Monad.Error (throwError)
#endif
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriterT, tell)
import Data.Foldable (forM_)
import Data.Graph (SCC (AcyclicSCC, CyclicSCC), stronglyConnComp)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat, mempty)
#endif
import qualified Data.Set as S
import Foreign.Hoppy.Generator.Common
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Language.Haskell
import System.FilePath ((<.>), pathSeparator)

-- | The in-memory result of generating Haskell code for an interface.
newtype Generation = Generation
  { generatedFiles :: M.Map FilePath String
    -- ^ A map from paths of generated files to the contents of those files.
    -- The file paths are relative paths below the Haskell generation root.
  }

-- | Runs the C++ code generator against an interface.
generate :: Interface -> ComputedInterfaceData -> Either ErrorMsg Generation
generate iface computed = do
  -- Build the partial generation of each module.
  modPartials <- forM (M.elems $ interfaceModules iface) $ \m ->
    (,) m <$> execGenerator iface computed m (generateSource m)

  -- Compute the strongly connected components.  If there is a nontrivial SCC,
  -- then there is a module import cycle that we'll have to break with hs-boot
  -- files.
  let partialsByHsName :: M.Map HsModuleName Partial
      partialsByHsName = M.fromList $ map ((partialModuleHsName &&& id) . snd) modPartials

      sccInput :: [((Module, Partial), Partial, [Partial])]
      sccInput = for modPartials $ \x@(_, p) ->
        (x, p,
         mapMaybe (flip M.lookup partialsByHsName . hsImportModule) $
         M.keys $ getHsImportSet $ outputImports $ partialOutput p)

      sccs :: [SCC (Module, Partial)]
      sccs = stronglyConnComp sccInput

  fileContents <- execWriterT $ forM_ sccs $ \case
    AcyclicSCC (_, p) -> tell [finishPartial p "hs"]
    CyclicSCC mps -> do
      let cycleModNames = S.fromList $ map (partialModuleHsName . snd) mps
      forM_ mps $ \(m, p) -> do
        -- Create a boot partial.
        pBoot <- lift $ execGenerator iface computed m (generateBootSource m)

        -- Change the source and boot partials so that all imports of modules in
        -- this cycle are {-# SOURCE #-} imports.
        let p' = setSourceImports cycleModNames p
            pBoot' = setSourceImports cycleModNames pBoot

        -- Emit the completed partials.
        tell [finishPartial p' "hs", finishPartial pBoot' "hs-boot"]

  return $ Generation $ M.fromList fileContents

  where finishPartial :: Partial -> String -> (FilePath, String)
        finishPartial p fileExt =
          (listSubst '.' pathSeparator (partialModuleHsName p) <.> fileExt,
           prependExtensions $ renderPartial p)

        setSourceImports :: S.Set HsModuleName -> Partial -> Partial
        setSourceImports modulesToSourceImport p =
          let output = partialOutput p
              imports = outputImports output
              imports' = makeHsImportSet $
                         M.mapWithKey (setSourceImportIfIn modulesToSourceImport) $
                         getHsImportSet imports
              output' = output { outputImports = imports' }
          in p { partialOutput = output' }

        setSourceImportIfIn :: S.Set HsModuleName -> HsImportKey -> HsImportSpecs -> HsImportSpecs
        setSourceImportIfIn modulesToSourceImport key specs =
          if hsImportModule key `S.member` modulesToSourceImport
          then specs { hsImportSource = True }
          else specs

prependExtensions :: String -> String
prependExtensions = (prependExtensionsPrefix ++)

prependExtensionsPrefix :: String
prependExtensionsPrefix =
  -- MultiParamTypeClasses is necessary for instances of Decodable and
  -- Encodable.  FlexibleContexts is needed for the type signature of the
  -- function that wraps the actual callback function in callback creation
  -- functions.
  --
  -- FlexibleInstances and TypeSynonymInstances are enabled to allow conversions
  -- to and from String, which is really [Char].
  --
  -- UndecidableInstances is needed for instances of the form "SomeClassConstPtr
  -- a => SomeClassValue a" (overlapping instances are used here too).
  concat $ "{-# LANGUAGE " : intersperse ", " extensions ++ [" #-}\n"]
  where extensions =
          [ "FlexibleContexts"
          , "FlexibleInstances"
          , "ForeignFunctionInterface"
          , "MonoLocalBinds"
          , "MultiParamTypeClasses"
          , "ScopedTypeVariables"
          , "TypeSynonymInstances"
          , "UndecidableInstances"
          ]

generateSource :: Module -> Generator ()
generateSource m = do
  forM_ (moduleExports m) $ sayExport SayExportForeignImports
  forM_ (moduleExports m) $ sayExport SayExportDecls

  iface <- askInterface
  when (interfaceExceptionSupportModule iface == Just m) $
    sayExceptionSupport True

  addendumHaskell $ getAddendum m

generateBootSource :: Module -> Generator ()
generateBootSource m = do
  forM_ (moduleExports m) $ sayExport SayExportBoot

  iface <- askInterface
  when (interfaceExceptionSupportModule iface == Just m) $
    sayExceptionSupport False

sayExport :: SayExportMode -> Export -> Generator ()
sayExport mode export = do
  sayExportHaskell mode export

  when (mode == SayExportDecls) $
    addendumHaskell $ getAddendum export

-- | Outputs the @ExceptionDb@ needed by all Haskell gateway functions that deal
-- with exceptions.
sayExceptionSupport :: Bool -> Generator ()
sayExceptionSupport doDecls = do
  iface <- askInterface
  addExport "exceptionDb'"
  addImports hsImportForRuntime
  ln
  sayLn "exceptionDb' :: HoppyFHR.ExceptionDb"
  when doDecls $ do
    addImports $ mconcat [hsImport1 "Prelude" "($)",
                          hsImportForMap]
    sayLn "exceptionDb' = HoppyFHR.ExceptionDb $ HoppyDM.fromList"
    indent $ do
      let classes = interfaceAllExceptionClasses iface
      case classes of
        [] -> sayLn "[]"
        _ -> do
          addImports hsImportForPrelude
          forM_ (zip classes (True : repeat False)) $ \(cls, first) -> do
            exceptionId <-
              fromMaybeM (throwError $ "sayExceptionSupport: Internal error, " ++ show cls ++
                          " has no exception ID.") $
              interfaceExceptionClassId iface cls
            typeName <- toHsDataTypeName Nonconst cls
            saysLn [if first then "[ (" else ", (",
                    "HoppyFHR.ExceptionId ", show $ getExceptionId exceptionId,
                    ", HoppyFHR.cppExceptionInfo (HoppyP.undefined :: ",
                    typeName, "))"]
          sayLn "]"
