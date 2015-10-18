-- | Provides access to Hoppy file paths.
module Foreign.Hoppy.Paths (
  includeDir,
  ) where

import Paths_hoppy (getDataFileName)

-- | The directory containing C++ header files used by some bindings provided by
-- Hoppy.
includeDir :: IO FilePath
includeDir = getDataFileName "include"
