-- | Miscellaneous test utilities
module Test.HsBindgen.Util.Clang (
    getClangArgs
  ) where

import Clang.Paths
import HsBindgen.Lib

-------------------------------------------------------------------------------
-- default ClangArgs used in tests
-------------------------------------------------------------------------------

getClangArgs :: FilePath -> [FilePath] -> ClangArgs
getClangArgs packageRoot extraIncludeDirs = def {
      clangTarget = Just $
        (Target_Linux_X86_64, TargetEnvOverride "gnu")
    , clangCStandard = Just $
        C23
    , clangSystemIncludePathDirs = [
          CIncludePathDir (packageRoot </> "musl-include/x86_64")
        ]
    , clangQuoteIncludePathDirs =
        map (CIncludePathDir . (</>) packageRoot) extraIncludeDirs
    }
