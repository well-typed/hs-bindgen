module HsBindgen.Clang.CompareVersions (
    CompareVersionsMsg(..)
  , compareClangVersions
  ) where

import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

import Clang.Version (clangVersionCompileTime, clang_getClangVersion)

import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data CompareVersionsMsg =
    CompileTimeAndRuntimeVersionMismatch Text Text
  deriving stock (Show)

instance PrettyForTrace CompareVersionsMsg where
  prettyForTrace = \case
    CompileTimeAndRuntimeVersionMismatch compileTimeVersion runtimeVersion ->
      PP.hangs' "clang version mismatch:" 2 [
          "clang compile time version:" <+> PP.textToCtxDoc compileTimeVersion
        , "clang runtime version:     " <+> PP.textToCtxDoc runtimeVersion
        ]

instance IsTrace Level CompareVersionsMsg where
  getDefaultLogLevel = \case
    CompileTimeAndRuntimeVersionMismatch _ _ -> Warning

  getSource  = const HsBindgen

  getTraceId = const "compare-clang-versions"

{-------------------------------------------------------------------------------
  Comparison functions
-------------------------------------------------------------------------------}

-- | Check if the compile time version of clang is different than the
-- runtime version, issue a warning if that's the case to inform the user
-- of this fact.
--
compareClangVersions :: Tracer CompareVersionsMsg -> IO ()
compareClangVersions tracer = do
  let compileTimeVersion = clangVersionCompileTime
  runtimeVersion <- liftIO clang_getClangVersion
  when (compileTimeVersion /= runtimeVersion) $
    traceWith tracer (CompileTimeAndRuntimeVersionMismatch compileTimeVersion runtimeVersion)

