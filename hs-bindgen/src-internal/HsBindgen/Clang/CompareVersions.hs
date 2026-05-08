module HsBindgen.Clang.CompareVersions (
    CompareVersionsMsg(..)
  , compareClangVersions
  ) where

import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

import Clang.Version

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
          "clang compile time version:" <+> PP.text compileTimeVersion
        , "clang runtime version:     " <+> PP.text runtimeVersion
        ]

instance IsTrace Level CompareVersionsMsg where
  getDefaultLogLevel = \case
    CompileTimeAndRuntimeVersionMismatch _ _ -> Warning

  getSource  = const HsBindgen

  getTraceId = const "compare-clang-versions"

{-------------------------------------------------------------------------------
  Comparison functions
-------------------------------------------------------------------------------}

-- | Check that the runtime version of libclang is compatible with the
-- compile-time version
--
-- A warning is issued if they are incompatible.
compareClangVersions :: Tracer CompareVersionsMsg -> IO ()
compareClangVersions tracer
    | isCompatibleClangVersion compileTimeClangVersion runtimeClangVersion =
        return ()
    | otherwise = traceWith tracer . withCallStack $
        CompileTimeAndRuntimeVersionMismatch
          compileTimeClangVersionString
          runtimeClangVersionString
