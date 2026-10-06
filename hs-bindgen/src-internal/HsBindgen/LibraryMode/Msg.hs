-- | Trace messages of library mode
module HsBindgen.LibraryMode.Msg (
    LibraryModeMsg(..)
  ) where

import Data.List qualified as List
import Text.SimplePrettyPrint (hsep, string)

import Clang.Paths

import HsBindgen.Config.Prelims (BaseModuleName, baseModuleNameToString)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

data LibraryModeMsg =
    -- | Generating one module from these headers (more than one when their
    -- declarations use each other)
    LibraryModeProcessing (NonEmpty RealPath) BaseModuleName
  deriving stock (Show)

instance PrettyForTrace LibraryModeMsg where
  prettyForTrace = \case
    LibraryModeProcessing headers moduleName -> hsep [
        "Processing:"
      , string $ List.intercalate ", " (map getRealPath (toList headers))
      , "->"
      , string $ baseModuleNameToString moduleName
      ]

instance IsTrace Level LibraryModeMsg where
  getDefaultLogLevel = \case
    LibraryModeProcessing{} -> Notice
  getSource  = const HsBindgen
  getTraceId = const "library-mode"
