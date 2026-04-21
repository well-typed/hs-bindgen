-- | Trace messages for doxygen integration in @hs-bindgen@
--
-- This module provides hs-bindgen-specific trace message types
-- ('DoxygenMsg') with 'PrettyForTrace' and 'IsTrace' instances.
-- All doxygen-parser types are imported directly from "Doxygen.Parser".
--
module HsBindgen.Doxygen (
    DoxygenMsg(..)
  ) where

import Data.Text qualified as Text
import GHC.Generics (Generic)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer (IsTrace (..), Level, PrettyForTrace (..),
                              Source (..))
import HsBindgen.Util.Tracer qualified as Tracer

import Doxygen.Parser (DoxygenException (..))
import Doxygen.Parser.Warning

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | Trace messages emitted during doxygen invocation
data DoxygenMsg
  = DoxygenWarning DoxygenException
    -- ^ A non-fatal doxygen error (emitted as a warning before falling back
    -- to empty state)
  | DoxygenUnsupported Warning
    -- ^ Structured warning about unsupported or degraded content
  deriving stock (Show, Generic)

instance PrettyForTrace DoxygenMsg where
  prettyForTrace = \case
    DoxygenWarning (DoxygenFailed code stderr) ->
      PP.string $ "doxygen failed (exit code " ++ show code ++ "): " ++ stderr
    DoxygenWarning (DoxygenXMLParseError path err) ->
      PP.string $ "doxygen XML parse error in " ++ path ++ ": " ++ err
    DoxygenWarning DoxygenNotFound ->
      PP.string "doxygen not found on PATH; documentation comments will not be generated"
    DoxygenWarning (DoxygenIOError ioErr) ->
      PP.string $ "doxygen IO error: " ++ show ioErr
    DoxygenWarning (DoxygenOutputDirMissing dir) ->
      PP.string $ "doxygen output directory does not exist: " ++ dir
    DoxygenUnsupported w ->
      PP.string $ "unsupported content: " ++ Text.unpack w.explanation

instance IsTrace Level DoxygenMsg where
  getDefaultLogLevel = \case
    DoxygenWarning{}    -> Tracer.Warning
    DoxygenUnsupported{} -> Tracer.Info
  getSource  = const HsBindgen
  getTraceId = const "doxygen"
