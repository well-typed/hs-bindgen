-- | Main entry point for using @hs-bindgen@ in IO mode.
--
-- Intended for unqualified import.

module HsBindgen.IO (
    hsBindgen
  , hsBindgenMacroLang
  ) where

import HsBindgen
import HsBindgen.Config.Internal
import HsBindgen.Frontend.RootHeader
import HsBindgen.Macro
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

-- | Convenience entry point using the default C macro language.
--
-- Use 'hsBindgenMacroLang' to supply a different macro-language backend.
hsBindgen ::
     TracerConfig Level     TraceMsg
  -> TracerConfig SafeLevel SafeTraceMsg
  -> BindgenConfig
  -> [UncheckedHashIncludeArg]
  -> Artefact CExpr a
  -> IO a
hsBindgen = hsBindgenMacroLang (pure . cExprLang)
