-- | Main entry point for using @hs-bindgen@ in TH mode
module HsBindgen.TH (generateBindingsFor) where

import Language.Haskell.TH

import HsBindgen.Spec
import HsBindgen.Spec.Execution
import HsBindgen.Util.Tracer

-- | Generate bindings for the given C header
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/11>
-- We need to think about how we want to handle configuration in TH mode.
generateBindingsFor :: FilePath -> Q [Dec]
generateBindingsFor fp =
    execSpec $ GenSplice (ParseCHeader (mkTracerQ False) [] fp) GenDecls
