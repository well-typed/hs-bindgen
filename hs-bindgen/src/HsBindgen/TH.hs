-- | Main entry point for using @hs-bindgen@ in TH mode
module HsBindgen.TH (generateBindingsFor) where

import Control.Monad.IO.Class
import Language.Haskell.TH

import HsBindgen.Clang.Args
import HsBindgen.Clang.Util.Diagnostics (Diagnostic)
import HsBindgen.Lib
import HsBindgen.Util.Tracer

-- | Generate bindings for the given C header
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/11>
-- We need to think about how we want to handle configuration in TH mode.
generateBindingsFor :: FilePath -> Q [Dec]
generateBindingsFor fp = do
    cHeader <- liftIO $ parseCHeader traceWarnings traceParseMsgs p args fp
    genDecls cHeader
  where
    traceWarnings :: Tracer IO Diagnostic
    traceWarnings = contramap show $ mkTracerQ False

    traceParseMsgs :: Tracer IO Skipped
    traceParseMsgs = contramap prettyLogMsg $ mkTracerQ False

    p :: Predicate
    p = SelectFromMainFile

    args :: ClangArgs
    args = defaultClangArgs

