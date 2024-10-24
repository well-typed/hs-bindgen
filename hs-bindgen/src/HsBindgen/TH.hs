-- | Main entry point for using @hs-bindgen@ in TH mode
module HsBindgen.TH (generateBindingsFor) where

import Control.Monad.IO.Class
import Language.Haskell.TH

import HsBindgen.Lib

-- | Generate bindings for the given C header
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/11>
-- We need to think about how we want to handle configuration in TH mode.
generateBindingsFor :: FilePath -> Q [Dec]
generateBindingsFor fp = do
    cHeader <- liftIO $ withTranslationUnit traceWarnings args fp $
                          parseCHeader traceSkipped p
    genTH cHeader
  where
    traceWarnings :: Tracer IO Diagnostic
    traceWarnings = contramap show $ mkTracerQ False

    traceSkipped :: Tracer IO Skipped
    traceSkipped = contramap prettyLogMsg $ mkTracerQ False

    p :: Predicate
    p = SelectFromMainFile

    args :: ClangArgs
    args = defaultClangArgs

