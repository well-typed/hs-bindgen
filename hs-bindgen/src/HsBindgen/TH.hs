-- | Main entry point for using @hs-bindgen@ in TH mode
module HsBindgen.TH (generateBindingsFor) where

import Control.Monad.IO.Class
import Language.Haskell.TH

import HsBindgen.Lib

-- | Generate bindings for the given C header
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/11>
-- We need to think about how we want to handle configuration in TH mode.
--
-- TODO: add TranslationOpts argument
--
generateBindingsFor ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> FilePath       -- ^ C header
  -> Q [Dec]
generateBindingsFor relPath fp = do
    cHeader <- liftIO $ withTranslationUnit relPath traceWarnings args fp $
                          parseCHeader relPath traceSkipped p
    genTH defaultTranslationOpts cHeader
  where
    traceWarnings :: Tracer IO Diagnostic
    traceWarnings = contramap show $ mkTracerQ False

    traceSkipped :: Tracer IO Skipped
    traceSkipped = contramap prettyLogMsg $ mkTracerQ False

    p :: Predicate
    p = SelectFromMainFile

    args :: ClangArgs
    args = defaultClangArgs

