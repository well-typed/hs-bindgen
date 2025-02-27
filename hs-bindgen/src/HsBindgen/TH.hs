-- | Main entry point for using @hs-bindgen@ in TH mode
module HsBindgen.TH (generateBindingsFor) where

import Control.Monad.IO.Class
import Language.Haskell.TH

import HsBindgen.Clang.Paths
import HsBindgen.Clang.Paths.Resolve
import HsBindgen.Lib

-- | Generate bindings for the given C header
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/11>
-- We need to think about how we want to handle configuration in TH mode.
--
-- TODO: add TranslationOpts argument
--
generateBindingsFor ::
     [CIncludePathDir]  -- ^ System include search path directories
  -> [CIncludePathDir]  -- ^ Quote include search path directories
  -> CHeaderIncludePath -- ^ Input header
  -> Q [Dec]
generateBindingsFor sysIncPathDirs quoteIncPathDirs headerIncludePath = do
    src <- liftIO $ resolveHeader' args headerIncludePath
    cHeader <- liftIO $
      withTranslationUnit traceWarnings args src $
        parseCHeader traceSkipped SelectFromMainFile
    genTH headerIncludePath defaultTranslationOpts cHeader
  where
    traceWarnings :: Tracer IO Diagnostic
    traceWarnings = contramap show $ mkTracerQ False

    traceSkipped :: Tracer IO Skipped
    traceSkipped = contramap prettyLogMsg $ mkTracerQ False

    args :: ClangArgs
    args = defaultClangArgs {
        clangSystemIncludePathDirs = sysIncPathDirs
      , clangQuoteIncludePathDirs  = quoteIncPathDirs
      }
