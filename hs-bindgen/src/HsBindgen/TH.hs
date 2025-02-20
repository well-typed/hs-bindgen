-- | Main entry point for using @hs-bindgen@ in TH mode
module HsBindgen.TH (generateBindingsFor) where

import Control.Monad.IO.Class
import Language.Haskell.TH

import HsBindgen.Clang.Paths
import HsBindgen.ExtBindings
import HsBindgen.Lib

-- | Generate bindings for the given C header
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/11>
-- We need to think about how we want to handle configuration in TH mode.
--
-- TODO: add TranslationOpts argument
--
generateBindingsFor ::
     [CIncludePathDir] -- ^ System include search path directories
  -> [CIncludePathDir] -- ^ Non-system include search path directories
  -> [FilePath]        -- ^ External bindings configuration
  -> CHeaderRelPath    -- ^ Input header
  -> Q [Dec]
generateBindingsFor sysIncPathDirs incPathDirs extBindingsPaths relPath = do
    cHeader <- liftIO $ do
      sysIncAbsPathDirs <- either fail return
        =<< resolveCIncludeAbsPathDirs sysIncPathDirs
      incAbsPathDirs <- either fail return
        =<< resolveCIncludeAbsPathDirs incPathDirs
      let includePathDirs = sysIncAbsPathDirs ++ incAbsPathDirs
      absPath <- either fail return =<< resolveHeader includePathDirs relPath
      let clangArgs = defaultClangArgs {
              clangSystemIncludePathDirs = sysIncPathDirs
            , clangIncludePathDirs       = incPathDirs
            }
      extBindings <- loadExtBindings' includePathDirs extBindingsPaths
      withTranslationUnit traceWarnings clangArgs absPath $
        parseCHeader traceSkipped SelectFromMainFile extBindings
    genTH relPath defaultTranslationOpts cHeader
  where
    traceWarnings :: Tracer IO Diagnostic
    traceWarnings = contramap show $ mkTracerQ False

    traceSkipped :: Tracer IO Skipped
    traceSkipped = contramap prettyLogMsg $ mkTracerQ False
