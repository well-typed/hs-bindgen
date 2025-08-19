module Main (main) where

import Control.Monad (void)

import HsBindgen.App.Common
import HsBindgen.App.Dev

import HsBindgen.Clang
import HsBindgen.Lib
import HsBindgen.Util.Tracer (natTracer)

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = execDev =<< getDev

execDev :: Dev -> IO ()
execDev Dev{..} = case devCmd of
    DevCmdClang clangArgs -> execClang devGlobalOpts clangArgs
    DevCmdParse cmdOpts   -> execParse devGlobalOpts cmdOpts

execClang :: GlobalOpts -> ClangArgs -> IO ()
execClang GlobalOpts{..} clangArgs =
    void . withTracer tracerConfig $ \tracerM ->
      let tracer = contramap (TraceFrontend. FrontendClang) $
            natTracer id tracerM
      in  void $ withClang tracer setup (const (return Nothing))
  where
    setup :: ClangSetup
    setup = defaultClangSetup clangArgs $ ClangInputMemory "hs-bindgen-nop.h" ""

execParse :: GlobalOpts -> ParseOpts -> IO ()
execParse GlobalOpts{..} ParseOpts{..} = do
    let artefacts = ReifiedC :* Nil
    (I decls :* Nil) <-
      hsBindgen tracerConfig bindgenConfig inputs artefacts
    print decls
