module Main (main) where

import Data.ByteString qualified as BS
import Data.Tree (drawForest)
import System.IO
import Text.Blaze.Html.Renderer.Utf8 qualified as Blaze

import HsBindgen.App.Cmdline
import HsBindgen.App.RenderComments
import HsBindgen.Bootstrap.Prelude (genPrelude)
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline@Cmdline{cmdVerbosity, cmdMode} <- getCmdline

    let tracer :: Tracer IO String
        tracer = mkTracerIO cmdVerbosity

    execMode cmdline tracer (cmdMode)

execMode :: Cmdline -> Tracer IO String -> Mode -> IO ()
execMode cmdline tracer = \case
    ModePreprocess{input, moduleOpts, renderOpts, output} ->
      preprocess $ Preprocess {
          preprocessTraceWarnings  = contramap show tracer
        , preprocessTraceParseMsgs = contramap prettyLogMsg tracer
        , preprocessPredicate      = cmdPredicate
        , preprocessClangArgs      = cmdClangArgs
        , preprocessInputPath      = input
        , preprocessModuleOpts     = moduleOpts
        , preprocessRenderOpts     = renderOpts
        , preprocessOutputPath     = output
        }
    ModeParseCHeader{input} -> do
      cHeader <-
        parseCHeader
          (contramap show tracer)
          (contramap prettyLogMsg tracer)
          cmdPredicate
          cmdClangArgs
          input
      prettyC cHeader
    ModeShowClangAST{input} -> do
      clangAST <-
        getClangAST
          (contramap show tracer)
          cmdPredicate
          cmdClangArgs
          input
      putStr . drawForest $ fmap (fmap show) clangAST
    ModeRenderComments{input, output} -> do
      comments <-
        getComments
          (contramap show tracer)
          cmdPredicate
          cmdClangArgs
          input
      withOutput output $ \h ->
        Blaze.renderHtmlToByteStringIO (BS.hPutStr h) $
          renderComments comments
    Dev devMode ->
      execDevMode cmdline tracer devMode
  where
    Cmdline{cmdPredicate, cmdClangArgs} = cmdline

execDevMode :: Cmdline -> Tracer IO String -> DevMode -> IO ()
execDevMode _cmdline tracer = \case
    DevModePrelude ->
      genPrelude (prettyLogMsg `contramap` tracer)

withOutput :: Maybe FilePath -> (Handle -> IO r) -> IO r
withOutput (Just fp) = withFile fp WriteMode
withOutput Nothing   = ($ stdout)

