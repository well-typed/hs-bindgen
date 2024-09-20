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
    cmdline@Cmdline{verbosity, mode} <- getCmdline

    let tracer :: Tracer IO String
        tracer = mkTracerIO verbosity

    execMode cmdline tracer (mode)

execMode :: Cmdline -> Tracer IO String -> Mode -> IO ()
execMode cmdline tracer = \case
    ModePreprocess{input, moduleOpts, renderOpts, output} ->
      preprocess $ Preprocess {
          preprocessTraceWarnings  = contramap show tracer
        , preprocessTraceParseMsgs = contramap prettyLogMsg tracer
        , preprocessPredicate      = predicate
        , preprocessClangArgs      = clangArgs
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
          predicate
          clangArgs
          input
      prettyC cHeader
    ModeShowClangAST{input} -> do
      clangAST <-
        getClangAST
          (contramap show tracer)
          predicate
          clangArgs
          input
      putStr . drawForest $ fmap (fmap show) clangAST
    ModeRenderComments{input, output} -> do
      comments <-
        getComments
          (contramap show tracer)
          predicate
          clangArgs
          input
      withOutput output $ \h ->
        Blaze.renderHtmlToByteStringIO (BS.hPutStr h) $
          renderComments comments
    Dev devMode ->
      execDevMode cmdline tracer devMode
  where
    Cmdline{predicate, clangArgs} = cmdline

execDevMode :: Cmdline -> Tracer IO String -> DevMode -> IO ()
execDevMode _cmdline tracer = \case
    DevModePrelude ->
      genPrelude (show `contramap` tracer)

withOutput :: Maybe FilePath -> (Handle -> IO r) -> IO r
withOutput (Just fp) = withFile fp WriteMode
withOutput Nothing   = ($ stdout)

