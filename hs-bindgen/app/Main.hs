module Main (main) where

import Data.ByteString qualified as BS
import Data.Tree (drawForest)
import Text.Blaze.Html.Renderer.Utf8 qualified as Blaze

import HsBindgen.App.Cmdline
import HsBindgen.App.RenderComments
import HsBindgen.Lib
import System.IO

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    Cmdline{
        verbosity
      , predicate
      , mode
      , clangArgs
      } <- getCmdline

    let tracer :: Tracer IO String
        tracer = mkTracerIO verbosity

    case mode of
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

withOutput :: Maybe FilePath -> (Handle -> IO r) -> IO r
withOutput (Just fp) = withFile fp WriteMode
withOutput Nothing   = ($ stdout)

