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
    let tracer :: forall a. PrettyLogMsg a => Tracer IO a
        tracer = contramap prettyLogMsg $ mkTracerIO verbosity

    case mode of
      Preprocess{input, moduleOpts, renderOpts, output} ->
        preprocess tracer predicate clangArgs input moduleOpts renderOpts output
      ParseCHeader{input} ->
        prettyC =<< parseCHeader tracer predicate clangArgs input
      ShowClangAST{input} -> do
        ast <- getClangAST predicate clangArgs input
        putStr . drawForest $ fmap (fmap show) ast
      RenderComments{input, output} -> do
        comments <- getComments predicate clangArgs input
        withOutput output $ \h ->
          Blaze.renderHtmlToByteStringIO (BS.hPutStr h) $
            renderComments comments

withOutput :: Maybe FilePath -> (Handle -> IO r) -> IO r
withOutput (Just fp) = withFile fp WriteMode
withOutput Nothing   = ($ stdout)

