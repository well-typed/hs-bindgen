module Main (main) where

import Data.Tree (drawForest)

import HsBindgen.Cmdline
import HsBindgen.Lib

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
