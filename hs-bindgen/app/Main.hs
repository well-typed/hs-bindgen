module Main (main) where

import Data.Tree (drawForest)

import HsBindgen.Cmdline
import HsBindgen.Lib

main :: IO ()
main = do
    Cmdline{
        verbosity
      , mode
      , clangArgs
      } <- getCmdline
    let tracer :: forall a. PrettyLogMsg a => Tracer IO a
        tracer = contramap prettyLogMsg $ mkTracerIO verbosity

    case mode of
      Preprocess{input, moduleOpts, renderOpts, output} ->
        preprocess tracer clangArgs input moduleOpts renderOpts output
      ParseCHeader{input} ->
        prettyC =<< parseCHeader tracer clangArgs input
      ShowClangAST{input} ->
        putStr . drawForest =<< showClangAST clangArgs input
