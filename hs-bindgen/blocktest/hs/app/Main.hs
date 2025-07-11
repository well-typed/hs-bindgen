module Main where

import Control.Exception
import Control.Monad

import Iterator

main :: IO ()
main = do
    bracket (makeToggle 0) releaseToggle $ \toggle ->
      replicateM_ 5 $
        print =<< toggleNext toggle

    bracket (makeCounter 5 2) releaseCounter $ \counter ->
      replicateM_ 5 $
        print =<< counterNext counter

    bracket (makeVarCounter 5) releaseVarCounter $ \varCounter ->
      forM_ [0 .. 4] $ \i ->
        print =<< varCounterNext varCounter i

