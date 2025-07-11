module Main where

import Control.Exception
import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit

import Iterator.Safe

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup "blocktest" [
      testCase "toggle" testToggle
    , testCase "counter" testCounter
    , testCase "varCounter" testVarCounter
    ]

{-------------------------------------------------------------------------------
  Individual tests
-------------------------------------------------------------------------------}

-- | Toggle between 0 and 1
testToggle :: Assertion
testToggle = do
    result <-
      bracket (makeToggle 0) releaseToggle $ \toggle -> do
        replicateM 5 $ toggleNext toggle
    assertEqual "" [0,1,0,1,0] $ result

-- | Count from 5, increment by 2
testCounter :: Assertion
testCounter = do
    result <-
      bracket (makeCounter 5 2) releaseCounter $ \counter ->
        replicateM 5 $ counterNext counter
    assertEqual "" [5,7,9,11,13] $ result

-- | Count from 5, variable increment
testVarCounter :: Assertion
testVarCounter = do
    result <-
      bracket (makeVarCounter 5) releaseVarCounter $ \varCounter ->
        forM [0 .. 4] $ \i -> varCounterNext varCounter i
    assertEqual "" [5,5,6,8,11] $ result
