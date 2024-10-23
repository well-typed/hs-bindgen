{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.TestTH.Spliced
import Foreign

main :: IO ()
main = defaultMain $ testGroup "test-th"
    [ testCase "constants" $ do
        sizeOf (undefined :: CMyStruct) @?= 8
        alignment (undefined :: CMyStruct) @?= 4
    ]

