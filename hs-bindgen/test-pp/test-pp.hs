{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))
import Foreign (sizeOf, alignment)
import Foreign.C.Types (CLong)

import Test01

main :: IO ()
main = defaultMain $ testGroup "test-pp"
    [ testCase "constants" $ do
        sizeOf (undefined :: MyStruct) @?= 8
        alignment (undefined :: MyStruct) @?= 4

    , testCase "function" $ do
        res <- my_fma 2 3 5
        res @?= 11
    ]

-- test that instances for using pLUS are present
_myPlus :: CLong -> CLong -> CLong
_myPlus x y = Test01.pLUS x y
