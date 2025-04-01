module Test.HsBindgen.C.Parser (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import HsBindgen.C.Parser (getTargetTriple)
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: ClangArgs -> TestTree
tests args = testGroup "HsBindgen.C.Parser"
    [ testGetTargetTriple args
    ]

testGetTargetTriple :: ClangArgs -> TestTree
testGetTargetTriple args = testCase "getTargetTriple" $ do
    triple <- getTargetTriple args

    -- macos-latest (macos-14) returns "arm64-apple-macosx14.0.0"
    -- windows-latest (???) returns "x86_64-pc-windows-msvc19.41.34120"
    triple @?= "x86_64-pc-linux-gnu"
