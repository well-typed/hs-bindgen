module Test.HsBindgen.C.Parser (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.C.Parser (getTargetTriple)
import HsBindgen.Lib
import Test.Internal.Tracer (withTracerTest)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO AnsiColor -> ClangArgs -> TestTree
tests getAnsiColor args = testGroup "HsBindgen.C.Parser"
    [ testGetTargetTriple getAnsiColor args
    ]

testGetTargetTriple :: IO AnsiColor -> ClangArgs -> TestTree
testGetTargetTriple getAnsiColor args = testCase "getTargetTriple" $ do
    triple <- withTracerTest getAnsiColor $ \tracer -> getTargetTriple (useTrace TraceExtraClangArgs tracer) args

    -- macos-latest (macos-14) returns "arm64-apple-macosx14.0.0"
    -- windows-latest (???) returns "x86_64-pc-windows-msvc19.41.34120"
    triple @?= "x86_64-pc-linux-gnu"
