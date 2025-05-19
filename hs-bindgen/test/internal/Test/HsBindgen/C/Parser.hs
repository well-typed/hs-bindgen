module Test.HsBindgen.C.Parser (tests) where

import Control.Tracer (Tracer)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.C.Parser (getTargetTriple)
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: Tracer IO (TraceWithCallStack Trace) -> ClangArgs -> TestTree
tests tracer args = testGroup "HsBindgen.C.Parser"
    [ testGetTargetTriple tracer args
    ]

testGetTargetTriple :: Tracer IO (TraceWithCallStack Trace) -> ClangArgs -> TestTree
testGetTargetTriple tracer args = testCase "getTargetTriple" $ do
    triple <- getTargetTriple (useTrace TraceExtraClangArgs tracer) args

    -- macos-latest (macos-14) returns "arm64-apple-macosx14.0.0"
    -- windows-latest (???) returns "x86_64-pc-windows-msvc19.41.34120"
    triple @?= "x86_64-pc-linux-gnu"
