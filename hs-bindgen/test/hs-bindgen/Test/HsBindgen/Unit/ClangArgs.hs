module Test.HsBindgen.Unit.ClangArgs (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Clang.LowLevel.Core

import HsBindgen.Clang
import HsBindgen.Clang.ExtraClangArgs (splitArguments)
import HsBindgen.Config.ClangArgs
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer

import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests testResources = testGroup "Test.HsBindgen.Unit.ClangArgs" [
      testCase "getTargetTriple" $ testGetTargetTriple testResources
    , splitArgumentsTests
    ]

{-------------------------------------------------------------------------------
  Target triple
-------------------------------------------------------------------------------}

testGetTargetTriple :: IO TestResources -> Assertion
testGetTargetTriple testResources = do
    clangArgsConfig <- getTestDefaultClangArgsConfig testResources []
    clangArgs <- either (panicIO . show) return $
      clangArgsConfigToClangArgs clangArgsConfig

    let setup :: ClangSetup
        setup = defaultClangSetup clangArgs $
                  ClangInputMemory "hs-bindgen-triple.h" ""

    triple <- withTracePredicate noReport defaultTracePredicate $ \tracer ->
      getTargetTriple tracer setup

    triple @?= "x86_64-pc-linux-musl"
  where
    getTargetTriple :: Tracer ClangMsg -> ClangSetup -> IO Text
    getTargetTriple tracer setup =
        withClang tracer setup $ \unit ->
          bracket
            (clang_getTranslationUnitTargetInfo unit)
            clang_TargetInfo_dispose
            clang_TargetInfo_getTriple

    noReport :: a -> IO ()
    noReport = const $ pure ()

{-------------------------------------------------------------------------------
  Split arguments
-------------------------------------------------------------------------------}

splitArgumentsTests :: TestTree
splitArgumentsTests = testGroup "splitStringArguments"
    [ testCase "simple"       $ splitArguments "a b"                   @?= ["a", "b"]
    , testCase "spacesPre"    $ splitArguments " \targ"                @?= ["arg"]
    , testCase "spacesSuf1"   $ splitArguments "arg\t "                @?= ["arg"]
    , testCase "spacesSuf2"   $ splitArguments "arg "                  @?= ["arg"]
    , testCase "spaces1"      $ splitArguments "  \t  "                @?= []
    , testCase "spaces2"      $ splitArguments " \t a b \t\n "         @?= ["a", "b"]
    , testCase "escape1"      $ splitArguments "a\\ b"                 @?= ["a b"]
    , testCase "escape2"      $ splitArguments "a1 arg\\ two a3"       @?= ["a1", "arg two", "a3"]
    , testCase "escapedEsape" $ splitArguments "c\\\\d"                @?= ["c\\d"]
    , testCase "escapedQuote" $ splitArguments "e\\\"f"                @?= ["e\"f"]
    , testCase "escaped^2"    $ splitArguments "c\\\\d e\\\"f"         @?= ["c\\d", "e\"f"]
    , testCase "escaped^3"    $ splitArguments " a\\ b c\\\\d e\\\"f " @?= ["a b", "c\\d", "e\"f"]
    , testCase "quote1"       $ splitArguments "\"a b\""               @?= ["a b"]
    , testCase "quote2"       $ splitArguments "a1 \"arg two\" a3"     @?= ["a1", "arg two", "a3"]
    , testCase "escape&quote" $ splitArguments "a\\ \"b c\"\\ d"       @?= ["a b c d"]
    ]
