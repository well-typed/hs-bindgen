module Test.HsBindgen.Unit.ClangArgs (tests) where

import System.Environment (setEnv, unsetEnv)
import Test.Tasty
import Test.Tasty.HUnit

import HsBindgen.C.Parser (getTargetTriple)
import HsBindgen.Clang (splitArguments, getExtraClangArgs)
import HsBindgen.Lib

import Test.Common.HsBindgen.TracePredicate
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests testResources = testGroup "Test.HsBindgen.Unit.ClangArgs" [
      testCase "getTargetTriple" $ testGetTargetTriple testResources
    , getExtraClangArgsTests
    , splitArgumentsTests
    ]

{-------------------------------------------------------------------------------
  Target triple
-------------------------------------------------------------------------------}

testGetTargetTriple :: IO TestResources -> Assertion
testGetTargetTriple testResources = do
    clangArgs <- getTestDefaultClangArgs testResources []

    triple <- withTracePredicate defaultTracePredicate $ \tracer ->
      getTargetTriple (contramap TraceClang tracer) clangArgs

    -- macos-latest (macos-14) returns "arm64-apple-macosx14.0.0"
    -- windows-latest (???) returns "x86_64-pc-windows-msvc19.41.34120"
    triple @?= "x86_64-pc-linux-gnu"

{-------------------------------------------------------------------------------
  Get extra clang args
-------------------------------------------------------------------------------}

getExtraClangArgsTests :: TestTree
getExtraClangArgsTests = testGroup "getExtraClangArgs" [
      testCase "!target" $
        assertExtraClangArgs [(eDef, "native")] Nothing ["native"]
    , -- Without target, we ignore target-specific `clang` arguments.
      testCase "!target+other" $
        assertExtraClangArgs [(eDef, "native"), (eLnx, "cross")] Nothing ["native"]
    , testCase "target" $
        assertExtraClangArgs [(eLnx, "cross")] (Just Target_Linux_X86_64) ["cross"]
    , -- With target, we exclusively use target-specific `clang` arguments,
      -- if present.
      testCase "target+other" $
        assertExtraClangArgs [(eDef, "native"), (eLnx, "cross")] (Just Target_Linux_X86_64) ["cross"]
    , -- With target, we fall back to the default `clang` arguments.
      testCase "target+!other" $
        assertExtraClangArgs [(eDef, "native")] (Just Target_Linux_X86_64) ["native"]
    , testCase "target+otherEmpty" $
        assertExtraClangArgs [(eDef, "native"), (eLnx, "")] (Just Target_Linux_X86_64) ["native"]
    ]
  where
    assertExtraClangArgs :: [(EnvVar, EnvVal)] -> Maybe Target -> [String] -> IO ()
    assertExtraClangArgs xs mtarget x = do
        withTracePredicate defaultTracePredicate $ \tracer ->
          assertWithEnv xs (getExtraClangArgs tracer mtarget) x

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

{-------------------------------------------------------------------------------
  Internal auxiliary: setup environment
-------------------------------------------------------------------------------}

type EnvVar = String
type EnvVal = String

eDef :: EnvVar
eDef = "BINDGEN_EXTRA_CLANG_ARGS"

eLnx :: EnvVal
eLnx = eDef <> "_x86_64-pc-linux"

withEnv :: [(EnvVar, EnvVal)] -> IO a -> IO a
withEnv xs k = do
    mapM_ unsetEnv [eDef, eLnx]
    mapM_ (uncurry setEnv) xs
    r <- k
    mapM_ (unsetEnv . fst) xs
    pure r

assertWithEnv :: (Eq a, Show a) => [(EnvVar, EnvVal)] -> IO a -> a -> Assertion
assertWithEnv xs k x = withEnv xs k >>= \r -> r @?= x

