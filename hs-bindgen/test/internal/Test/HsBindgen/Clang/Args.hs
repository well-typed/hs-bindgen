module Test.HsBindgen.Clang.Args (tests) where

import Test.Tasty (TestTree, testGroup)

import Control.Tracer (Tracer)
import System.Environment (setEnv, unsetEnv)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import HsBindgen.Clang.Args
import HsBindgen.Lib

type EnvVar = String
type Content = String

eDef, eLnx :: String
eDef = "BINDGEN_EXTRA_CLANG_ARGS"
eLnx = eDef <> "_x86_64-pc-linux"

withEnv :: [(EnvVar, Content)] -> IO a -> IO a
withEnv xs k = do
  mapM_ unsetEnv [eDef, eLnx]
  mapM_ (uncurry setEnv) xs
  r <- k
  mapM_ (unsetEnv . fst) xs
  pure r

assertWithEnv :: (Eq a, Show a) => [(EnvVar, Content)] -> IO a -> a -> Assertion
assertWithEnv xs k x = withEnv xs k >>= \r -> r @?= x

tests :: Tracer IO (TraceWithCallStack Trace) -> TestTree
tests tracer = testGroup "HsBindgen.Clang.Args" [ getExtraClangArgsTests (useTrace TraceExtraClangArgs tracer)
                                                , splitArgumentsTests
                                                ]

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

getExtraClangArgsTests :: Tracer IO (TraceWithCallStack ExtraClangArgsLog) -> TestTree
getExtraClangArgsTests tracer = testGroup "getExtraClangArgs" [
          testCase "!target" $
            assertWithEnv [(eDef, "native")]
              (getExtraClangArgs tracer Nothing) ["native"]
        , -- Without target, we ignore target-specific `clang` arguments.
          testCase "!target+other" $
            assertWithEnv [(eDef, "native"), (eLnx, "cross")]
              (getExtraClangArgs tracer Nothing) ["native"]
        , testCase "target" $
            assertWithEnv [(eLnx, "cross")]
              (getExtraClangArgs tracer (Just Target_Linux_X86_64)) ["cross"]
        , -- With target, we exclusively use target-specific `clang` arguments,
          -- if present.
          testCase "target+other" $
            assertWithEnv [(eDef, "native"), (eLnx, "cross")]
              (getExtraClangArgs tracer (Just Target_Linux_X86_64)) ["cross"]
        , -- With target, we fall back to the default `clang` arguments.
          testCase "target+!other" $
            assertWithEnv [(eDef, "native")]
              (getExtraClangArgs tracer (Just Target_Linux_X86_64)) ["native"]
        , testCase "target+otherEmpty" $
            assertWithEnv [(eDef, "native"), (eLnx, "")]
              (getExtraClangArgs tracer (Just Target_Linux_X86_64)) ["native"]
        ]
