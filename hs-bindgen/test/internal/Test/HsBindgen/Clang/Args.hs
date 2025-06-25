module Test.HsBindgen.Clang.Args (tests) where

import System.Environment (setEnv, unsetEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import HsBindgen.Clang.Args
import HsBindgen.Lib
import Test.Internal.Tracer (defaultTracePredicate, withTracePredicate)

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

tests :: TestTree
tests = do
  testGroup "HsBindgen.Clang.Args" [ getExtraClangArgsTests
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
  where assertExtraClangArgs :: [(EnvVar, Content)] -> Maybe Target -> [String] -> IO ()
        assertExtraClangArgs xs mtarget x = do
          withTracePredicate defaultTracePredicate $ \tracer ->
            assertWithEnv xs (getExtraClangArgs (contramap TraceExtraClangArgs tracer) mtarget) x
