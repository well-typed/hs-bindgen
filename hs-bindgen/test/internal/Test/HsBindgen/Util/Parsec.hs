module Test.HsBindgen.Util.Parsec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Parsec (ParseError, Parsec, parse)

import HsBindgen.Util.Parsec (arguments)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

parseSimple :: Parsec String () a -> String -> Either ParseError a
parseSimple p = parse p ""

parseArgs :: String -> Either ParseError [String]
parseArgs = parseSimple arguments

tests :: TestTree
tests = testGroup "HsBindgen.Util.Parsec"
    [ testCase "simple"       $ parseArgs "a b"                   @?= Right ["a", "b"]
    , testCase "spacesPre"    $ parseArgs " \targ"                @?= Right ["arg"]
    , testCase "spacesSuf1"   $ parseArgs "arg\t "                @?= Right ["arg"]
    , testCase "spacesSuf2"   $ parseArgs "arg "                  @?= Right ["arg"]
    , testCase "spaces1"      $ parseArgs "  \t  "                @?= Right []
    , testCase "spaces2"      $ parseArgs " \t a b \t\n "         @?= Right ["a", "b"]
    , testCase "escape1"      $ parseArgs "a\\ b"                 @?= Right ["a b"]
    , testCase "escape2"      $ parseArgs "a1 arg\\ two a3"       @?= Right ["a1", "arg two", "a3"]
    , testCase "escapedEsape" $ parseArgs "c\\\\d"                @?= Right ["c\\d"]
    , testCase "escapedQuote" $ parseArgs "e\\\"f"                @?= Right ["e\"f"]
    , testCase "escaped^2"    $ parseArgs "c\\\\d e\\\"f"         @?= Right ["c\\d", "e\"f"]
    , testCase "escaped^3"    $ parseArgs " a\\ b c\\\\d e\\\"f " @?= Right ["a b", "c\\d", "e\"f"]
    , testCase "quote1"       $ parseArgs "\"a b\""               @?= Right ["a b"]
    , testCase "quote2"       $ parseArgs "a1 \"arg two\" a3"     @?= Right ["a1", "arg two", "a3"]
    , testCase "escape&quote" $ parseArgs "a\\ \"b c\"\\ d"       @?= Right ["a b c d"]
    ]
