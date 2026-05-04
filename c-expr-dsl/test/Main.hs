module Main (main) where

import Test.Tasty

import Test.CExpr.Parse qualified as Parse
import Test.CExpr.Typecheck qualified as Typecheck

main :: IO ()
main = defaultMain $ testGroup "c-expr-dsl" [
      Parse.tests
    , Typecheck.tests
    ]
