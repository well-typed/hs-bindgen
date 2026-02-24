module Test.HsBindgen.Unit.Pretty (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Text.SimplePrettyPrint (Pretty (..))

import HsBindgen.Backend.HsModule.Pretty.Expr ()
import HsBindgen.Backend.SHs.AST

int :: Integer -> ClosedExpr
int n = (EIntegral n Nothing)

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.Pretty" [
      testGroup "Tuples" [
          testCase "saturated 2-tuple"     $ testPrettyExpr tup2E  tup2R
        , testCase "saturated 4-tuple"     $ testPrettyExpr tup4E  tup4R
        , testCase "2-section"             $ testPrettyExpr sec2E  sec2R
        , testCase "4-section"             $ testPrettyExpr sec4E  sec4R
        , testCase "unsaturated 2-section" $ testPrettyExpr usec2E usec2R
        , testCase "unsaturated 4-section" $ testPrettyExpr usec4E usec4R
        ]
    ]
  where
    tup2E :: ClosedExpr
    tup2R :: String
    tup2E = EApp (EApp (EBoxedTup (Plus2 0)) (int 0)) (int 1)
    tup2R = "(0, 1)"

    tup4E :: ClosedExpr
    tup4R :: String
    tup4E = EApp (EApp (EApp (EApp (EBoxedTup (Plus2 2)) (int 0)) (int 1)) (int 2)) (int 3)
    tup4R = "(0, 1, 2, 3)"

    sec2E :: ClosedExpr
    sec2R :: String
    sec2E = EBoxedTup (Plus2 0)
    sec2R = "(, )"

    sec4E :: ClosedExpr
    sec4R :: String
    sec4E = EBoxedTup (Plus2 2)
    sec4R = "(, , , )"

    usec2E :: ClosedExpr
    usec2R :: String
    usec2E = EApp (EBoxedTup (Plus2 0)) (int 0)
    usec2R = "(0, )"

    usec4E :: ClosedExpr
    usec4R :: String
    usec4E = EApp (EApp (EBoxedTup (Plus2 2)) (int 0)) (int 1)
    usec4R = "(0, 1, , )"

testPrettyExpr :: ClosedExpr -> String -> Assertion
testPrettyExpr expr res = show (pretty expr) @?= res
