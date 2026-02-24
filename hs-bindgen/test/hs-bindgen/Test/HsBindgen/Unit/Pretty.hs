module Test.HsBindgen.Unit.Pretty (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Text.SimplePrettyPrint (Pretty (..))

import HsBindgen.Backend.Hs.Name (unsafeHsIdHsName)
import HsBindgen.Backend.HsModule.Pretty.Expr ()
import HsBindgen.Backend.SHs.AST
import HsBindgen.Language.Haskell qualified as Hs

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.Pretty" [
      tupleExprTests
    , tupleTypeTests
    ]

tupleExprTests :: TestTree
tupleExprTests =
    testGroup "Tuple expressions" [
        testCase "saturated 2-tuple"     $ testPretty tup2E  tup2R
      , testCase "saturated 4-tuple"     $ testPretty tup4E  tup4R
      , testCase "2-section"             $ testPretty sec2E  sec2R
      , testCase "4-section"             $ testPretty sec4E  sec4R
      , testCase "unsaturated 2-section" $ testPretty usec2E usec2R
      , testCase "unsaturated 4-section" $ testPretty usec4E usec4R
      ]
  where
    int :: Integer -> ClosedExpr
    int n = (EIntegral n Nothing)

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

tupleTypeTests :: TestTree
tupleTypeTests =
    testGroup "Tuple types" [
        testCase "saturated 2-tuple"     $ testPretty tup2T  tup2R
      , testCase "saturated 4-tuple"     $ testPretty tup4T  tup4R
      , testCase "2-section"             $ testPretty sec2T  sec2R
      , testCase "4-section"             $ testPretty sec4T  sec4R
      , testCase "unsaturated 2-section" $ testPretty usec2T usec2R
      , testCase "unsaturated 4-section" $ testPretty usec4T usec4R
      ]
  where
    free :: Hs.Identifier -> SType ctx
    free = TFree . unsafeHsIdHsName

    tup2T :: ClosedType
    tup2R :: String
    tup2T = TApp (TApp (TBoxedTup (Plus2 0)) (free "a")) (free "b")
    tup2R = "(a, b)"

    tup4T :: ClosedType
    tup4R :: String
    tup4T = TApp (TApp (TApp (TApp (TBoxedTup (Plus2 2)) (free "a")) (free "b")) (free "c")) (free "d")
    tup4R = "(a, b, c, d)"

    sec2T :: ClosedType
    sec2R :: String
    sec2T = TBoxedTup (Plus2 0)
    sec2R = "(, )"

    sec4T :: ClosedType
    sec4R :: String
    sec4T = TBoxedTup (Plus2 2)
    sec4R = "(, , , )"

    usec2T :: ClosedType
    usec2R :: String
    usec2T = TApp (TBoxedTup (Plus2 0)) (free "a")
    usec2R = "(a, )"

    usec4T :: ClosedType
    usec4R :: String
    usec4T = TApp (TApp (TBoxedTup (Plus2 2)) (free "a")) (free "b")
    usec4R = "(a, b, , )"

testPretty :: Pretty a => a -> String -> Assertion
testPretty x res = show (pretty x) @?= res
