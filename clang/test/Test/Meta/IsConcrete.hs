-- | Test the invariant of 'IsConcrete'
module Test.Meta.IsConcrete (tests) where

import Data.Proxy
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Util.AST qualified as AST
import Test.Util.Input (TestInput)
import Test.Util.Input.Examples
import Test.Util.Input.StructForest (StructForest)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Meta.IsConcrete" [
      testGroup "sanity" [
          testCase "SingleFunction" $ test SingleFunction
        , testCase "SingleStruct"   $ test SingleStruct
        , testCase "TwoStructs"     $ test ThreeStructs
        ]
    , testGroup "random" [
          testProperty "StructForest" $ prop (Proxy @(StructForest ()))
        ]
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

test :: AST.IsConcrete a => a -> Assertion
test x = do
    actualAST <- AST.parse (AST.toTestInput x)
    assertEqual "" (AST.toAbstractAST x) $ actualAST

prop ::
     AST.IsConcrete a
  => Proxy a -- ^ For which type should we generate a random value/
  -> a -> Property
prop _ x =
    counterexample ("test input: " ++ show testInput) $
      monadicIO go
  where
    testInput :: TestInput
    testInput = AST.toTestInput x

    expectedAST :: AST.AST AST.Descr
    expectedAST = AST.toAbstractAST x

    go :: PropertyM IO Property
    go = do
        actualAST <- run $ AST.parse testInput
        return $ expectedAST === actualAST

