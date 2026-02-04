module Test.HsBindgen.Runtime.CBool (tests) where

import Control.Monad.Trans.Writer.Strict (Writer, execWriter, tell)
import Data.Monoid (Sum (getSum))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import HsBindgen.Runtime.CBool qualified as CBool

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.Runtime.CBool" [
      testTrue
    , testFalse
    , testIsTrue
    , testIsFalse
    , testFromBool
    , testToBool
    , testAnd
    , testOr
    , testNot
    , testBool
    , testIf_
    , testWhen
    , testUnless
    ]

newtype TestBool = TestBool Int
  deriving newtype (Eq, Num, Show) -- do not add any more instances

testTrue :: TestTree
testTrue = testCase "true" $ TestBool 1 @=? CBool.true

testFalse :: TestTree
testFalse = testCase "false" $ TestBool 0 @=? CBool.false

testIsTrue :: TestTree
testIsTrue = testGroup "isTrue" [
      testCase "0"   $ False @=? CBool.isTrue @TestBool 0
    , testCase "1"   $ True  @=? CBool.isTrue @TestBool 1
    , testCase "2"   $ True  @=? CBool.isTrue @TestBool 2
    , testCase "0.1" $ True  @=? CBool.isTrue @Float    0.1
    ]

testIsFalse :: TestTree
testIsFalse = testGroup "isFalse" [
      testCase "0"   $ True  @=? CBool.isFalse @TestBool 0
    , testCase "1"   $ False @=? CBool.isFalse @TestBool 1
    , testCase "2"   $ False @=? CBool.isFalse @TestBool 2
    , testCase "0.1" $ False @=? CBool.isFalse @Float    0.1
    ]

testFromBool :: TestTree
testFromBool = testGroup "fromBool" [
      testCase "True"  $ TestBool 1 @=? CBool.fromBool True
    , testCase "False" $ TestBool 0 @=? CBool.fromBool False
    ]

testToBool :: TestTree
testToBool = testGroup "toBool" [
      testCase "0"   $ False @=? CBool.toBool @TestBool 0
    , testCase "1"   $ True  @=? CBool.toBool @TestBool 1
    , testCase "2"   $ True  @=? CBool.toBool @TestBool 2
    , testCase "0.1" $ True  @=? CBool.toBool @Float    0.1
    ]

testAnd :: TestTree
testAnd = testGroup "(&&)" [
      testCase "bothT"  $ TestBool 1 @=? TestBool 2 CBool.&& TestBool 3
    , testCase "leftF"  $ TestBool 0 @=? TestBool 0 CBool.&& undefined
    , testCase "rightF" $ TestBool 0 @=? TestBool 1 CBool.&& TestBool 0
    ]

testOr :: TestTree
testOr = testGroup "(||)" [
      testCase "bothF"  $ TestBool 0 @=? TestBool 0 CBool.|| TestBool 0
    , testCase "leftT"  $ TestBool 1 @=? TestBool 2 CBool.|| undefined
    , testCase "rightT" $ TestBool 1 @=? TestBool 0 CBool.|| TestBool 2
    ]

testNot :: TestTree
testNot = testGroup "not" [
      testCase "0"   $ TestBool 1 @=? CBool.not @TestBool 0
    , testCase "1"   $ TestBool 0 @=? CBool.not @TestBool 1
    , testCase "2"   $ TestBool 0 @=? CBool.not @TestBool 2
    , testCase "0.1" $ 0.0        @=? CBool.not @Float    0.1
    ]

testBool :: TestTree
testBool = testGroup "bool" [
      testCase "0"   $ 'f' @=? CBool.bool @TestBool 'f' 't' 0
    , testCase "1"   $ 't' @=? CBool.bool @TestBool 'f' 't' 1
    , testCase "2"   $ 't' @=? CBool.bool @TestBool 'f' 't' 2
    , testCase "0.1" $ 't' @=? CBool.bool @Float    'f' 't' 0.1
    ]

testIf_ :: TestTree
testIf_ = testGroup "if_" [
      testCase "0"   $ 'f' @=? CBool.if_ @TestBool 0   't' 'f'
    , testCase "1"   $ 't' @=? CBool.if_ @TestBool 1   't' 'f'
    , testCase "2"   $ 't' @=? CBool.if_ @TestBool 2   't' 'f'
    , testCase "0.1" $ 't' @=? CBool.if_ @Float    0.1 't' 'f'
    ]

testWhen :: TestTree
testWhen = testGroup "when" [
      testCase "0"   $ 0 @=? aux (CBool.when @TestBool 0   act)
    , testCase "1"   $ 1 @=? aux (CBool.when @TestBool 1   act)
    , testCase "2"   $ 1 @=? aux (CBool.when @TestBool 1   act)
    , testCase "0.1" $ 1 @=? aux (CBool.when @Float    0.1 act)
    ]
  where
    aux :: Writer (Sum Int) () -> Int
    aux = getSum . execWriter

    act :: Writer (Sum Int) ()
    act = tell (pure 1)

testUnless :: TestTree
testUnless = testGroup "unless" [
      testCase "0"   $ 1 @=? aux (CBool.unless @TestBool 0   act)
    , testCase "1"   $ 0 @=? aux (CBool.unless @TestBool 1   act)
    , testCase "2"   $ 0 @=? aux (CBool.unless @TestBool 1   act)
    , testCase "0.1" $ 0 @=? aux (CBool.unless @Float    0.1 act)
    ]
  where
    aux :: Writer (Sum Int) () -> Int
    aux = getSum . execWriter

    act :: Writer (Sum Int) ()
    act = tell (pure 1)
