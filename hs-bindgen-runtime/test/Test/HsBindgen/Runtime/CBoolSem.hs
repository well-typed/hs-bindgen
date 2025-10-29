module Test.HsBindgen.Runtime.CBoolSem (tests) where

import Control.Monad.Trans.Writer.Strict (Writer, execWriter, tell)
import Data.Monoid (Sum (getSum))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import HsBindgen.Runtime.CBoolSem qualified as CBoolSem

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.Runtime.CBoolSem" [
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
testTrue = testCase "true" $ TestBool 1 @=? CBoolSem.true

testFalse :: TestTree
testFalse = testCase "false" $ TestBool 0 @=? CBoolSem.false

testIsTrue :: TestTree
testIsTrue = testGroup "isTrue" [
      testCase "0"   $ False @=? CBoolSem.isTrue @TestBool 0
    , testCase "1"   $ True  @=? CBoolSem.isTrue @TestBool 1
    , testCase "2"   $ True  @=? CBoolSem.isTrue @TestBool 2
    , testCase "0.1" $ True  @=? CBoolSem.isTrue @Float    0.1
    ]

testIsFalse :: TestTree
testIsFalse = testGroup "isFalse" [
      testCase "0"   $ True  @=? CBoolSem.isFalse @TestBool 0
    , testCase "1"   $ False @=? CBoolSem.isFalse @TestBool 1
    , testCase "2"   $ False @=? CBoolSem.isFalse @TestBool 2
    , testCase "0.1" $ False @=? CBoolSem.isFalse @Float    0.1
    ]

testFromBool :: TestTree
testFromBool = testGroup "fromBool" [
      testCase "True"  $ TestBool 1 @=? CBoolSem.fromBool True
    , testCase "False" $ TestBool 0 @=? CBoolSem.fromBool False
    ]

testToBool :: TestTree
testToBool = testGroup "toBool" [
      testCase "0"   $ False @=? CBoolSem.toBool @TestBool 0
    , testCase "1"   $ True  @=? CBoolSem.toBool @TestBool 1
    , testCase "2"   $ True  @=? CBoolSem.toBool @TestBool 2
    , testCase "0.1" $ True  @=? CBoolSem.toBool @Float    0.1
    ]

testAnd :: TestTree
testAnd = testGroup "(&&)" [
      testCase "bothT"  $ TestBool 1 @=? TestBool 2 CBoolSem.&& TestBool 3
    , testCase "leftF"  $ TestBool 0 @=? TestBool 0 CBoolSem.&& undefined
    , testCase "rightF" $ TestBool 0 @=? TestBool 1 CBoolSem.&& TestBool 0
    ]

testOr :: TestTree
testOr = testGroup "(||)" [
      testCase "bothF"  $ TestBool 0 @=? TestBool 0 CBoolSem.|| TestBool 0
    , testCase "leftT"  $ TestBool 1 @=? TestBool 2 CBoolSem.|| undefined
    , testCase "rightT" $ TestBool 1 @=? TestBool 0 CBoolSem.|| TestBool 2
    ]

testNot :: TestTree
testNot = testGroup "not" [
      testCase "0"   $ TestBool 1 @=? CBoolSem.not @TestBool 0
    , testCase "1"   $ TestBool 0 @=? CBoolSem.not @TestBool 1
    , testCase "2"   $ TestBool 0 @=? CBoolSem.not @TestBool 2
    , testCase "0.1" $ 0.0        @=? CBoolSem.not @Float    0.1
    ]

testBool :: TestTree
testBool = testGroup "bool" [
      testCase "0"   $ 'f' @=? CBoolSem.bool @TestBool 'f' 't' 0
    , testCase "1"   $ 't' @=? CBoolSem.bool @TestBool 'f' 't' 1
    , testCase "2"   $ 't' @=? CBoolSem.bool @TestBool 'f' 't' 2
    , testCase "0.1" $ 't' @=? CBoolSem.bool @Float    'f' 't' 0.1
    ]

testIf_ :: TestTree
testIf_ = testGroup "if_" [
      testCase "0"   $ 'f' @=? CBoolSem.if_ @TestBool 0   't' 'f'
    , testCase "1"   $ 't' @=? CBoolSem.if_ @TestBool 1   't' 'f'
    , testCase "2"   $ 't' @=? CBoolSem.if_ @TestBool 2   't' 'f'
    , testCase "0.1" $ 't' @=? CBoolSem.if_ @Float    0.1 't' 'f'
    ]

testWhen :: TestTree
testWhen = testGroup "when" [
      testCase "0"   $ 0 @=? aux (CBoolSem.when @TestBool 0   act)
    , testCase "1"   $ 1 @=? aux (CBoolSem.when @TestBool 1   act)
    , testCase "2"   $ 1 @=? aux (CBoolSem.when @TestBool 1   act)
    , testCase "0.1" $ 1 @=? aux (CBoolSem.when @Float    0.1 act)
    ]
  where
    aux :: Writer (Sum Int) () -> Int
    aux = getSum . execWriter

    act :: Writer (Sum Int) ()
    act = tell (pure 1)

testUnless :: TestTree
testUnless = testGroup "unless" [
      testCase "0"   $ 1 @=? aux (CBoolSem.unless @TestBool 0   act)
    , testCase "1"   $ 0 @=? aux (CBoolSem.unless @TestBool 1   act)
    , testCase "2"   $ 0 @=? aux (CBoolSem.unless @TestBool 1   act)
    , testCase "0.1" $ 0 @=? aux (CBoolSem.unless @Float    0.1 act)
    ]
  where
    aux :: Writer (Sum Int) () -> Int
    aux = getSum . execWriter

    act :: Writer (Sum Int) ()
    act = tell (pure 1)
