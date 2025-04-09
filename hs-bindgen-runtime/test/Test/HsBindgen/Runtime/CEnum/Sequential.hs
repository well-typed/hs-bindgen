{-# LANGUAGE TypeFamilies #-}

module Test.HsBindgen.Runtime.CEnum.Sequential (tests) where

import Control.Monad (forM_)
import qualified Foreign.C as FC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)

import HsBindgen.Runtime.CEnum.Exception
import HsBindgen.Runtime.CEnum.Sequential

import Test.Internal.Tasty

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.Runtime.CEnum.Sequential" [
      testSingleValue
    , testPosValue
    , testNegValue
    ]

{-------------------------------------------------------------------------------
  Single value
-------------------------------------------------------------------------------}

newtype SingleValue = SingleValue {
      un_SingleValue :: FC.CUInt
    }
  deriving stock Show
  deriving newtype Eq

instance SequentialCEnum SingleValue where
  type SequentialCEnumZ SingleValue = FC.CUInt

  toSequentialCEnum    = SingleValue
  fromSequentialCEnum  = un_SingleValue
  sequentialCEnumMin _ = 1
  sequentialCEnumMax _ = 1

testSingleValue :: TestTree
testSingleValue = testGroup "SingleValue" [
      testCase "sequentialCEnumMinBound" $
        SingleValue 1 @=? sequentialCEnumMinBound
    , testCase "sequentialCEnumMaxBound" $
        SingleValue 1 @=? sequentialCEnumMaxBound
    , testGroup "sequentialCEnumSucc" [
          testCase "no successor" $
            CEnumNoSuccessor 1 @=?! sequentialCEnumSucc (SingleValue 1)
        , testCase "invalid" $
            CEnumInvalid 0 @=?! sequentialCEnumSucc (SingleValue 0)
        ]
    , testGroup "sequentialCEnumPred" [
          testCase "no predecessor" $
            CEnumNoPredecessor 1 @=?! sequentialCEnumPred (SingleValue 1)
        , testCase "invalid" $
            CEnumInvalid 2 @=?! sequentialCEnumPred (SingleValue 2)
        ]
    , testGroup "sequentialCEnumToEnum" [
          testCase "valid" $ SingleValue 1 @=? sequentialCEnumToEnum 1
        , testCase "invalid" $
            CEnumInvalid 2 @=?! sequentialCEnumToEnum @SingleValue 2
        ]
    , testGroup "sequentialCEnumFromEnum" [
          testCase "valid" $ 1 @=? sequentialCEnumFromEnum (SingleValue 1)
        , testCase "invalid" $
            CEnumInvalid 2 @=?! sequentialCEnumFromEnum (SingleValue 2)
        ]
    , testGroup "sequentialCEnumEnumFrom" [
          testCase "valid" $
            [SingleValue 1] @=? sequentialCEnumEnumFrom (SingleValue 1)
        , testCase "invalid" $
            CEnumInvalid 2 @=?! sequentialCEnumEnumFrom (SingleValue 2)
        ]
    , testGroup "sequentialCEnumEnumFromThen" [
          testCase "from equal then" $
            CEnumFromEqThen 1
              @=?! sequentialCEnumEnumFromThen (SingleValue 1) (SingleValue 1)
        , testCase "invalid from" $
            CEnumInvalid 0
              @=?! sequentialCEnumEnumFromThen (SingleValue 0) (SingleValue 1)
        , testCase "invalid then" $
            CEnumInvalid 2
              @=?! sequentialCEnumEnumFromThen (SingleValue 1) (SingleValue 2)
        ]
    , testGroup "sequentialCEnumEnumFromTo" [
          testCase "valid" $
            [SingleValue 1]
              @=? sequentialCEnumEnumFromTo (SingleValue 1) (SingleValue 1)
        , testCase "invalid from" $
            CEnumInvalid 0
              @=?! sequentialCEnumEnumFromTo (SingleValue 0) (SingleValue 1)
        , testCase "invalid to" $
            CEnumInvalid 2
              @=?! sequentialCEnumEnumFromTo (SingleValue 1) (SingleValue 2)
        ]
    , testGroup "sequentialCEnumEnumFromThenTo" [
          testCase "from equal then" $
            CEnumFromEqThen 1
              @=?! sequentialCEnumEnumFromThenTo
                     (SingleValue 1)
                     (SingleValue 1)
                     (SingleValue 1)
        , testCase "invalid from" $
            CEnumInvalid 0
              @=?! sequentialCEnumEnumFromThenTo
                     (SingleValue 0)
                     (SingleValue 1)
                     (SingleValue 1)
        , testCase "invalid then" $
            CEnumInvalid 0
              @=?! sequentialCEnumEnumFromThenTo
                     (SingleValue 1)
                     (SingleValue 0)
                     (SingleValue 1)
        , testCase "invalid to" $
            CEnumInvalid 2
              @=?! sequentialCEnumEnumFromThenTo
                     (SingleValue 1)
                     (SingleValue 1)
                     (SingleValue 2)
        ]
    ]

{-------------------------------------------------------------------------------
  Positive values
-------------------------------------------------------------------------------}

newtype PosValue = PosValue {
      un_PosValue :: FC.CUInt
    }
  deriving stock Show
  deriving newtype Eq

instance SequentialCEnum PosValue where
  type SequentialCEnumZ PosValue = FC.CUInt

  toSequentialCEnum    = PosValue
  fromSequentialCEnum  = un_PosValue
  sequentialCEnumMin _ = 1
  sequentialCEnumMax _ = 10

testPosValue :: TestTree
testPosValue = testGroup "PosValue" [
      testCase "sequentialCEnumMinBound" $
        PosValue 1 @=? sequentialCEnumMinBound
    , testCase "sequentialCEnumMaxBound" $
        PosValue 10 @=? sequentialCEnumMaxBound
    , testGroup "sequentialCEnumSucc" [
          testCase "valid" . forM_ [1..9] $ \i ->
            PosValue (i + 1) @=? sequentialCEnumSucc (PosValue i)
        , testCase "no successor" $
            CEnumNoSuccessor 10 @=?! sequentialCEnumSucc (PosValue 10)
        , testCase "invalid" $
            CEnumInvalid 0 @=?! sequentialCEnumSucc (PosValue 0)
        ]
    , testGroup "sequentialCEnumPred" [
          testCase "valid" . forM_ [2..10] $ \i ->
            PosValue (i - 1) @=? sequentialCEnumPred (PosValue i)
        , testCase "no predecessor" $
            CEnumNoPredecessor 1 @=?! sequentialCEnumPred (PosValue 1)
        , testCase "invalid" $
            CEnumInvalid 11 @=?! sequentialCEnumPred (PosValue 11)
        ]
    , testGroup "sequentialCEnumToEnum" [
          testCase "valid" . forM_ [1..10] $ \i ->
            PosValue (fromIntegral i) @=? sequentialCEnumToEnum i
        , testCase "invalid" $
            CEnumInvalid 11 @=?! sequentialCEnumToEnum @PosValue 11
        ]
    , testGroup "sequentialCEnumFromEnum" [
          testCase "valid" . forM_ [1..10] $ \i ->
            fromIntegral i @=? sequentialCEnumFromEnum (PosValue i)
        , testCase "invalid" $
            CEnumInvalid 11 @=?! sequentialCEnumFromEnum (PosValue 11)
        ]
    , testGroup "sequentialCEnumEnumFrom" [
          testCase "valid" $
            [PosValue i | i <- [5..10]]
              @=? sequentialCEnumEnumFrom (PosValue 5)
        , testCase "max" $
            [PosValue 10] @=? sequentialCEnumEnumFrom (PosValue 10)
        , testCase "invalid" $
            CEnumInvalid 11 @=?! sequentialCEnumEnumFrom (PosValue 11)
        ]
    , testGroup "sequentialCEnumEnumFromThen" [
          testCase "valid" $
            [PosValue i | i <- [2, 4 .. 10]]
              @=? sequentialCEnumEnumFromThen (PosValue 2) (PosValue 4)
        , testCase "from equal then" $
            CEnumFromEqThen 2
              @=?! sequentialCEnumEnumFromThen (PosValue 2) (PosValue 2)
        , testCase "invalid from" $
            CEnumInvalid 0
              @=?! sequentialCEnumEnumFromThen (PosValue 0) (PosValue 2)
        , testCase "invalid then" $
            CEnumInvalid 11
              @=?! sequentialCEnumEnumFromThen (PosValue 2) (PosValue 11)
        ]
    , testGroup "sequentialCEnumEnumFromTo" [
          testCase "valid" $
            [PosValue i | i <- [4..8]]
              @=? sequentialCEnumEnumFromTo (PosValue 4) (PosValue 8)
        , testCase "invalid from" $
            CEnumInvalid 0
              @=?! sequentialCEnumEnumFromTo (PosValue 0) (PosValue 8)
        , testCase "invalid to" $
            CEnumInvalid 11
              @=?! sequentialCEnumEnumFromTo (PosValue 4) (PosValue 11)
        ]
    , testGroup "sequentialCEnumEnumFromThenTo" [
          testCase "valid" $
            [PosValue i | i <- [2, 4 .. 9]]
              @=? sequentialCEnumEnumFromThenTo
                    (PosValue 2)
                    (PosValue 4)
                    (PosValue 9)
        , testCase "from equal then" $
            CEnumFromEqThen 2
              @=?! sequentialCEnumEnumFromThenTo
                     (PosValue 2)
                     (PosValue 2)
                     (PosValue 9)
        , testCase "invalid from" $
            CEnumInvalid 0
              @=?! sequentialCEnumEnumFromThenTo
                     (PosValue 0)
                     (PosValue 2)
                     (PosValue 9)
        , testCase "invalid then" $
            CEnumInvalid 11
              @=?! sequentialCEnumEnumFromThenTo
                     (PosValue 2)
                     (PosValue 11)
                     (PosValue 10)
        , testCase "invalid to" $
            CEnumInvalid 11
              @=?! sequentialCEnumEnumFromThenTo
                     (PosValue 2)
                     (PosValue 4)
                     (PosValue 11)
        ]
    ]

{-------------------------------------------------------------------------------
  Negative values
-------------------------------------------------------------------------------}

newtype NegValue = NegValue {
      un_NegValue :: FC.CInt
    }
  deriving stock Show
  deriving newtype Eq

instance SequentialCEnum NegValue where
  type SequentialCEnumZ NegValue = FC.CInt

  toSequentialCEnum    = NegValue
  fromSequentialCEnum  = un_NegValue
  sequentialCEnumMin _ = (-5)
  sequentialCEnumMax _ = 5

testNegValue :: TestTree
testNegValue = testGroup "NegValue" [
      testCase "sequentialCEnumMinBound" $
        NegValue (-5) @=? sequentialCEnumMinBound
    , testCase "sequentialCEnumMaxBound" $
        NegValue 5 @=? sequentialCEnumMaxBound
    , testGroup "sequentialCEnumSucc" [
          testCase "valid" . forM_ [-5..4] $ \i ->
            NegValue (i + 1) @=? sequentialCEnumSucc (NegValue i)
        , testCase "no successor" $
            CEnumNoSuccessor 5 @=?! sequentialCEnumSucc (NegValue 5)
        , testCase "invalid" $
            CEnumInvalid (-6) @=?! sequentialCEnumSucc (NegValue (-6))
        ]
    , testGroup "sequentialCEnumPred" [
          testCase "valid" . forM_ [-4..5] $ \i ->
            NegValue (i - 1) @=? sequentialCEnumPred (NegValue i)
        , testCase "no predecessor" $
            CEnumNoPredecessor (-5) @=?! sequentialCEnumPred (NegValue (-5))
        , testCase "invalid" $
            CEnumInvalid 6 @=?! sequentialCEnumPred (NegValue 6)
        ]
    , testGroup "sequentialCEnumToEnum" [
          testCase "valid" . forM_ [-5..5] $ \i ->
            NegValue (fromIntegral i) @=? sequentialCEnumToEnum i
        , testCase "invalid" $
            CEnumInvalid 6 @=?! sequentialCEnumToEnum @NegValue 6
        ]
    , testGroup "sequentialCEnumFromEnum" [
          testCase "valid" . forM_ [-5..5] $ \i ->
            fromIntegral i @=? sequentialCEnumFromEnum (NegValue i)
        , testCase "invalid" $
            CEnumInvalid 6 @=?! sequentialCEnumFromEnum (NegValue 6)
        ]
    , testGroup "sequentialCEnumEnumFrom" [
          testCase "valid" $
            [NegValue i | i <- [-2..5]]
              @=? sequentialCEnumEnumFrom (NegValue (-2))
        , testCase "max" $
            [NegValue 5] @=? sequentialCEnumEnumFrom (NegValue 5)
        , testCase "invalid" $
            CEnumInvalid 6 @=?! sequentialCEnumEnumFrom (NegValue 6)
        ]
    , testGroup "sequentialCEnumEnumFromThen" [
          testCase "valid" $
            [NegValue i | i <- [-3, -1 .. 5]]
              @=? sequentialCEnumEnumFromThen (NegValue (-3)) (NegValue (-1))
        , testCase "from equal then" $
            CEnumFromEqThen (-2)
              @=?! sequentialCEnumEnumFromThen (NegValue (-2)) (NegValue (-2))
        , testCase "invalid from" $
            CEnumInvalid (-6)
              @=?! sequentialCEnumEnumFromThen (NegValue (-6)) (NegValue (-4))
        , testCase "invalid then" $
            CEnumInvalid 6
              @=?! sequentialCEnumEnumFromThen (NegValue (-4)) (NegValue 6)
        ]
    , testGroup "sequentialCEnumEnumFromTo" [
          testCase "valid" $
            [NegValue i | i <- [-3..3]]
              @=? sequentialCEnumEnumFromTo (NegValue (-3)) (NegValue 3)
        , testCase "invalid from" $
            CEnumInvalid (-6)
              @=?! sequentialCEnumEnumFromTo (NegValue (-6)) (NegValue 3)
        , testCase "invalid to" $
            CEnumInvalid 6
              @=?! sequentialCEnumEnumFromTo (NegValue (-3)) (NegValue 6)
        ]
    , testGroup "sequentialCEnumEnumFromThenTo" [
          testCase "valid" $
            [NegValue i | i <- [-3, -1 .. 4]]
              @=? sequentialCEnumEnumFromThenTo
                    (NegValue (-3))
                    (NegValue (-1))
                    (NegValue 4)
        , testCase "from equal then" $
            CEnumFromEqThen (-3)
              @=?! sequentialCEnumEnumFromThenTo
                     (NegValue (-3))
                     (NegValue (-3))
                     (NegValue 4)
        , testCase "invalid from" $
            CEnumInvalid (-6)
              @=?! sequentialCEnumEnumFromThenTo
                     (NegValue (-6))
                     (NegValue (-1))
                     (NegValue 3)
        , testCase "invalid then" $
            CEnumInvalid 6
              @=?! sequentialCEnumEnumFromThenTo
                     (NegValue (-3))
                     (NegValue 6)
                     (NegValue 3)
        , testCase "invalid to" $
            CEnumInvalid 6
              @=?! sequentialCEnumEnumFromThenTo
                     (NegValue (-3))
                     (NegValue (-1))
                     (NegValue 6)
        ]
    ]
