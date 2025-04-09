{-# LANGUAGE TypeFamilies #-}

module Test.HsBindgen.Runtime.CEnum.General (tests) where

--import Control.Monad (forM_)
import qualified Foreign.C as FC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)

import HsBindgen.Runtime.CEnum.Exception
import HsBindgen.Runtime.CEnum.General

import Test.Internal.Tasty

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.Runtime.CEnum.General" [
      testNoValue
    , testPosValue
    , testNegValue
    ]

{-------------------------------------------------------------------------------
  No values (invalid)
-------------------------------------------------------------------------------}

newtype NoValue = NoValue {
      un_NoValue :: FC.CUInt
    }
  deriving stock Show
  deriving newtype Eq

instance GeneralCEnum NoValue where
  type GeneralCEnumZ NoValue = FC.CUInt

  toGeneralCEnum       = NoValue
  fromGeneralCEnum     = un_NoValue
  generalCEnumValues _ = [] -- invariant violation

testNoValue :: TestTree
testNoValue = testGroup "NoValue" [
      testCase "generalCEnumMinBound" $
        CEnumEmpty @=?! generalCEnumMinBound @NoValue
    , testCase "generalCEnumMaxBound" $
        CEnumEmpty @=?! generalCEnumMaxBound @NoValue
    ]

{-------------------------------------------------------------------------------
  Positive values
-------------------------------------------------------------------------------}

newtype PosValue = PosValue {
      un_PosValue :: FC.CUInt
    }
  deriving stock Show
  deriving newtype Eq

instance GeneralCEnum PosValue where
  type GeneralCEnumZ PosValue = FC.CUInt

  toGeneralCEnum       = PosValue
  fromGeneralCEnum     = un_PosValue
  generalCEnumValues _ = [100, 101, 200, 201, 202, 301, 400, 401, 403, 404]

testPosValue :: TestTree
testPosValue = testGroup "PosValue" [
      testCase "generalCEnumMinBound" $
        PosValue 100 @=? generalCEnumMinBound
    , testCase "generalCEnumMaxBound" $
        PosValue 404 @=? generalCEnumMaxBound
    , testGroup "generalCEnumSucc" [
          testCase "valid" $ PosValue 301 @=? generalCEnumSucc (PosValue 202)
        , testCase "no successor" $
            CEnumNoSuccessor 404 @=?! generalCEnumSucc (PosValue 404)
        , testCase "invalid" $
            CEnumInvalid 300 @=?! generalCEnumSucc (PosValue 300)
        ]
    , testGroup "generalCEnumPred" [
          testCase "valid" $ PosValue 202 @=? generalCEnumPred (PosValue 301)
        , testCase "no predecessor" $
            CEnumNoPredecessor 100 @=?! generalCEnumPred (PosValue 100)
        , testCase "invalid" $
            CEnumInvalid 300 @=?! generalCEnumPred (PosValue 300)
        ]
    , testGroup "generalCEnumToEnum" [
          testCase "valid" $ PosValue 301 @=? generalCEnumToEnum 301
        , testCase "invalid" $
            CEnumInvalid 300 @=?! generalCEnumToEnum @PosValue 300
        ]
    , testGroup "generalCEnumFromEnum" [
          testCase "valid" $ 301 @=? generalCEnumFromEnum (PosValue 301)
        , testCase "invalid" $
            CEnumInvalid 300 @=?! generalCEnumFromEnum (PosValue 300)
        ]
    , testGroup "generalCEnumEnumFrom" [
          testCase "valid" $
            [PosValue i | i <- [301, 400, 401, 403, 404]]
              @=? generalCEnumEnumFrom (PosValue 301)
        , testCase "max" $
            [PosValue 404] @=? generalCEnumEnumFrom (PosValue 404)
        , testCase "invalid" $
            CEnumInvalid 300 @=?! generalCEnumEnumFrom (PosValue 300)
        ]
    , testGroup "generalCEnumEnumFromThen" [
          testCase "valid" $
            [PosValue i | i <- [200, 202, 400, 403]]
              @=? generalCEnumEnumFromThen (PosValue 200) (PosValue 202)
        , testCase "from equal then" $
            CEnumFromEqThen 200
              @=?! generalCEnumEnumFromThen (PosValue 200) (PosValue 200)
        , testCase "invalid from" $
            CEnumInvalid 300
              @=?! generalCEnumEnumFromThen (PosValue 300) (PosValue 301)
        , testCase "invalid then" $
            CEnumInvalid 300
              @=?! generalCEnumEnumFromThen (PosValue 200) (PosValue 300)
        ]
    , testGroup "generalCEnumEnumFromTo" [
          testCase "valid" $
            [PosValue i | i <- [201, 202, 301, 400, 401]]
              @=? generalCEnumEnumFromTo (PosValue 201) (PosValue 401)
        , testCase "invalid from" $
            CEnumInvalid 300
              @=?! generalCEnumEnumFromTo (PosValue 300) (PosValue 401)
        , testCase "invalid to" $
            CEnumInvalid 300
              @=?! generalCEnumEnumFromTo (PosValue 201) (PosValue 300)
        ]
    , testGroup "generalCEnumEnumFromThenTo" [
          testCase "valid (2)" $
            [PosValue i | i <- [200, 202, 400]]
              @=? generalCEnumEnumFromThenTo
                    (PosValue 200)
                    (PosValue 202)
                    (PosValue 401)
        , testCase "valid (3)" $
            [PosValue i | i <- [101, 202, 401]]
              @=? generalCEnumEnumFromThenTo
                    (PosValue 101)
                    (PosValue 202)
                    (PosValue 403)
        , testCase "from equal then" $
            CEnumFromEqThen 200
              @=?! generalCEnumEnumFromThenTo
                     (PosValue 200)
                     (PosValue 200)
                     (PosValue 401)
        , testCase "invalid from" $
            CEnumInvalid 300
              @=?! generalCEnumEnumFromThenTo
                     (PosValue 300)
                     (PosValue 401)
                     (PosValue 403)
        , testCase "invalid then" $
            CEnumInvalid 300
              @=?! generalCEnumEnumFromThenTo
                     (PosValue 200)
                     (PosValue 300)
                     (PosValue 401)
        , testCase "invalid to" $
            CEnumInvalid 300
              @=?! generalCEnumEnumFromThenTo
                     (PosValue 100)
                     (PosValue 200)
                     (PosValue 300)
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

instance GeneralCEnum NegValue where
  type GeneralCEnumZ NegValue = FC.CInt

  toGeneralCEnum       = NegValue
  fromGeneralCEnum     = un_NegValue
  generalCEnumValues _ = [-201, -200, -101, -100, 0, 100, 101, 200, 201]

testNegValue :: TestTree
testNegValue = testGroup "NegValue" [
      testCase "generalCEnumMinBound" $
        NegValue (-201) @=? generalCEnumMinBound
    , testCase "generalCEnumMaxBound" $
        NegValue 201 @=? generalCEnumMaxBound
    , testGroup "generalCEnumSucc" [
          testCase "valid" $ NegValue 0 @=? generalCEnumSucc (NegValue (-100))
        , testCase "no successor" $
            CEnumNoSuccessor 201 @=?! generalCEnumSucc (NegValue 201)
        , testCase "invalid" $ CEnumInvalid 1 @=?! generalCEnumSucc (NegValue 1)
        ]
    , testGroup "generalCEnumPred" [
          testCase "valid" $ NegValue (-100) @=? generalCEnumPred (NegValue 0)
        , testCase "no predecessor" $
            CEnumNoPredecessor (-201) @=?! generalCEnumPred (NegValue (-201))
        , testCase "invalid" $ CEnumInvalid 1 @=?! generalCEnumPred (NegValue 1)
        ]
    , testGroup "generalCEnumToEnum" [
          testCase "valid" $ NegValue (-101) @=? generalCEnumToEnum (-101)
        , testCase "invalid" $
            CEnumInvalid 1 @=?! generalCEnumToEnum @NegValue 1
        ]
    , testGroup "generalCEnumFromEnum" [
          testCase "valid" $ (-101) @=? generalCEnumFromEnum (NegValue (-101))
        , testCase "invalid" $
            CEnumInvalid 1 @=?! generalCEnumFromEnum (NegValue 1)
        ]
    , testGroup "generalCEnumEnumFrom" [
          testCase "valid" $
            [NegValue i | i <- [-101, -100, 0, 100, 101, 200, 201]]
              @=? generalCEnumEnumFrom (NegValue (-101))
        , testCase "max" $
            [NegValue 201] @=? generalCEnumEnumFrom (NegValue 201)
        , testCase "invalid" $
            CEnumInvalid 1 @=?! generalCEnumEnumFrom (NegValue 1)
        ]
    , testGroup "generalCEnumEnumFromThen" [
          testCase "valid" $
            [NegValue i | i <- [-200, -100, 100, 200]]
              @=? generalCEnumEnumFromThen (NegValue (-200)) (NegValue (-100))
        , testCase "from equal then" $
            CEnumFromEqThen (-200)
              @=?! generalCEnumEnumFromThen (NegValue (-200)) (NegValue (-200))
        , testCase "invalid from" $
            CEnumInvalid 1
              @=?! generalCEnumEnumFromThen (NegValue 1) (NegValue 100)
        , testCase "invalid then" $
            CEnumInvalid 1
              @=?! generalCEnumEnumFromThen (NegValue (-100)) (NegValue 1)
        ]
    , testGroup "generalCEnumEnumFromTo" [
          testCase "valid" $
            [NegValue i | i <- [-100, 0, 100]]
              @=? generalCEnumEnumFromTo (NegValue (-100)) (NegValue 100)
        , testCase "invalid from" $
            CEnumInvalid 1
              @=?! generalCEnumEnumFromTo (NegValue 1) (NegValue 100)
        , testCase "invalid to" $
            CEnumInvalid 1
              @=?! generalCEnumEnumFromTo (NegValue (-100)) (NegValue 1)
        ]
    , testGroup "generalCEnumEnumFromThenTo" [
          testCase "valid (2)" $
            [NegValue i | i <- [-200, -100, 100]]
              @=? generalCEnumEnumFromThenTo
                    (NegValue (-200))
                    (NegValue (-100))
                    (NegValue 101)
        , testCase "valid (3)" $
            [NegValue i | i <- [-200, 0, 200]]
              @=? generalCEnumEnumFromThenTo
                    (NegValue (-200))
                    (NegValue 0)
                    (NegValue 200)
        , testCase "from equal then" $
            CEnumFromEqThen (-200)
              @=?! generalCEnumEnumFromThenTo
                     (NegValue (-200))
                     (NegValue (-200))
                     (NegValue 101)
        , testCase "invalid from" $
            CEnumInvalid (-1)
              @=?! generalCEnumEnumFromThenTo
                     (NegValue (-1))
                     (NegValue 100)
                     (NegValue 200)
        , testCase "invalid then" $
            CEnumInvalid 1
              @=?! generalCEnumEnumFromThenTo
                     (NegValue (-100))
                     (NegValue 1)
                     (NegValue 101)
        , testCase "invalid to" $
            CEnumInvalid 1
              @=?! generalCEnumEnumFromThenTo
                     (NegValue (-200))
                     (NegValue (-100))
                     (NegValue 1)
        ]
    ]
