{-# LANGUAGE TypeFamilies #-}

module Test.HsBindgen.Runtime.CEnum (tests) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Foreign.C qualified as FC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)

import HsBindgen.Runtime.CEnum
  ( AsCEnum(..), AsSequentialCEnum(..), CEnum, CEnumException(..)
  , SequentialCEnum
  )
import HsBindgen.Runtime.CEnum qualified as CEnum

import Test.Internal.Tasty

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.Runtime.CEnum" [
    testGroup "CEnum" [
        testNoValue
      , testGenSingleValue
      , testGenPosValue
      , testGenNegValue
      ]
  , testGroup "SequentialCEnum" [
        testSeqSingleValue
      , testSeqPosValue
      , testSeqNegValue
      ]
    ]

{-------------------------------------------------------------------------------
  CEnum: no values
-------------------------------------------------------------------------------}

newtype NoValue = NoValue {
      un_NoValue :: FC.CUInt
    }
  deriving stock Show
  deriving newtype Eq

instance CEnum NoValue where
  type CEnumZ NoValue = FC.CUInt

  declaredValues _ = CEnum.declaredValuesFromList []
  showsUndeclared  = CEnum.showsWrappedUndeclared "NoValue"

deriving via AsCEnum NoValue instance Bounded NoValue

deriving via AsCEnum NoValue instance Enum NoValue

testNoValue :: TestTree
testNoValue = testGroup "NoValue" [
      testCase "isDeclared" $ False @=? CEnum.isDeclared v1
    , testCase "mkDeclared" $ Nothing @=? CEnum.mkDeclared @NoValue 1
    , testCase "showCEnum" $ "NoValue 1" @=? CEnum.showCEnum v1
    , testCase "getNames" $ [] @=? CEnum.getNames v1
    , testGroup "Bounded" [
          testCase "minBound" $ CEnumEmpty @=?! minBound @NoValue
        , testCase "maxBound" $ CEnumEmpty @=?! maxBound @NoValue
        ]
    , testGroup "Enum" [
          testCase "succ" $ CEnumNotDeclared 1 @=?! succ v1
        , testCase "pred" $ CEnumNotDeclared 1 @=?! pred v1
        , testCase "toEnum" $ CEnumNotDeclared 1 @=?! toEnum @NoValue 1
        , testCase "fromEnum" $ CEnumNotDeclared 1 @=?! fromEnum v1
        , testCase "enumFrom" $ CEnumNotDeclared 1 @=?! enumFrom v1
        , testGroup "enumFromThen"  [
              testCase "from==then" $
                CEnumFromEqThen 1 @=?! enumFromThen v1 v1
            , testCase "!declared" $
                CEnumNotDeclared 1 @=?! enumFromThen v1 v2
            ]
        , testCase "enumFromTo" $ CEnumNotDeclared 1 @=?! enumFromTo v1 v2
        , testGroup "enumFromThenTo" [
              testCase "from==then" $
                CEnumFromEqThen 1 @=?! enumFromThenTo v1 v1 v2
            , testCase "!declared" $
                CEnumNotDeclared 1 @=?! enumFromThenTo v1 v2 v2
            ]
        ]
    ]
  where
    v1, v2 :: NoValue
    v1 = NoValue 1
    v2 = NoValue 2

{-------------------------------------------------------------------------------
  CEnum: single value
-------------------------------------------------------------------------------}

newtype GenSingleValue = GenSingleValue {
      un_GenSingleValue :: FC.CUInt
    }
  deriving stock Show
  deriving newtype Eq

instance CEnum GenSingleValue where
  type CEnumZ GenSingleValue = FC.CUInt

  declaredValues _ = CEnum.declaredValuesFromList [(1, "OK" :| ["SUCCESS"])]
  showsUndeclared  = CEnum.showsWrappedUndeclared "GenSingleValue"

deriving via AsCEnum GenSingleValue instance Bounded GenSingleValue

deriving via AsCEnum GenSingleValue instance Enum GenSingleValue

testGenSingleValue :: TestTree
testGenSingleValue = testGroup "GenSingleValue" [
      testGroup "isDeclared" [
          testCase "True" $ True @=? CEnum.isDeclared v1
        , testCase "False" $ False @=? CEnum.isDeclared v2
        ]
    , testGroup "mkDeclared" [
          testCase "declared" $ Just v1 @=? CEnum.mkDeclared 1
        , testCase "!declared" $ Nothing @=? CEnum.mkDeclared @GenSingleValue 2
        ]
    , testGroup "showCEnum" [
          testCase "declared" $ "OK" @=? CEnum.showCEnum v1
        , testCase "!declared" $
            "GenSingleValue 2" @=? CEnum.showCEnum v2
        ]
    , testGroup "getNames" [
          testCase "declared" $ ["OK", "SUCCESS"] @=? CEnum.getNames v1
        , testCase "!declared" $ [] @=? CEnum.getNames v2
        ]
    , testGroup "Bounded" [
          testCase "minBound" $ v1 @=? minBound
        , testCase "maxBound" $ v1 @=? maxBound
        ]
    , testGroup "Enum" [
          testGroup "succ" [
              testCase "!successor" $ CEnumNoSuccessor 1 @=?! succ v1
            , testCase "!declared" $ CEnumNotDeclared 2 @=?! succ v2
            ]
        , testGroup "pred" [
              testCase "!predecessor" $ CEnumNoPredecessor 1 @=?! pred v1
            , testCase "!declared" $ CEnumNotDeclared 2 @=?! pred v2
            ]
        , testGroup "toEnum" [
              testCase "declared" $ v1 @=? toEnum 1
            , testCase "!declared" $
                CEnumNotDeclared 2 @=?! toEnum @GenSingleValue 2
            ]
        , testGroup "fromEnum" [
              testCase "declared" $ 1 @=? fromEnum v1
            , testCase "!declared" $ CEnumNotDeclared 2 @=?! fromEnum v2
            ]
        , testGroup "enumFrom" [
              testCase "declared" $ [v1] @=? enumFrom v1
            , testCase "!declared" $ CEnumNotDeclared 2 @=?! enumFrom v2
            ]
        , testGroup "enumFromThen" [
              testCase "from==then" $
                CEnumFromEqThen 1 @=?! enumFromThen v1 v1
            , testCase "!declared from" $
                CEnumNotDeclared 2 @=?! enumFromThen v2 v1
            , testCase "!declared then" $
                CEnumNotDeclared 2 @=?! enumFromThen v1 v2
            ]
        , testGroup "enumFromTo" [
              testCase "declared" $ [v1] @=? enumFromTo v1 v1
            , testCase "!declared from" $
                CEnumNotDeclared 2 @=?! enumFromTo v2 v1
            , testCase "!declared to" $
                CEnumNotDeclared 2 @=?! enumFromTo v1 v2
            ]
        , testGroup "enumFromThenTo" [
              testCase "from==then" $
                CEnumFromEqThen 1 @=?! enumFromThenTo v1 v1 v2
            , testCase "!declared from" $
                CEnumNotDeclared 2 @=?! enumFromThenTo v2 v1 v1
            , testCase "!declared then" $
                CEnumNotDeclared 2 @=?! enumFromThenTo v1 v2 v2
            ]
        ]
    ]
  where
    v1, v2 :: GenSingleValue
    v1 = GenSingleValue 1
    v2 = GenSingleValue 2

{-------------------------------------------------------------------------------
  CEnum: positive values
-------------------------------------------------------------------------------}

newtype GenPosValue = GenPosValue {
      un_GenPosValue :: FC.CUInt
    }
  deriving stock Show
  deriving newtype Eq

instance CEnum GenPosValue where
  type CEnumZ GenPosValue = FC.CUInt

  declaredValues _ = CEnum.declaredValuesFromList [
      (100, NonEmpty.singleton "CONTINUE")
    , (101, NonEmpty.singleton "SWITCHING_PROTOCOLS")
    , (200, NonEmpty.singleton "OK")
    , (201, NonEmpty.singleton "CREATED")
    , (202, NonEmpty.singleton "ACCEPTED")
    , (301, NonEmpty.singleton "MOVED_PERMANENTLY")
    , (400, NonEmpty.singleton "BAD_REQUEST")
    , (401, NonEmpty.singleton "UNAUTHORIZED")
    , (403, NonEmpty.singleton "FORBIDDEN")
    , (404, NonEmpty.singleton "NOT_FOUND")
    ]
  showsUndeclared = CEnum.showsWrappedUndeclared "GenPosValue"

deriving via AsCEnum GenPosValue instance Bounded GenPosValue

deriving via AsCEnum GenPosValue instance Enum GenPosValue

testGenPosValue :: TestTree
testGenPosValue = testGroup "GenPosValue" [
      testGroup "isDeclared" [
          testCase "declared" $ True @=? CEnum.isDeclared v301
        , testCase "!declared" $ False @=? CEnum.isDeclared v300
        ]
    , testGroup "mkDeclared" [
          testCase "declared" $ Just v301 @=? CEnum.mkDeclared 301
        , testCase "!declared" $ Nothing @=? CEnum.mkDeclared @GenPosValue 300
        ]
    , testGroup "showCEnum" [
          testCase "declared" $ "OK" @=? CEnum.showCEnum v200
        , testCase "!declared" $
            "GenPosValue 300" @=? CEnum.showCEnum v300
        ]
    , testGroup "getNames" [
          testCase "declared" $ ["OK"] @=? CEnum.getNames v200
        , testCase "!declared" $ [] @=? CEnum.getNames v300
        ]
    , testGroup "Bounded" [
          testCase "minBound" $ v100 @=? minBound
        , testCase "maxBound" $ v404 @=? maxBound
        ]
    , testGroup "Enum" [
          testGroup "succ" [
              testCase "valid" $ v301 @=? succ v202
            , testCase "!successor" $ CEnumNoSuccessor 404 @=?! succ v404
            , testCase "!declared" $ CEnumNotDeclared 300 @=?! succ v300
            ]
        , testGroup "pred" [
              testCase "valid" $ v202 @=? pred v301
            , testCase "!predecessor" $ CEnumNoPredecessor 100 @=?! pred v100
            , testCase "!declared" $ CEnumNotDeclared 300 @=?! pred v300
            ]
        , testGroup "toEnum" [
              testCase "declared" $ v301 @=? toEnum 301
            , testCase "!declared" $
                CEnumNotDeclared 300 @=?! toEnum @GenPosValue 300
            ]
        , testGroup "fromEnum" [
              testCase "declared" $ 301 @=? fromEnum v301
            , testCase "!declared" $ CEnumNotDeclared 300 @=?! fromEnum v300
            ]
        , testGroup "enumFrom" [
              testCase "valid" $
                [v301, v400, v401, v403, v404] @=? enumFrom v301
            , testCase "max" $ [v404] @=? enumFrom v404
            , testCase "!declared" $ CEnumNotDeclared 300 @=?! enumFrom v300
            ]
        , testGroup "enumFromThen" [
              testCase "increasing 1" $
                [v301, v400, v401, v403, v404] @=? enumFromThen v301 v400
            , testCase "increasing 2" $
                [v200, v202, v400, v403] @=? enumFromThen v200 v202
            , testCase "increasing 3" $
                [v101, v202, v401] @=? enumFromThen v101 v202
            , testCase "decreasing 1" $
                [v202, v201, v200, v101, v100] @=? enumFromThen v202 v201
            , testCase "decreasing 2" $
                [v404, v401, v301, v201, v101] @=? enumFromThen v404 v401
            , testCase "decreasing 3" $
                [v403, v301, v200] @=? enumFromThen v403 v301
            , testCase "from==then" $
                CEnumFromEqThen 200 @=?! enumFromThen v200 v200
            , testCase "!declared from" $
                CEnumNotDeclared 300 @=?! enumFromThen v300 v404
            , testCase "!declared then" $
                CEnumNotDeclared 300 @=?! enumFromThen v200 v300
            ]
        , testGroup "enumFromTo" [
              testCase "valid" $
                [v202, v301, v400, v401] @=? enumFromTo v202 v401
            , testCase "!declared from" $
                CEnumNotDeclared 300 @=?! enumFromTo v300 v401
            , testCase "!declared to" $
                CEnumNotDeclared 300 @=?! enumFromTo v200 v300
            ]
        , testGroup "enumFromThenTo" [
              testCase "increasing 1" $
                [v301, v400, v401] @=? enumFromThenTo v301 v400 v401
            , testCase "increasing 2" $
                [v200, v202, v400] @=? enumFromThenTo v200 v202 v401
            , testCase "increasing 3" $
                [v100, v201, v400] @=? enumFromThenTo v100 v201 v403
            , testCase "decreasing 1" $
                [v202, v201, v200] @=? enumFromThenTo v202 v201 v200
            , testCase "decreasing 2" $
                [v404, v401, v301, v201] @=? enumFromThenTo v404 v401 v200
            , testCase "decreasing 3" $
                [v404, v400, v201] @=? enumFromThenTo v404 v400 v200
            , testCase "from==then" $
                CEnumFromEqThen 200 @=?! enumFromThenTo v200 v200 v400
            , testCase "!declared from" $
                CEnumNotDeclared 300 @=?! enumFromThenTo v300 v400 v403
            , testCase "!declared then" $
                CEnumNotDeclared 300 @=?! enumFromThenTo v200 v300 v400
            , testCase "!declared to" $
                CEnumNotDeclared 300 @=?! enumFromThenTo v100 v200 v300
            ]
        ]
    ]
  where
    v100, v101, v200, v201, v202, v301, v400, v401, v403, v404 :: GenPosValue
    v100 = GenPosValue 100
    v101 = GenPosValue 101
    v200 = GenPosValue 200
    v201 = GenPosValue 201
    v202 = GenPosValue 202
    v301 = GenPosValue 301
    v400 = GenPosValue 400
    v401 = GenPosValue 401
    v403 = GenPosValue 403
    v404 = GenPosValue 404

    v300 :: GenPosValue
    v300 = GenPosValue 300

{-------------------------------------------------------------------------------
  CEnum: negative values
-------------------------------------------------------------------------------}

newtype GenNegValue = GenNegValue {
      un_GenNegValue :: FC.CInt
    }
  deriving stock Show
  deriving newtype Eq

instance CEnum GenNegValue where
  type CEnumZ GenNegValue = FC.CInt

  declaredValues _ = CEnum.declaredValuesFromList [
      (-201, NonEmpty.singleton "REALLY_TERRIBLE")
    , (-200, NonEmpty.singleton "TERRIBLE")
    , (-101, NonEmpty.singleton "REALLY_BAD")
    , (-100, NonEmpty.singleton "BAD")
    , (0,    "NEUTRAL" :| ["UNREMARKABLE"])
    , (100,  NonEmpty.singleton "GOOD")
    , (101,  NonEmpty.singleton "REALLY_GOOD")
    , (200,  NonEmpty.singleton "GREAT")
    , (201,  NonEmpty.singleton "REALLY_GREAT")
    ]
  showsUndeclared = CEnum.showsWrappedUndeclared "GenNegValue"

deriving via AsCEnum GenNegValue instance Bounded GenNegValue

deriving via AsCEnum GenNegValue instance Enum GenNegValue

testGenNegValue :: TestTree
testGenNegValue = testGroup "GenNegValue" [
      testGroup "isDeclared" [
          testCase "declared" $ True @=? CEnum.isDeclared n100
        , testCase "!declared" $ False @=? CEnum.isDeclared n202
        ]
    , testGroup "mkDeclared" [
          testCase "declared" $ Just n100 @=? CEnum.mkDeclared (-100)
        , testCase "!declared" $ Nothing @=? CEnum.mkDeclared @GenNegValue 300
        ]
    , testGroup "showCEnum" [
          testCase "declared" $ "NEUTRAL" @=? CEnum.showCEnum z
        , testCase "!declared" $
            "GenNegValue (-202)" @=? CEnum.showCEnum n202
        ]
    , testGroup "getNames" [
          testCase "declared" $ ["NEUTRAL", "UNREMARKABLE"] @=? CEnum.getNames z
        , testCase "!declared" $ [] @=? CEnum.getNames n202
        ]
    , testGroup "Bounded" [
          testCase "minBound" $ n201 @=? minBound
        , testCase "maxBound" $ p201 @=? maxBound
        ]
    , testGroup "Enum" [
          testGroup "succ" [
              testCase "valid" $ n101 @=? succ n200
            , testCase "!successor" $ CEnumNoSuccessor 201 @=?! succ p201
            , testCase "!declared" $ CEnumNotDeclared (-202) @=?! succ n202
            ]
        , testGroup "pred" [
              testCase "valid" $ n100 @=? pred z
            , testCase "!predecessor" $ CEnumNoPredecessor (-201) @=?! pred n201
            , testCase "!declared" $ CEnumNotDeclared 202 @=?! pred p202
            ]
        , testGroup "toEnum" [
              testCase "declared" $ n101 @=? toEnum (-101)
            , testCase "!declared" $
                CEnumNotDeclared (-102) @=?! toEnum @GenNegValue (-102)
            ]
        , testGroup "fromEnum" [
              testCase "declared" $ 101 @=? fromEnum p101
            , testCase "!declared" $ CEnumNotDeclared (-202) @=?! fromEnum n202
            ]
        , testGroup "enumFrom" [
              testCase "valid" $
                [n100, z, p100, p101, p200, p201] @=? enumFrom n100
            , testCase "max" $ [p201] @=? enumFrom p201
            , testCase "!declared" $ CEnumNotDeclared (-202) @=?! enumFrom n202
            ]
        , testGroup "enumFromThen" [
              testCase "increasing 1" $
                [n101, n100, z, p100, p101, p200, p201]
                  @=? enumFromThen n101 n100
            , testCase "increasing 2" $
                [n101, z, p101, p201] @=? enumFromThen n101 z
            , testCase "increasing 3" $
                [n101, p100, p201] @=? enumFromThen n101 p100
            , testCase "decreasing 1" $
                [p100, z, n100, n101, n200, n201] @=? enumFromThen p100 z
            , testCase "decreasing 2" $
                [p100, n100, n200] @=? enumFromThen p100 n100
            , testCase "decreasing 3" $
                [p201, p100, n101] @=? enumFromThen p201 p100
            , testCase "from==then" $
                CEnumFromEqThen (-100) @=?! enumFromThen n100 n100
            , testCase "!declared from" $
                CEnumNotDeclared (-202) @=?! enumFromThen n202 n200
            , testCase "!declared then" $
                CEnumNotDeclared 202 @=?! enumFromThen p200 p202
            ]
        , testGroup "enumFromTo" [
              testCase "valid" $
                [n101, n100, z, p100, p101] @=? enumFromTo n101 p101
            , testCase "!declared from" $
                CEnumNotDeclared (-202) @=?! enumFromTo n202 p101
            , testCase "!declared to" $
                CEnumNotDeclared 202 @=?! enumFromTo n101 p202
            ]
        , testGroup "enumFromThenTo" [
              testCase "increasing 1" $
                [n101, n100, z, p100, p101] @=? enumFromThenTo n101 n100 p101
            , testCase "increasing 2" $
                [n101, z, p101] @=? enumFromThenTo n101 z p200
            , testCase "increasing 3" $
                [n201, n100] @=? enumFromThenTo n201 n100 p100
            , testCase "decreasing 1" $
                [p101, p100, z, n100, n101] @=? enumFromThenTo p101 p100 n101
            , testCase "decreasing 2" $
                [p101, z, n101] @=? enumFromThenTo p101 z n200
            , testCase "decreasing 3" $
                [p201, p100] @=? enumFromThenTo p201 p100 n100
            , testCase "from==then" $
                CEnumFromEqThen (-101) @=?! enumFromThenTo n101 n101 p100
            , testCase "!declared from" $
                CEnumNotDeclared (-202) @=?! enumFromThenTo n202 n200 p200
            , testCase "!declared then" $
                CEnumNotDeclared 202 @=?! enumFromThenTo z p202 p202
            , testCase "!declared to" $
                CEnumNotDeclared 202 @=?! enumFromThenTo z p100 p202
            ]
        ]
    ]
  where
    n201, n200, n101, n100, z, p100, p101, p200, p201 :: GenNegValue
    n201 = GenNegValue (-201)
    n200 = GenNegValue (-200)
    n101 = GenNegValue (-101)
    n100 = GenNegValue (-100)
    z    = GenNegValue 0
    p100 = GenNegValue 100
    p101 = GenNegValue 101
    p200 = GenNegValue 200
    p201 = GenNegValue 201

    n202 :: GenNegValue
    n202 = GenNegValue (-202)
    p202 = GenNegValue 202

{-------------------------------------------------------------------------------
  SequentialCEnum: single value
-------------------------------------------------------------------------------}

newtype SeqSingleValue = SeqSingleValue {
      un_SeqSingleValue :: FC.CUInt
    }
  deriving stock Show
  deriving newtype Eq

instance CEnum SeqSingleValue where
  type CEnumZ SeqSingleValue = FC.CUInt

  declaredValues _ = CEnum.declaredValuesFromList [(1, "OK" :| ["SUCCESS"])]
  showsUndeclared  = CEnum.showsWrappedUndeclared "SeqSingleValue"

instance SequentialCEnum SeqSingleValue where
  minDeclaredValue = SeqSingleValue 1
  maxDeclaredValue = SeqSingleValue 1

deriving via AsCEnum SeqSingleValue instance Bounded SeqSingleValue

deriving via AsCEnum SeqSingleValue instance Enum SeqSingleValue

testSeqSingleValue :: TestTree
testSeqSingleValue = testGroup "SeqSingleValue" [
      testGroup "isDeclared" [
          testCase "True" $ True @=? CEnum.isDeclared v1
        , testCase "False" $ False @=? CEnum.isDeclared v2
        ]
    , testGroup "mkDeclared" [
          testCase "declared" $ Just v1 @=? CEnum.mkDeclared 1
        , testCase "!declared" $ Nothing @=? CEnum.mkDeclared @SeqSingleValue 2
        ]
    , testGroup "showCEnum" [
          testCase "declared" $ "OK" @=? CEnum.showCEnum v1
        , testCase "!declared" $
            "SeqSingleValue 2" @=? CEnum.showCEnum v2
        ]
    , testGroup "getNames" [
          testCase "declared" $ ["OK", "SUCCESS"] @=? CEnum.getNames v1
        , testCase "!declared" $ [] @=? CEnum.getNames v2
        ]
    , testGroup "Bounded" [
          testCase "minBound" $ v1 @=? minBound
        , testCase "maxBound" $ v1 @=? maxBound
        ]
    , testGroup "Enum" [
          testGroup "succ" [
              testCase "!successor" $ CEnumNoSuccessor 1 @=?! succ v1
            , testCase "!declared" $ CEnumNotDeclared 2 @=?! succ v2
            ]
        , testGroup "pred" [
              testCase "!predecessor" $ CEnumNoPredecessor 1 @=?! pred v1
            , testCase "!declared" $ CEnumNotDeclared 2 @=?! pred v2
            ]
        , testGroup "toEnum" [
              testCase "declared" $ v1 @=? toEnum 1
            , testCase "!declared" $
                CEnumNotDeclared 2 @=?! toEnum @SeqSingleValue 2
            ]
        , testGroup "fromEnum" [
              testCase "declared" $ 1 @=? fromEnum v1
            , testCase "!declared" $ CEnumNotDeclared 2 @=?! fromEnum v2
            ]
        , testGroup "enumFrom" [
              testCase "declared" $ [v1] @=? enumFrom v1
            , testCase "!declared" $ CEnumNotDeclared 2 @=?! enumFrom v2
            ]
        , testGroup "enumFromThen" [
              testCase "from==then" $
                CEnumFromEqThen 1 @=?! enumFromThen v1 v1
            , testCase "!declared from" $
                CEnumNotDeclared 2 @=?! enumFromThen v2 v1
            , testCase "!declared then" $
                CEnumNotDeclared 2 @=?! enumFromThen v1 v2
            ]
        , testGroup "enumFromTo" [
              testCase "declared" $ [v1] @=? enumFromTo v1 v1
            , testCase "!declared from" $
                CEnumNotDeclared 2 @=?! enumFromTo v2 v1
            , testCase "!declared to" $
                CEnumNotDeclared 2 @=?! enumFromTo v1 v2
            ]
        , testGroup "enumFromThenTo" [
              testCase "from==then" $
                CEnumFromEqThen 1 @=?! enumFromThenTo v1 v1 v2
            , testCase "!declared from" $
                CEnumNotDeclared 2 @=?! enumFromThenTo v2 v1 v1
            , testCase "!declared then" $
                CEnumNotDeclared 2 @=?! enumFromThenTo v1 v2 v2
            ]
        ]
    ]
  where
    v1, v2 :: SeqSingleValue
    v1 = SeqSingleValue 1
    v2 = SeqSingleValue 2

{-------------------------------------------------------------------------------
  SequentialCEnum: positive values
-------------------------------------------------------------------------------}

newtype SeqPosValue = SeqPosValue {
      un_SeqPosValue :: FC.CUInt
    }
  deriving stock Show
  deriving newtype Eq

instance CEnum SeqPosValue where
  type CEnumZ SeqPosValue = FC.CUInt

  declaredValues _ = CEnum.declaredValuesFromList [
      (1,  "A" :| ["ALPHA"])
    , (2,  "B" :| ["BETA"])
    , (3,  NonEmpty.singleton "C")
    , (4,  NonEmpty.singleton "D")
    , (5,  NonEmpty.singleton "E")
    , (6,  NonEmpty.singleton "F")
    , (7,  NonEmpty.singleton "G")
    , (8,  NonEmpty.singleton "H")
    , (9,  NonEmpty.singleton "I")
    , (10, NonEmpty.singleton "J")
    ]
  showsUndeclared = CEnum.showsWrappedUndeclared "SeqPosValue"

instance SequentialCEnum SeqPosValue where
  minDeclaredValue = SeqPosValue 1
  maxDeclaredValue = SeqPosValue 10

deriving via AsSequentialCEnum SeqPosValue instance Bounded SeqPosValue

deriving via AsSequentialCEnum SeqPosValue instance Enum SeqPosValue

testSeqPosValue :: TestTree
testSeqPosValue = testGroup "SeqPosValue" [
      testGroup "isDeclared" [
          testCase "declared" . forM_ [1..10] $ \i ->
            True @=? CEnum.isDeclared (SeqPosValue i)
        , testCase "low" $ False @=? CEnum.isDeclared v0
        , testCase "high" $ False @=? CEnum.isDeclared v11
        ]
    , testGroup "mkDeclared" [
          testCase "declared" . forM_ [1..10] $ \i ->
            Just (SeqPosValue i) @=? CEnum.mkDeclared i
        , testCase "low" $ Nothing @=? CEnum.mkDeclared @SeqPosValue 0
        , testCase "high" $ Nothing @=? CEnum.mkDeclared @SeqPosValue 11
        ]
    , testGroup "showCEnum" [
          testCase "declared" $ "A" @=? CEnum.showCEnum v1
        , testCase "!declared" $
            "SeqPosValue 0" @=? CEnum.showCEnum v0
        ]
    , testGroup "getNames" [
          testCase "declared" $ ["A", "ALPHA"] @=? CEnum.getNames v1
        , testCase "!declared" $ [] @=? CEnum.getNames v0
        ]
    , testGroup "Bounded" [
          testCase "minBound" $ v1 @=? minBound
        , testCase "maxBound" $ v10 @=? maxBound
        ]
    , testGroup "Enum" [
          testGroup "succ" [
              testCase "valid" . forM_ [1..9] $ \i ->
                SeqPosValue (i + 1) @=? succ (SeqPosValue i)
            , testCase "max" $ CEnumNoSuccessor 10 @=?! succ v10
            , testCase "low" $ CEnumNotDeclared 0 @=?! succ v0
            , testCase "high" $ CEnumNotDeclared 11 @=?! succ v11
            ]
        , testGroup "pred" [
              testCase "valid" . forM_ [2..10] $ \i ->
                SeqPosValue (i - 1) @=? pred (SeqPosValue i)
            , testCase "min" $ CEnumNoPredecessor 1 @=?! pred v1
            , testCase "low" $ CEnumNotDeclared 0 @=?! pred v0
            , testCase "high" $ CEnumNotDeclared 11 @=?! pred v11
            ]
        , testGroup "toEnum" [
              testCase "declared" . forM_ [1..10] $ \i ->
                SeqPosValue (fromIntegral i) @=? toEnum i
            , testCase "low" $ CEnumNotDeclared 0 @=?! toEnum @SeqPosValue 0
            , testCase "high" $ CEnumNotDeclared 11 @=?! toEnum @SeqPosValue 11
            ]
        , testGroup "fromEnum" [
              testCase "declared" . forM_ [1..10] $ \i ->
                fromIntegral i @=? fromEnum (SeqPosValue i)
            , testCase "low" $ CEnumNotDeclared 0 @=?! fromEnum v0
            , testCase "high" $ CEnumNotDeclared 11 @=?! fromEnum v11
            ]
        , testGroup "enumFrom" [
              testCase "valid" $ [SeqPosValue i | i <- [5..10]] @=? enumFrom v5
            , testCase "max" $ [SeqPosValue 10] @=? enumFrom v10
            , testCase "low" $ CEnumNotDeclared 0 @=?! enumFrom v0
            , testCase "high" $ CEnumNotDeclared 11 @=?! enumFrom v11
            ]
        , testGroup "enumFromThen" [
              testCase "increasing" $
                [SeqPosValue i | i <- [2, 4 .. 10]] @=? enumFromThen v2 v4
            , testCase "decreasing" $
                [SeqPosValue i | i <- [10, 6 .. 1]] @=? enumFromThen v10 v6
            , testCase "from==then" $ CEnumFromEqThen 2 @=?! enumFromThen v2 v2
            , testCase "!declared from" $
                CEnumNotDeclared 0 @=?! enumFromThen v0 v1
            , testCase "!declared then" $
                CEnumNotDeclared 11 @=?! enumFromThen v5 v11
            ]
        , testGroup "enumFromTo" [
              testCase "valid" $
                [SeqPosValue i | i <- [4..6]] @=? enumFromTo v4 v6
            , testCase "!declared from" $
                CEnumNotDeclared 0 @=?! enumFromTo v0 v5
            , testCase "!declared to" $
                CEnumNotDeclared 11 @=?! enumFromTo v5 v11
            ]
        , testGroup "enumFromThenTo" [
              testCase "increasing" $
                [SeqPosValue i | i <- [2, 4 .. 9]] @=? enumFromThenTo v2 v4 v9
            , testCase "decreasing" $
                [SeqPosValue i | i <- [10, 8 .. 4]] @=? enumFromThenTo v10 v8 v4
            , testCase "from==then" $
                CEnumFromEqThen 2 @=?! enumFromThenTo v2 v2 v9
            , testCase "!declared from" $
                CEnumNotDeclared 0 @=?! enumFromThenTo v0 v2 v9
            , testCase "!declared then" $
                CEnumNotDeclared 11 @=?! enumFromThenTo v9 v11 v11
            , testCase "!declared to" $
                CEnumNotDeclared 11 @=?! enumFromThenTo v5 v9 v11
            ]
        ]
    ]
  where
    v0, v1, v2, v4, v5, v6, v8, v9, v10, v11 :: SeqPosValue
    v0  = SeqPosValue 0
    v1  = SeqPosValue 1
    v2  = SeqPosValue 2
    v4  = SeqPosValue 4
    v5  = SeqPosValue 5
    v6  = SeqPosValue 6
    v8  = SeqPosValue 8
    v9  = SeqPosValue 9
    v10 = SeqPosValue 10
    v11 = SeqPosValue 11

{-------------------------------------------------------------------------------
  SequentialCEnum: negative values
-------------------------------------------------------------------------------}

newtype SeqNegValue = SeqNegValue {
      un_SeqNegValue :: FC.CInt
    }
  deriving stock Show
  deriving newtype Eq

instance CEnum SeqNegValue where
  type CEnumZ SeqNegValue = FC.CInt

  declaredValues _ = CEnum.declaredValuesFromList [
      (-5, NonEmpty.singleton "GARBAGE")
    , (-4, NonEmpty.singleton "TERRIBLE")
    , (-3, NonEmpty.singleton "BAD")
    , (-2, NonEmpty.singleton "DISAPPOINTING")
    , (-1, NonEmpty.singleton "UGH")
    , (0,  "NEUTRAL" :| ["UNREMARKABLE"])
    , (1,  NonEmpty.singleton "MEH")
    , (2,  NonEmpty.singleton "OK")
    , (3,  NonEmpty.singleton "GOOD")
    , (4,  NonEmpty.singleton "GREAT")
    , (5,  NonEmpty.singleton "SPECTACULAR")
    ]
  showsUndeclared = CEnum.showsWrappedUndeclared "SeqNegValue"

instance SequentialCEnum SeqNegValue where
  minDeclaredValue = SeqNegValue (-5)
  maxDeclaredValue = SeqNegValue 5

deriving via AsSequentialCEnum SeqNegValue instance Bounded SeqNegValue

deriving via AsSequentialCEnum SeqNegValue instance Enum SeqNegValue

testSeqNegValue :: TestTree
testSeqNegValue = testGroup "SeqNegValue" [
      testGroup "isDeclared" [
          testCase "declared" . forM_ [-5..5] $ \i ->
            True @=? CEnum.isDeclared (SeqNegValue i)
        , testCase "low" $ False @=? CEnum.isDeclared n6
        , testCase "high" $ False @=? CEnum.isDeclared p6
        ]
    , testGroup "mkDeclared" [
          testCase "declared" . forM_ [-5..5] $ \i ->
            Just (SeqNegValue i) @=? CEnum.mkDeclared i
        , testCase "low" $ Nothing @=? CEnum.mkDeclared @SeqNegValue (-6)
        , testCase "high" $ Nothing @=? CEnum.mkDeclared @SeqNegValue 6
        ]
    , testGroup "showCEnum" [
          testCase "declared" $ "NEUTRAL" @=? CEnum.showCEnum z
        , testCase "!declared" $
            "SeqNegValue (-6)" @=? CEnum.showCEnum n6
        ]
    , testGroup "getNames" [
          testCase "declared" $
            ["NEUTRAL", "UNREMARKABLE"] @=? CEnum.getNames z
        , testCase "!declared" $ [] @=? CEnum.getNames n6
        ]
    , testGroup "Bounded" [
          testCase "minBound" $ n5 @=? minBound
        , testCase "maxBound" $ p5 @=? maxBound
        ]
    , testGroup "Enum" [
          testGroup "succ" [
              testCase "valid" . forM_ [-5..4] $ \i ->
                SeqNegValue (i + 1) @=? succ (SeqNegValue i)
            , testCase "max" $ CEnumNoSuccessor 5 @=?! succ p5
            , testCase "low" $ CEnumNotDeclared (-6) @=?! succ n6
            , testCase "high" $ CEnumNotDeclared 6 @=?! succ p6
            ]
        , testGroup "pred" [
              testCase "valid" . forM_ [-4..5] $ \i ->
                SeqNegValue (i - 1) @=? pred (SeqNegValue i)
            , testCase "min" $ CEnumNoPredecessor (-5) @=?! pred n5
            , testCase "low" $ CEnumNotDeclared (-6) @=?! pred n6
            , testCase "high" $ CEnumNotDeclared 6 @=?! pred p6
            ]
        , testGroup "toEnum" [
              testCase "declared" . forM_ [-5..5] $ \i ->
                SeqNegValue (fromIntegral i) @=? toEnum i
            , testCase "low" $
                 CEnumNotDeclared (-6) @=?! toEnum @SeqNegValue (-6)
            , testCase "high" $ CEnumNotDeclared 6 @=?! toEnum @SeqNegValue 6
            ]
        , testGroup "fromEnum" [
              testCase "declared" . forM_ [-5..5] $ \i ->
                fromIntegral i @=? fromEnum (SeqNegValue i)
            , testCase "low" $ CEnumNotDeclared (-6) @=?! fromEnum n6
            , testCase "high" $ CEnumNotDeclared 6 @=?! fromEnum p6
            ]
        , testGroup "enumFrom" [
              testCase "valid" $ [SeqNegValue i | i <- [-1..5]] @=? enumFrom n1
            , testCase "max" $ [SeqNegValue 5] @=? enumFrom p5
            , testCase "low" $ CEnumNotDeclared (-6) @=?! enumFrom n6
            , testCase "high" $ CEnumNotDeclared 6 @=?! enumFrom p6
            ]
        , testGroup "enumFromThen" [
              testCase "increasing" $
                [SeqNegValue i | i <- [-3, -1 .. 5]] @=? enumFromThen n3 n1
            , testCase "decreasing" $
                [SeqNegValue i | i <- [4, 1 .. -5]] @=? enumFromThen p4 p1
            , testCase "from==then" $
                CEnumFromEqThen (-3) @=?! enumFromThen n3 n3
            , testCase "!declared from" $
                CEnumNotDeclared (-6) @=?! enumFromThen n6 n3
            , testCase "!declared then" $
                CEnumNotDeclared 6 @=?! enumFromThen n1 p6
            ]
        , testGroup "enumFromTo" [
              testCase "valid" $
                [SeqNegValue i | i <- [-3..3]] @=? enumFromTo n3 p3
            , testCase "!declared from" $
                CEnumNotDeclared (-6) @=?! enumFromTo n6 p1
            , testCase "!declared to" $
                CEnumNotDeclared 6 @=?! enumFromTo n1 p6
            ]
        , testGroup "enumFromThenTo" [
              testCase "increasing" $
                [SeqNegValue i | i <- [-3, -1 .. 4]] @=? enumFromThenTo n3 n1 p4
            , testCase "decreasing" $
                [SeqNegValue i | i <- [5, 3 .. -3]] @=? enumFromThenTo p5 p3 n3
            , testCase "from==then" $
                CEnumFromEqThen (-3) @=?! enumFromThenTo n3 n3 p3
            , testCase "!declared from" $
                CEnumNotDeclared (-6) @=?! enumFromThenTo n6 n3 p4
            , testCase "!declared then" $
                CEnumNotDeclared 6 @=?! enumFromThenTo n1 p6 p6
            , testCase "!declared to" $
                CEnumNotDeclared 6 @=?! enumFromThenTo n3 n1 p6
            ]
        ]
    ]
  where
    n6, n5, n3, n1, z, p1, p3, p4, p5, p6 :: SeqNegValue
    n6 = SeqNegValue (-6)
    n5 = SeqNegValue (-5)
    n3 = SeqNegValue (-3)
    n1 = SeqNegValue (-1)
    z  = SeqNegValue 0
    p1 = SeqNegValue 1
    p3 = SeqNegValue 3
    p4 = SeqNegValue 4
    p5 = SeqNegValue 5
    p6 = SeqNegValue 6
