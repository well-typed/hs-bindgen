{-# LANGUAGE TypeFamilies #-}

module Test.HsBindgen.Runtime.CEnum (tests) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Foreign.C qualified as FC
import GHC.Show (appPrec1)
import Test.HsBindgen.Runtime.CEnumArbitrary ()
import Test.Internal.Tasty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Property, chooseInt,
                              counterexample, testProperty, (===))
import Text.Read (Read (readPrec), minPrec, readMaybe)

import HsBindgen.Runtime.CEnum (AsCEnum (..), AsSequentialCEnum (..),
                                CEnum (CEnumZ), CEnumException (..),
                                SequentialCEnum)
import HsBindgen.Runtime.CEnum qualified as CEnum

read_show_prop :: forall a. (Show a, Read a, Eq a) => a -> Property
read_show_prop x = read (show x) === x

newtype Precedence = MkPrecedence Int
  deriving Show

instance Arbitrary Precedence where
  arbitrary = MkPrecedence <$> chooseInt (minPrec, appPrec1)

readsPrec_showsPrec_prop
  :: forall a. (Show a, Read a, Eq a)
  => a -> Precedence -> Property
readsPrec_showsPrec_prop x (MkPrecedence d) = counterexample errMsg isElem
  where
    parsedValues = readsPrec d (showsPrec d x "")
    isElem = (x,"") `elem` parsedValues
    errMsg = "expected element " <> show x
             <> " not found in possible parsed values " <> show parsedValues

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
      , testNastyValue
      ]
    ]

{-------------------------------------------------------------------------------
  CEnum: no values
-------------------------------------------------------------------------------}

newtype NoValue = NoValue {
      _un_NoValue :: FC.CUInt
    }
  deriving newtype Eq

instance CEnum NoValue where
  type CEnumZ NoValue = FC.CUInt

  declaredValues _ = CEnum.declaredValuesFromList []
  showsUndeclared  = CEnum.showsWrappedUndeclared "NoValue"
  readPrecUndeclared = CEnum.readPrecWrappedUndeclared "NoValue"

instance Show NoValue where
  showsPrec = CEnum.showsCEnum

instance Read NoValue where
  readPrec = CEnum.readPrecCEnum

deriving via AsCEnum NoValue instance Bounded NoValue

deriving via AsCEnum NoValue instance Enum NoValue

deriving via AsCEnum NoValue instance Arbitrary NoValue

testNoValue :: TestTree
testNoValue = testGroup "NoValue" [
      testCase "isDeclared" $ False @=? CEnum.isDeclared v1
    , testCase "mkDeclared" $ Nothing @=? CEnum.mkDeclared @NoValue 1
    , testCase "Show" $ "NoValue 1" @=? show v1
    , testGroup "Read" [
          testCase "!declared/pos" $ (Just v1) @=? readMaybe "NoValue 1"
          -- We inherit the behavior of downstream Read instances. For example,
          -- we parse negative values even for unsigned types such as
          -- 'FC.CUInt'.
        , expectFail
          $ testCase "!declared/neg" $ Nothing @=? readMaybe @NoValue "NoValue (-1)"
        , testCase "noparse" $ Nothing @=? readMaybe @NoValue "noparse"
          -- Similarly, we parse invalid Haskell; the same is true for derived
          -- instances,
        , expectFail
          $ testCase "noparse/neg" $ Nothing @=? readMaybe @NoValue "NoValue -1"
        , testProperty "read . show" (read_show_prop @NoValue)
        , testProperty "readsPrec . showsPrec" (readsPrec_showsPrec_prop @NoValue)
        ]
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
      _un_GenSingleValue :: FC.CUInt
    }
  deriving newtype Eq

instance CEnum GenSingleValue where
  type CEnumZ GenSingleValue = FC.CUInt

  declaredValues _ = CEnum.declaredValuesFromList [(1, "OK" :| ["SUCCESS"])]
  showsUndeclared  = CEnum.showsWrappedUndeclared "GenSingleValue"
  readPrecUndeclared = CEnum.readPrecWrappedUndeclared "GenSingleValue"

instance Show GenSingleValue where
  showsPrec = CEnum.showsCEnum

instance Read GenSingleValue where
  readPrec = CEnum.readPrecCEnum

deriving via AsCEnum GenSingleValue instance Bounded GenSingleValue

deriving via AsCEnum GenSingleValue instance Enum GenSingleValue

deriving via AsCEnum GenSingleValue instance Arbitrary GenSingleValue

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
    , testGroup "Show" [
          testCase "declared" $ "OK" @=? show v1
        , testCase "!declared" $ "GenSingleValue 2" @=? show v2
        ]
    , testGroup "Read" [
          testCase "declared/ok" $ (Just v1) @=? readMaybe "OK"
        , testCase "declared/sc" $ (Just v1) @=? readMaybe "SUCCESS"
        , testCase "declared/parentheses" $ (Just v1) @=? readMaybe "(OK)"
        , testCase "declared/parentheses/spaces" $ (Just v1) @=? readMaybe "( OK )"
        , testCase "declared/spaces" $ (Just v1) @=? readMaybe " OK "
        , testCase "!declared" $ (Just v2) @=? readMaybe "GenSingleValue 2"
        , testCase "noParse" $ Nothing @=? readMaybe @GenSingleValue "noParse"
        , testProperty "read . show" (read_show_prop @GenSingleValue)
        , testProperty "readsPrec . showsPrec" (readsPrec_showsPrec_prop @GenSingleValue)
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
      _un_GenPosValue :: FC.CUInt
    }
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
  readPrecUndeclared = CEnum.readPrecWrappedUndeclared "GenPosValue"

instance Show GenPosValue where
  showsPrec = CEnum.showsCEnum

instance Read GenPosValue where
  readPrec = CEnum.readPrecCEnum

deriving via AsCEnum GenPosValue instance Bounded GenPosValue

deriving via AsCEnum GenPosValue instance Enum GenPosValue

deriving via AsCEnum GenPosValue instance Arbitrary GenPosValue

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
    , testGroup "Show" [
          testCase "declared" $ "OK" @=? show v200
        , testCase "!declared" $ "GenPosValue 300" @=? show v300
        ]
    , testGroup "Read" [
          testCase "declared" $ (Just v200) @=? readMaybe "OK"
        , testCase "declared/parentheses" $ (Just v200) @=? readMaybe "(OK)"
        , testCase "declared/parentheses/spaces" $ (Just v200) @=? readMaybe "( OK )"
        , testCase "declared/spaces" $ (Just $ v200) @=? readMaybe " OK "
        , testCase "!declared" $ (Just v300) @=? readMaybe "GenPosValue 300"
        , testCase "noparse" $ Nothing @=? readMaybe @GenPosValue "noparse"
        , testProperty "read . show" (read_show_prop @GenPosValue)
        , testProperty "readsPrec . showsPrec" (readsPrec_showsPrec_prop @GenPosValue)
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
      _un_GenNegValue :: FC.CInt
    }
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
  readPrecUndeclared = CEnum.readPrecWrappedUndeclared "GenNegValue"

instance Show GenNegValue where
  showsPrec = CEnum.showsCEnum

instance Read GenNegValue where
  readPrec = CEnum.readPrecCEnum

deriving via AsCEnum GenNegValue instance Bounded GenNegValue

deriving via AsCEnum GenNegValue instance Enum GenNegValue

deriving via AsCEnum GenNegValue instance Arbitrary GenNegValue

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
    , testGroup "Show" [
          testCase "declared" $ "NEUTRAL" @=? show z
        , testCase "!declared" $ "GenNegValue (-202)" @=? show n202
        ]
    , testGroup "Read" [
          testCase "declared" $ (Just $ GenNegValue 0) @=? readMaybe "NEUTRAL"
        , testCase "declared/parentheses" $ (Just p200) @=? readMaybe "(GREAT)"
        , testCase "declared/parentheses/spaces" $ (Just p200) @=? readMaybe "( GREAT )"
        , testCase "declared/spaces" $ (Just n100) @=? readMaybe " BAD "
        , testCase "!declared" $ (Just n100) @=? readMaybe "GenNegValue (-100)"
        , testCase "noparse" $ Nothing @=? readMaybe @GenNegValue "noparse"
        , testProperty "read . show" (read_show_prop @GenNegValue)
        , testProperty "readsPrec . showsPrec" (readsPrec_showsPrec_prop @GenNegValue)
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
      _un_SeqSingleValue :: FC.CUInt
    }
  deriving newtype Eq

instance CEnum SeqSingleValue where
  type CEnumZ SeqSingleValue = FC.CUInt

  declaredValues _ = CEnum.declaredValuesFromList [(1, "OK" :| ["SUCCESS"])]
  showsUndeclared  = CEnum.showsWrappedUndeclared "SeqSingleValue"
  readPrecUndeclared = CEnum.readPrecWrappedUndeclared "SeqSingleValue"

instance Show SeqSingleValue where
  showsPrec = CEnum.showsCEnum

instance Read SeqSingleValue where
  readPrec = CEnum.readPrecCEnum

instance SequentialCEnum SeqSingleValue where
  minDeclaredValue = SeqSingleValue 1
  maxDeclaredValue = SeqSingleValue 1

deriving via AsCEnum SeqSingleValue instance Bounded SeqSingleValue

deriving via AsCEnum SeqSingleValue instance Enum SeqSingleValue

deriving via AsCEnum SeqSingleValue instance Arbitrary SeqSingleValue

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
    , testGroup "Show" [
          testCase "declared" $ "OK" @=? show v1
        , testCase "!declared" $ "SeqSingleValue 2" @=? show v2
        ]
    , testGroup "Read" [
          testCase "declared" $ (Just v1) @=? readMaybe "OK"
        , testCase "!declared" $ (Just v2) @=? readMaybe "SeqSingleValue 2"
        , testCase "noparse" $ Nothing @=? readMaybe @SeqSingleValue "noparse"
        , testProperty "read . show" (read_show_prop @SeqSingleValue)
        , testProperty "readsPrec . showsPrec" (readsPrec_showsPrec_prop @GenSingleValue)
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
      _un_SeqPosValue :: FC.CUInt
    }
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
  readPrecUndeclared = CEnum.readPrecWrappedUndeclared "SeqPosValue"

instance Show SeqPosValue where
  showsPrec = CEnum.showsCEnum

instance Read SeqPosValue where
  readPrec = CEnum.readPrecCEnum

instance SequentialCEnum SeqPosValue where
  minDeclaredValue = SeqPosValue 1
  maxDeclaredValue = SeqPosValue 10

deriving via AsSequentialCEnum SeqPosValue instance Bounded SeqPosValue

deriving via AsSequentialCEnum SeqPosValue instance Enum SeqPosValue

deriving via AsSequentialCEnum SeqPosValue instance Arbitrary SeqPosValue

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
    , testGroup "Show" [
          testCase "declared" $ "A" @=? show v1
        , testCase "!declared" $ "SeqPosValue 0" @=? show v0
        ]
    , testGroup "Read" [
          testCase "declared" $ (Just v3) @=? readMaybe "C"
        , testCase "!declared" $ (Just v11) @=? readMaybe "SeqPosValue 11"
        , testCase "noparse" $ Nothing @=? readMaybe @SeqPosValue "noparse"
        , testProperty "read . show" (read_show_prop @SeqPosValue)
        , testProperty "readsPrec . showsPrec" (readsPrec_showsPrec_prop @SeqPosValue)
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
    v3  = SeqPosValue 3
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
      _un_SeqNegValue :: FC.CInt
    }
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
  readPrecUndeclared = CEnum.readPrecWrappedUndeclared "SeqNegValue"

instance Show SeqNegValue where
  showsPrec = CEnum.showsCEnum

instance Read SeqNegValue where
  readPrec = CEnum.readPrecCEnum

instance SequentialCEnum SeqNegValue where
  minDeclaredValue = SeqNegValue (-5)
  maxDeclaredValue = SeqNegValue 5

deriving via AsSequentialCEnum SeqNegValue instance Bounded SeqNegValue

deriving via AsSequentialCEnum SeqNegValue instance Enum SeqNegValue

deriving via AsSequentialCEnum SeqNegValue instance Arbitrary SeqNegValue

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
    , testGroup "Show" [
          testCase "declared" $ "NEUTRAL" @=? show z
        , testCase "!declared" $ "SeqNegValue (-6)" @=? show n6
        ]
    , testGroup "Read" [
          testCase "declared" $ (Just n3) @=? readMaybe "BAD"
        , testCase "!declared" $ (Just $ SeqNegValue (-100)) @=? readMaybe "SeqNegValue (-100)"
        , testCase "noparse" $ Nothing @=? readMaybe @SeqNegValue "noparse"
        , testProperty "read . show" (read_show_prop @SeqNegValue)
        , testProperty "readsPrec . showsPrec" (readsPrec_showsPrec_prop @SeqNegValue)
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

{-------------------------------------------------------------------------------
  NastiEnum
-------------------------------------------------------------------------------}

newtype NastyValue = NastyValue {
      _un_NastyValue :: FC.CInt
    }
  deriving newtype Eq

instance CEnum NastyValue where
  type CEnumZ NastyValue = FC.CInt

  declaredValues _ = CEnum.declaredValuesFromList [
      (-2, NonEmpty.singleton "Nas")
    , (-1,  NonEmpty.singleton "NastyValueNeg")
    , (0,  "Nasty" :| ["NastyValue"])
    , (1,  NonEmpty.singleton "NastyVal")
    , (2,  NonEmpty.singleton "NastyValuePos")
    ]
  showsUndeclared = CEnum.showsWrappedUndeclared "NastyValue"
  readPrecUndeclared = CEnum.readPrecWrappedUndeclared "NastyValue"

instance Show NastyValue where
  showsPrec = CEnum.showsCEnum

instance Read NastyValue where
  readPrec = CEnum.readPrecCEnum

instance SequentialCEnum NastyValue where
  minDeclaredValue = NastyValue (-2)
  maxDeclaredValue = NastyValue 2

deriving via AsSequentialCEnum NastyValue instance Bounded NastyValue

deriving via AsSequentialCEnum NastyValue instance Enum NastyValue

deriving via AsSequentialCEnum NastyValue instance Arbitrary NastyValue

testNastyValue :: TestTree
testNastyValue = testGroup "NastyValue" [
      testGroup "Show" [
          testCase "declared" $ "Nasty" @=? show z
        , testCase "!declared" $ "NastyValue (-6)" @=? show n6
        ]
    , testGroup "Read" [
          testCase "declared" $ (Just n2) @=? readMaybe "Nas"
        , testCase "declared" $ (Just n1) @=? readMaybe "NastyValueNeg"
        , testCase "declared" $ (Just z) @=? readMaybe "Nasty"
        , testCase "declared" $ (Just z) @=? readMaybe "NastyValue"
        , testCase "declared" $ (Just p1) @=? readMaybe "NastyVal"
        , testCase "declared" $ (Just p2) @=? readMaybe "NastyValuePos"
        , testCase "!declared" $ (Just $ NastyValue (-100)) @=? readMaybe "NastyValue (-100)"
        , testCase "!declared" $ (Just $ NastyValue (100)) @=? readMaybe "NastyValue (100)"
        , testCase "noparse" $ Nothing @=? readMaybe @NastyValue "noparse"
        , testProperty "read . show" (read_show_prop @NastyValue)
        , testProperty "readsPrec . showsPrec" (readsPrec_showsPrec_prop @NastyValue)
        ]
    ]
  where
    n6, n1, z, p1, p2 :: NastyValue
    n6 = NastyValue (-6)
    n2 = NastyValue (-2)
    n1 = NastyValue (-1)
    z  = NastyValue 0
    p1 = NastyValue 1
    p2 = NastyValue 2
