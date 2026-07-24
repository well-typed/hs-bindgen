-- | Error-aware combinators: 'throwOnNonZero' and 'throwOn' on the return value,
-- 'throwOnOut' on an out-parameter, and the guarantee that a throw unwinds the
-- brackets opened before it. 'resultIO' may also throw from its converter.
module Test.HsBindgen.HighLevel.Errors (tests) where

import Control.Exception (Exception, finally, throwIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.HighLevel (discardResult, dropTrailingUnit, input, output,
                            resultIO, resultPure, throwOn, throwOnNonZero,
                            throwOnOut, toHighLevel)
import HsBindgen.HighLevel.Marshaller (bracket, scalar)

import Test.HsBindgen.HighLevel.Util (assertThrows, peekIntOut, returnsStatus)

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

newtype SampleErr = SampleErr CInt
  deriving stock Show
instance Exception SampleErr

-- | Writes @7@ to the out-parameter and returns the given status.
outWithStatus :: CInt -> Ptr CInt -> IO CInt
outWithStatus status p = poke p (CInt 7) >> pure status

-- | Writes its first argument into the out-parameter and returns @0@.
writeOutCode :: CInt -> Ptr CInt -> IO CInt
writeOutCode code outp = poke outp code >> pure 0

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "errors"
    [ testCase "throwOnNonZero: status 0 returns ()" $
        hsCheckStatus 0

    , testCase "throwOnNonZero: a non-zero status throws" $
        assertThrows (hsCheckStatus 42) $ \(SampleErr n) -> n @?= 42

    , testCase "throwOn: the value is transformed (hs /= hs')" $ do
        hsThrowOnNegBool 7 >>= (@?= True)
        assertThrows (hsThrowOnNegBool (-3)) $ \(SampleErr n) -> n @?= (-3)

    , testCase "throwOnOut: a non-negative out value flows through" $
        hsCheckOut 7 >>= (@?= 7)

    , testCase "throwOnOut: a negative out value throws" $
        assertThrows (hsCheckOut (-2)) $ \(SampleErr n) -> n @?= (-2)

    , testCase "dropTrailingUnit over throwOnNonZero: status 0 keeps just the output" $
        hsOutDrop 0 >>= (@?= 7)

    , testCase "dropTrailingUnit over throwOnNonZero: a non-zero status throws" $
        assertThrows (hsOutDrop 9) $ \(SampleErr n) -> n @?= 9

    , testCase "resultIO can throw from its converter" $ do
        hsResultIOMaybe 5 >>= (@?= 5)
        assertThrows (hsResultIOMaybe (-3)) $ \(SampleErr n) -> n @?= (-3)

    , testCase "a throw unwinds the brackets opened before it" $ do
        released <- newIORef False
        let wrapped = toHighLevel
              ( input (bracket (\v k -> k v `finally` writeIORef released True))
              $ throwOnNonZero SampleErr
              ) returnsStatus
        assertThrows (wrapped 1) $ \(SampleErr _) -> pure ()
        readIORef released >>= (@?= True)
    ]

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

hsCheckStatus :: CInt -> IO ()
hsCheckStatus = toHighLevel
    (input (scalar id) $ throwOnNonZero SampleErr) returnsStatus

hsThrowOnNegBool :: CInt -> IO Bool
hsThrowOnNegBool = toHighLevel
    (input (scalar id) $ throwOn classify (resultPure fromIntegral)) returnsStatus
  where
    classify :: Int -> Either SampleErr Bool
    classify n
      | n < 0     = Left (SampleErr (fromIntegral n))
      | otherwise = Right (n > 0)

hsCheckOut :: CInt -> IO Int
hsCheckOut = toHighLevel
    ( dropTrailingUnit
    $ input (scalar id)
    $ output (throwOnOut classify)
    $ discardResult
    ) writeOutCode
  where
    classify :: CInt -> Either SampleErr Int
    classify n
      | n < 0     = Left (SampleErr n)
      | otherwise = Right (fromIntegral n)

-- | @output peekIntOut@ beside a void error closer leaves @(Int, ())@;
-- 'dropTrailingUnit' collapses it to @Int@. A non-zero status throws before the
-- value is returned.
hsOutDrop :: CInt -> IO Int
hsOutDrop status = toHighLevel
    ( dropTrailingUnit
    $ output peekIntOut
    $ throwOnNonZero SampleErr
    ) (outWithStatus status)

hsResultIOMaybe :: CInt -> IO Int
hsResultIOMaybe = toHighLevel (input (scalar id) $ resultIO convert) returnsStatus
  where
    convert :: CInt -> IO Int
    convert (CInt n)
      | n < 0     = throwIO (SampleErr (CInt n))
      | otherwise = pure (fromIntegral n)
