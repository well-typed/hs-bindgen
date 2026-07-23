{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- | The unlifted by-value marshallers for small structs GHC's FFI cannot pass
-- directly: 'outputUnlifted' for a @W@ out-parameter and 'bracketUnlifted' for
-- an @R@ argument. The fixtures store a single machine 'Int' in a byte array,
-- standing in for the unlifted payloads a real by-value binding uses.
module Test.HsBindgen.HighLevel.Unlifted (tests) where

import Foreign.C.Types (CInt (..))
import GHC.Exts (ByteArray#, Int (..), MutableByteArray#, RealWorld,
                 indexIntArray#, newByteArray#, unsafeFreezeByteArray#,
                 writeIntArray#)
import GHC.IO (IO (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.HighLevel (discardResult, dropTrailingUnit, input, resultPure,
                            toHighLevel)
import HsBindgen.HighLevel.Marshaller (Unmarshaller (..), scalar)
import HsBindgen.HighLevel.Unlifted (bracketUnlifted, outputUnlifted)

{-------------------------------------------------------------------------------
  Fixtures: an unlifted R / W byte-array payload holding one machine Int
-------------------------------------------------------------------------------}

newtype ByRef = ByRef ByteArray#                     -- like R: read-only
newtype ByOut = ByOut (MutableByteArray# RealWorld)  -- like W: write buffer
data    Boxed = Boxed ByteArray#                     -- like OnHaskellHeap

-- | Allocate a byte array, run @fill@, freeze it, and return the frozen wrapper
-- alongside the fill's result.
mkByteArray ::
     Int
  -> (ByteArray# -> a)
  -> (MutableByteArray# RealWorld -> IO b)
  -> IO (a, b)
mkByteArray (I# sz) wrap fill = IO $ \s0 ->
    case newByteArray# sz s0 of
      (# s1, marr #) -> case unIO (fill marr) s1 of
        (# s2, b #)  -> case unsafeFreezeByteArray# marr s2 of
          (# s3, arr #) -> (# s3, (wrap arr, b) #)
  where
    unIO (IO f) = f

writeIntBA :: MutableByteArray# RealWorld -> Int -> IO ()
writeIntBA marr (I# n) = IO $ \s -> (# writeIntArray# marr 0# n s, () #)

readIntBA :: ByteArray# -> Int
readIntBA arr = I# (indexIntArray# arr 0#)

mkBoxed :: Int -> IO Boxed
mkBoxed n = fst <$> mkByteArray 8 Boxed (`writeIntBA` n)

-- | Supply an @R@ argument from a heap-boxed struct.
onBoxed :: Boxed -> (ByRef -> IO r) -> IO r
onBoxed (Boxed arr) k = k (ByRef arr)

-- | The @R@ side: read the int out of a by-value argument.
callReadRef :: ByRef -> IO CInt
callReadRef (ByRef arr) = pure (fromIntegral (readIntBA arr))

-- | The @W@ side: write the int into a by-value out-parameter.
callFillOut :: CInt -> ByOut -> IO CInt
callFillOut v (ByOut marr) = writeIntBA marr (fromIntegral v) >> pure 0

-- | An 'Unmarshaller' fronting the unlifted @W@ out-parameter.
byOutUnmarshaller :: Unmarshaller ByOut Int
byOutUnmarshaller = Unmarshaller $ \k -> do
    (Boxed arr, r) <- mkByteArray 8 Boxed (\marr -> k (ByOut marr))
    pure (readIntBA arr, r)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "unlifted by-value marshallers"
    [ testCase "outputUnlifted: an unlifted W out-parameter reads back" $
        hsByValueOut 21 >>= (@?= 21)
    , testCase "bracketUnlifted: an unlifted R by-value argument is read" $
        hsByValueIn 34 >>= (@?= 34)
    ]

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

-- | 'outputUnlifted' on the @W@ out-parameter (tail position), a scalar input
-- before it, and 'dropTrailingUnit' collapsing the void closer's unit.
hsByValueOut :: Int -> IO Int
hsByValueOut = toHighLevel
    ( dropTrailingUnit
    $ input          (scalar (CInt . fromIntegral))
    $ outputUnlifted byOutUnmarshaller
    $ discardResult
    ) callFillOut

hsByValueIn :: Int -> IO Int
hsByValueIn n = do
    boxed <- mkBoxed n
    toHighLevel (input (bracketUnlifted onBoxed) $ resultPure fromIntegral)
                callReadRef boxed
