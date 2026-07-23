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

import HsBindgen.HighLevel (input, resultPure, toHighLevel)
import HsBindgen.HighLevel.Marshaller (Unmarshaller (..), scalar)
import HsBindgen.HighLevel.Unlifted (bracketUnlifted, outputUnlifted)

{-------------------------------------------------------------------------------
  Fixtures: an unlifted R / W byte-array payload holding one machine Int
-------------------------------------------------------------------------------}

newtype ByRef = ByRef ByteArray#                     -- like R: read-only
newtype ByOut = ByOut (MutableByteArray# RealWorld)  -- like W: write buffer
data    Boxed = Boxed ByteArray#                     -- like OnHaskellHeap

-- | Allocate a byte array and hand the mutable buffer to a continuation.
withNewByteArray :: Int -> (MutableByteArray# RealWorld -> IO r) -> IO r
withNewByteArray (I# sz) k = IO $ \s0 ->
    case newByteArray# sz s0 of
      (# s1, marr #) -> unIO (k marr) s1
  where
    unIO (IO f) = f

-- | Freeze a mutable byte array in place and read the frozen bytes. The frozen
-- array is handed to a continuation rather than returned, because @ByteArray#@ is
-- unlifted and so cannot be an @IO@ result.
withFrozenByteArray :: MutableByteArray# RealWorld -> (ByteArray# -> a) -> IO a
withFrozenByteArray marr f = IO $ \s0 ->
    case unsafeFreezeByteArray# marr s0 of
      (# s1, arr #) -> (# s1, f arr #)

writeIntBA :: MutableByteArray# RealWorld -> Int -> IO ()
writeIntBA marr (I# n) = IO $ \s -> (# writeIntArray# marr 0# n s, () #)

readIntBA :: ByteArray# -> Int
readIntBA arr = I# (indexIntArray# arr 0#)

mkBoxed :: Int -> IO Boxed
mkBoxed n = withNewByteArray 8 $ \marr ->
    writeIntBA marr n >> withFrozenByteArray marr Boxed

-- | Supply an @R@ argument from a heap-boxed struct.
onBoxed :: Boxed -> (ByRef -> IO r) -> IO r
onBoxed (Boxed arr) k = k (ByRef arr)

-- | The @R@ side: read the int out of a by-value argument.
callReadRef :: ByRef -> IO CInt
callReadRef (ByRef arr) = pure (fromIntegral (readIntBA arr))

-- | The @W@ side: write the int into a by-value out-parameter.
callFillOut :: CInt -> ByOut -> IO CInt
callFillOut v (ByOut marr) = writeIntBA marr (fromIntegral v) >> pure 0

-- | As 'callFillOut', but the by-value out-parameter comes /first/, so the spec
-- has a wrapper argument after the unlifted output.
callFillOutFirst :: ByOut -> CInt -> IO CInt
callFillOutFirst (ByOut marr) v = writeIntBA marr (fromIntegral v) >> pure 0

-- | An 'Unmarshaller' fronting the unlifted @W@ out-parameter, in its two halves.
-- This is the split a real by-value binding needs: libclang's @preallocate@
-- allocates, fills and freezes in one bracket, so the allocation becomes the
-- allocator and the freeze becomes the reader.
byOutUnmarshaller :: Unmarshaller ByOut Int
byOutUnmarshaller = Unmarshaller allocByOut readByOut
  where
    allocByOut :: (ByOut -> IO r) -> IO r
    -- An explicit lambda, not (k . ByOut): (.) takes a lifted argument.
    allocByOut k = withNewByteArray 8 (\marr -> k (ByOut marr))

    readByOut :: ByOut -> IO Int
    readByOut (ByOut marr) = withFrozenByteArray marr readIntBA

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "unlifted by-value marshallers"
    [ testCase "outputUnlifted: an unlifted W out-parameter reads back" $
        hsByValueOut 21 >>= (@?= 21)
    , testCase "outputUnlifted: a wrapper argument may follow an unlifted output" $
        hsByValueOutThenArg 55 >>= (@?= 55)
    , testCase "bracketUnlifted: an unlifted R by-value argument is read" $
        hsByValueIn 34 >>= (@?= 34)
    ]

{-------------------------------------------------------------------------------
  Wrappers under test
-------------------------------------------------------------------------------}

-- | 'outputUnlifted' on the @W@ out-parameter, a scalar input before it, and the
-- assembler ignoring the C return.
hsByValueOut :: Int -> IO Int
hsByValueOut = toHighLevel
    ( input          (scalar (CInt . fromIntegral))
    $ outputUnlifted byOutUnmarshaller
    $ resultPure (\v _ -> v)
    ) callFillOut

-- | An unlifted output with a wrapper argument /after/ it. This works only because
-- the threading instances never bind the value the bracket supplies: binding it
-- would be a representation-polymorphic binder, which is illegal.
hsByValueOutThenArg :: Int -> IO Int
hsByValueOutThenArg = toHighLevel
    ( outputUnlifted byOutUnmarshaller
    $ input          (scalar (CInt . fromIntegral))
    $ resultPure (\v _ -> v)
    ) callFillOutFirst

hsByValueIn :: Int -> IO Int
hsByValueIn n = do
    boxed <- mkBoxed n
    toHighLevel (input (bracketUnlifted onBoxed) $ resultPure fromIntegral)
                callReadRef boxed
