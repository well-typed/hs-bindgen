{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiWayIf #-}

module Test.HsBindgen.Runtime.Support.ByteArray (tests) where

import Data.Bifunctor (Bifunctor (first))
import Data.Either (partitionEithers)
import Data.Primitive.ByteArray (ByteArray)
import Data.Primitive.ByteArray qualified as P
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable (sizeOf))
import GHC.Exts (IsList (fromList, toList))
import GHC.Stack (HasCallStack)
import Test.QuickCheck (Arbitrary (..), Arbitrary2 (liftShrink2), Gen,
                        Large (Large, getLarge), Property, chooseInt,
                        shrinkList, sized, tabulate, vectorOf, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Printf (printf)

import HsBindgen.Runtime.Support.Bitfield (Bitfield (narrow))
import HsBindgen.Runtime.Support.ByteArray (getUnionPayload,
                                            getUnionPayloadBits,
                                            setUnionPayload,
                                            setUnionPayloadBits)

import Test.Util.QC (Arbitrary4 (liftShrink4))
import Test.Util.Show (showRangesOf)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Runtime.Support.ByteArray" [
      -- prop_setGet
      testProperty "prop_setGet @Word8"  $ prop_setGet (Proxy @Word8)
    , testProperty "prop_setGet @Word16" $ prop_setGet (Proxy @Word16)
    , testProperty "prop_setGet @Word32" $ prop_setGet (Proxy @Word32)
    , testProperty "prop_setGet @Word64" $ prop_setGet (Proxy @Word64)
    , testProperty "prop_setGet @Int"    $ prop_setGet (Proxy @Int)

      -- prop_getSet
    , testProperty "prop_getSet @Word8"  $ prop_getSet (Proxy @Word8)
    , testProperty "prop_getSet @Word16" $ prop_getSet (Proxy @Word16)
    , testProperty "prop_getSet @Word32" $ prop_getSet (Proxy @Word32)
    , testProperty "prop_getSet @Word64" $ prop_getSet (Proxy @Word64)
    , testProperty "prop_getSet @Int"    $ prop_getSet (Proxy @Int)

      -- prop_setGetBits
    , testProperty "prop_setGetBits @Word8"  $ prop_setGetBits (Proxy @Word8)
    , testProperty "prop_setGetBits @Word16" $ prop_setGetBits (Proxy @Word16)
    , testProperty "prop_setGetBits @Word32" $ prop_setGetBits (Proxy @Word32)
    , testProperty "prop_setGetBits @Word64" $ prop_setGetBits (Proxy @Word64)
    , testProperty "prop_setGetBits @Int"    $ prop_setGetBits (Proxy @Int)

      -- prop_getSetBits
    , testProperty "prop_getSetBits @Word8"  $ prop_getSetBits (Proxy @Word8)
    , testProperty "prop_getSetBits @Word16" $ prop_getSetBits (Proxy @Word16)
    , testProperty "prop_getSetBits @Word32" $ prop_getSetBits (Proxy @Word32)
    , testProperty "prop_getSetBits @Word64" $ prop_getSetBits (Proxy @Word64)
    , testProperty "prop_getSetBits @Int"    $ prop_getSetBits (Proxy @Int)
    ]

{-------------------------------------------------------------------------------
  Roundtrip: set and get
-------------------------------------------------------------------------------}

-- | Roundtrip: get after set should return the set value
prop_setGet ::
     forall a. (Storable a, Eq a, Show a)
  => Proxy a -> SetGetParams a -> Property
prop_setGet _ params =
    -- the byte array can be larger than the value we want to get/set so we
    -- tabulate the number of unused bytes in the byte array
    tabulate "# of unused bytes" [showRangesOf 25 (bytesSz - valueSz)] $
    params.value === getUnionPayload (setUnionPayload params.value params.bytes)
  where
    valueSz = sizeOf (undefined :: a)
    bytesSz = P.sizeofByteArray params.bytes

-- | Roundtrip: set after get should return the byte array to its original state
--
-- This test is interesting because it tests that the setter /only/ modifies
-- bytes within the range of the byte array that contains the @a@ value. It used
-- to be the case that setters would zero out all bytes outside of the byte
-- range of @a@. This was a bug. See issue #2183 for more background:
-- <https://github.com/well-typed/hs-bindgen/issues/2183>
prop_getSet ::
     forall a. Storable a
  => Proxy a -> SetGetParams a -> Property
prop_getSet _ params =
    -- the byte array can be larger than the value we want to get/set so we
    -- tabulate the number of unused bytes in the byte array
    tabulate "# of unused bytes" [showRangesOf 25 (bytesSz - valueSz)] $
    params.bytes === setUnionPayload @a (getUnionPayload params.bytes) params.bytes
  where
    valueSz = sizeOf (undefined :: a)
    bytesSz = P.sizeofByteArray params.bytes

-- | Parameters for 'prop_setGet' and 'prop_getSet'
--
-- INVARIANT: see the 'CheckInvariant' instance
data SetGetParams a = SetGetParams {
    value :: a
  , bytes :: ByteArray
  }
  deriving stock (Show, Eq)

instance (Show a, Storable a) => CheckInvariant (SetGetParams a) where
  checkInvariant params = first (printf "For params (%s), " (show params) ++) go
    where
      go
        | let vSz = sizeOf v
        , let bsSz = P.sizeofByteArray bs
        , vSz > bsSz
        = Left $ printf "A: %d > %d" vSz bsSz
        | otherwise
        = Right params

      v = params.value
      bs = params.bytes

mkSetGetParams ::
     forall a. (Storable a, Show a)
  => a -> ByteArray -> Either String (SetGetParams a)
mkSetGetParams value bytes = checkInvariant $ SetGetParams { value = value,bytes = bytes }

mkSetGetParams' ::
     forall a. (HasCallStack, Storable a, Show a)
  => a -> ByteArray -> SetGetParams a
mkSetGetParams' value bytes = checkInvariant' $ SetGetParams { value = value,bytes = bytes }

instance (Storable a, Arbitrary a, Show a) => Arbitrary (SetGetParams a) where
  arbitrary = do
      bytes <- genBytes
      value <- arbitrary
      pure $ mkSetGetParams' value bytes
   where
      valueSz = sizeOf (undefined :: a)

      genBytes = sized $ \n -> do
        k <- chooseInt (valueSz, max valueSz n)
        let genByte = getLarge <$> arbitrary
        byteArrayOf k genByte

  shrink params = snd $ partitionEithers [
        mkSetGetParams value' bytes'
      | (value', bytes') <- liftShrink2 shrink shrinkBytes (params.value, params.bytes)
      ]
    where
      valueSz = sizeOf (undefined :: a)

      shrinkBytes bytes = [
            bytes'
          | let shrinkByte = fmap getLarge . shrink . Large
          , bytes' <- shrinkByteArray shrinkByte bytes
          , valueSz <= P.sizeofByteArray bytes'
          ]

{-------------------------------------------------------------------------------
  Roundtrip: set bits and get bits
-------------------------------------------------------------------------------}

-- | Roundtrip: get bits after set bits should return the set value
prop_setGetBits ::
     forall a. Bitfield a
  => Proxy a -> SetGetBitsParams a -> Property
prop_setGetBits _ params =
    tabulate "# of unused leading bits" [showRangesOf 64 o] $
    tabulate "# of unused trailing bits" [showRangesOf 64 (bsSz * 8 - (o + w))] $
    narrow v w === getUnionPayloadBits o w (setUnionPayloadBits o w v bs)
  where
    o = params.bitOffset
    w = params.bitWidth
    v = params.value
    bs = params.bytes
    bsSz = P.sizeofByteArray bs

-- | Roundtrip: set after get should return the byte array to its original state
--
-- See als 'prop_getSet'.
prop_getSetBits ::
     forall a. Bitfield a
  => Proxy a -> SetGetBitsParams a -> Property
prop_getSetBits _ params =
    tabulate "# of unused leading bits" [showRangesOf 64 o] $
    tabulate "# of unused trailing bits" [showRangesOf 64 (bsSz * 8 - (o + w))] $
    bs === setUnionPayloadBits @a o w (getUnionPayloadBits o w bs) bs
  where
    o = params.bitOffset
    w = params.bitWidth
    bs = params.bytes
    bsSz = P.sizeofByteArray bs


-- | Parameters for 'prop_setGetBits' and 'prop_getSetBits'
--
-- INVARIANT: see the 'CheckInvariant' instance
data SetGetBitsParams a = SetGetBitsParams {
    value :: a
  , bitOffset :: Int
  , bitWidth :: Int
  , bytes :: ByteArray
  }
  deriving stock (Show, Eq)

instance (Show a, Storable a) => CheckInvariant (SetGetBitsParams a) where
  checkInvariant params = first (printf "For params (%s), " (show params) ++) go
    where
      go
        | o < 0
        = Left $ printf "A: %d < 0" o
        | w < 1 || w > 64
        = Left $ printf "B: %d < 1 || %d > 64" w w
        | let vSz = sizeOf v
        , w > vSz * 8
        = Left $ printf "C: %d + %d > %d * 8" o w vSz
        | let bsSz = P.sizeofByteArray bs
        , o + w > bsSz * 8
        = Left $ printf "D: %d + %d > %d * 8" o w bsSz
        | otherwise
        = Right params

      o = params.bitOffset
      w = params.bitWidth
      v = params.value
      bs = params.bytes

mkSetGetBitsParams ::
     forall a. (Storable a, Show a)
  => a -> Int -> Int -> ByteArray -> Either String (SetGetBitsParams a)
mkSetGetBitsParams value bitOffset bitWidth bytes =
    checkInvariant $
      SetGetBitsParams {
          value = value, bitOffset = bitOffset, bitWidth = bitWidth, bytes = bytes
        }

mkSetGetBitsParams' ::
     forall a. (HasCallStack, Storable a, Show a)
  => a -> Int -> Int -> ByteArray ->  SetGetBitsParams a
mkSetGetBitsParams' value bitOffset bitWidth bytes =
    checkInvariant'
      SetGetBitsParams {
          value = value, bitOffset = bitOffset, bitWidth = bitWidth, bytes = bytes
        }

instance (Storable a, Show a, Arbitrary a) => Arbitrary (SetGetBitsParams a) where
  arbitrary = do
      bitWidth <- chooseInt (1, valueSz * 8 - 1)
      let i = ceilDiv8 bitWidth
      bytes <- genBytes i
      bitOffset <- chooseInt (0, P.sizeofByteArray bytes * 8 - bitWidth)
      value <- arbitrary
      pure $ mkSetGetBitsParams' value bitOffset bitWidth bytes
   where
      valueSz = sizeOf (undefined :: a)

      genBytes i = sized $ \n -> do
        k <- chooseInt (i, max i n)
        let genByte = getLarge <$> arbitrary
        byteArrayOf k genByte

  shrink params = snd $ partitionEithers [
        mkSetGetBitsParams value' bitOffset' bitWidth' bytes'
      | (value', bitOffset', bitWidth', bytes')
          <- liftShrink4 shrink shrink shrink shrinkBytes
              (params.value, params.bitOffset, params.bitWidth, params.bytes)
      ]
    where
      shrinkBytes bytes = [
            bytes'
          | let shrinkByte = fmap getLarge . shrink . Large
          , bytes' <- shrinkByteArray shrinkByte bytes
          ]

{-------------------------------------------------------------------------------
  Invariants
-------------------------------------------------------------------------------}

class CheckInvariant a where
  -- | Check whether a value satisfies the type's invariant
  --
  -- @Left msg@ if the invariant is not satisified, @Right _@ otherwise.
  checkInvariant :: a -> Either String a

-- | Like 'checkInvariant', but throws an error if the invariant is not
-- satisfied
checkInvariant' :: (HasCallStack, CheckInvariant a) => a -> a
checkInvariant' x = case checkInvariant x of
    Left msg -> error $ msg
    Right y -> y

{-------------------------------------------------------------------------------
  Arbitrary byte arrays
-------------------------------------------------------------------------------}

byteArrayOf :: Int -> Gen Word8 -> Gen ByteArray
byteArrayOf n genByte = do
    bytes <- vectorOf n genByte
    pure $ fromList bytes

shrinkByteArray :: (Word8 -> [Word8]) -> ByteArray -> [ByteArray]
shrinkByteArray shrinkByte bytes = [
      fromList bytes'
    | bytes' <- shrinkList shrinkByte (toList bytes)
    ]

{-------------------------------------------------------------------------------
  Numeric
-------------------------------------------------------------------------------}

-- | Divide by 8 and round up
ceilDiv8 :: Int -> Int
ceilDiv8 x = (x + 7) `div` 8
