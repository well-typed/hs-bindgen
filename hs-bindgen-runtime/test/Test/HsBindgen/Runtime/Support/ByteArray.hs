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

import HsBindgen.Runtime.Support.ByteArray (getUnionPayload, setUnionPayload)

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
