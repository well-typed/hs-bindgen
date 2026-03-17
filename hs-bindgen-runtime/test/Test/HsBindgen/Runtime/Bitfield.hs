-- Record fields are just used to make QuickCheck failures easy to read.
{-# LANGUAGE DuplicateRecordFields #-}

module Test.HsBindgen.Runtime.Bitfield (tests) where

import Data.Bits (Bits (..), FiniteBits (..))
import Data.Int
import Data.Proxy
import Data.Word
import Foreign qualified
import Test.QuickCheck ((===))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import HsBindgen.Runtime.BitfieldPtr qualified as BitfieldPtr
import HsBindgen.Runtime.Internal.Bitfield (Bitfield)
import HsBindgen.Runtime.Internal.Bitfield qualified as Bitfield
import HsBindgen.Runtime.Marshal qualified as Marshal

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.Runtime.Internal.Bitfield" [
      testGroup "extend . narrow" [
          testProperty "Word8"  (unsigned_extend_narrow_prop @Word8)
        , testProperty "Word16" (unsigned_extend_narrow_prop @Word16)
        , testProperty "Word32" (unsigned_extend_narrow_prop @Word32)
        , testProperty "Word64" (unsigned_extend_narrow_prop @Word64)
        , testProperty "Int8"   (signed_extend_narrow_prop   @Int8)
        , testProperty "Int16"  (signed_extend_narrow_prop   @Int16)
        , testProperty "Int32"  (signed_extend_narrow_prop   @Int32)
        , testProperty "Int64"  (signed_extend_narrow_prop   @Int64)
        ]
    , testGroup "peek . poke" [
          testProperty "Struct1"  (peek_poke_prop @Struct1)
        , testProperty "Struct2"  (peek_poke_prop @Struct2)
        , testProperty "Struct3"  (peek_poke_prop @Struct3)
        , testProperty "Struct4"  (peek_poke_prop @Struct4)
        , testProperty "Struct5"  (peek_poke_prop @Struct5)
        , testProperty "Struct8"  (peek_poke_prop @Struct8)
        , testProperty "Struct11" (peek_poke_prop @Struct11)
        ]
    , testProperty
        "getBitfieldLE . putBitfieldLE"
        getBitfieldLE_putBitfieldLE_prop
    , testProperty
        "getBitfieldBE . putBitfieldBE"
        getBitfieldBE_putBitfieldBE_prop
    ]

{-------------------------------------------------------------------------------
   extend . narrow tests
-------------------------------------------------------------------------------}

-- | Unsigned value with a given bit-width
--
-- Constraints:
--
-- * @width@ is in range @[1, typeWidth]@
-- * Non-bit-field bits in @x@ are cleared
data UnsignedValue a = UnsignedValue {
      x     :: a
    , width :: Int
    }
  deriving stock Show

-- | Construct an 'UnsignedValue'
mkUnsignedValue :: (FiniteBits a, Num a) => a -> Int -> UnsignedValue a
mkUnsignedValue x' width =
    let x = x' .&. Bitfield.loMask width
    in  UnsignedValue x width

instance
       (Bounded a, FiniteBits a, Integral a, QC.Arbitrary a)
    => QC.Arbitrary (UnsignedValue a)
    where
  arbitrary = do
    let typeWidth = finiteBitSize @a 0
    width <- QC.chooseInt (1, typeWidth)
    x' <- QC.arbitrarySizedBoundedIntegral
    return $ mkUnsignedValue x' width

  -- Shrinks value, keeping width
  shrink (UnsignedValue x width) =
    map (`mkUnsignedValue` width) (QC.shrink x)

-- | Narrowing and then extending an unsigned value results in the same value
unsigned_extend_narrow_prop ::
     (Bitfield a, Eq a, Show a)
  => UnsignedValue a
  -> QC.Property
unsigned_extend_narrow_prop (UnsignedValue x width) =
    Bitfield.extend (Bitfield.narrow x width) width === x

-- | Signed value with a given bit-width
--
-- Constraints:
--
-- * @width@ is in range @[1, typeWidth]@
-- * Non-bit-field bits in @x@ are filled/cleared, depending on the sign
data SignedValue a = SignedValue {
      x     :: a
    , width :: Int
    }
  deriving stock Show

-- | Construct a 'SignedValue'
--
-- The most significant bit of the bit-field determines the sign.
mkSignedValue :: (FiniteBits a, Num a) => a -> Int -> SignedValue a
mkSignedValue x' width =
    let negative      = testBit x' (width - 1)
        x | negative  = x' .|. Bitfield.hiMask width
          | otherwise = x' .&. Bitfield.loMask width
    in  SignedValue x width

instance
       (Bounded a, FiniteBits a, Integral a, QC.Arbitrary a)
    => QC.Arbitrary (SignedValue a)
    where
  arbitrary = do
    let typeWidth = finiteBitSize @a 0
    width <- QC.chooseInt (1, typeWidth)
    x' <- QC.arbitrarySizedBoundedIntegral
    return $ mkSignedValue x' width

  -- Shrinks value, keeping width and sign
  shrink (SignedValue x width) =
    let negative                    = x < 0
        sameSign (SignedValue x' _) = (x' < 0) == negative
    in  filter sameSign $ map (`mkSignedValue` width) (QC.shrink x)

-- | Narrowing and then extending a signed value results in the same value
signed_extend_narrow_prop ::
     (Bitfield a, Num a, Ord a, Show a)
  => SignedValue a
  -> QC.Property
signed_extend_narrow_prop (SignedValue x width) =
    QC.label (if x < 0 then "negative" else "non-negative") $
      Bitfield.extend (Bitfield.narrow x width) width === x

{-------------------------------------------------------------------------------
  peek . poke tests
-------------------------------------------------------------------------------}

-- | Bit-field offset (bits) and width (bits) within @struct@ @a@
--
-- These parameters are mutually dependent on the size of the @struct@.
--
-- Constraints:
--
-- * @off@ is in range @[0, structWidth)@
-- * @width@ is in range @[1, min structWidth 64]@
-- * @off + width <= structWidth@
data BitfieldParams a = BitfieldParams {
      off   :: Int
    , width :: Int
    }
  deriving stock Show

instance Marshal.StaticSize a => QC.Arbitrary (BitfieldParams a) where
  arbitrary = do
    let structWidth = 8 * Marshal.staticSizeOf @a Proxy
    width <- QC.chooseInt (1, min structWidth 64)
    off   <- QC.chooseInt (0, structWidth - width)
    return $ BitfieldParams off width

  -- Shrinks offset then width one at a time to ensure minimal counterexample
  shrink (BitfieldParams off width) =
    let shrinkOffs
          | off == 0  = []
          | otherwise = [BitfieldParams o width | o <- [0 .. off - 1]]
        shrinkWidths
          | width == 1 = []
          | otherwise  = [BitfieldParams off w | w <- [1 .. width - 1]]
    in  shrinkOffs ++ shrinkWidths

-- | Poking and then peeking gets the same value (when narrowed)
peek_poke_prop :: forall a.
     Marshal.StaticSize a
  => BitfieldParams a  -- ^ Bitfield offset and width
  -> QC.Large Word64   -- ^ Test value
  -> QC.Property
peek_poke_prop (BitfieldParams off width) (QC.Large x) =
    QC.label label . QC.ioProperty $ do
      y <- Foreign.allocaBytesAligned @a size 8 $ \ptr -> do
        Foreign.fillBytes ptr 0xAA size
        let bitfieldPtr = BitfieldPtr.mkBitfieldPtr ptr off width
        BitfieldPtr.poke bitfieldPtr x
        BitfieldPtr.peek bitfieldPtr
      return $ y === Bitfield.narrow x width
  where
    size, structWidth :: Int
    size        = Marshal.staticSizeOf @a Proxy
    structWidth = 8 * size

    -- This is a simplification that assumes the following alignment:
    --
    -- > Marshal.staticAlignment @Word8  Proxy == 1
    -- > Marshal.staticAlignment @Word16 Proxy == 2
    -- > Marshal.staticAlignment @Word32 Proxy == 4
    -- > Marshal.staticAlignment @Word64 Proxy == 8
    --
    -- This is only possible because the test ensures that the @struct@ pointer
    -- is 64-bit aligned.
    --
    -- Labels may be inaccurate if run on an architecture where the alignment
    -- differs, but the actual implementation does /not/ make such assumptions.
    label :: String
    label
      | rem8  + width <=  8                                    = "aligned:1"
      | rem16 + width <= 16 && 16 * quot16 + 16 <= structWidth = "aligned:2"
      | rem32 + width <= 32 && 32 * quot32 + 32 <= structWidth = "aligned:4"
      | rem64 + width <= 64 && 64 * quot64 + 64 <= structWidth = "aligned:8"
      | otherwise = "bytes:" ++ show minBytes

    rem8, quot16, rem16, quot32, rem32, quot64, rem64 :: Int
    rem8            = rem     off  8
    (quot16, rem16) = quotRem off 16
    (quot32, rem32) = quotRem off 32
    (quot64, rem64) = quotRem off 64

    minBytes :: Int
    minBytes = case quotRem (rem8 + width) 8 of
      (bytes, 0) -> bytes
      (bytes, _) -> bytes + 1

{-------------------------------------------------------------------------------
  getBitfield?E . putBitfield?E tests
-------------------------------------------------------------------------------}

-- | Bit-field value, normalized offset (bits), and width (bits)
--
-- Constraints:
--
-- * @off@ is in range @[0, 7)@
-- * @width@ is in range @[1, 64]@
-- * Non-bit-field bits in @x@ are cleared
data BitfieldEndianParams = BitfieldEndianParams {
      x     :: Word64
    , off   :: Int
    , width :: Int
    }
  deriving stock Show

-- | Construct a 'BitfieldEndianParams'
mkBitfieldEndianParams :: Word64 -> Int -> Int -> BitfieldEndianParams
mkBitfieldEndianParams x' off width =
    let x = x' .&. Bitfield.loMask width
    in  BitfieldEndianParams x off width

instance QC.Arbitrary BitfieldEndianParams where
  arbitrary = do
    off   <- QC.chooseInt (0,  7)
    width <- QC.chooseInt (1, 64)
    x'    <- QC.arbitrarySizedBoundedIntegral
    return $ mkBitfieldEndianParams x' off width

  -- Shrinks value, then offset then width one at a time to ensure minimal
  -- counterexample
  shrink (BitfieldEndianParams x off width) =
    let shrinkValues
          | x == 0    = []
          | otherwise =
              map (\x' -> mkBitfieldEndianParams x' off width) (QC.shrink x)
        shrinkOffs
          | off == 0  = []
          | otherwise =
              map (\o -> mkBitfieldEndianParams x o width) [0 .. off - 1]
        shrinkWidths
          | width == 1 = []
          | otherwise  =
              map (\w -> mkBitfieldEndianParams x off w) [1 .. width - 1]
    in  shrinkValues ++ shrinkOffs ++ shrinkWidths

-- | Encoding a bit-field as bytes and then decoding the bytes results in the
-- same value (little endian)
getBitfieldLE_putBitfieldLE_prop :: BitfieldEndianParams -> QC.Property
getBitfieldLE_putBitfieldLE_prop (BitfieldEndianParams x off width) =
    let bytes = Bitfield.putBitfieldLE off width 0xAA 0xAA x
        eey   = Bitfield.getBitfieldLE off width bytes
    in  eey === Right x

-- | Encoding a bit-field as bytes and then decoding the bytes results in the
-- same value (big endian)
getBitfieldBE_putBitfieldBE_prop :: BitfieldEndianParams -> QC.Property
getBitfieldBE_putBitfieldBE_prop (BitfieldEndianParams x off width) =
    let bytes = Bitfield.putBitfieldBE off width 0xAA 0xAA x
        eey   = Bitfield.getBitfieldBE off width bytes
    in  eey === Right x

{-------------------------------------------------------------------------------
  Auxiliary types

  These types are needed in order to use the "HsBindgen.Runtime.BitfieldPtr"
  API.  They are just used to specify the size of a @struct@ that contains a
  bit-field.
-------------------------------------------------------------------------------}

type Struct1 = Word8

type Struct2 = Word16

data Struct3

instance Marshal.StaticSize Struct3 where
  staticSizeOf    _ = 3
  staticAlignment _ = 1

type Struct4 = Word32

data Struct5

instance Marshal.StaticSize Struct5 where
  staticSizeOf    _ = 5
  staticAlignment _ = 1

type Struct8 = Word64

data Struct11

instance Marshal.StaticSize Struct11 where
  staticSizeOf    _ = 11
  staticAlignment _ = 1
