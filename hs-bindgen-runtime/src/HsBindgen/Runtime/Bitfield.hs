module HsBindgen.Runtime.Bitfield (
    Bitfield (..),
    defaultNarrow,
    signedExtend,
    unsignedExtend,
    loMask,
    hiMask,
) where

-- $setup
-- >>> import Data.Word
-- >>> import Numeric
-- >>> import Foreign.C.Types

import Data.Bits
import Data.Word (Word64)
import Foreign.C.Types

-- | Class for types which can be a bitfield in C struct.
--
-- The members converts to/from Word64 to make the use in the implementation of bitfields in @hs-bindgen@ easier.
-- We could not convert or have a smallest @WordN@ which would fit the type, but it complicates the usage for little benefit.
--
-- >>> let width = 5 :: Int
-- >>> extend (narrow (3 :: CSChar) width) width :: CSChar
-- 3
--
-- >>> let width = 5 :: Int
-- >>> extend (narrow (-7 :: CSChar) width) width :: CSChar
-- -7
--
-- overflow case, the result is undefined behavior:
--
-- >>> let width = 5 :: Int
-- >>> extend (narrow (-100 :: CSChar) width) width :: CSChar
-- -4
--
-- Development note: we could get away without a type-class using 'defaultNarrow', 'signedExtend' or 'unsignedExtend' directly.
-- However, we would need to encode the signedness of a target type somewhere anyway.
--
class Bitfield a where
    -- | Narrow the value so that only @n@ lowest bits are set.
    narrow :: a -> Int -> Word64
    default narrow :: Integral a => a -> Int -> Word64
    narrow = defaultNarrow

    -- | Extend the value from only @n@ lowest bits representation.
    -- For example this may extend the sign for signed types.
    extend :: Word64 -> Int -> a

instance Bitfield CUChar where extend = unsignedExtend
instance Bitfield CUInt where extend = unsignedExtend
instance Bitfield CULong where extend = unsignedExtend

instance Bitfield CSChar where extend = signedExtend
instance Bitfield CInt where extend = signedExtend
instance Bitfield CLong where extend = signedExtend

-- | Default 'narrow' implementation. Takes the lowest @n@ bits.
defaultNarrow :: Integral a => a -> Int -> Word64
defaultNarrow x n = fromIntegral x .&. loMask n

-- | Unsigned extend. Does nothing.
unsignedExtend :: Num a => Word64 -> Int -> a
unsignedExtend x _n = fromIntegral x

-- | Signed extend, performs [sign extension](https://en.wikipedia.org/wiki/Sign_extension).
--
-- @n > 0@ (zero width bitfields are not allowed)
--
signedExtend :: Num a => Word64 -> Int -> a
signedExtend x n
    | x .&. b == 0 = fromIntegral x -- no sign, just convert
    | otherwise    = fromIntegral $ x .|. complement (unsafeShiftL b 1 - 1) -- extend the sign
  where
    b = unsafeShiftL 1 (n - 1)

-- | Generate a low mask
--
-- >>> map (flip showBin "" . loMask @Word16) [0, 1, 2, 5, 8, 16]
-- ["0","1","11","11111","11111111","1111111111111111"]
--
loMask :: (Num a, Bits a) => Int -> a
loMask n = unsafeShiftL 1 n - 1

-- | Generate a high mask
--
-- >>> map (flip showBin "" . hiMask @Word16) [0, 1, 2, 5, 8, 16]
-- ["1111111111111111","1111111111111110","1111111111111100","1111111111100000","1111111100000000","0"]
--
hiMask :: (Num a, Bits a) => Int -> a
hiMask n = complement (loMask n)
