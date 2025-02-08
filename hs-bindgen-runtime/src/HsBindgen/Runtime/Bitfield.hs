module HsBindgen.Runtime.Bitfield (
    Bitfield (..),
    defaultNarrow,
    signedExtend,
    unsignedExtend,
    loMask,
    hiMask,
    peekBitOffWidth,
    pokeBitOffWidth,
) where

-- $setup
-- >>> import Data.Word
-- >>> import Numeric
-- >>> import Foreign.C.Types

import Data.Bits
import Data.Word (Word64, Word32, Word16, Word8)
import Foreign (Ptr, peekByteOff, pokeByteOff)
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
instance Bitfield Word64 where extend = unsignedExtend

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

-------------------------------------------------------------------------------
-- Storable
-------------------------------------------------------------------------------

-- | Peek a "bitfield" from a memory; we assume that offset and width are such
-- that we can read&write whole bitfield with one (read or write) operation.
-- Which means that bitfields cannot span multiple Word64s etc: at the extreme: if width is 64, the offset can only be multiple of 64.
--
-- Note: the implementation may do unaligned reads.
peekBitOffWidth :: Bitfield a => Ptr b -> Int -> Int -> IO a
peekBitOffWidth ptr off width
    | (0x7 .&. off) + width <= 8 = do
        w <- peekByteOff ptr (complement 0x7 .&. off) :: IO Word8
        return $! extend (fromIntegral (unsafeShiftR w (0x7 .&. off) .&. loMask width)) width

    | (0xf .&. off) + width <= 16 = do
        w <- peekByteOff ptr (complement 0xf .&. off) :: IO Word16
        return $! extend (fromIntegral (unsafeShiftR w (0xf .&. off) .&. loMask width)) width

    | (0x1f .&. off) + width <= 32 = do
        w <- peekByteOff ptr (complement 0x1f .&. off) :: IO Word32
        return $! extend (fromIntegral (unsafeShiftR w (0x1f .&. off) .&. loMask width)) width

    | (0x3f .&. off) + width <= 64 = do
        w <- peekByteOff ptr (complement 0x3f .&. off) :: IO Word64
        return $! extend (fromIntegral (unsafeShiftR w (0x3f .&. off) .&. loMask width)) width

    | otherwise = fail $ "peekBitOffWidth _ " ++ show off ++ " " ++ show ptr ++ ": too wide"

-- As far as I can tell there are no more clever bit fiddling way to do this:
-- See e.g. https://godbolt.org/z/cfcdc1eKd
-- for @ptr->y = y@; we get
--
-- @
-- mov     ecx, dword ptr [rbp - 12]
-- mov     rax, qword ptr [rbp - 8]
-- mov     dl, cl
-- mov     cl, byte ptr [rax]
-- and     dl, 7
-- shl     dl, 2
-- and     cl, -29
-- or      cl, dl
-- mov     byte ptr [rax], cl
-- @
--
-- 7 is mask 0b111 for three bits (unsigned int y : 3) (it's used by 'narrow')
-- -29 is another mask @1111111111100011@, which is @complement (unsafeShiftL (loMask width) ...)@ in the pokeBitOffWidth
--
pokeBitOffWidth :: Bitfield a => Ptr b -> Int -> Int -> a -> IO ()
pokeBitOffWidth ptr off width x
    | (0x7 .&. off) + width <= 8 = do
        w <- peekByteOff ptr (complement 0x7 .&. off) :: IO Word8
        let x' = fromIntegral (narrow x width) :: Word8
        let mask = unsafeShiftL (loMask width) (0x7 .&. off)
            x''  = unsafeShiftL x'             (0x7 .&. off)

        pokeByteOff ptr (complement 0x7 .&. off) ((w .&. complement mask) .|. x'')

    | (0xf .&. off) + width <= 16 = do
        w <- peekByteOff ptr (complement 0xf .&. off) :: IO Word16
        let x' = fromIntegral (narrow x width) :: Word16
        let mask = unsafeShiftL (loMask width) (0xf .&. off)
            x''  = unsafeShiftL x'             (0xf .&. off)

        pokeByteOff ptr (complement 0xf .&. off) ((w .&. complement mask) .|. x'')

    | (0x1f .&. off) + width <= 32 = do
        w <- peekByteOff ptr (complement 0x1f .&. off) :: IO Word32
        let x' = fromIntegral (narrow x width) :: Word32
        let mask = unsafeShiftL (loMask width) (0x1f .&. off)
            x''  = unsafeShiftL x'             (0x1f .&. off)

        pokeByteOff ptr (complement 0x1f .&. off) ((w .&. complement mask) .|. x'')

    | (0x3f .&. off) + width <= 64 = do
        w <- peekByteOff ptr (complement 0x3f .&. off) :: IO Word64
        let x' = fromIntegral (narrow x width) :: Word64
        let mask = unsafeShiftL (loMask width) (0x3f .&. off)
            x''  = unsafeShiftL x'             (0x3f .&. off)

        pokeByteOff ptr (complement 0x3f .&. off) ((w .&. complement mask) .|. x'')

    | otherwise = fail $ "pokeBitOffWidth _ " ++ show off ++ " " ++ show ptr ++ " _: too wide"
