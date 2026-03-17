{-# OPTIONS_HADDOCK hide #-}

module HsBindgen.Runtime.Internal.Bitfield (
    -- * Bitfield
    Bitfield(..)
  , defaultNarrow
  , signedExtend
  , unsignedExtend
  , loMask
  , hiMask
    -- * Storable
  , peekBitOffWidth
  , pokeBitOffWidth
    -- * Auxiliary functions (exported for testing)
  , getBitfield
  , getBitfieldLE
  , getBitfieldBE
  , putBitfield
  , putBitfieldLE
  , putBitfieldBE
) where

-- $setup
-- >>> import Data.Word
-- >>> import Numeric
-- >>> import Foreign.C.Types

import Data.Bits
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

import HsBindgen.Runtime.Marshal qualified as Marshal

{-------------------------------------------------------------------------------
  Bitfield
-------------------------------------------------------------------------------}

-- | Types which can be a bit-field in a C @struct@
--
-- The members convert to/from 'Word64' to make the use in the implementation of
-- bitfields in @hs-bindgen@ easier.  We could not convert or have a smallest
-- @WordN@ that best fits the type, but doing so would complicate usage for
-- little benefit.
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
class Bitfield a where
    -- | Narrow the value so that only @width@ lowest bits are set
    narrow :: a -> Int -> Word64
    default narrow :: Integral a => a -> Int -> Word64
    narrow = defaultNarrow

    -- | Extend the value from only @width@ lowest bits representation
    --
    -- This should extend the sign for signed types.
    extend :: Word64 -> Int -> a

-- NOTE We could get away without a type class, using 'defaultNarrow',
-- 'signedExtend', and 'unsignedExtend' directly, but we would still need to
-- encode the signedness of a target type somewhere anyway.

-- Instances for "Data.Int" signed integral types
instance Bitfield Int   where extend = signedExtend
instance Bitfield Int8  where extend = signedExtend
instance Bitfield Int16 where extend = signedExtend
instance Bitfield Int32 where extend = signedExtend
instance Bitfield Int64 where extend = signedExtend

-- Instances for "Data.Word" unsigned integral types
instance Bitfield Word   where extend = unsignedExtend
instance Bitfield Word8  where extend = unsignedExtend
instance Bitfield Word16 where extend = unsignedExtend
instance Bitfield Word32 where extend = unsignedExtend
instance Bitfield Word64 where extend = unsignedExtend

-- Instances for "Foreign.C.Types" signed integral types
instance Bitfield CChar      where extend = signedExtend
instance Bitfield CSChar     where extend = signedExtend
instance Bitfield CShort     where extend = signedExtend
instance Bitfield CInt       where extend = signedExtend
instance Bitfield CLong      where extend = signedExtend
instance Bitfield CPtrdiff   where extend = signedExtend
instance Bitfield CWchar     where extend = signedExtend
instance Bitfield CSigAtomic where extend = signedExtend
instance Bitfield CLLong     where extend = signedExtend
instance Bitfield CIntPtr    where extend = signedExtend
instance Bitfield CIntMax    where extend = signedExtend

-- Instances for "Foreign.C.Types" unsigned integral types
instance Bitfield CUChar   where extend = unsignedExtend
instance Bitfield CUShort  where extend = unsignedExtend
instance Bitfield CUInt    where extend = unsignedExtend
instance Bitfield CULong   where extend = unsignedExtend
instance Bitfield CSize    where extend = unsignedExtend
instance Bitfield CULLong  where extend = unsignedExtend
instance Bitfield CBool    where extend = unsignedExtend
instance Bitfield CUIntPtr where extend = unsignedExtend
instance Bitfield CUIntMax where extend = unsignedExtend

-- | Default 'narrow' implementation
--
-- This function takes the lowest @width@ bits.
defaultNarrow ::
     Integral a
  => a    -- ^ Value of the bit-field
  -> Int  -- ^ Width of the bit-field (1 to 64 bits)
  -> Word64
defaultNarrow x width = fromIntegral x .&. loMask width

-- | Unsigned extend, just converts types
unsignedExtend ::
     Num a
  => Word64  -- ^ Value of the bit-field
  -> Int     -- ^ Width of the bit-field (1 to 64 bits)
  -> a
unsignedExtend x _width = fromIntegral x

-- | Signed extend, performs
-- [sign extension](https://en.wikipedia.org/wiki/Sign_extension)
signedExtend ::
     Num a
  => Word64  -- ^ Value of the bit-field
  -> Int     -- ^ Width of the bit-field (1 to 64 bits)
  -> a
signedExtend x width
    -- Negative: extend the sign
    | testBit x (width - 1) = fromIntegral $ hiMask width .|. x
    -- Non-negative: just convert
    | otherwise             = fromIntegral x

-- | Generate a low mask
--
-- The argument essentially specifies the number of least significant bits that
-- are one.  It should be in range @[0 .. typeWidth]@, where @typeWidth@ is the
-- width of the type.
--
-- >>> map (flip showBin "" . loMask @Word16) [0, 1, 2, 5, 8, 16]
-- ["0","1","11","11111","11111111","1111111111111111"]
loMask :: forall a. (FiniteBits a, Num a) => Int -> a
loMask n
    | n >= 0 && n < finiteBitSize @a 0 = unsafeShiftL 1 n - 1
    -- For other values of @n@, 'unsafeShiftL' has undefined behavior.  Shifting
    -- an 'Int64' by 64 bits is particularly problematic.
    | otherwise                        = complement 0

-- | Generate a high mask
--
-- The argument essentially specifies the number of least significant bits that
-- are zero.  It should be in range @[0 .. typeWidth]@, where @typeWidth@ is
-- the width of the type.
--
-- >>> map (flip showBin "" . hiMask @Word16) [0, 1, 2, 5, 8, 16]
-- ["1111111111111111","1111111111111110","1111111111111100","1111111111100000","1111111100000000","0"]
hiMask :: (FiniteBits a, Num a) => Int -> a
hiMask = complement . loMask

{------------------------------------------------------------------------------
  Storable
------------------------------------------------------------------------------}

-- | Read a bit-field from memory
--
-- This function only uses aligned reads, so that it is safe to use on any
-- architecture.
--
-- A bit-field is read using a single peek when possible, as determined by
-- alignment and the passed memory bounds.  This should always be the case with
-- normal @struct@s.
--
-- When it is not possible to read a bit-field using a single peek, multiple
-- peeks are used to read the bytes of the bit-field.  This may happen with
-- (poorly-designed) packed @struct@s.
--
-- The memory bounds should generally be the bounds of the @struct@ object.  In
-- this case, concurrent access to different objects (in an array) is safe.
-- Concurrent access to bit-fields within a single @struct@ object is generally
-- unsafe.
peekBitOffWidth :: forall a.
     Bitfield a
  => Ptr ()            -- ^ Pointer to the byte where the bit-field starts
  -> Int               -- ^ Offset of the bit-field (0 to 7 bits)
  -> Int               -- ^ Width of the bit-field (1 to 64 bits)
  -> (Ptr (), Ptr ())  -- ^ Memory bounds, must contain the bit-field
  -> IO a
peekBitOffWidth ptrB off width bounds@(ptrL, ptrH)
    | off   < 0    || off   >  7   = fail' "invalid offset"
    | width < 1    || width > 64   = fail' "invalid width"
    | ptrB  < ptrL || ptrBH > ptrH = fail' "out of bounds"
    | Just (ptr8,  _, r) <- getAligned @Word8  (ptrB, ptrBH) off width bounds =
        readAligned ptr8  r
    | Just (ptr16, _, r) <- getAligned @Word16 (ptrB, ptrBH) off width bounds =
        readAligned ptr16 r
    | Just (ptr32, _, r) <- getAligned @Word32 (ptrB, ptrBH) off width bounds =
        readAligned ptr32 r
    | Just (ptr64, _, r) <- getAligned @Word64 (ptrB, ptrBH) off width bounds =
        readAligned ptr64 r
    | otherwise = readBytes
  where
    -- Minimum number of bytes that must be read
    minBytes :: Int
    minBytes = case (off + width) `quotRem` 8 of
      (bytes, 0) -> bytes
      (bytes, _) -> bytes + 1

    -- High bound of bytes that must be read
    ptrBH :: Ptr ()
    ptrBH = ptrB `plusPtr` minBytes

    -- Read the bit-field using a single, aligned word
    readAligned ::
         (FiniteBits w, Integral w, Marshal.ReadRaw w)
      => Ptr w  -- ^ Pointer to aligned word
      -> Int    -- ^ Right offset (bits)
      -> IO a
    readAligned ptr roff = do
      w <- Marshal.readRaw ptr
      let w' = unsafeShiftR w roff .&. loMask width
      return $! extend (fromIntegral w') width

    -- Read the bit-field using bytes
    readBytes :: IO a
    readBytes = do
      w8s <- peekArray minBytes (castPtr ptrB)
      case getBitfield off width w8s of
        Right w64 -> return $! extend w64 width
        Left  msg -> fail' msg

    fail' :: String -> IO a
    fail' msg = fail $ concat [
        "peekBitOffWidth "
      , show ptrB,  " "
      , show off,   " "
      , show width, " ("
      , show ptrL,  ", "
      , show ptrH,  "): "
      , msg
      ]

-- | Write a bit-field to memory
--
-- This function only uses aligned reads and writes, so that it is safe to use
-- on any architecture.
--
-- When a bit-field can be written using a single poke, as determined by
-- alignment and the passed memory bounds, a single peek reads any extra bits
-- when necessary, and then a single poke writes the bit-field.  This should
-- always be the case with normal @struct@s.
--
-- When it is not possible to write a bit-field using a single poke, the byte(s)
-- containing any extra bits are read when necessary, and then multiple pokes
-- are used to write the bytes of the bit-field.  This may happen with
-- (poorly-designed) packed @struct@s.
--
-- The memory bounds should generally be the bounds of the @struct@ object.  In
-- this case, concurrent access to different objects (in an array) is safe.
-- Concurrent access to bit-fields within a single @struct@ object is generally
-- unsafe.
pokeBitOffWidth :: forall a.
     Bitfield a
  => Ptr ()            -- ^ Pointer to the byte where the bit-field starts
  -> Int               -- ^ Offset of the bit-field (0 to 7 bits)
  -> Int               -- ^ Width of the bit-field (1 to 64 bits)
  -> (Ptr (), Ptr ())  -- ^ Memory bounds, must contain the bit-field
  -> a                 -- ^ Bit-field value
  -> IO ()
pokeBitOffWidth ptrB off width bounds@(ptrL, ptrH) x
    | off   < 0    || off   >  7   = fail' "invalid offset"
    | width < 1    || width > 64   = fail' "invalid width"
    | ptrB  < ptrL || ptrBH > ptrH = fail' "out of bounds"
    | Just (ptr8,  l, r) <- getAligned @Word8  (ptrB, ptrBH) off width bounds =
        writeAligned ptr8  l r
    | Just (ptr16, l, r) <- getAligned @Word16 (ptrB, ptrBH) off width bounds =
        writeAligned ptr16 l r
    | Just (ptr32, l, r) <- getAligned @Word32 (ptrB, ptrBH) off width bounds =
        writeAligned ptr32 l r
    | Just (ptr64, l, r) <- getAligned @Word64 (ptrB, ptrBH) off width bounds =
        writeAligned ptr64 l r
    | otherwise = writeBytes
  where
    -- Minimum number of bytes that must be written
    minBytes :: Int
    minBytes = case (off + width) `quotRem` 8 of
      (bytes, 0) -> bytes
      (bytes, _) -> bytes + 1

    -- High bound of bytes that must be written
    ptrBH :: Ptr ()
    ptrBH = ptrB `plusPtr` minBytes

    -- Write the bit-field using a single, aligned word
    writeAligned ::
         (FiniteBits w, Integral w, Marshal.ReadRaw w, Marshal.WriteRaw w)
      => Ptr w  -- ^ Pointer to aligned word
      -> Int    -- ^ Left offset (bits)
      -> Int    -- ^ Right offset (bits)
      -> IO ()
    writeAligned ptr loff roff
      | loff == 0 && roff == 0 =
          Marshal.writeRaw ptr (fromIntegral (narrow x width))
      | otherwise = do
          w <- Marshal.readRaw ptr
          let x'   = unsafeShiftL (fromIntegral (narrow x width)) roff
              mask = unsafeShiftL (loMask width) roff
          Marshal.writeRaw ptr ((w .&. complement mask) .|. x')

    -- Write the bit-field using bytes
    writeBytes :: IO ()
    writeBytes = do
      let ptrL8 = castPtr ptrB
          ptrH8 = castPtr (ptrBH `plusPtr` (-1))
      l8 <-
        if off == 0
          then return 0x00
          else Marshal.readRaw ptrL8
      h8 <-
        if 8 * minBytes - width - off == 0
          then return 0x00
          else if ptrH8 == ptrL8 then return l8 else Marshal.readRaw ptrH8
      pokeArray ptrL8 $ putBitfield off width l8 h8 (narrow x width)

    fail' :: String -> IO ()
    fail' msg = fail $ concat [
        "pokeBitOffWidth "
      , show ptrB,  " "
      , show off,   " "
      , show width, " ("
      , show ptrL,  ", "
      , show ptrH,  ") x: "
      , msg
      ]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Is the system little endian?
--
-- This value is computed (once) by allocating two bytes of memory, writing a
-- @1 :: 'Word16'@, and checking the value of the first byte.  With a little
-- endian architecture, we get @01 00@ byte order, and the first byte is 1.
-- With a big endian architecture, we get a @00 01@ byte order, and the first
-- byte is 0.
--
-- Reference: <https://en.wikipedia.org/wiki/Endianness>
isLittleEndian :: Bool
isLittleEndian = unsafePerformIO $ alloca $ \ptr -> do
    poke @Word16 ptr 1
    (== 1) <$> peekByteOff @Word8 ptr 0
{-# NOINLINE isLittleEndian #-}

-- | Get a pointer to an aligned word that spans the whole bit-field, when
-- possible
--
-- This function also returns the number of bits to the left and right of the
-- bit-field within the aligned word.
getAligned :: forall w.
     Marshal.StaticSize w
  => (Ptr (), Ptr ())  -- ^ Minimal bit-field bounds
  -> Int               -- ^ Offset of the bit-field (0 to 7 bits)
  -> Int               -- ^ Width of the bit-field (1 to 64 bits)
  -> (Ptr (), Ptr ())  -- ^ Memory bounds, must contain the bit-field
  -> Maybe (Ptr w, Int, Int)
getAligned (ptrB, ptrBH) off width (ptrL, ptrH)
    | ptrA  < ptrL  = Nothing  -- Lower bound of the word is out of bounds
    | ptrAH < ptrBH = Nothing  -- Whole bit-field is not within the word
    | ptrAH > ptrH  = Nothing  -- Higher bound of the word is out of bounds
    | otherwise     = Just (castPtr ptrA, lBits, rBits)
  where
    wSize, wAlign :: Int
    wSize  = Marshal.staticSizeOf    @w Proxy
    wAlign = Marshal.staticAlignment @w Proxy

    -- Aligned word bounds, where @ptrA@ is a pointer to the highest aligned
    -- word of the specified size such that @ptrA <= ptrB@
    ptrA, ptrAH :: Ptr ()
    (ptrA, ptrAH) = case alignPtr ptrB wAlign of
      ptr
        | ptr == ptrB -> (ptr, ptr `plusPtr` wSize)
        | otherwise   -> (ptr `plusPtr` negate wSize, ptr)

    -- Number of bits to the left and right of the bit-field within the aligned
    -- word
    --
    -- Example @struct@:
    --
    -- * a:10 1000000011
    -- * b:24 000000000000000000000000
    -- * c:24 110000000000000000000111
    lBits, rBits :: Int
    (lBits, rBits)
      -- Little endian: bit-field offsets are from the least significant bit
      --
      -- Field @b@ has an offset of 2, shown as @xx@ in this memory diagram:
      --
      --          bbbbbbxx bbbbbbbb bbbbbbbb       bb
      -- 00000011 00000010 00000000 00000000 00011100 00000000 00000000 00000011
      -- |        |                                   |
      -- ptrA     ptrB                                ptrBH
      --
      -- Left bits are shown as @l@s and right bits are shown as @r@s in this
      -- 'Word64' diagram:
      --
      -- llllllll llllllll llllllll llllllbb bbbbbbbb bbbbbbbb bbbbbbrr rrrrrrrr
      -- 00000011 00000000 00000000 00011100 00000000 00000000 00000010 00000011
      | isLittleEndian =
          let r = off + 8 * (ptrB `minusPtr` ptrA)
          in  (wSize - width - r, r)

      -- Big endian: bit-field offsets are from the most significant bit
      --
      -- Field @b@ has an offset of 2, shown as @xx@ in this memory diagram:
      --
      --          xxbbbbbb bbbbbbbb bbbbbbbb bb
      -- 10000000 11000000 00000000 00000000 00110000 00000000 00000001 11000000
      -- |        |                                   |
      -- ptrA     ptrB                                ptrBH
      --
      -- Left bits are shown as @l@s and right bits are shown as @r@s in this
      -- 'Word64' diagram:
      --
      -- llllllll llbbbbbb bbbbbbbb bbbbbbbb bbrrrrrr rrrrrrrr rrrrrrrr rrrrrrrr
      -- 10000000 11000000 00000000 00000000 00110000 00000000 00000001 11000000
      | otherwise =
          let l = 8 * (ptrB `minusPtr` ptrA) + off
          in  (l, wSize - width - l)

-- | Get a bit-field value from a list of bytes (native byte order)
getBitfield ::
     Int      -- ^ Offset of the bit-field (0 to 7 bits)
  -> Int      -- ^ Width of the bit-field (1 to 64 bits)
  -> [Word8]  -- ^ Bytes as read from memory
  -> Either String Word64
getBitfield
    | isLittleEndian = getBitfieldLE
    | otherwise      = getBitfieldBE

-- | Get a bit-field value from a list of bytes (little endian)
getBitfieldLE ::
     Int      -- ^ Offset of the bit-field (0 to 7 bits)
  -> Int      -- ^ Width of the bit-field (1 to 64 bits)
  -> [Word8]  -- ^ Bytes as read from memory
  -> Either String Word64
getBitfieldLE off width = auxFirst
  where
    -- With little endian, @struct@s are stored in reverse byte order, and
    -- bit-field offsets are from the least significant bit.
    --
    -- * Single byte: @| loff? | numFieldBits | off? |@
    --
    -- * First byte:  @| numFieldBits | off? |@
    -- * Middle byte: @| numFieldBits = 8 |@
    -- * Last byte:   @| loff? | numFieldBits |@

    auxFirst ::
         [Word8]  -- ^ Bytes as read from memory
      -> Either String Word64
    auxFirst = \case
      -- First byte or single byte
      (b:bs) ->
        let numFieldBits = min width (8 - off)
            fieldByte    = unsafeShiftR b off .&. loMask numFieldBits
            fieldWord    = fromIntegral fieldByte
        in  auxNext fieldWord numFieldBits (width - numFieldBits) bs
      [] -> Left "not enough bytes"

    auxNext ::
         Word64   -- ^ Bit-field value accumulator
      -> Int      -- ^ Number of bit-field bits in the accumulator
      -> Int      -- ^ Remaining number of bits in the bit-field
      -> [Word8]  -- ^ Remaining bytes as read from memory
      -> Either String Word64
    auxNext !acc numAccBits width' = \case
      (b:bs)
        -- Middle byte
        | width' >= 8 ->
            let fieldWord = unsafeShiftL (fromIntegral b) numAccBits
            in  auxNext (fieldWord .|. acc) (numAccBits + 8) (width' - 8) bs
        -- Last byte
        | width' > 0 ->
            let fieldByte = b .&. loMask width'
                fieldWord = unsafeShiftL (fromIntegral fieldByte) numAccBits
            in  auxNext (fieldWord .|. acc) (numAccBits + width') 0 bs
        | otherwise -> Left "too many bytes"
      []
        | width' == 0 -> Right acc
        | otherwise   -> Left "not enough bytes"

-- | Get a bit-field value from a list of bytes (big endian)
getBitfieldBE ::
     Int      -- ^ Offset of the bit-field (0 to 7 bits)
  -> Int      -- ^ Width of the bit-field (1 to 64 bits)
  -> [Word8]  -- ^ Bytes as read from memory
  -> Either String Word64
getBitfieldBE off width = auxFirst
  where
    -- With big endian, @struct@s are stored in byte order, and bit-field
    -- offsets are from the most significant bit.
    --
    -- * Single byte: @| off? | numFieldBits | roff? |@
    --
    -- * First byte:  @| off? | numFieldBits |@
    -- * Middle byte: @| numFieldBits = 8 |@
    -- * Last byte:   @| numFieldBits | roff? |@

    off8 :: Int
    off8 = 8 - off

    auxFirst ::
         [Word8]  -- ^ Bytes as read from memory
      -> Either String Word64
    auxFirst = \case
      (b:bs)
        -- First byte
        | width >= off8 ->
            let width'    = width - off8
                fieldByte = b .&. loMask off8
                fieldWord = unsafeShiftL (fromIntegral fieldByte) width'
            in  auxNext fieldWord width' bs
        -- Single byte
        | otherwise ->
            let fieldByte = unsafeShiftR (b .&. loMask off8) (off8 - width)
                fieldWord = fromIntegral fieldByte
            in  auxNext fieldWord 0 bs
      [] -> Left "not enough bytes"

    auxNext ::
         Word64   -- ^ Bit-field value accumulator
      -> Int      -- ^ Remaining number of bits in the bit-field
      -> [Word8]  -- ^ Remaining bytes as read from memory
      -> Either String Word64
    auxNext !acc width' = \case
      (b:bs)
        -- Middle byte
        | width' >= 8 ->
            let width'' = width' - 8
                x       = unsafeShiftL (fromIntegral b) width''
            in  auxNext (acc .|. x) width'' bs
        -- Last byte
        | width' > 0 ->
            let b' = unsafeShiftR b (8 - width')
                x  = fromIntegral b'
            in  auxNext (acc .|. x) 0 bs
        | otherwise -> Left "too many bytes"
      []
        | width' == 0 -> Right acc
        | otherwise   -> Left "not enough bytes"

-- | Put a bit-field value into a list of bytes (native byte order)
putBitfield ::
     Int     -- ^ Offset of the bit-field (0 to 7 bits)
  -> Int     -- ^ Width of the bit-field (1 to 64 bits)
  -> Word8   -- ^ Existing low byte, not used when not needed
  -> Word8   -- ^ Existing high byte, not used when not needed
  -> Word64  -- ^ Bit-field value
  -> [Word8]
putBitfield
    | isLittleEndian = putBitfieldLE
    | otherwise      = putBitfieldBE

-- | Put a bit-field value into a list of bytes (little endian)
putBitfieldLE ::
     Int     -- ^ Offset of the bit-field (0 to 7 bits)
  -> Int     -- ^ Width of the bit-field (1 to 64 bits)
  -> Word8   -- ^ Existing low byte, not used when not needed
  -> Word8   -- ^ Existing high byte, not used when not needed
  -> Word64  -- ^ Bit-field value
  -> [Word8]
putBitfieldLE off width l8 h8 = auxFirst
  where
    auxFirst ::
         Word64  -- ^ Bit-field value
      -> [Word8]
    auxFirst x =
      -- Handle low offset of single/first byte
      let b'            = fromIntegral (x .&. 0xFF)
          b | off > 0   = unsafeShiftL b' off .|. (l8 .&. loMask off)
            | otherwise = b'  -- optimization
          acc           = NonEmpty.singleton b
          numFieldBits  = min width (8 - off)
      in  auxNext acc (width + off - 8) (unsafeShiftR x numFieldBits)

    auxNext ::
         NonEmpty Word8  -- ^ Accumulator (reverse order)
      -> Int             -- ^ Remaining number of bits (may be negative)
      -> Word64          -- ^ Remaining bit-field value
      -> [Word8]
    auxNext acc width' x
      -- Middle/last byte
      | width' > 0 =
          let b = fromIntegral (x .&. 0xFF)
          in  auxNext (NonEmpty.cons b acc) (width' - 8) (unsafeShiftR x 8)
      -- Handle high offset of single/last byte
      | otherwise =
          let hoff          = negate width'
              b'            = NonEmpty.head acc
              b | hoff > 0  =
                    let mask = unsafeShiftL (loMask hoff) (8 - hoff)
                    in  (h8 .&. mask) .|. (b' .&. complement mask)
                | otherwise = b'  -- optimization
          in  reverse (b : NonEmpty.tail acc)

-- | Put a bit-field value into a list of bytes (big endian)
putBitfieldBE ::
     Int     -- ^ Offset of the bit-field (0 to 7 bits)
  -> Int     -- ^ Width of the bit-field (1 to 64 bits)
  -> Word8   -- ^ Existing low byte, not used when not needed
  -> Word8   -- ^ Existing high byte, not used when not needed
  -> Word64  -- ^ Bit-field value
  -> [Word8]
putBitfieldBE off width l8 h8 = auxFirst
  where
    auxFirst ::
         Word64  -- ^ Bit-field value
      -> [Word8]
    auxFirst x =
      -- Handle high offset of single/last byte
      let hoff = (8 - ((width + off) `mod` 8)) `mod` 8
          b'            = fromIntegral (x .&. 0xFF)
          b | hoff > 0  = unsafeShiftL b' hoff .|. (h8 .&. loMask hoff)
            | otherwise = b'  -- optimization
          acc           = NonEmpty.singleton b
          numFieldBits  = min width (8 - hoff)
      in  auxNext acc (width + hoff - 8) (unsafeShiftR x numFieldBits)

    auxNext ::
         NonEmpty Word8  -- ^ Accumulator
      -> Int             -- ^ Remaining number of bits (may be negative)
      -> Word64          -- ^ Remaining bit-field value
      -> [Word8]
    auxNext acc width' x
      -- Middle/first byte
      | width' > 0 =
          let b = fromIntegral (x .&. 0xFF)
          in  auxNext (NonEmpty.cons b acc) (width' - 8) (unsafeShiftR x 8)
      -- Handle low offset of single/first byte
      | otherwise =  -- off == negate width'
          let b'            = NonEmpty.head acc
              b | off > 0   =
                    let mask = unsafeShiftL (loMask off) (8 - off)
                    in  (l8 .&. mask) .|. (b' .&. complement mask)
                | otherwise = b'  -- optimization
          in  b : NonEmpty.tail acc
