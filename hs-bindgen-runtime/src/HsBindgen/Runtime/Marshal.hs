module HsBindgen.Runtime.Marshal (
    -- * Type Classes
    StaticSize(..)
  , ReadRaw(..)
  , WriteRaw(..)
  , EquivStorable(..)

    -- * Utility Functions
  , readRawByteOff
  , writeRawByteOff
  , readRawElemOff
  , writeRawElemOff
  , maybeReadRaw
  , with
  , withZero
  , new
  , newZero
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Proxy (Proxy(Proxy))
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C qualified as C
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Marshal.Utils qualified as Utils
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Ptr qualified as Ptr
import Foreign.StablePtr (StablePtr)
import Foreign.Storable (Storable)
import Foreign.Storable qualified as Storable
import GHC.ForeignPtr (mallocForeignPtrAlignedBytes)

{-------------------------------------------------------------------------------
  Type Classes
-------------------------------------------------------------------------------}

-- | Size and alignment for values that have a static size in memory
--
-- Types that are instances of 'Storable' can derive this instance.
class StaticSize a where

  -- | Storage requirements (bytes)
  staticSizeOf :: Proxy a -> Int

  default staticSizeOf :: Storable a => Proxy a -> Int
  staticSizeOf _proxy = Storable.sizeOf @a undefined

  -- | Alignment (bytes)
  staticAlignment :: Proxy a -> Int

  default staticAlignment :: Storable a => Proxy a -> Int
  staticAlignment _proxy = Storable.alignment @a undefined

-- | Values that can be read from memory
--
-- Types that are instances of 'Storable' can derive this instance.
class ReadRaw a where

  -- | Read a value from the given memory location
  --
  -- This function might require a properly aligned address to function
  -- correctly, depending on the architecture.
  readRaw :: Ptr a -> IO a

  default readRaw :: Storable a => Ptr a -> IO a
  readRaw = Storable.peek

-- | Values that can be written to memory
--
-- Types that are instances of 'Storable' can derive this instance.
class WriteRaw a where

  -- | Write a value to the given memory location
  --
  -- This function might require a properly aligned address to function
  -- correctly, depending on the architecture.
  writeRaw :: Ptr a -> a -> IO ()

  default writeRaw :: Storable a => Ptr a -> a -> IO ()
  writeRaw = Storable.poke

-- | Type used to derive a 'Storable' instance when the type has 'StaticSize',
-- 'ReadRaw', and 'WriteRaw' instances
--
-- Use the @DerivingVia@ GHC extension as follows:
--
-- @
-- {-# LANGUAGE DerivingVia #-}
--
-- data Foo = Foo { ... }
--   deriving Storable via EquivStorable Foo
-- @
newtype EquivStorable a = EquivStorable a

instance
     (ReadRaw a, StaticSize a, WriteRaw a)
  => Storable (EquivStorable a)
  where
    sizeOf    _ = staticSizeOf    @a undefined
    alignment _ = staticAlignment @a undefined

    peek ptr = EquivStorable <$> readRaw (Ptr.castPtr ptr)

    poke ptr (EquivStorable x) = writeRaw (Ptr.castPtr ptr) x

{-------------------------------------------------------------------------------
  Utility Functions
-------------------------------------------------------------------------------}

-- | Read a value from the given memory location, given by a base address and an
-- offset
readRawByteOff :: ReadRaw a => Ptr b -> Int -> IO a
readRawByteOff ptr off = readRaw (ptr `Ptr.plusPtr` off)

-- | Write a value to the given memory location, given by a base address and an
-- offset
writeRawByteOff :: WriteRaw a => Ptr b -> Int -> a -> IO ()
writeRawByteOff ptr off = writeRaw (ptr `Ptr.plusPtr` off)

-- | Read a value from a memory area regarded as an array of values of the same
-- kind
--
-- The first argument specifies the start address of the array.  The second
-- specifies the (zero-based) index into the array.
readRawElemOff :: forall a. (ReadRaw a, StaticSize a) => Ptr a -> Int -> IO a
readRawElemOff ptr off = readRawByteOff ptr $ off * staticSizeOf @a Proxy

-- | Write a value to a memory area regarded as an array of values of the same
-- kind
--
-- The first argument specifies the start address of the array.  The second
-- specifies the (zero-based) index into the array.
writeRawElemOff :: forall a.
     (StaticSize a, WriteRaw a)
  => Ptr a
  -> Int
  -> a
  -> IO ()
writeRawElemOff ptr off = writeRawByteOff ptr $ off * staticSizeOf @a Proxy

-- | Read a value from memory when passed a non-null pointer
maybeReadRaw :: ReadRaw a => Ptr a -> IO (Maybe a)
maybeReadRaw ptr
    | ptr == Ptr.nullPtr = return Nothing
    | otherwise          = Just <$> readRaw ptr

-- | Allocate local memory, write the specified value, and call a function with
-- the pointer
--
-- The allocated memory is aligned.
--
-- Memory that is not written to by 'poke' may contain arbitrary data.
--
-- The allocated memory is freed when the function terminates, either normally
-- or via an exception.  The passed pointer must therefore /not/ be used after
-- this.
with :: forall a b.
     (StaticSize a, WriteRaw a)
  => a
  -> (Ptr a -> IO b)
  -> IO b
with x f = Alloc.allocaBytesAligned size align$ \ptr -> do
    writeRaw ptr x
    f ptr
  where
    size, align :: Int
    size  = staticSizeOf    @a Proxy
    align = staticAlignment @a Proxy

-- | Allocate local memory, write the specified value, and call a function with
-- the pointer
--
-- The allocated memory is aligned.
--
-- The memory is filled with bytes of value zero before the value is written.
-- Memory that is not written to by 'poke' contains zeros, not arbitrary data.
--
-- The allocated memory is freed when the function terminates, either normally
-- or via an exception.  The passed pointer must therefore /not/ be used after
-- this.
withZero :: forall a b.
     (StaticSize a, WriteRaw a)
  => a
  -> (Ptr a -> IO b)
  -> IO b
withZero x f = Alloc.allocaBytesAligned size align$ \ptr -> do
    Utils.fillBytes ptr 0 size
    writeRaw ptr x
    f ptr
  where
    size, align :: Int
    size  = staticSizeOf    @a Proxy
    align = staticAlignment @a Proxy

-- | Allocate memory, write the specified value, and the 'ForeignPtr'
--
-- The allocated memory is aligned.
--
-- Memory that is not written to by 'writeRaw' may contain arbitrary data.
new :: forall a.
     (StaticSize a, WriteRaw a)
  => a
  -> IO (ForeignPtr a)
new x = do
    fptr <- mallocForeignPtrAlignedBytes size align
    withForeignPtr fptr $ \ptr -> writeRaw ptr x
    return fptr
  where
    size, align :: Int
    size  = staticSizeOf    @a Proxy
    align = staticAlignment @a Proxy

-- | Allocate memory, write the specified value, and the 'ForeignPtr'
--
-- The allocated memory is aligned.
--
-- The memory is filled with bytes of value zero before the value is written.
-- Memory that is not written to by 'writeRaw' contains zeros, not arbitrary
-- data.
newZero :: forall a.
     (StaticSize a, WriteRaw a)
  => a
  -> IO (ForeignPtr a)
newZero x = do
    fptr <- mallocForeignPtrAlignedBytes size align
    withForeignPtr fptr $ \ptr -> do
      Utils.fillBytes ptr 0 size
      writeRaw ptr x
    return fptr
  where
    size, align :: Int
    size  = staticSizeOf    @a Proxy
    align = staticAlignment @a Proxy

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance StaticSize C.CChar
instance ReadRaw    C.CChar
instance WriteRaw   C.CChar

instance StaticSize C.CSChar
instance ReadRaw    C.CSChar
instance WriteRaw   C.CSChar

instance StaticSize C.CUChar
instance ReadRaw    C.CUChar
instance WriteRaw   C.CUChar

instance StaticSize C.CShort
instance ReadRaw    C.CShort
instance WriteRaw   C.CShort

instance StaticSize C.CUShort
instance ReadRaw    C.CUShort
instance WriteRaw   C.CUShort

instance StaticSize C.CInt
instance ReadRaw    C.CInt
instance WriteRaw   C.CInt

instance StaticSize C.CUInt
instance ReadRaw    C.CUInt
instance WriteRaw   C.CUInt

instance StaticSize C.CLong
instance ReadRaw    C.CLong
instance WriteRaw   C.CLong

instance StaticSize C.CULong
instance ReadRaw    C.CULong
instance WriteRaw   C.CULong

instance StaticSize C.CPtrdiff
instance ReadRaw    C.CPtrdiff
instance WriteRaw   C.CPtrdiff

instance StaticSize C.CSize
instance ReadRaw    C.CSize
instance WriteRaw   C.CSize

instance StaticSize C.CWchar
instance ReadRaw    C.CWchar
instance WriteRaw   C.CWchar

instance StaticSize C.CSigAtomic
instance ReadRaw    C.CSigAtomic
instance WriteRaw   C.CSigAtomic

instance StaticSize C.CLLong
instance ReadRaw    C.CLLong
instance WriteRaw   C.CLLong

instance StaticSize C.CULLong
instance ReadRaw    C.CULLong
instance WriteRaw   C.CULLong

instance StaticSize C.CBool
instance ReadRaw    C.CBool
instance WriteRaw   C.CBool

instance StaticSize C.CIntPtr
instance ReadRaw    C.CIntPtr
instance WriteRaw   C.CIntPtr

instance StaticSize C.CUIntPtr
instance ReadRaw    C.CUIntPtr
instance WriteRaw   C.CUIntPtr

instance StaticSize C.CIntMax
instance ReadRaw    C.CIntMax
instance WriteRaw   C.CIntMax

instance StaticSize C.CUIntMax
instance ReadRaw    C.CUIntMax
instance WriteRaw   C.CUIntMax

instance StaticSize C.CClock
instance ReadRaw    C.CClock
instance WriteRaw   C.CClock

instance StaticSize C.CTime
instance ReadRaw    C.CTime
instance WriteRaw   C.CTime

instance StaticSize C.CUSeconds
instance ReadRaw    C.CUSeconds
instance WriteRaw   C.CUSeconds

instance StaticSize C.CSUSeconds
instance ReadRaw    C.CSUSeconds
instance WriteRaw   C.CSUSeconds

instance StaticSize C.CFloat
instance ReadRaw    C.CFloat
instance WriteRaw   C.CFloat

instance StaticSize C.CDouble
instance ReadRaw    C.CDouble
instance WriteRaw   C.CDouble

instance StaticSize (Ptr a)
instance ReadRaw    (Ptr a)
instance WriteRaw   (Ptr a)

-- TODO Foreign.C.ConstPtr from base-4.18.0.0 ?

instance StaticSize (FunPtr a)
instance ReadRaw    (FunPtr a)
instance WriteRaw   (FunPtr a)

instance StaticSize (StablePtr a)
instance ReadRaw    (StablePtr a)
instance WriteRaw   (StablePtr a)

instance StaticSize Int8
instance ReadRaw    Int8
instance WriteRaw   Int8

instance StaticSize Int16
instance ReadRaw    Int16
instance WriteRaw   Int16

instance StaticSize Int32
instance ReadRaw    Int32
instance WriteRaw   Int32

instance StaticSize Int64
instance ReadRaw    Int64
instance WriteRaw   Int64

instance StaticSize Word8
instance ReadRaw    Word8
instance WriteRaw   Word8

instance StaticSize Word16
instance ReadRaw    Word16
instance WriteRaw   Word16

instance StaticSize Word32
instance ReadRaw    Word32
instance WriteRaw   Word32

instance StaticSize Word64
instance ReadRaw    Word64
instance WriteRaw   Word64

instance StaticSize Int
instance ReadRaw    Int
instance WriteRaw   Int

instance StaticSize Word
instance ReadRaw    Word
instance WriteRaw   Word

instance StaticSize Float
instance ReadRaw    Float
instance WriteRaw   Float

instance StaticSize Double
instance ReadRaw    Double
instance WriteRaw   Double

instance StaticSize Char
instance ReadRaw    Char
instance WriteRaw   Char

instance StaticSize Bool
instance ReadRaw    Bool
instance WriteRaw   Bool

instance StaticSize ()
instance ReadRaw    ()
instance WriteRaw   ()
