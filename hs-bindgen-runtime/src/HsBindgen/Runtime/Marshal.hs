module HsBindgen.Runtime.Marshal (
    -- * Type Classes
    HasStaticSize(..)
  , Peekable(..)
  , Pokable(..)
  , EquivStorable(..)

    -- * Utility Functions
  , peekByteOff
  , pokeByteOff
  , peekElemOff
  , pokeElemOff
  , maybePeek
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
class HasStaticSize a where

  -- | Storage requirements (bytes)
  sizeOf :: Proxy a -> Int

  default sizeOf :: Storable a => Proxy a -> Int
  sizeOf _proxy = Storable.sizeOf @a undefined

  -- | Alignment (bytes)
  alignment :: Proxy a -> Int

  default alignment :: Storable a => Proxy a -> Int
  alignment _proxy = Storable.alignment @a undefined

-- | Values that can be read from memory
--
-- Types that are instances of 'Storable' can derive this instance.
class Peekable a where

  -- | Read a value from the given memory location
  --
  -- This function might require a properly aligned address to function
  -- correctly, depending on the architecture.
  peek :: Ptr a -> IO a

  default peek :: Storable a => Ptr a -> IO a
  peek = Storable.peek

-- | Values that can be written to memory
--
-- Types that are instances of 'Storable' can derive this instance.
class Pokable a where

  -- | Write a value to the given memory location
  --
  -- This function might require a properly aligned address to function
  -- correctly, depending on the architecture.
  poke :: Ptr a -> a -> IO ()

  default poke :: Storable a => Ptr a -> a -> IO ()
  poke = Storable.poke

-- | Type used to derive a 'Storable' instance when the type has
-- 'HasStaticSize', 'Peekable', and 'Pokable' instances
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
     (HasStaticSize a, Peekable a, Pokable a)
  => Storable (EquivStorable a)
  where
    sizeOf    _ = sizeOf    @a undefined
    alignment _ = alignment @a undefined

    peek ptr = EquivStorable <$> peek (Ptr.castPtr ptr)

    poke ptr (EquivStorable x) = poke (Ptr.castPtr ptr) x

{-------------------------------------------------------------------------------
  Utility Functions
-------------------------------------------------------------------------------}

-- | Read a value from the given memory location, given by a base address and an
-- offset
peekByteOff :: Peekable a => Ptr b -> Int -> IO a
peekByteOff ptr off = peek (ptr `Ptr.plusPtr` off)

-- | Write a value to the given memory location, given by a base address and an
-- offset
pokeByteOff :: Pokable a => Ptr b -> Int -> a -> IO ()
pokeByteOff ptr off = poke (ptr `Ptr.plusPtr` off)

-- | Read a value from a memory area regarded as an array of values of the same
-- kind
--
-- The first argument specifies the start address of the array.  The second
-- specifies the (zero-based) index into the array.
peekElemOff :: forall a. (HasStaticSize a, Peekable a) => Ptr a -> Int -> IO a
peekElemOff ptr off = peekByteOff ptr $ off * sizeOf @a Proxy

-- | Write a value to a memory area regarded as an array of values of the same
-- kind
--
-- The first argument specifies the start address of the array.  The second
-- specifies the (zero-based) index into the array.
pokeElemOff :: forall a.
     (HasStaticSize a, Pokable a)
  => Ptr a
  -> Int
  -> a
  -> IO ()
pokeElemOff ptr off = pokeByteOff ptr $ off * sizeOf @a Proxy

-- | Read a value from memory when passed a non-null pointer
maybePeek :: Peekable a => Ptr a -> IO (Maybe a)
maybePeek ptr
    | ptr == Ptr.nullPtr = return Nothing
    | otherwise          = Just <$> peek ptr

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
     (HasStaticSize a, Pokable a)
  => a
  -> (Ptr a -> IO b)
  -> IO b
with x f = Alloc.allocaBytesAligned size align$ \ptr -> do
    poke ptr x
    f ptr
  where
    size, align :: Int
    size  = sizeOf    @a Proxy
    align = alignment @a Proxy

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
     (HasStaticSize a, Pokable a)
  => a
  -> (Ptr a -> IO b)
  -> IO b
withZero x f = Alloc.allocaBytesAligned size align$ \ptr -> do
    Utils.fillBytes ptr 0 size
    poke ptr x
    f ptr
  where
    size, align :: Int
    size  = sizeOf    @a Proxy
    align = alignment @a Proxy

-- | Allocate memory, write the specified value, and the 'ForeignPtr'
--
-- The allocated memory is aligned.
--
-- Memory that is not written to by 'poke' may contain arbitrary data.
new :: forall a.
     (HasStaticSize a, Pokable a)
  => a
  -> IO (ForeignPtr a)
new x = do
    fptr <- mallocForeignPtrAlignedBytes size align
    withForeignPtr fptr $ \ptr -> poke ptr x
    return fptr
  where
    size, align :: Int
    size  = sizeOf    @a Proxy
    align = alignment @a Proxy

-- | Allocate memory, write the specified value, and the 'ForeignPtr'
--
-- The allocated memory is aligned.
--
-- The memory is filled with bytes of value zero before the value is written.
-- Memory that is not written to by 'poke' contains zeros, not arbitrary data.
newZero :: forall a.
     (HasStaticSize a, Pokable a)
  => a
  -> IO (ForeignPtr a)
newZero x = do
    fptr <- mallocForeignPtrAlignedBytes size align
    withForeignPtr fptr $ \ptr -> do
      Utils.fillBytes ptr 0 size
      poke ptr x
    return fptr
  where
    size, align :: Int
    size  = sizeOf    @a Proxy
    align = alignment @a Proxy

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance HasStaticSize C.CChar
instance Peekable      C.CChar
instance Pokable       C.CChar

instance HasStaticSize C.CSChar
instance Peekable      C.CSChar
instance Pokable       C.CSChar

instance HasStaticSize C.CUChar
instance Peekable      C.CUChar
instance Pokable       C.CUChar

instance HasStaticSize C.CShort
instance Peekable      C.CShort
instance Pokable       C.CShort

instance HasStaticSize C.CUShort
instance Peekable      C.CUShort
instance Pokable       C.CUShort

instance HasStaticSize C.CInt
instance Peekable      C.CInt
instance Pokable       C.CInt

instance HasStaticSize C.CUInt
instance Peekable      C.CUInt
instance Pokable       C.CUInt

instance HasStaticSize C.CLong
instance Peekable      C.CLong
instance Pokable       C.CLong

instance HasStaticSize C.CULong
instance Peekable      C.CULong
instance Pokable       C.CULong

instance HasStaticSize C.CPtrdiff
instance Peekable      C.CPtrdiff
instance Pokable       C.CPtrdiff

instance HasStaticSize C.CSize
instance Peekable      C.CSize
instance Pokable       C.CSize

instance HasStaticSize C.CWchar
instance Peekable      C.CWchar
instance Pokable       C.CWchar

instance HasStaticSize C.CSigAtomic
instance Peekable      C.CSigAtomic
instance Pokable       C.CSigAtomic

instance HasStaticSize C.CLLong
instance Peekable      C.CLLong
instance Pokable       C.CLLong

instance HasStaticSize C.CULLong
instance Peekable      C.CULLong
instance Pokable       C.CULLong

instance HasStaticSize C.CBool
instance Peekable      C.CBool
instance Pokable       C.CBool

instance HasStaticSize C.CIntPtr
instance Peekable      C.CIntPtr
instance Pokable       C.CIntPtr

instance HasStaticSize C.CUIntPtr
instance Peekable      C.CUIntPtr
instance Pokable       C.CUIntPtr

instance HasStaticSize C.CIntMax
instance Peekable      C.CIntMax
instance Pokable       C.CIntMax

instance HasStaticSize C.CUIntMax
instance Peekable      C.CUIntMax
instance Pokable       C.CUIntMax

instance HasStaticSize C.CClock
instance Peekable      C.CClock
instance Pokable       C.CClock

instance HasStaticSize C.CTime
instance Peekable      C.CTime
instance Pokable       C.CTime

instance HasStaticSize C.CUSeconds
instance Peekable      C.CUSeconds
instance Pokable       C.CUSeconds

instance HasStaticSize C.CSUSeconds
instance Peekable      C.CSUSeconds
instance Pokable       C.CSUSeconds

instance HasStaticSize C.CFloat
instance Peekable      C.CFloat
instance Pokable       C.CFloat

instance HasStaticSize C.CDouble
instance Peekable      C.CDouble
instance Pokable       C.CDouble

instance HasStaticSize (Ptr a)
instance Peekable      (Ptr a)
instance Pokable       (Ptr a)

-- TODO Foreign.C.ConstPtr from base-4.18.0.0 ?

instance HasStaticSize (FunPtr a)
instance Peekable      (FunPtr a)
instance Pokable       (FunPtr a)

instance HasStaticSize (StablePtr a)
instance Peekable      (StablePtr a)
instance Pokable       (StablePtr a)

instance HasStaticSize Int8
instance Peekable      Int8
instance Pokable       Int8

instance HasStaticSize Int16
instance Peekable      Int16
instance Pokable       Int16

instance HasStaticSize Int32
instance Peekable      Int32
instance Pokable       Int32

instance HasStaticSize Int64
instance Peekable      Int64
instance Pokable       Int64

instance HasStaticSize Word8
instance Peekable      Word8
instance Pokable       Word8

instance HasStaticSize Word16
instance Peekable      Word16
instance Pokable       Word16

instance HasStaticSize Word32
instance Peekable      Word32
instance Pokable       Word32

instance HasStaticSize Word64
instance Peekable      Word64
instance Pokable       Word64

instance HasStaticSize Int
instance Peekable      Int
instance Pokable       Int

instance HasStaticSize Word
instance Peekable      Word
instance Pokable       Word

instance HasStaticSize Float
instance Peekable      Float
instance Pokable       Float

instance HasStaticSize Double
instance Peekable      Double
instance Pokable       Double

instance HasStaticSize Char
instance Peekable      Char
instance Pokable       Char

instance HasStaticSize Bool
instance Peekable      Bool
instance Pokable       Bool

instance HasStaticSize ()
instance Peekable      ()
instance Pokable       ()
