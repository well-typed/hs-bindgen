-- | Pointers to bitfields
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.BitfieldPtr qualified as BitfieldPtr
module HsBindgen.Runtime.BitfieldPtr (
    Bitfield(..)
  , BitfieldPtr -- opaque
  , mkBitfieldPtr
  , startingByte
  , offset
  , width
  , bounds
  , peek
  , poke
  ) where

import Data.Kind
import Data.Proxy
import Foreign.Ptr

import HsBindgen.Runtime.Internal.Bitfield as Bitfield
import HsBindgen.Runtime.Marshal qualified as Marshal

-- | Pointer to a bit-field of a C object
--
-- A @BitfieldPtr a@ is for a bit-field of type @a@.  For example, a
-- @Bitfield CUInt@ points to a bit-field of type @CUInt@.
type BitfieldPtr :: Type -> Type
type role BitfieldPtr nominal
data BitfieldPtr a = UnsafeBitfieldPtr {
      -- | Pointer to the byte where the bit-field starts
      --
      -- We do /not/ assume that the pointer is aligned.
      startingByte :: Ptr ()
      -- | Offset of the bit-field (0 to 7 bits)
    , offset :: Int
      -- | Width of the bit-field (1 to 64 bits)
    , width :: Int
      -- | Memory bounds of the @struct@
      --
      -- The lower bound is inclusive, and the upper bound is exclusive.
      --
      -- To peek/poke a bit-field, we may peek/poke any memory within these
      -- bounds.  We must not peek/poke memory outside of these bounds.
    , bounds :: (Ptr (), Ptr ())
    }

-- | Construct a 'BitfieldPtr' given the C object pointer, offset, and width
mkBitfieldPtr :: forall s a.
     Marshal.StaticSize s
  => Ptr s  -- ^ Pointer to the C object with the bit-field
  -> Int    -- ^ Offset of the bit-field (0 or more bits)
  -> Int    -- ^ Width of the bit-field (1 to 64 bits)
  -> BitfieldPtr a
mkBitfieldPtr ptr off width = UnsafeBitfieldPtr{
      startingByte = castPtr $ ptr `plusPtr` offBytes
    , offset       = offBits
    , width        = width
    , bounds       = (ptrL, ptrH)
    }
  where
    offBytes, offBits :: Int
    (offBytes, offBits) = off `quotRem` 8

    ptrL, ptrH :: Ptr ()
    ptrL = castPtr ptr
    ptrH = ptrL `plusPtr` Marshal.staticSizeOf @s Proxy

{-# INLINE peek #-}
-- | Read from a bit-field
peek :: Bitfield a => BitfieldPtr a -> IO a
peek (UnsafeBitfieldPtr p o w b) = Bitfield.peekBitOffWidth p o w b

{-# INLINE poke #-}
-- | Write to a bit-field
poke :: Bitfield a => BitfieldPtr a -> a -> IO ()
poke (UnsafeBitfieldPtr p o w b) = Bitfield.pokeBitOffWidth p o w b
