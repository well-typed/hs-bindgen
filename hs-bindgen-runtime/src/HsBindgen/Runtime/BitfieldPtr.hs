-- | Pointers to bitfields
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.BitfieldPtr qualified as BitfieldPtr
module HsBindgen.Runtime.BitfieldPtr (
    BitfieldPtr(BitfieldPtr)
  , peek
  , poke
  ) where

import Data.Kind
import Foreign.Ptr

import HsBindgen.Runtime.Internal.Bitfield as Bitfield

-- | A pointer to a bit-field of a C object
type BitfieldPtr :: Type -> Type
type role BitfieldPtr nominal
-- The constructor is unsafe because the offset must be normalized (modulo 8
-- bits).
data BitfieldPtr a = UnsafeBitfieldPtr {
      _ptr    :: Ptr a
      -- | The offset (in number of bits) of the bit-field from the pointer
    , _offset :: Int
      -- | The width (in number of bits) of the bit-field
    , _width  :: Int
    }

{-# COMPLETE BitfieldPtr #-}
pattern BitfieldPtr :: Ptr a -> Int -> Int -> BitfieldPtr a
pattern BitfieldPtr p o w <- UnsafeBitfieldPtr p o w where
  BitfieldPtr p o w = UnsafeBitfieldPtr (p `plusPtr` nBytes) o' w
    where (nBytes, o') = o `quotRem` 8

{-# INLINE peek #-}
-- | Read from a bit-field using a pointer
peek :: Bitfield a => BitfieldPtr a -> IO a
peek (BitfieldPtr p o w) = Bitfield.peekBitOffWidth p o w

{-# INLINE poke #-}
-- | Write to a bit-field using a pointer
poke :: Bitfield a => BitfieldPtr a -> a -> IO ()
poke (BitfieldPtr p o w) = Bitfield.pokeBitOffWidth p o w
