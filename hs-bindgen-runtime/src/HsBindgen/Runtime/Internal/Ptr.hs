{-# OPTIONS_HADDOCK hide #-}

-- NOTE: For now, this module is classified "Internal" because the definitions
-- are re-exported from the runtime prelude. Should we add definitions intended
-- for qualified import, we need to add a public module.

-- | Pointer utilities
module HsBindgen.Runtime.Internal.Ptr (
    plusPtrElem
  ) where

import Foreign.Ptr
import Foreign.Storable

-- | Advances the given address by the given offset in number of elements of
-- type @a@.
--
-- Examples:
--
-- > plusPtr (_ :: Ptr Word32) 2 -- moves the pointer by 2 bytes
-- > plusPtrElem (_ :: Ptr Word32) 2 -- moves the pointer by 2*4=8 bytes
--
-- NOTE: if @a@ is an instance of 'Data.Primitive.Types.Prim', then you can
-- alternatively use 'Data.Primitive.Ptr.advancePtr'.
plusPtrElem :: forall a. Storable a => Ptr a -> Int -> Ptr a
plusPtrElem ptr i = ptr `plusPtr` (i * sizeOf (undefined :: a))
