{-# LANGUAGE MagicHash #-}

-- | Declarations with C bitfields
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.HasCBitfield qualified as HasCBitfield
--
-- TODO #1633: Verify.
--
-- Most users do not directly need to use @HasCBitfield@. Instead, we provide
-- @HasField@ instances for pointers, and so we can use record dot syntax and
-- 'HsBindgen.Runtime.BitfieldPtr.peek' and
-- 'HsBindgen.Runtime.BitfieldPtr.poke'.
--
-- TODO #1633: Show example.
module HsBindgen.Runtime.HasCBitfield (
    HasCBitfield(..)
  , offset
  , width
  , toPtr
  , peek
  , poke
  ) where

import Data.Kind
import Data.Proxy
import Foreign.Ptr
import GHC.Exts (Proxy#, proxy#)
import GHC.TypeLits

import HsBindgen.Runtime.BitfieldPtr (BitfieldPtr (BitfieldPtr))
import HsBindgen.Runtime.BitfieldPtr qualified as BitfieldPtr
import HsBindgen.Runtime.Internal.Bitfield (Bitfield)

-- | Evidence that a C object @a@ has a bit-field with the name @field@.
--
-- Bit-fields can be part of structs and unions.
--
-- === Struct
--
-- If we have the C struct @S@:
--
-- > struct S {
-- >   int x : 2;
-- >   int y : 3;
-- > }
--
-- And an accompanying Haskell datatype @S@:
--
-- > data S = S { s_x :: CInt, s_y :: CInt }
--
-- Then we can define two instances
--
-- > HasCBitfield S "s_x"
-- > HasCBitfield S "s_y"
--
-- === Union
--
-- If we have the C union @U@:
--
-- > union U {
-- >   int x : 2;
-- >   int y : 3;
-- > }
--
-- And an accompanying Haskell datatype @U@:
--
-- > data U = U ... {- details elided -}
-- > ... {- getters and setters elided -}
--
-- Then we can define two instances
--
-- > HasCBitfield U "u_x"
-- > HasCBitfield U "u_y"
class HasCBitfield (a :: Type) (field :: Symbol) where
  -- | The type of the bit field
  type CBitfieldType (a :: Type) (field :: Symbol) :: Type

  -- | The offset (in number of bits) of the bit-field with respect to the parent
  -- object.
  bitfieldOffset# :: Proxy# a -> Proxy# field -> Int

  -- | The width (in number of bits) of the bit-field.
  bitfieldWidth# :: Proxy# a -> Proxy# field -> Int

{-# INLINE offset #-}
-- | The offset (in number of bits) of the bit-field with respect to the
-- parent object.
offset ::
     forall a field. HasCBitfield a field
  => Proxy a
  -> Proxy field
  -> Int
offset = \_ _ -> bitfieldOffset# (proxy# @a) (proxy# @field)

{-# INLINE width #-}
-- | The width (in number of bits) of the bit-field.
width ::
     forall a field. HasCBitfield a field
  => Proxy a
  -> Proxy field
  -> Int
width = \_ _ -> bitfieldWidth# (proxy# @a) (proxy# @field)

{-# INLINE toPtr #-}
-- | Convert a pointer to a C object to a pointer to one of the object's
-- bit-fields.
toPtr ::
     forall a field. HasCBitfield a field
  => Proxy field
  -> Ptr a
  -> BitfieldPtr (CBitfieldType a field)
toPtr _ ptr = BitfieldPtr (castPtr ptr) o w
  where
    o = bitfieldOffset# (proxy# @a) (proxy# @field)
    w = bitfieldWidth#  (proxy# @a) (proxy# @field)

{-# INLINE peek #-}
-- | Using a pointer to a C object, read from one of the object's bit-fields.
peek ::
     forall a field. (
       HasCBitfield a field
     , Bitfield (CBitfieldType a field)
     )
  => Proxy field
  -> Ptr a
  -> IO (CBitfieldType a field)
peek field ptr = BitfieldPtr.peek (toPtr field ptr)

{-# INLINE poke #-}
-- | Using a pointer to a C object, write to one of the object's bit-fields.
poke ::
     forall a field. (
       HasCBitfield a field
     , Bitfield (CBitfieldType a field)
     )
  => Proxy field
  -> Ptr a
  -> CBitfieldType a field
  -> IO ()
poke field ptr val = BitfieldPtr.poke (toPtr field ptr) val
