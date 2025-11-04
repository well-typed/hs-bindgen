{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}

module HsBindgen.Runtime.HasCField (
    -- * Fields
    HasCField(..)
  , offset
  , ptrToCField
  , pokeCField
  , peekCField
    -- * Bit-fields
  , HasCBitfield(..)
  , bitOffset
  , bitWidth
  , ptrToCBitfield
  , peekCBitfield
  , pokeCBitfield
    -- * Bit-field pointer
  , BitfieldPtr (..)
  , pokeBits
  , peekBits
  ) where

import Data.Kind
import Data.Proxy
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts (Proxy#, proxy#)
import GHC.TypeLits

import HsBindgen.Runtime.Bitfield as Bitfield

{-------------------------------------------------------------------------------
  HasCField
-------------------------------------------------------------------------------}

-- | Evidence that a C object @a@ has a field with the name @field@.
--
-- Fields can be part of structs and unions. Typedefs are a degenerate use case.
--
-- === Struct
--
-- If we have the C struct @S@:
--
-- > struct S {
-- >   int x;
-- >   int y;
-- > }
--
-- And an accompanying Haskell datatype @S@:
--
-- > data S = S { s_x :: CInt, s_y :: CInt }
--
-- Then we can define two instances
--
-- > HasCField S "s_x"
-- > HasCField S "s_y"
--
-- === Union
--
-- If we have the C union @U@:
--
-- > union U {
-- >   int x;
-- >   int y;
-- > }
--
-- And an accompanying Haskell datatype @U@:
--
-- > data U = U ... {- details elided -}
-- > ... {- getters and setters elided -}
--
-- Then we can define two instances
--
-- > HasCField U "u_x"
-- > HasCField U "u_y"
--
-- === Typedef
--
-- If we have the C typedef @T@:
--
-- > typedef int T;
--
-- And an accompanying Haskell newtype @T@:
--
-- > newtype T = T { un_T :: Int }
--
-- Then we can define the instance:
--
-- > HasCField T "un_T"
class HasCField (a :: Type) (field :: Symbol) where
  type CFieldType (a :: Type) (field :: Symbol) :: Type

  -- | The offset (in number of bytes) of the field with respect to the parent
  -- object.
  offset# :: Proxy# a -> Proxy# field -> Int

{-# INLINE offset #-}
-- | The offset (in number of bytes) of the field with respect to the parent
-- object.
offset ::
     forall a field. HasCField a field
  => Proxy a
  -> Proxy field
  -> Int
offset = \_ _ -> offset# (proxy# @a) (proxy# @field)

{-# INLINE ptrToCField #-}
-- | Convert a pointer to a C object to a pointer to one of the object's fields.
ptrToCField ::
     forall a field. HasCField a field
  => Proxy field
  -> Ptr a
  -> Ptr (CFieldType a field)
ptrToCField _ ptr = ptr `plusPtr` offset# (proxy# @a) (proxy# @field)

{-# INLINE pokeCField #-}
-- | Using a pointer to a C object, write to one of the object's fields.
pokeCField ::
     (HasCField a field, Storable (CFieldType a field))
  => Proxy field
  -> Ptr a
  -> CFieldType a field
  -> IO ()
pokeCField field ptr val = poke (ptrToCField field ptr) val

{-# INLINE peekCField #-}
-- | Using a pointer to a C object, read from one of the object's fields.
peekCField ::
     (HasCField a field, Storable (CFieldType a field))
  => Proxy field
  -> Ptr a
  -> IO (CFieldType a field)
peekCField field ptr = peek (ptrToCField field ptr)

{-------------------------------------------------------------------------------
  HasCBitField
-------------------------------------------------------------------------------}

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
  bitOffset# :: Proxy# a -> Proxy# field -> Int

  -- | The width (in number of bits) of the bit-field.
  bitWidth# :: Proxy# a -> Proxy# field -> Int

{-# INLINE bitOffset #-}
-- | The offset (in number of bits) of the bit-field with respect to the
-- parent object.
bitOffset ::
     forall a field. HasCBitfield a field
  => Proxy a
  -> Proxy field
  -> Int
bitOffset = \_ _ -> bitOffset# (proxy# @a) (proxy# @field)

{-# INLINE bitWidth #-}
-- | The width (in number of bits) of the bit-field.
bitWidth ::
     forall a field. HasCBitfield a field
  => Proxy a
  -> Proxy field
  -> Int
bitWidth = \_ _ -> bitWidth# (proxy# @a) (proxy# @field)

{-# INLINE ptrToCBitfield #-}
-- | Convert a pointer to a C object to a pointer to one of the object's
-- bit-fields.
--
-- NOTE: this doesn't really move the pointer. Rather, the pointer to the C
-- object is wrapped in a newtype that changes its interpretation. See
-- 'BitfieldPtr'.
ptrToCBitfield ::
     forall a field. HasCBitfield a field
  => Proxy field
  -> Ptr a
  -> BitfieldPtr a field
ptrToCBitfield _ ptr = BitfieldPtr $ ptr
  where
    -- This is just here to prevent a "redundant constraint" warning for
    -- 'HasCBitfield'.
    _unused = bitOffset# (proxy# @a) (proxy# @field)

{-# INLINE pokeCBitfield #-}
-- | Using a pointer to a C object, write to one of the object's bit-fields.
pokeCBitfield ::
     forall a field. (
       HasCBitfield a field
     , Bitfield (CBitfieldType a field)
     )
  => Proxy field
  -> Ptr a
  -> CBitfieldType a field
  -> IO ()
pokeCBitfield field ptr val = pokeBits (ptrToCBitfield field ptr) val

{-# INLINE peekCBitfield #-}
-- | Using a pointer to a C object, read from one of the object's bit-fields.
peekCBitfield ::
     forall a field. (
       HasCBitfield a field
     , Bitfield (CBitfieldType a field)
     )
  => Proxy field
  -> Ptr a
  -> IO (CBitfieldType a field)
peekCBitfield field ptr = peekBits (ptrToCBitfield field ptr)

{-------------------------------------------------------------------------------
  BitfieldPtr
-------------------------------------------------------------------------------}

-- | A pointer to a bit-field of a C object
type BitfieldPtr :: Type -> Symbol -> Type
type role BitfieldPtr nominal nominal
newtype BitfieldPtr a field = BitfieldPtr
    -- | This is a pointer to the parent C object. The newtype ensures that
    -- 'peekBits' and 'pokeBits' can interpret this pointer as a pointer to one
    -- of the object's bit-fields.
    (Ptr a)

{-# INLINE pokeBits #-}
-- | Write to a bit-field using a pointer
pokeBits ::
     forall a field. (
       HasCBitfield a field
     , Bitfield (CBitfieldType a field)
     )
  => BitfieldPtr a field
  -> CBitfieldType a field
  -> IO ()
pokeBits (BitfieldPtr ptr) val = Bitfield.pokeBitOffWidth ptr o w val
  where
    o = bitOffset# (proxy# @a) (proxy# @field)
    w = bitWidth# (proxy# @a) (proxy# @field)

{-# INLINE peekBits #-}
-- | Read from a bit-field using a pointer
peekBits ::
     forall a field. (
       HasCBitfield a field
     , Bitfield (CBitfieldType a field)
     )
  => BitfieldPtr a field
  -> IO (CBitfieldType a field)
peekBits (BitfieldPtr ptr) = Bitfield.peekBitOffWidth ptr o w
  where
    o = bitOffset# (proxy# @a) (proxy# @field)
    w = bitWidth# (proxy# @a) (proxy# @field)
