{-# LANGUAGE MagicHash #-}

-- | Declarations with C fields
--
-- Most users do not directly need to use @HasCField@, and can use record dot
-- syntax instead. For example, given
--
-- > struct DriverInfo {
-- >   char* name;
-- >   int version;
-- > };
-- >
-- > struct Driver {
-- >   struct DriverInfo info;
-- > };
--
-- @hs-bindgen@ will generate code such that if
--
-- > driverPtr :: Ptr Driver
--
-- then
--
-- > driverPtr.driver_version                    :: Ptr DriverInfo
-- > driverPtr.driver_version.driverInfo_version :: Ptr CInt
--
-- Note that chaining like this can only be done for nested structs; no actual
-- dereferencing takes place! For example, if we additionally had
--
-- > struct DriverCode {
-- >   ..
-- > };
-- >
-- > struct Driver {
-- >   ..
-- >   struct DriverCode* code;
-- > };
-- >
--
-- then
--
-- > driverPtr.driver_code :: Ptr (Ptr DriverCode)
--
-- (note the double 'Ptr'), which must be dereferenced before any fields of
-- @DriverCode@ can be accessed.
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.HasCField qualified as HasCField
module HsBindgen.Runtime.HasCField (
    -- * Fields
    HasCField(..)
  , offset
  , fromPtr
  , peek
  , poke
  , readRaw
  , writeRaw
  ) where

import Data.Kind
import Data.Proxy
import Foreign.Ptr
import Foreign.Storable hiding (peek, poke)
import Foreign.Storable qualified
import GHC.Exts (Proxy#, proxy#)
import GHC.TypeLits

import HsBindgen.Runtime.Marshal (ReadRaw, WriteRaw)
import HsBindgen.Runtime.Marshal qualified

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
-- > newtype T = T { unwrapT :: Int }
--
-- Then we can define the instance:
--
-- > HasCField T "unwrapT"
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

{-# INLINE fromPtr #-}
-- | Convert a pointer to a C object to a pointer to one of the object's fields.
fromPtr ::
     forall a field. HasCField a field
  => Proxy field
  -> Ptr a
  -> Ptr (CFieldType a field)
fromPtr _ ptr = ptr `plusPtr` offset# (proxy# @a) (proxy# @field)

{-# INLINE peek #-}
-- | Using a pointer to a C object, read from one of the object's fields.
peek ::
     (HasCField a field, Storable (CFieldType a field))
  => Proxy field
  -> Ptr a
  -> IO (CFieldType a field)
peek field ptr = Foreign.Storable.peek (fromPtr field ptr)

{-# INLINE poke #-}
-- | Using a pointer to a C object, write to one of the object's fields.
poke ::
     (HasCField a field, Storable (CFieldType a field))
  => Proxy field
  -> Ptr a
  -> CFieldType a field
  -> IO ()
poke field ptr val = Foreign.Storable.poke (fromPtr field ptr) val

-- | Read a field using a pointer to a C object
{-# INLINE readRaw #-}
readRaw ::
     (HasCField a field, ReadRaw (CFieldType a field))
  => Proxy field
  -> Ptr a
  -> IO (CFieldType a field)
readRaw field ptr = HsBindgen.Runtime.Marshal.readRaw (fromPtr field ptr)

-- | Write a field using a pointer to a C object
{-# INLINE writeRaw #-}
writeRaw ::
     (HasCField a field, WriteRaw (CFieldType a field))
  => Proxy field
  -> Ptr a
  -> CFieldType a field
  -> IO ()
writeRaw field ptr = HsBindgen.Runtime.Marshal.writeRaw (fromPtr field ptr)
