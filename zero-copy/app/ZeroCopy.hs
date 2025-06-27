{-# LANGUAGE CApiFFI               #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module ZeroCopy where

import Data.Array.Byte (ByteArray)
import Data.Kind
import Data.Proxy
import Foreign
import Foreign.C
import GHC.Exts
import GHC.Records qualified as GHC

import HsBindgen.Runtime.ByteArray qualified as ByteArray
import HsBindgen.Runtime.ConstantArray (ConstantArray)
import HsBindgen.Runtime.SizedByteArray (SizedByteArray(..))

{-------------------------------------------------------------------------------
  Infrastructure (this would live in hs-bindgen-runtime)
-------------------------------------------------------------------------------}

class Storable (FieldType a field)
   => HasCField (a :: Type) (field :: Symbol) where
  type FieldType a field :: Type
  ptrToField :: Proxy field -> Ptr a -> Ptr (FieldType a field)

pokeField ::
     HasCField a field
  => Proxy field -> Ptr a -> FieldType a field -> IO ()
pokeField field ptr val = poke (ptrToField field ptr) val

peekField ::
     HasCField a field
  => Proxy field -> Ptr a -> IO (FieldType a field)
peekField field ptr = peek (ptrToField field ptr)

{-------------------------------------------------------------------------------
  Example (this would be generated code)
-------------------------------------------------------------------------------}

data Point = Point {
      x :: CInt
    , y :: CInt
    }
  deriving stock (Show, Eq)

data Rect = Rect {
      topleft     :: Point
    , bottomright :: Point
    }
  deriving stock (Show, Eq)

instance HasCField Point "x" where
  type FieldType Point "x" = CInt
  ptrToField _ ptr = ptr `plusPtr` 0

instance HasCField Point "y" where
  type FieldType Point "y" = CInt
  ptrToField _ ptr = ptr `plusPtr` 4

instance HasCField Rect "topleft" where
  type FieldType Rect "topleft" = Point
  ptrToField _ ptr = ptr `plusPtr` 0

instance HasCField Rect "bottomright" where
  type FieldType Rect "bottomright" = Point
  ptrToField _ ptr = ptr `plusPtr` 8

{-------------------------------------------------------------------------------
  Storable instance
-------------------------------------------------------------------------------}

instance Storable Point where
  sizeOf    _ = 8
  alignment _ = 4

  peek ptr =
          pure Point
      <*> peekField (Proxy @"x") ptr
      <*> peekField (Proxy @"y") ptr

  poke ptr Point{x, y} = do
      pokeField (Proxy @"x") ptr x
      pokeField (Proxy @"y") ptr y

instance Storable Rect where
  sizeOf    _ = 16
  alignment _ = 4

  peek ptr =
          pure Rect
      <*> peekField (Proxy @"topleft")     ptr
      <*> peekField (Proxy @"bottomright") ptr

  poke ptr Rect{topleft, bottomright} = do
      pokeField (Proxy @"topleft")     ptr topleft
      pokeField (Proxy @"bottomright") ptr bottomright

{-------------------------------------------------------------------------------
  (Optional) integration with 'HasField'
-------------------------------------------------------------------------------}

instance (HasCField Point field, ty ~ FieldType Point field)
      => GHC.HasField field (Ptr Point) (Ptr ty) where
  getField = ptrToField (Proxy @field)

instance (HasCField Rect field, ty ~ FieldType Rect field)
      => GHC.HasField field (Ptr Rect) (Ptr ty) where
  getField = ptrToField (Proxy @field)

{-------------------------------------------------------------------------------
  Function imports are unchanged (omitting CAPI for simplicity)
-------------------------------------------------------------------------------}

foreign import capi safe "cbits.h show_rect"
  show_rect :: Ptr Rect -> IO ()

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

newtype PointVsArray = PointVsArray ByteArray
  deriving Storable via SizedByteArray 8 4

get_pointVsArray_asArray :: PointVsArray -> ConstantArray 2 CInt
get_pointVsArray_asPoint :: PointVsArray -> Point
set_pointVsArray_asArray :: ConstantArray 2 CInt -> PointVsArray
set_pointVsArray_asPoint :: Point                -> PointVsArray

get_pointVsArray_asArray = ByteArray.getUnionPayload
get_pointVsArray_asPoint = ByteArray.getUnionPayload
set_pointVsArray_asArray = ByteArray.setUnionPayload
set_pointVsArray_asPoint = ByteArray.setUnionPayload

instance HasCField PointVsArray "asPoint" where
  type FieldType PointVsArray "asPoint" = Point
  ptrToField _ = castPtr

instance HasCField PointVsArray "asArray" where
  type FieldType PointVsArray "asArray" = ConstantArray 2 CInt
  ptrToField _ = castPtr

instance (HasCField PointVsArray field, ty ~ FieldType PointVsArray field)
      => GHC.HasField field (Ptr PointVsArray) (Ptr ty) where
  getField = ptrToField (Proxy @field)

