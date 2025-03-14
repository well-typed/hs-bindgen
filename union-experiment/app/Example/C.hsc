module Example.C (
    Dim2(..)
  , Dim3(..)
  , DimPayload(..)
  , Dim(..)
  , set_DimPayload_dim2
  , set_DimPayload_dim3
  , get_DimPayload_dim2
  , get_DimPayload_dim3
  , max
  , grow
  ) where

import Prelude hiding (max)

import Data.Primitive.ByteArray
import Foreign
import Foreign.C
import System.IO.Unsafe

import Util.PeekPokeByteArray

#include "example.h"

{-------------------------------------------------------------------------------
  Dim2 and Dim3
-------------------------------------------------------------------------------}

data Dim2 = Dim2 CInt CInt
data Dim3 = Dim3 CInt CInt CInt

instance Storable Dim2 where
  sizeOf    _ = #size      Dim2_t
  alignment _ = #alignment Dim2_t

  peek ptr = do
      x <- (#peek Dim2_t, x) ptr
      y <- (#peek Dim2_t, y) ptr
      return (Dim2 x y)

  poke ptr (Dim2 x y) = do
      (#poke Dim2_t, x) ptr x
      (#poke Dim2_t, y) ptr y

instance Storable Dim3 where
  sizeOf    _ = #size      Dim3_t
  alignment _ = #alignment Dim3_t

  peek ptr = do
      x <- (#peek Dim3_t, x) ptr
      y <- (#peek Dim3_t, y) ptr
      z <- (#peek Dim3_t, z) ptr
      return (Dim3 x y z)

  poke ptr (Dim3 x y z) = do
      (#poke Dim3_t, x) ptr x
      (#poke Dim3_t, y) ptr y
      (#poke Dim3_t, z) ptr z

{-------------------------------------------------------------------------------
  DimPayload
-------------------------------------------------------------------------------}

newtype DimPayload = UnsafeDimPayload {
      getDimPayload :: ByteArray
    }

mkDimPayload :: ByteArray -> DimPayload
mkDimPayload bytes
  | sizeofByteArray bytes == (#size DimPayload_t)
  = UnsafeDimPayload bytes

  | otherwise
  = error $ concat [
        "mkDimPayload: expected "
      , show ((#size DimPayload_t) :: CInt)
      , " bytes, but got "
      , show (sizeofByteArray bytes)
      ]

instance Storable DimPayload where
  sizeOf    _ = #size      DimPayload_t
  alignment _ = #alignment DimPayload_t

  peek = \ptr -> mkDimPayload <$> peekByteArray ptr (#size DimPayload_t)
  poke = \ptr -> pokeByteArray ptr . getDimPayload

set_DimPayload_dim2 :: Dim2 -> DimPayload
set_DimPayload_dim2 = setUnionPayload

set_DimPayload_dim3 :: Dim3 -> DimPayload
set_DimPayload_dim3 = setUnionPayload

get_DimPayload_dim2 :: DimPayload -> Dim2
get_DimPayload_dim2 = getUnionPayload

get_DimPayload_dim3 :: DimPayload -> Dim3
get_DimPayload_dim3 = getUnionPayload

{-------------------------------------------------------------------------------
  Dim
-------------------------------------------------------------------------------}

data Dim = Dim {
      tag     :: CInt
    , payload :: DimPayload
    }

instance Storable Dim where
  sizeOf    _ = #size      Dim_t
  alignment _ = #alignment Dim_t

  peek ptr = do
      tag     <- (#peek Dim_t, tag)     ptr
      payload <- (#peek Dim_t, payload) ptr
      return Dim{tag, payload}

  poke ptr Dim{tag, payload} = do
      (#poke Dim_t, tag)     ptr tag
      (#poke Dim_t, payload) ptr payload

{-------------------------------------------------------------------------------
  Foreign imports
-------------------------------------------------------------------------------}

foreign import capi "example.h dim_max"
  c_dim_max :: Ptr Dim -> IO CInt

foreign import capi "example.h dim_grow"
  c_dim_grow :: Ptr Dim -> Ptr Dim -> IO ()

{-------------------------------------------------------------------------------
  Wrapper functions
-------------------------------------------------------------------------------}

max :: Dim -> CInt
max inp = unsafePerformIO $
    with inp $ \inpPtr -> c_dim_max inpPtr

grow :: Dim -> Dim
grow inp = unsafePerformIO $
    with inp $ \inpPtr ->
    alloca   $ \outPtr -> do
      c_dim_grow inpPtr outPtr
      peek outPtr

