module Runtime where

import Data.Kind
import Foreign

class HasFFIType a where
  type FFIType a :: Type
  type FFIType a = a

  coerceFFI :: a -> FFIType a

  default coerceFFI :: (a ~ FFIType a) => a -> FFIType a
  coerceFFI = id

instance HasFFIType Int
-- other instances for marshallable types

instance HasFFIType (Ptr a)

