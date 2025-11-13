module C where

import B
import Foreign
import Runtime

newtype C = C { unC :: B }
  deriving stock (Show)
  deriving newtype (Num, Storable)

instance HasFFIType C where
  type FFIType C = Int
  coerceFFI = coerceFFI . unC
