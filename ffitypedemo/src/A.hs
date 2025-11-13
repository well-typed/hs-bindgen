module A where

import Foreign
import Runtime

newtype A = A { unA :: Int }
  deriving stock (Show)
  deriving newtype (Num, Storable)

instance HasFFIType A where
  type FFIType A = Int
  coerceFFI = coerceFFI . unA

