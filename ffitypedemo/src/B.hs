module B where

import Foreign
import Runtime
import A

newtype B = B { unB :: A }
  deriving stock (Show)
  deriving newtype (Num, Storable)

instance HasFFIType B where
  type FFIType B = Int
  coerceFFI = coerceFFI . unB
