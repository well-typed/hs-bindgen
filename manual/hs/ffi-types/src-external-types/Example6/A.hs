module Example6.A (A(..), CInt(..)) where

import Foreign.C.Types

import HsBindgen.Runtime.HasFFIType

newtype A = A CInt

instance HasFFIType A where
  type FFIType A = A
  toFFIType = id
  fromFFIType = id
