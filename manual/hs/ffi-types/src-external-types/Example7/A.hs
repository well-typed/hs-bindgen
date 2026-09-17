{-# LANGUAGE UndecidableInstances #-}

module Example7.A (A(..)) where

import Foreign.C.Types

import HsBindgen.Runtime.HasFFIType

newtype A = A CInt
  deriving newtype HasFFIType
