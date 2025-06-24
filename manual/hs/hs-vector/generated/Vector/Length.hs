{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Vector.Length where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (Enum, Eq, Floating, Fractional, IO, Num, Ord, Read, Real, RealFloat, RealFrac, Show)
import qualified Vector

$(CAPI.addCSource "#include \"vector_length.h\"\nlen VectorLength_vector_length (vector *arg1) { return vector_length(arg1); }\n")

newtype Len = Len
  { un_Len :: FC.CDouble
  }

deriving newtype instance F.Storable Len

deriving stock instance Eq Len

deriving stock instance Ord Len

deriving stock instance Read Len

deriving stock instance Show Len

deriving newtype instance Enum Len

deriving newtype instance Floating Len

deriving newtype instance Fractional Len

deriving newtype instance Num Len

deriving newtype instance Real Len

deriving newtype instance RealFloat Len

deriving newtype instance RealFrac Len

foreign import ccall safe "VectorLength_vector_length" vector_length :: (F.Ptr Vector.Vector) -> IO Len
