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
import Prelude (Eq, IO, Show)
import qualified Vector
import qualified Vector.Types

$(CAPI.addCSource "#include \"vector_length.h\"\nvector *VectorLength_new_vector (double arg1, double arg2) { return new_vector(arg1, arg2); }\nlen VectorLength_vector_length (vector *arg1) { return vector_length(arg1); }\n")

newtype Vector = Vector
  { un_Vector :: Vector.Vector
  }

deriving newtype instance F.Storable Vector

deriving stock instance Eq Vector

deriving stock instance Show Vector

foreign import ccall safe "VectorLength_new_vector" new_vector :: FC.CDouble -> FC.CDouble -> IO (F.Ptr Vector)

foreign import ccall safe "VectorLength_vector_length" vector_length :: (F.Ptr Vector) -> IO Vector.Types.Length
