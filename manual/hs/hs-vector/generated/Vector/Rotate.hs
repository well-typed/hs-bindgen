{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Vector.Rotate where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (Eq, IO, Show)
import qualified Vector

$(CAPI.addCSource "#include \"vector_rotate.h\"\nvector *VectorRotate_new_vector (double arg1, double arg2) { return new_vector(arg1, arg2); }\nvector *VectorRotate_vector_rotate (vector *arg1, double arg2) { return vector_rotate(arg1, arg2); }\n")

newtype Vector = Vector
  { un_Vector :: Vector.Vector
  }

deriving newtype instance F.Storable Vector

deriving stock instance Eq Vector

deriving stock instance Show Vector

foreign import ccall safe "VectorRotate_new_vector" new_vector :: FC.CDouble -> FC.CDouble -> IO (F.Ptr Vector)

foreign import ccall safe "VectorRotate_vector_rotate" vector_rotate :: (F.Ptr Vector) -> FC.CDouble -> IO (F.Ptr Vector)
