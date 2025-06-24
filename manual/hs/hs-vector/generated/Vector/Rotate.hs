{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Vector.Rotate where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)
import qualified Vector

$(CAPI.addCSource "#include \"vector_rotate.h\"\nvector *VectorRotate_vector_rotate (vector *arg1, double arg2) { return vector_rotate(arg1, arg2); }\n")

foreign import ccall safe "VectorRotate_vector_rotate" vector_rotate :: (F.Ptr Vector.Vector) -> FC.CDouble -> IO (F.Ptr Vector.Vector)
