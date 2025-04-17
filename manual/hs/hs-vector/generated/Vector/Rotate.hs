{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Vector.Rotate where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude (IO)
import qualified Vector

foreign import capi safe "vector_rotate.h vector_rotate" vector_rotate :: (F.Ptr Vector.Vector) -> FC.CDouble -> IO (F.Ptr Vector.Vector)
