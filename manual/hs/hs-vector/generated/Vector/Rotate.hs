{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Vector.Rotate where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude (IO)
import qualified Vector

-- #include "vector_rotate.h"
-- vector *Vector.Rotate_vector_rotate (vector *arg1, double arg2);

foreign import capi safe "vector_rotate.h vector_rotate" vector_rotate :: (F.Ptr Vector.Vector) -> FC.CDouble -> IO (F.Ptr Vector.Vector)
