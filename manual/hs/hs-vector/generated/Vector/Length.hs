{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Vector.Length where

import qualified Foreign as F
import Prelude (IO)
import qualified Vector
import qualified Vector.Types

foreign import capi safe "vector_length.h vector_length" vector_length :: (F.Ptr Vector.Vector) -> IO Vector.Types.Length
