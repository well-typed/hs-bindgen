{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Vector.Length where

import qualified Foreign as F
import Prelude (IO)
import qualified Vector
import qualified Vector.Types

-- #include "vector_length.h"
-- len Vector.Length_vector_length (vector *arg1);

foreign import capi safe "vector_length.h vector_length" vector_length :: (F.Ptr Vector.Vector) -> IO Vector.Types.Length
