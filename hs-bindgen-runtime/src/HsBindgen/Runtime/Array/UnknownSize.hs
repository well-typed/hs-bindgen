module HsBindgen.Runtime.Array.UnknownSize (
    Array (..)
  ) where

import Data.Vector.Storable qualified as VS

type role Array nominal

-- | An immutable C array of unknown size
newtype Array a = Array (VS.Vector a)
  deriving stock (Eq, Show)

