module HsBindgen.Runtime.Array.KnownSize (
    Array (..)
  ) where

import Data.Vector.Storable qualified as VS
import GHC.TypeNats (Nat)

type role Array nominal nominal

-- | An immutable C array of known size
newtype Array (n :: Nat) a = Array (VS.Vector a)
  deriving stock (Eq, Show)

