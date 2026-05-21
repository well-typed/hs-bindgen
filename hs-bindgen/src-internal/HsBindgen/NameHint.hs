module HsBindgen.NameHint (
    NameHint(..)
  , toNameHint
  ) where

import HsBindgen.Language.Haskell qualified as Hs

-- | Hint used to render bound variables
--
-- Invariant: the name hint must be valid Haskell for its intended context.
newtype NameHint = NameHint String
deriving newtype instance Show NameHint

toNameHint :: Hs.Name 'Hs.NsVar -> NameHint
toNameHint = NameHint . Hs.nameToStr
