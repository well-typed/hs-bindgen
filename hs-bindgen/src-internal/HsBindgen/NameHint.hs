module HsBindgen.NameHint (
    NameHint(..)
  ) where

-- | Hint used to render bound variables
--
-- Invariant: the name hint must be valid Haskell for its intended context.
newtype NameHint = NameHint String
deriving newtype instance Show NameHint
