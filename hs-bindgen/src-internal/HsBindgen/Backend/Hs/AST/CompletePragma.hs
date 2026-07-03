module HsBindgen.Backend.Hs.AST.CompletePragma (
    -- * Type
    CompletePragma(..)
  ) where

import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | @COMPLETE@ pragma
--
-- This corresponds to a @COMPLETE@ pragma listing the specified pattern names.
-- No type is specified.
data CompletePragma = CompletePragma {
      patterns :: [Hs.Name Hs.NsConstr]
    }
  deriving stock (Generic, Show)
