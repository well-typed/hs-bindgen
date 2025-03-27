module HsBindgen.Hs.NameMangler (
    module X
    -- * Standard instances
  , nameManglerDefault
  , nameManglerHaskell
  ) where

import HsBindgen.Hs.NameMangler.API as X
import HsBindgen.Hs.NameMangler.DSL qualified as DSL

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

-- | Default name mangler
--
-- With this name mangler, names are changed as little as possible.
nameManglerDefault :: NameMangler' Maybe
nameManglerDefault =
    DSL.nameManglerFromCandidates
      DSL.produceCandidateDefault
      DSL.fixCandidateDefault

-- | Haskell-style name mangler
--
-- Provides Haskell-style names with a higher risk of name collision.
nameManglerHaskell :: NameMangler' Maybe
nameManglerHaskell =
    DSL.nameManglerFromCandidates
      DSL.produceCandidateHaskell
      DSL.fixCandidateHaskell
