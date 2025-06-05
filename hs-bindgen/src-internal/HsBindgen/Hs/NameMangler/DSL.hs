{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Building blocks for constructing name manglers
--
-- Intended for qualified import.
--
-- > import HsBindgen.Hs.NameMangler.DSL qualified as DSL
module HsBindgen.Hs.NameMangler.DSL (
    module X
  ) where

import HsBindgen.Hs.NameMangler.DSL.FixCandidate   as X
import HsBindgen.Hs.NameMangler.DSL.ReservedNames  as X
