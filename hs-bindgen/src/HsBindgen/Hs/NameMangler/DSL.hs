{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Building blocks for constructing name manglers
--
-- This does not depend on any other @NameMangler.*@ module.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Hs.NameMangler.DSL qualified as DSL
module HsBindgen.Hs.NameMangler.DSL (
    module X
  ) where

import HsBindgen.Hs.NameMangler.DSL.GenerateName  as X
import HsBindgen.Hs.NameMangler.DSL.Overrides     as X
import HsBindgen.Hs.NameMangler.DSL.ReservedNames as X
