{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Building blocks for constructing name manglers
--
-- Intended for qualified import.
--
-- > import HsBindgen.Hs.NameMangler.DSL qualified as DSL
module HsBindgen.Hs.NameMangler.DSL (
    module X
  , nameManglerFromCandidates
  ) where

import HsBindgen.Hs.NameMangler.DSL.FixCandidate     as X
import HsBindgen.Hs.NameMangler.DSL.Overrides        as X
import HsBindgen.Hs.NameMangler.DSL.ProduceCandidate as X
import HsBindgen.Hs.NameMangler.DSL.ReservedNames    as X

import HsBindgen.Hs.NameMangler.API

{-------------------------------------------------------------------------------
  Constructing name manglers
-------------------------------------------------------------------------------}

-- | Construct 'NameMangler'
--
-- This is one particular way to construct name manglers. It proceeds in two
-- steps:
--
-- 1. Produce a candidate name
-- 2. Fix that candidate to conform to the specific rules for names in the
--    relevant name space.
nameManglerFromCandidates :: forall m.
     Monad m
  => ProduceCandidate
  -> FixCandidate m
  -> NameMangler' m
nameManglerFromCandidates produce fix = nameMangler
  where
    -- This definition is recursive, because producing candidates for one
    -- kind of 'NameSpec' may rely on generating names for another.
    nameMangler :: NameMangler' m
    nameMangler = NameMangler $ \spec -> do
         candidate <- produceCandidate nameMangler produce spec
         fixCandidate fix candidate
