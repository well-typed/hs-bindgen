-- | External binding annotations
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.Pass" should be used.
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Pass.ExtBinding
module HsBindgen.IR.Pass.ExtBinding (
    -- * Associated type families
    PassExtBinding(..)
  ) where

import HsBindgen.Imports
import HsBindgen.IR.Pass.Definition
import HsBindgen.IR.Pass.Id

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

-- | External binding annotations vary across passes
class (
      Eq   (ExtBinding p)  -- For @Eq DeclKind@ in @DeclIndex@ construction
    , Ord  (ExtBinding p)  -- For de-duplicating types
    , Show (ExtBinding p)  -- For debugging
    ) => PassExtBinding (p :: Pass) where

  -- | External binding specification
  --
  -- 1. Before
  --   'HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass.ResolveBindingSpecs',
  --   this is 'Void'.
  -- 2. After
  --   'HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass.ResolveBindingSpecs',
  --   this is 'ResolvedExtBinding'.
  type ExtBinding p :: Star
  type ExtBinding p = Void

  extBindingId :: Proxy p -> ExtBinding p -> Id p
  default extBindingId :: ExtBinding p ~ Void => Proxy p -> ExtBinding p -> Id p
  extBindingId _ = absurd
