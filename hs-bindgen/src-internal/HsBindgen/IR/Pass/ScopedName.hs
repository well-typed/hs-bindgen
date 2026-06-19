-- | Scoped names
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.Pass" should be used.
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Pass.ScopedName
module HsBindgen.IR.Pass.ScopedName (
    -- * Associated type families
    PassScopedName(..)
  ) where

import HsBindgen.Imports
import HsBindgen.IR.C.Naming qualified as C
import HsBindgen.IR.Pass.Definition

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

-- | Scoped names vary across passes
class (
      Eq   (ScopedName p)  -- For @Eq DeclKind@ in @DeclIndex@ construction
    , Ord  (ScopedName p)  -- For using as map key
    , Show (ScopedName p)  -- For debugging
    ) => PassScopedName (p :: Pass) where

  -- | Scoped name
  --
  -- This represents a name within a local scope, such as the name of a struct
  -- fields, a function argument, etc.
  --
  -- 1. After 'HsBindgen.Frontend.Pass.Parse.IsPass.Parse', this is
  --   'C.ScopedName'.
  -- 2. After 'HsBindgen.Frontend.Pass.MangleNames.IsPass.MangleNames', this is
  --   'C.ScopedNamePair'.
  type ScopedName p :: Star
  type ScopedName p = C.ScopedName
