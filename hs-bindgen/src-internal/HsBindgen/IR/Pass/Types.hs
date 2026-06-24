-- | Types (use sites)
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.Pass" should be used.
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Pass.Types
module HsBindgen.IR.Pass.Types (
    -- * Associated type families
    PassTypes(..)
    -- * Coercion
  , CoercePassTypes(..)
  ) where

import HsBindgen.Imports
import HsBindgen.IR.C.Type qualified as C
import HsBindgen.IR.Pass.Definition

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

-- | Types (use sites) vary across passes
class (
      Eq   (Types p)
    , Show (Types p)  -- For debugging
    ) => PassTypes (p :: Pass) where

  -- | Types (use sites)
  --
  -- 1. After 'HsBindgen.Frontend.Pass.Parse.IsPass.Parse', this is
  --    @'C.Type' p@.
  type Types p :: Star
  type Types p = C.Type p

  -- | C type
  cType :: Proxy p -> Types p -> C.Type p
  default cType :: Types p ~ C.Type p => Proxy p -> Types p -> C.Type p
  cType _ = id

{-------------------------------------------------------------------------------
  Coercion
-------------------------------------------------------------------------------}

class CoercePassTypes (p :: Pass) (p' :: Pass) where
  coercePassTypes :: Proxy '(p, p') -> Types p -> Types p'

  default coercePassTypes ::
       (CoercePass C.Type p p', Types p ~ C.Type p, Types p' ~ C.Type p')
    => Proxy '(p, p') -> Types p -> Types p'
  coercePassTypes _ = coercePass
