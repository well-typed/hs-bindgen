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
  , CoercePassAnonRef(..)
  ) where

import HsBindgen.Imports
import HsBindgen.IR.C.Type qualified as C
import HsBindgen.IR.Pass.Definition

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

-- | Types (use sites) vary across passes
class (
      Eq   (AnonRef p)
    , Eq   (Types p)

    -- For debugging
    , Show (AnonRef p)
    , Show (Types p)
    ) => PassTypes (p :: Pass) where

  -- | Types (use sites)
  --
  -- 1. After 'HsBindgen.Frontend.Pass.Parse.IsPass.Parse', this is
  --    @'C.Type' p@.
  type Types p :: Star
  type Types p = C.Type p

  type AnonRef p :: Star
  type AnonRef p = C.AnonRef p

  -- | C type
  cType :: Proxy p -> Types p -> C.Type p
  default cType :: Types p ~ C.Type p => Proxy p -> Types p -> C.Type p
  cType _ = id

  anonRefTypes :: Proxy p -> AnonRef p -> Types p
  default anonRefTypes ::
       (AnonRef p ~ C.AnonRef p, Types p ~ C.Type p)
    => Proxy p
    -> AnonRef p
    -> Types p
  anonRefTypes _ = \case
      C.AnonRef ref -> C.TypeRef ref
      C.AnonExtBinding ext -> C.TypeExtBinding ext

{-------------------------------------------------------------------------------
  Coercion
-------------------------------------------------------------------------------}

class CoercePassTypes (p :: Pass) (p' :: Pass) where
  coercePassTypes :: Proxy '(p, p') -> Types p -> Types p'

  default coercePassTypes ::
       (CoercePass C.Type p p', Types p ~ C.Type p, Types p' ~ C.Type p')
    => Proxy '(p, p') -> Types p -> Types p'
  coercePassTypes _ = coercePass

class CoercePassAnonRef (p :: Pass) (p' :: Pass) where
  coercePassAnonRef :: Proxy '(p, p') -> AnonRef p -> AnonRef p'

  default coercePassAnonRef ::
       (CoercePass C.AnonRef p p', AnonRef p ~ C.AnonRef p, AnonRef p' ~ C.AnonRef p')
    => Proxy '(p, p') -> AnonRef p -> AnonRef p'
  coercePassAnonRef _ = coercePass
