-- | Pass definition
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.Pass" should be used.
--
-- Intended for unqualified import.
--
-- > import HsBindgen.IR.Pass.Definition
module HsBindgen.IR.Pass.Definition (
    -- * Definition
    Pass
    -- * Coercion
  , CoercePass(..)
  , CoercePassParam(..)
  ) where

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Kind of passes
--
-- Example:
--
-- > type Parsed :: Pass
-- > data Parsed a
--
-- This is an open kind, primarily to avoid 'HsBindgen.IR.Pass.IsPass'
-- orphans.
type Pass = PassSimulatedOpenKind -> Star

-- | Internal type used only to simulate an open kind. Not exported.
data PassSimulatedOpenKind

{-------------------------------------------------------------------------------
  Coercion
-------------------------------------------------------------------------------}

class CoercePass a p p' where
  coercePass :: a p -> a p'

class CoercePassParam a p p' where
  coercePassParam :: a p l -> a p' l
