module HsBindgen.C.Raw.Pass (
    Pass
  , IsPass(..)
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
type Pass = PassSimulatedOpenKind -> Star

-- | Internal type used only to simulate an open kind. Not exported.
data PassSimulatedOpenKind

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

-- | Pass definition
class ( Show (Id p)
      ) => IsPass (p :: Pass) where
  -- | Previous pass ('None' if this is the first pass)
  type Previous p :: Pass
  type Previous p = None

  -- | Identity of declarations
  type Id p :: Star
  type Id p = Id (Previous p)

type None :: Pass
data None a


