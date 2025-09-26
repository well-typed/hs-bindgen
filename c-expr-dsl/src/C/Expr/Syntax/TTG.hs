module C.Expr.Syntax.TTG (
    Pass
    -- * TTG-style type families
  , XApp
  , XVar
  ) where

import Data.Kind

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Kind of passes
--
-- Example:
--
-- > type Ps :: Pass
-- > data Ps a
type Pass = PassSimulatedOpenKind -> Type

-- | Internal type used only to simulate an open kind. Not exported.
data PassSimulatedOpenKind

{-------------------------------------------------------------------------------
  TTG-style type families
-------------------------------------------------------------------------------}

type XApp :: Pass -> Type
type XVar :: Pass -> Type

data family XApp p
data family XVar p
