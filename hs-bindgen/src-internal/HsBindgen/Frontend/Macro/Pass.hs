module HsBindgen.Frontend.Macro.Pass (
    Pass
  , Ps
    -- * TTG-style type families
  , XApp(..)
  , XVar(..)
  ) where

import Data.Kind
import GHC.Generics (Generic)

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

{-------------------------------------------------------------------------------
  First pass: parser
-------------------------------------------------------------------------------}

type Ps :: Pass
data Ps a

data instance XApp Ps = NoXApp
  deriving stock ( Eq, Ord, Show, Generic )
data instance XVar Ps = NoXVar
  deriving stock ( Eq, Ord, Show, Generic )
