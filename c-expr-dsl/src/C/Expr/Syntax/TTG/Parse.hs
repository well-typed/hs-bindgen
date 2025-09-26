module C.Expr.Syntax.TTG.Parse (
    Ps
  , XApp(..)
  , XVar(..)
  ) where

import GHC.Generics (Generic)

import C.Expr.Syntax.TTG

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Ps :: Pass
data Ps a

{-------------------------------------------------------------------------------
  Pass-indexed type families
-------------------------------------------------------------------------------}

data instance XApp Ps = NoXApp deriving stock ( Eq, Ord, Show, Generic )
data instance XVar Ps = NoXVar deriving stock ( Eq, Ord, Show, Generic )
