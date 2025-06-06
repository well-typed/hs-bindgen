module HsBindgen.Hs.AST.Strategy (
    Strategy (..),
) where

import HsBindgen.Imports

-- | Deriving strategy
data Strategy ty =
    DeriveNewtype
  | DeriveStock
  | DeriveVia ty
  deriving stock (Generic, Show, Functor, Foldable, Traversable)
