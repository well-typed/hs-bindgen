module HsBindgen.Backend.Hs.AST.Strategy (
    Strategy (..),
) where

import HsBindgen.Imports

-- | Deriving strategy
data Strategy ty =
    DeriveNewtype
  | DeriveStock
  | DeriveVia ty
  deriving stock (Generic, Show, Eq, Functor, Foldable, Traversable)
