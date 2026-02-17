module HsBindgen.Backend.Level (
    Level(..)
  ) where

-- | Separate things that live on the type and term level
data Level = LvlType | LvlTerm
  deriving stock (Eq, Ord, Show)
