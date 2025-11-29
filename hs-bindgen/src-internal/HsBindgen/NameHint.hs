module HsBindgen.NameHint (
    NameHint(..)
  ) where

import Data.Char (toLower)

import HsBindgen.Imports

newtype NameHint = NameHint String
deriving newtype instance Show NameHint

-- | This instance tries to make name hints valid variable names in Haskell.
instance IsString NameHint where
    fromString []     = NameHint "x"
    fromString (x:xs) = NameHint (toLower x : xs)

-- | All name hints are equal.
instance Eq NameHint where
    _ == _ = True
