module HsBindgen.NameHint where

import HsBindgen.Imports
import Data.Char (toLower)

newtype NameHint = NameHint String
deriving newtype instance Show NameHint

-- | This instance tries to make name hints valid variable names in Haskell.
instance IsString NameHint where
    fromString []     = NameHint "x"
    fromString (x:xs) = NameHint (toLower x : xs)

-- | All name hints are equal.
instance Eq NameHint where
    _ == _ = True
