module HsBindgen.Util.List (
    (!?)
  ) where

import Data.Maybe (listToMaybe)

--------------------------------------------------------------------------------

-- | List index (subscript) operator, starting from 0
--
-- Returns 'Nothing' if the index is out of bounds
(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n < 0     = Nothing
    | otherwise = listToMaybe $ drop n xs
infixl 9 !?
{-# INLINABLE (!?) #-}
