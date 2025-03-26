module Clang.CNameSpelling (
    -- * Type
    CNameSpelling(..)
  ) where

import Data.Text (Text)

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | C name spelling
--
-- A value must specify @struct@, @union@, or @enum@ when required.
--
-- Examples: @int8_t@, @struct tm@
newtype CNameSpelling = CNameSpelling { getCNameSpelling :: Text }
  deriving newtype (Eq, Ord, Show)
