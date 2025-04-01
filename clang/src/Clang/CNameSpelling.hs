module Clang.CNameSpelling (
    -- * Type
    CNameSpelling(..)
  ) where

import Data.String
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
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Eq, IsString, Ord, Show)
