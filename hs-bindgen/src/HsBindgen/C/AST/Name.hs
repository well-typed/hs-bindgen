-- | Names
--
-- This is re-exported in "HsBindgen.C.AST".
module HsBindgen.C.AST.Name (
    CName(..)
  ) where

import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

-- | Name appearing in the C source
newtype CName = CName { getCName :: Text }
  -- 'Show' instance valid due to 'IsString' instance
  deriving newtype (Show, Eq, Ord, IsString)
  deriving stock (Generic)
  deriving anyclass (PrettyVal)
