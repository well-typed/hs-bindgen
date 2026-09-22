-- |
--
-- Intended for unqualified import.
module HsBindgen.Macro.UniqueExpansion.Types (
    Definition (..)
  , Invocation (..)
  , Name (..)
  ) where

import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)

-- | A macro definition, reduced to the names it depends on
--
-- The ambiguity analysis is not interested in anything else the body contains.
-- The macro's own parameters are not dependencies, and have already been
-- removed here.
data Definition = Definition {
      name :: Name
    , deps :: Set Name
    }
  deriving stock (Show, Eq)

data Invocation = Invocation {
      name   :: Name
      -- | All names used in the argument list
      --
      -- The number of names here does /not/ have any relationship with the
      -- number of parameters for the macro definition that is being invoked. It
      -- is simply a collection of all names referenced anywehere in the
      -- invocation's argument list.
    , args   :: [Name]
    }
  deriving stock (Show, Eq)

newtype Name = Name {
      unwrap :: Text
    }
  deriving newtype (Show, Eq, Ord)
  deriving newtype IsString
