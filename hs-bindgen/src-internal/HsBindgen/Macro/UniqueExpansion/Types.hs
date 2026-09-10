-- |
--
-- Intended for unqualified import.
module HsBindgen.Macro.UniqueExpansion.Types (
    Definition
  , Invocation (..)
  , Name (..)
  ) where

import Data.String (IsString)
import Data.Text (Text)

import HsBindgen.Runtime.Macro qualified as RawMacro

-- | A macro definition, reduced to the names it mentions
--
-- The body is the list of identifiers occurring in it; the ambiguity analysis
-- is not interested in anything else. Whether a name in the body refers to
-- another macro or to a parameter of this one follows from 'RawMacro.params'.
type Definition = RawMacro.Raw Name

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
