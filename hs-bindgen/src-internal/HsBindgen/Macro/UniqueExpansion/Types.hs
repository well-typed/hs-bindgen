-- |
--
-- Intended for unqualified import.
module HsBindgen.Macro.UniqueExpansion.Types (
    Definition (..)
  , Invocation (..)
  , Name (..)
  , Redefinition (..)
  , Ambiguity (..)
  ) where

import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)

import HsBindgen.Runtime.Macro qualified as Runtime.Macro

-- | A macro definition, reduced to what the ambiguity analysis needs
data Definition = Definition {
      name    :: Name
      -- | The names the definition depends on
      --
      -- The macro's own parameters are not dependencies, and have already been
      -- removed here.
    , deps    :: Set Name
      -- | The definition, with each token reduced to its spelling
      --
      -- Two definitions are identical iff their spellings are equal.
    , spelled :: Runtime.Macro.Raw Text
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

-- | May a macro that is defined more than once be treated as defined once?
data Redefinition =
    -- | All definitions are identical
    Benign
    -- | The definitions differ, or one of them could not be split
  | NotBenign
  deriving stock (Show, Eq)

-- | Does the expansion of a macro depend on where it is invoked?
data Ambiguity = Unambiguous | Ambiguous
  deriving stock (Show, Eq)
