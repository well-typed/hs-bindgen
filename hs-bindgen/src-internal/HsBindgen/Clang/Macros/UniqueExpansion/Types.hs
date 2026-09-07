-- |
--
-- Intended for unqualified import.
module HsBindgen.Clang.Macros.UniqueExpansion.Types (
    Definition (..)
  , Var (..)
  , Invocation (..)
  , Name (..)
  ) where

import Data.String (IsString)
import Data.Text (Text)

data Definition = Definition {
      name     :: Name
    , params   :: [Name]
      -- | A macro definition is variadic if it has @...@ at the end of its
      -- parameter list
      --
      -- Variadic macros are technically a C99 feature, but @libclang@ has
      -- backported them to C89 as well. We follow @libclang@ behaviour here,
      -- and support variadic macros regardless of the C standard that is
      -- configured (because C89 is the first standard).
      --
      -- <https://clang.llvm.org/docs/LanguageExtensions.html#language-extensions-back-ported-to-previous-standards>
    , variadic :: Bool
    , body     :: [Var]
    }
  deriving stock (Show, Eq)

data Var =
    LocalParam Name
  | FreeVar Name
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
