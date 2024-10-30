module HsBindgen.C.AST.Literal (
    Literal(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Literal
data Literal a = Literal {
      -- | The representation of the literal in the original source
      --
      -- We include this to generate better bindings and better documentation.
      -- For example, flags specified in hexadecimal would become quite
      -- unreadable in decimal.
      literalText  :: Text

      -- | The (parsed) value of the literal
    , literalValue :: a
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

