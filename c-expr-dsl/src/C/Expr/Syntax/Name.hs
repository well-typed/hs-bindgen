module C.Expr.Syntax.Name (
    Name(..)
  ) where

import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Macro arguments
newtype Name = Name {
      getName :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString, Semigroup)
  deriving stock (Generic)
