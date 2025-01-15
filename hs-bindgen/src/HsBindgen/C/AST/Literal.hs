module HsBindgen.C.AST.Literal (
    IntegerLiteral(..), FloatingLiteral(..), canBeRepresentedAsRational
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

import HsBindgen.C.AST.Type

{-------------------------------------------------------------------------------
  Integer literals
-------------------------------------------------------------------------------}

-- | Integer literal
data IntegerLiteral =
  IntegerLiteral {
      -- | The representation of the literal in the original source
      --
      -- We include this to generate better bindings and better documentation.
      -- For example, flags specified in hexadecimal would become quite
      -- unreadable in decimal.
      integerLiteralText  :: Text

      -- | The type of the integer literal, as determined from suffixes.
    , integerLiteralType  :: Maybe (PrimIntType, PrimSign)
        -- TODO: re-use 'IntegralType' from @c-expr@ library.

      -- | The (parsed) value of the literal
    , integerLiteralValue :: Integer
    }
  deriving stock ( Eq, Show, Generic )
  deriving anyclass PrettyVal

{-------------------------------------------------------------------------------
  Floating-point literals
-------------------------------------------------------------------------------}

-- | Floating-point literal
data FloatingLiteral =
  FloatingLiteral {
      -- | The representation of the literal in the original source
      --
      -- We include this to generate better bindings and better documentation.
      floatingLiteralText  :: Text

      -- | The type of the floating-point literal, as determined from suffixes.
    , floatingLiteralType  :: Maybe PrimFloatType

      -- | The (parsed) value of the literal, when parsed as a single precision
      -- floating-point value.
    , floatingLiteralFloatValue :: Float

      -- | The (parsed) value of the literal, when parsed as a double precision
      -- floating-point value.
    , floatingLiteralDoubleValue :: Double
    }
  deriving stock ( Eq, Show, Generic )
  deriving anyclass PrettyVal

{-# SPECIALISE canBeRepresentedAsRational :: Float -> Bool #-}
{-# SPECIALISE canBeRepresentedAsRational :: Double -> Bool #-}
-- | Can this floating-point value be represented (losslessly) as a 'Rational'?
canBeRepresentedAsRational :: RealFloat a => a -> Bool
canBeRepresentedAsRational f = not $ or
  [ isNaN f
  , isInfinite f
  , isNegativeZero f
  , isDenormalized f -- not strictly necessary, but let's be conservative
  ]
