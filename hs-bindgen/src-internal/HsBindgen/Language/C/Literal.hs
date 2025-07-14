-- | C literal types
--
-- Not intended for direct import; import via @HsBindgen.Language.C@.
module HsBindgen.Language.C.Literal (
  -- * Integer literals
    IntegerLiteral(..)
  -- * Floating-point literals
  , FloatingLiteral(..), canBeRepresentedAsRational
  -- * Character and string literals
  , CharLiteral(..)
  , StringLiteral(..)
  , fromBytes
  ) where

import Data.Bits

import C.Char qualified
import C.Type qualified

import HsBindgen.Imports

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
    , integerLiteralType  :: C.Type.IntLikeType

      -- | The (parsed) value of the literal
    , integerLiteralValue :: Integer
    }
  deriving stock ( Eq, Ord, Show, Generic )

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
    , floatingLiteralType  :: C.Type.FloatingType

      -- | The (parsed) value of the literal, when parsed as a single precision
      -- floating-point value.
    , floatingLiteralFloatValue :: Float

      -- | The (parsed) value of the literal, when parsed as a double precision
      -- floating-point value.
    , floatingLiteralDoubleValue :: Double
    }
  deriving stock ( Eq, Ord, Show, Generic )

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

{-------------------------------------------------------------------------------
  Character and string literals
-------------------------------------------------------------------------------}

-- | A C character literal, with the original source text.
--
-- See 'CharLiteralValue'.
data CharLiteral =
  CharLiteral
    { charLiteralText :: Text
    , charLiteralValue :: C.Char.CharValue
    }
  deriving stock ( Eq, Ord, Show, Generic )

data StringLiteral =
  StringLiteral
    { stringLiteralText :: Text
    , stringLiteralValue :: [ C.Char.CharValue ]
    }
  deriving stock ( Eq, Ord, Show, Generic )

fromBytes :: Bits i => [i] -> i
fromBytes = foldl' (\ acc b -> ( acc `shiftL` 8 ) .|. b) zeroBits
{-# INLINEABLE fromBytes #-}
