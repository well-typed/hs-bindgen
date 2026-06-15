module C.Expr.Syntax.Literal (
    IntegerLiteral(..)
  , FloatingLiteral(..)
  , CharLiteral(..)
  , StringLiteral(..)
    -- * Auxiliary
  , canBeRepresentedAsRational
  ) where

import Data.ByteString (ByteString)
import Foreign.C (CChar)
import GHC.Generics (Generic)

import C.Type qualified as Runtime

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Integer literal
data IntegerLiteral =
  IntegerLiteral {
      -- | The type of the integer literal, as determined from suffixes.
      integerLiteralType  :: Runtime.IntLikeType

      -- | The (parsed) value of the literal
    , integerLiteralValue :: Integer
    }
  deriving stock ( Eq, Ord, Show, Generic )

-- | Floating-point literal
data FloatingLiteral =
  FloatingLiteral {
      -- | The type of the floating-point literal, as determined from suffixes.
      floatingLiteralType  :: Runtime.FloatingType

      -- | The (parsed) value of the literal, when parsed as a single precision
      -- floating-point value.
    , floatingLiteralFloatValue :: Float

      -- | The (parsed) value of the literal, when parsed as a double precision
      -- floating-point value.
    , floatingLiteralDoubleValue :: Double
    }
  deriving stock ( Eq, Ord, Show, Generic )

-- | A C character literal.
--
-- The value is represented as a 'CChar'. Wide character literals (prefixed
-- with @L@, @u@, @U@, or @u8@) and characters whose value does not fit in a
-- single byte are rejected during parsing (see
-- 'C.Expr.Parse.Literal.parseLiteralChar').
newtype CharLiteral = CharLiteral { charLiteralValue :: CChar }
  deriving stock ( Eq, Ord, Show, Generic )

-- | A C string literal.
--
-- 'stringLiteralValue' holds the /execution-encoding bytes/ of the literal,
-- assuming a UTF-8 execution character set.  The representation is
-- /bit-for-bit accurate/: the bytes are exactly what a C compiler targeting a
-- UTF-8 execution charset would embed in the object file.  This property is
-- required because the generated Haskell bindings may be passed directly to C
-- functions that expect the same byte sequence (see
-- 'C.Expr.Parse.Literal.parseLiteralString' for the encoding rules).
newtype StringLiteral = StringLiteral { stringLiteralValue :: ByteString }
  deriving stock ( Eq, Ord, Show, Generic )

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

{-# SPECIALISE canBeRepresentedAsRational :: Float  -> Bool #-}
{-# SPECIALISE canBeRepresentedAsRational :: Double -> Bool #-}

-- | Can this floating-point value be represented (losslessly) as a 'Rational'?
canBeRepresentedAsRational :: RealFloat a => a -> Bool
canBeRepresentedAsRational f = not $ or
  [ isNaN f
  , isInfinite f
  , isNegativeZero f
  , isDenormalized f -- not strictly necessary, but let's be conservative
  ]
