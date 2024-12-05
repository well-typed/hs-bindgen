
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module C.Type
  ( Platform(..), WordWidth(..)
  , Type(..)
  , ArithmeticType(..)
  , IntegralType(..)
  , CharLikeType(..)
  , IntLikeType(..)
  , Sign(..)
  , FloatLikeType(..)
  , IntegerConversionRank(..)
  , intLikeTypeSign
  , charLikeTypeSizeInBits
  , intLikeTypeSizeInBits
  , intLikeTypeConversionRank
  , showTypeAsCType
  ) where

-- base
import Control.Arrow
  ( second )

--------------------------------------------------------------------------------

data WordWidth = WordWidth32 | WordWidth64
  deriving stock ( Eq, Ord, Show )

data Platform = Platform { platformWordWidth :: !WordWidth }

data Type
  = Void
  | Arithmetic !ArithmeticType
  | Ptr        !Type
  | Struct     !String
  deriving stock ( Eq, Show )

data ArithmeticType
  = Integral  !IntegralType
  | FloatLike !FloatLikeType
  deriving stock ( Eq, Show )

data FloatLikeType = FloatType | DoubleType
  deriving stock ( Eq, Ord, Show )


data IntegralType
  = Bool
  | CharLike !CharLikeType
  | IntLike  !IntLikeType
  deriving stock ( Eq, Show )

data CharLikeType = Char | SChar | UChar
  deriving stock ( Eq, Ord, Show )

data Sign = Signed | Unsigned
  deriving stock ( Eq, Ord, Show )

data IntLikeType
  = Short    !Sign
  | Int      !Sign
  | Long     !Sign
  | LongLong !Sign
  | PtrDiff
  deriving stock ( Eq, Show )

newtype IntegerConversionRank = IntegerConversionRank Rational
  deriving stock ( Eq, Ord, Show )

intLikeTypeSign :: IntLikeType -> Sign
intLikeTypeSign = \case
  Short     s -> s
  Int       s -> s
  Long      s -> s
  LongLong  s -> s
  PtrDiff     -> Signed

charLikeTypeSizeInBits :: Platform -> CharLikeType -> Word
charLikeTypeSizeInBits _ = \case
  -- NB: this would need to change if we wanted to support
  -- platforms on which char is not 8 bits wide.
  Char  -> 8
  SChar -> 8
  UChar -> 8

intLikeTypeSizeInBits :: Platform -> IntLikeType -> Word
intLikeTypeSizeInBits plat i =
  case platformWordWidth plat of
    WordWidth32 ->
      case i of
        Short    {} -> 16
        Int      {} -> 32
        Long     {} -> 32
        LongLong {} -> 64
        PtrDiff     -> 32
    WordWidth64 ->
      case i of
        Short    {} -> 16
        Int      {} -> 32
        Long     {} -> 64
        LongLong {} -> 64
        PtrDiff     -> 64

intLikeTypeConversionRank :: Platform -> IntLikeType -> IntegerConversionRank
intLikeTypeConversionRank plat = IntegerConversionRank . \case
  -- Rules for integer conversion ranks:
  --
  --  1. No two signed integer types other than char and signed char (if char is signed)
  --     have the same rank, even if they have the same representation.
  --  2. The rank of a signed integer type is greater than the rank of any
  --     signed integer type with a smaller width.
  --  3. The ranks of char/short/int/long/long long increase in order.
  --  4. The rank of any unsigned integer type equals the rank of the
  --     corresponding signed integer type.
  --  5. The rank of any standard integer type is greater than the rank of
  --     any extended integer type with the same width.
  --  6. The rank of bool is less than the rank of all standard integer types.
  --  7. The rank of any extended signed integer type relative to another extended
  --     signed integer type with the same width is implementation-defined.

  -- Standard types.
  -- Implement (3), ignoring sign as per (4).
  Short     {} -> 3
  Int       {} -> 4
  Long      {} -> 5
  LongLong  {} -> 6

  -- Extended types.
  PtrDiff ->
    -- The following logic comes from (5).
    case platformWordWidth plat of
      WordWidth32 -> 3.9 -- greater than short, less than both int and long
      WordWidth64 -> 4.9 -- greater than int, less than both long and long long

--------------------------------------------------------------------------------

showTypeAsCType :: Type -> String -> String
showTypeAsCType ty x = finish $ go ty
  where
    finish ( tyStr, ptrs )
      | ptrs <= 0
      = tyStr ++ " " ++ x
      | otherwise
      = tyStr ++ " " ++ replicate ptrs '*' ++ x
    go :: Type -> ( String, Int )
    go = \case
      Void         -> ( "void", 0 )
      Arithmetic a -> ( showArithmeticTypeAsCType a, 0 )
      Ptr a        -> second ( +1 ) $ go a
      Struct str   -> ( str, 0 )

showArithmeticTypeAsCType :: ArithmeticType -> String
showArithmeticTypeAsCType = \case
  Integral i ->
    showIntegralTypeAsCType i
  FloatLike f ->
    case f of
      FloatType  -> "float"
      DoubleType -> "double"

showIntegralTypeAsCType :: IntegralType -> String
showIntegralTypeAsCType = \case
  Bool -> "bool"
  CharLike c ->
    case c of
      Char -> "char"
      SChar -> "signed char"
      UChar -> "unsigned char"
  IntLike i ->
    showIntLikeTypeAsCType i

showIntLikeTypeAsCType :: IntLikeType -> String
showIntLikeTypeAsCType = \case
  Short    s -> withSign s "short"
  Int      s -> withSign s "int"
  Long     s -> withSign s "long"
  LongLong s -> withSign s "long long"
  PtrDiff -> "ptrdiff_t"
  where
    withSign s = case s of
      Signed   -> id
      Unsigned -> ( "unsigned " ++ )
