{-# LANGUAGE ScopedTypeVariables #-}

module C.Type
  ( -- * C types
    Type(..)
  , ArithmeticType(..)
  , IntegralType(..)
  , CharLikeType(..)
  , IntLikeType(..)
  , Sign(..)
  , FloatingType(..)
  , IntegerConversionRank(..)
  , intLikeTypeSign
  , charLikeTypeSizeInBits
  , intLikeTypeSizeInBits
  , intLikeTypeConversionRank
  , intLikeTypeFitsInInt
  , showTypeAsCType

  -- * Platform
  , Platform(..), WordWidth(..), OS(..)
  , hostPlatform

  ) where

-- base
import Data.Semigroup
  ( Arg(..) )
import Foreign.Ptr qualified as Foreign
  ( Ptr )
import Foreign.Storable
  ( sizeOf )
import GHC.Generics
  ( Generic )
import System.Info qualified
  ( os )

--------------------------------------------------------------------------------

data Type a
  = Void
  | Arithmetic !ArithmeticType
  | Ptr        !a
  deriving stock ( Eq, Ord, Show, Functor, Foldable, Traversable, Generic )

data ArithmeticType
  = Integral  !IntegralType
  | FloatLike !FloatingType
  deriving stock ( Eq, Ord, Show, Generic )

data FloatingType = FloatType | DoubleType
  deriving stock ( Eq, Ord, Show, Generic )

data IntegralType
  = Bool
  | CharLike !CharLikeType
  | IntLike  !IntLikeType
  deriving stock ( Eq, Ord, Show, Generic )

data CharLikeType = Char | SChar | UChar
  deriving stock ( Eq, Ord, Show, Generic )

data Sign = Signed | Unsigned
  deriving stock ( Eq, Ord, Show, Generic )

data IntLikeType
  = Short    !Sign
  | Int      !Sign
  | Long     !Sign
  | LongLong !Sign
  | PtrDiff
  deriving stock ( Eq, Ord, Show, Generic )

--------------------------------------------------------------------------------

data WordWidth = WordWidth32 | WordWidth64
  deriving stock ( Eq, Ord, Show, Generic )

wordWidthInBits :: WordWidth -> Word
wordWidthInBits = \case
  WordWidth32 -> 32
  WordWidth64 -> 64

data OS = Windows | Posix
  deriving stock ( Eq, Ord, Show, Generic )

data Platform = Platform { platformWordWidth :: !WordWidth
                         , platformOS        :: !OS }
  deriving stock ( Eq, Show, Generic )

hostPlatform :: Platform
hostPlatform =
  Platform
    { platformWordWidth =
        case sizeOf @( Foreign.Ptr () ) undefined of
          4 -> WordWidth32
          8 -> WordWidth64
          w -> error $ "hostPlatform: unsupported word width (" ++ show (8 * w) ++ " bits)"
    , platformOS =
        case System.Info.os of
          "mingw32" -> Windows
          _         -> Posix
    }

newtype IntegerConversionRank = IntegerConversionRank Rational
  deriving stock ( Eq, Ord, Show, Generic )

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
        Long     {} ->
          case platformOS plat of
            Windows -> 32
            Posix   -> 64
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

  -- Standard integer types.
  -- Implement (3), ignoring sign as per (4).
  Short     {} -> 3
  Int       {} -> 4
  Long      {} -> 5
  LongLong  {} -> 6

  -- Extended types.
  PtrDiff ->
    -- The following logic comes from (1) and (5), which dictate that the
    -- integer conversion rank of ptrdiff_t must be:
    --
    --  (a) strictly greater than the integer conversion rank of any
    --      standard integer type whose size is less than the word width,
    --  (b) strictly less than the integer conversion rank of any standard
    --      integer type whose size is greater than or equal to the word width.
    case minimum [ Arg rk ty
                 | ty <- [ Short Signed, Int Signed, Long Signed, LongLong Signed ]
                 , let sz = intLikeTypeSizeInBits plat ty
                       rk = intLikeTypeConversionRank plat ty
                 , sz >= wordWidthInBits ( platformWordWidth plat )
                 ] of
      Arg ( IntegerConversionRank rk ) _ ->
        rk - 0.1
          -- 0.1 is an arbitrary value in the open interval ]0,1[
          --
          -- This assumes that the standard integer types are given
          -- integral integer conversion ranks.

-- | Does the given 'IntLikeType' fit inside the (signed) @int@ type
-- on this platform?
intLikeTypeFitsInInt :: Platform -> IntLikeType -> Bool
intLikeTypeFitsInInt plat ty =
  -- TODO: this logic is questionable, as in theory I think we could have
  -- an 'IntLike' type of a small size but of an entirely distinct range,
  -- e.g. an 8-bit unsigned integer type that can store values in the range
  -- [2^32, 2^32+2^8-1].
  case intLikeTypeSign ty of
    Signed ->
      sz <= intSz
    Unsigned ->
      sz < intSz
  where
    sz, intSz :: Word
    sz = intLikeTypeSizeInBits plat ty
    intSz = intLikeTypeSizeInBits plat (Int Signed)

--------------------------------------------------------------------------------

showTypeAsCType :: Show a => Type a -> String -> String
showTypeAsCType ty s =
  case ty of
    Void         -> "void" +++ s
    Arithmetic a -> showArithmeticTypeAsCType a +++ s
    Ptr a        -> addStar (show a) ++ s
  where
    x +++ "" = x
    x +++ y = x ++ " " ++ y
    addStar x@(_:_)
      | last x == '*'
      = x ++ "*"
    addStar x
      = x ++ " *"

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
