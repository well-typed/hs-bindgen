{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

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

  -- * Singletons for C types
  , SType(..)
  , SArithmeticType(..)
  , SIntegralType(..)
  , SCharLikeType(..)
  , SIntLikeType(..)
  , SFloatingType(..)

  -- ** Promotion
  , promoteType
  , promoteArithmeticType
  , promoteIntegralType
  , promoteCharLikeType
  , promoteIntLikeType
  , promoteFloatingType

  -- ** Demotion
  , demoteType
  , demoteArithmeticType
  , demoteIntegralType
  , demoteCharLikeType
  , demoteIntLikeType
  , demoteFloatingType

  -- ** Utilities
  , witnessType
  , witnessArithmeticType
  , witnessFloatingType
  , witnessIntegralType
  , witnessCharLike
  , witnessIntLike

  ) where

-- base
import Data.Kind qualified as Hs
import Data.Semigroup
  ( Arg(..) )
import Data.GADT.Compare
import Data.Type.Equality
import Foreign.C.Types
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
  | Size
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
  Size        -> Unsigned

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
        Size        -> 32
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
        Size        -> 64

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
  _extended_ty ->
    -- The following logic comes from (1) and (5), which dictate that the
    -- integer conversion rank of ptrdiff_t and size_t must be:
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
        rk - 0.5
          -- 0.5 is an arbitrary value in the open interval ]0,1[
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
  Size    -> "size_t"
  where
    withSign s = case s of
      Signed   -> id
      Unsigned -> ( "unsigned " ++ )

--------------------------------------------------------------------------------
-- Singletons

type SType :: ( Hs.Type -> Hs.Type ) -> Hs.Type -> Hs.Type
data SType rec a where
  SVoid :: SType rec ()
  SArithmetic :: !( SArithmeticType ty ) -> SType rec ty
  SPtr :: rec ty -> SType rec ( Foreign.Ptr ty )
deriving stock instance ( forall x. Show ( rec x ) ) => Show ( SType rec a )

data SArithmeticType ty where
  SIntegral  :: !( SIntegralType ty ) -> SArithmeticType ty
  SFloatLike :: !( SFloatingType ty ) -> SArithmeticType ty
deriving stock instance Show ( SArithmeticType ty )

data SFloatingType ty where
  SFloatType  :: SFloatingType CFloat
  SDoubleType :: SFloatingType CDouble
deriving stock instance Show ( SFloatingType ty )

data SIntegralType ty where
  SBool :: SIntegralType CBool
  SCharLike :: !( SCharLikeType ty ) -> SIntegralType ty
  SIntLike :: !( SIntLikeType ty ) -> SIntegralType ty
deriving stock instance Show ( SIntegralType ty )

data SCharLikeType ty where
  S_Char  :: SCharLikeType CChar
  S_SChar :: SCharLikeType CSChar
  S_UChar :: SCharLikeType CUChar
deriving stock instance Show ( SCharLikeType ty )

data SIntLikeType ty where
  SShort     :: SIntLikeType CShort
  SUShort    :: SIntLikeType CUShort
  SInt       :: SIntLikeType CInt
  SUInt      :: SIntLikeType CUInt
  SLong      :: SIntLikeType CLong
  SULong     :: SIntLikeType CULong
  SLongLong  :: SIntLikeType CLLong
  SULongLong :: SIntLikeType CULLong
  SPtrDiff   :: SIntLikeType CPtrdiff
  SSize      :: SIntLikeType CSize
  -- NB: make sure to update 'GEq SIntLikeType' when updating this datatype
deriving stock instance Show ( SIntLikeType ty )

instance GEq rec => GEq ( SType rec ) where
  geq SVoid SVoid = Just Refl
  geq (SArithmetic a) (SArithmetic b) = geq a b
  geq (SPtr a) (SPtr b) =
    case geq a b of
      Just Refl -> Just Refl
      Nothing   -> Nothing
  geq _ _ = Nothing
instance GEq SArithmeticType where
  geq (SIntegral a) (SIntegral b) = geq a b
  geq (SFloatLike a) (SFloatLike b) = geq a b
  geq _ _ = Nothing

instance GEq SFloatingType where
  geq SFloatType  SFloatType  = Just Refl
  geq SDoubleType SDoubleType = Just Refl
  geq _ _ = Nothing
instance GEq SCharLikeType where
  geq S_Char  S_Char  = Just Refl
  geq S_SChar S_SChar = Just Refl
  geq S_UChar S_UChar = Just Refl
  geq _       _       = Nothing

instance GEq SIntegralType where
  geq SBool          SBool        = Just Refl
  geq (SCharLike a) (SCharLike b) = geq a b
  geq (SIntLike  a) (SIntLike  b) = geq a b
  geq _ _ = Nothing

instance GEq SIntLikeType where
  geq SShort     SShort     = Just Refl
  geq SUShort    SUShort    = Just Refl
  geq SInt       SInt       = Just Refl
  geq SUInt      SUInt      = Just Refl
  geq SLong      SLong      = Just Refl
  geq SULong     SULong     = Just Refl
  geq SLongLong  SLongLong  = Just Refl
  geq SULongLong SULongLong = Just Refl
  geq SPtrDiff   SPtrDiff   = Just Refl
  geq SSize      SSize      = Just Refl
  geq _ _ = Nothing

promoteType :: ( a -> ( forall ty. rec ty -> r ) -> r ) -> Type a -> ( forall ty. ( Ord ty, Show ty ) => SType rec ty -> r ) -> r
promoteType recur ty f = case ty of
  Void -> f SVoid
  Arithmetic i -> promoteArithmeticType i ( f . SArithmetic )
  Ptr p -> recur p ( f . SPtr )

promoteArithmeticType :: ArithmeticType -> ( forall ty. ( Ord ty, Show ty ) => SArithmeticType ty -> r ) -> r
promoteArithmeticType ty f = case ty of
  Integral  t -> promoteIntegralType t ( f . SIntegral )
  FloatLike t -> promoteFloatingType t ( f . SFloatLike )

promoteFloatingType :: FloatingType -> ( forall ty. ( Ord ty, Show ty ) => SFloatingType ty -> r ) -> r
promoteFloatingType ty f = case ty of
  FloatType  -> f SFloatType
  DoubleType -> f SDoubleType

promoteIntegralType :: IntegralType -> ( forall ty. ( Show ty, Integral ty ) => SIntegralType ty -> r ) -> r
promoteIntegralType ty f = case ty of
  Bool -> f SBool
  CharLike c -> promoteCharLikeType c ( f . SCharLike )
  IntLike i  -> promoteIntLikeType  i ( f . SIntLike )

promoteCharLikeType :: CharLikeType -> ( forall ty. ( Show ty, Integral ty ) => SCharLikeType ty -> r ) -> r
promoteCharLikeType ty f = case ty of
  Char  -> f S_Char
  UChar -> f S_UChar
  SChar -> f S_SChar

promoteIntLikeType :: IntLikeType -> ( forall ty. ( Show ty, Integral ty ) => SIntLikeType ty -> r ) -> r
promoteIntLikeType ty f = case ty of
  Short s ->
    case s of
      Signed   -> f SShort
      Unsigned -> f SUShort
  Int s ->
    case s of
      Signed   -> f SInt
      Unsigned -> f SUInt
  Long s ->
    case s of
      Signed   -> f SLong
      Unsigned -> f SULong
  LongLong s ->
    case s of
      Signed   -> f SLongLong
      Unsigned -> f SULongLong
  PtrDiff -> f SPtrDiff
  Size    -> f SSize

demoteType :: ( forall ty'. rec ty' -> a ) -> SType rec ty -> Type a
demoteType recur = \case
  SVoid -> Void
  SArithmetic a -> Arithmetic $ demoteArithmeticType a
  SPtr a -> Ptr $ recur a

demoteArithmeticType :: SArithmeticType ty -> ArithmeticType
demoteArithmeticType = \case
  SIntegral  i -> Integral  $ demoteIntegralType i
  SFloatLike f -> FloatLike $ demoteFloatingType f

demoteIntegralType :: SIntegralType ty -> IntegralType
demoteIntegralType = \case
  SBool -> Bool
  SCharLike c -> CharLike $ demoteCharLikeType c
  SIntLike i  -> IntLike  $ demoteIntLikeType  i

demoteFloatingType :: SFloatingType ty -> FloatingType
demoteFloatingType = \case
  SFloatType  -> FloatType
  SDoubleType -> DoubleType

demoteCharLikeType :: SCharLikeType ty -> CharLikeType
demoteCharLikeType = \case
  S_Char  -> Char
  S_UChar -> UChar
  S_SChar -> SChar

demoteIntLikeType :: SIntLikeType ty -> IntLikeType
demoteIntLikeType = \case
  SShort     -> Short Signed
  SUShort    -> Short Unsigned
  SInt       -> Int Signed
  SUInt      -> Int Unsigned
  SLong      -> Long Signed
  SULong     -> Long Unsigned
  SLongLong  -> LongLong Signed
  SULongLong -> LongLong Unsigned
  SPtrDiff   -> PtrDiff
  SSize      -> Size

witnessType
  :: forall c ty rec r
  . ( forall x. c ( Foreign.Ptr x )
    , c CChar, c CSChar, c CUChar, c CShort, c CUShort, c CInt
    , c CUInt, c CLong, c CULong, c CLLong, c CULLong, c CPtrdiff
    , c CSize, c CBool, c CFloat, c CDouble, c () )
  => ( forall ty'. rec ty' -> ( c ty' => r ) -> r )
  -> SType rec ty -> ( c ty => r ) -> r
witnessType recur ty f =
  case ty of
    SVoid  -> f
    SArithmetic i -> witnessArithmeticType @c i f
    SPtr p -> recur p f

witnessArithmeticType
  :: forall c ty r
  . ( c CChar, c CSChar, c CUChar, c CShort, c CUShort, c CInt
    , c CUInt, c CLong, c CULong, c CLLong, c CULLong, c CPtrdiff
    , c CSize, c CBool, c CFloat, c CDouble )
  => SArithmeticType ty -> ( c ty => r ) -> r
witnessArithmeticType ty f =
  case ty of
    SIntegral i -> witnessIntegralType @c i f
    SFloatLike k -> witnessFloatingType @c k f

witnessFloatingType
  :: forall c ty r
  . ( c CFloat, c CDouble )
  => SFloatingType ty -> ( c ty => r ) -> r
witnessFloatingType ty f =
  case ty of
    SFloatType -> f
    SDoubleType -> f

witnessIntegralType
  :: forall c ty r
  . ( c CChar, c CSChar, c CUChar, c CShort, c CUShort, c CInt
    , c CUInt, c CLong, c CULong, c CLLong, c CULLong, c CPtrdiff
    , c CSize, c CBool )
  => SIntegralType ty -> ( c ty => r ) -> r
witnessIntegralType ty f =
  case ty of
    SBool       -> f
    SCharLike c -> witnessCharLike @c c f
    SIntLike  i -> witnessIntLike @c i f

witnessCharLike
  :: forall c ty r
  .  ( c CChar, c CSChar, c CUChar )
  => SCharLikeType ty -> ( c ty => r ) -> r
witnessCharLike ty f =
  case ty of
    S_Char -> f
    S_SChar -> f
    S_UChar -> f

witnessIntLike
  :: forall c ty r
  . ( c CShort, c CUShort, c CInt, c CUInt, c CLong, c CULong
    , c CLLong, c CULLong, c CPtrdiff, c CSize )
  => SIntLikeType ty -> ( c ty => r ) -> r
witnessIntLike ty f =
  case ty of
    SShort     -> f
    SUShort    -> f
    SInt       -> f
    SUInt      -> f
    SLong      -> f
    SULong     -> f
    SLongLong  -> f
    SULongLong -> f
    SPtrDiff   -> f
    SSize      -> f
