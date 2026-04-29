-- | Standard C language types
--
-- Intended for qualified import.
--
-- > import HsBindgen.Language.C qualified as C
module HsBindgen.Language.C (
    -- * C primitive types
    PrimType(..)
  , PrimIntType(..)
  , PrimFloatType(..)
  , PrimSign(..)
  , PrimSignChar(..)
    -- ** Sign
  , primTypeSign
    -- ** Pretty-printing
  , showsPrimType
    -- * @sizeof@
  , Sizeofs (..)
  , NumBytes (..)
  ) where

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Primitive types
-------------------------------------------------------------------------------}

data PrimType =
    -- | @[signed | unsigned] char@
    PrimChar PrimSignChar

    -- | An integral type, such as @int@ or @unsigned long int@.
  | PrimIntegral PrimIntType PrimSign

    -- | A floating-point type, such as @float@ or @long double@.
  | PrimFloating PrimFloatType

    -- | @_Bool@
  | PrimBool
  deriving stock (Show, Eq, Ord, Generic)

-- | An integral type, such as @int@ or @unsigned long int@.
data PrimIntType
    -- | @[signed | unsigned] short [int]@
  = PrimShort

    -- | @[signed | unsigned] int@
  | PrimInt

    -- | @[signed | unsigned] long [int]@
  | PrimLong

    -- | @[signed | unsigned] long long [int]@
  | PrimLongLong
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Primitive floating point types
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/349>
-- We don't currently support @long double@.
data PrimFloatType
    -- | @float@
  = PrimFloat

    -- | @double@
  | PrimDouble
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Sign of a primitive type
data PrimSign = Signed | Unsigned
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Sign for @char@
--
-- The C standard distinguishes between /three/ kinds of @char@: @char@, @signed
-- char@ and @unsigned char@. Unlike the other integer types, the interpretation
-- of @char@ as either @signed char@ or @unsigned char@ is implementation
-- defined (see also <https://eel.is/c++draft/basic#fundamental>).
--
-- Our general approach in @hs-bindgen@ is to generate machine code but with a
-- machine independent API. For example, we might know that @int@ is 32 bits on
-- a particular platform, and use this information to define 'Foreign.Storable.sizeOf' in
-- 'Foreign.Storable.Storable' instances, but still use 'Foreign.C.Types.CInt' in the type definition (rather
-- than 'Data.Word.Word32'). For this reason, /if/ the sign was compiler inferred, we
-- record this as a special case, so that we can generate 'Foreign.C.Types.CChar' instead of
-- 'Foreign.C.Types.CUChar' or 'Foreign.C.Types.CSChar'.
data PrimSignChar =
    -- | User explicitly specified sign
    PrimSignExplicit PrimSign

    -- | Sign was left implicit
    --
    -- In most cases we know the compiler-determined sign, but currently not in
    -- all cases. That's probably fixable but it's not trivial; at present we
    -- don't need the information and so we can leave this as a 'Maybe'.
  | PrimSignImplicit (Maybe PrimSign)
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

showsPrimType :: PrimType -> ShowS
showsPrimType (PrimChar (PrimSignImplicit _)) = showString "char"
showsPrimType (PrimChar (PrimSignExplicit s)) = showsPrimSign s . showString " char"
showsPrimType (PrimIntegral i s) = showsPrimSign s . showChar ' ' . showsPrimIntType i
showsPrimType (PrimFloating f) = showsPrimFloatType f
showsPrimType PrimBool = showString "_Bool"

showsPrimIntType :: PrimIntType -> ShowS
showsPrimIntType PrimShort = showString "short"
showsPrimIntType PrimInt = showString "int"
showsPrimIntType PrimLong = showString "long"
showsPrimIntType PrimLongLong = showString "long long"

showsPrimFloatType :: PrimFloatType -> ShowS
showsPrimFloatType PrimFloat = showString "float"
showsPrimFloatType PrimDouble = showString "double"

showsPrimSign :: PrimSign -> ShowS
showsPrimSign Signed = showString "signed"
showsPrimSign Unsigned = showString "unsigned"

{-------------------------------------------------------------------------------
  Sign
-------------------------------------------------------------------------------}

-- | Determine the sign of a primitive type
--
primTypeSign :: PrimType -> PrimSign
primTypeSign = \case
  PrimChar (PrimSignExplicit sign)        -> sign
  PrimChar (PrimSignImplicit (Just sign)) -> sign
  PrimChar (PrimSignImplicit Nothing)     -> Signed
  PrimIntegral _ sign                     -> sign
  PrimFloating _                          -> Signed
  PrimBool                                -> Unsigned

{-------------------------------------------------------------------------------
  @sizeof@
-------------------------------------------------------------------------------}

-- | @sizeof@ information for all arithmetic types
--
-- <https://en.cppreference.com/w/c/language/arithmetic_types.html>
--
-- In the backend we generate foreign import declarations at FFI types only.
-- That is, types like 'Data.Word.Word8', 'Float', and 'Data.Int.Int32'. The FFI type corresponding
-- to arithmetic C types like @int@ depends on the size of the C type, which in
-- turn depends on the target system. For example, if @int@ is 32-bits on a
-- given machine, then the corresponding FFI type would be 'Data.Int.Int32'. For
-- @unsigned int@ of the same size, it would be 'Data.Word.Word32'. The t'Sizeofs' record
-- stores the size of each of these arithmetic types. This record is created in
-- the @Boot@ phase.
--
-- One might ask: could we not annotate types with their size in the @Parse@
-- frontend pass instead? Unfortunately, that would not be sufficient. The
-- @ReparseMacroExpansions@ pass outputs expressions and types involving basic
-- arithmetic types using @language-c@, but @language-c@ does not give us any
-- size information. So, instead we pre-determine the sizes of arithmetic types
-- in the @Boot@ phase, and pass along that information where it is needed: the
-- backend.
data Sizeofs = Sizeofs {
      -- * Character types
      schar      :: !NumBytes
    , uchar      :: !NumBytes
    , char       :: !NumBytes
      -- * Integer types
    , short      :: !NumBytes
    , ushort     :: !NumBytes
    , int        :: !NumBytes
    , uint       :: !NumBytes
    , long       :: !NumBytes
    , ulong      :: !NumBytes
    , longlong   :: !NumBytes
    , ulonglong  :: !NumBytes
      -- * Floating point types
    , float      :: !NumBytes
    , double     :: !NumBytes
      -- NOTE: long double is not supported
      -- * Others
    , bool       :: !NumBytes
    }
    deriving stock (Show, Eq)

data NumBytes = One | Two | Four | Eight
  deriving stock (Show, Eq)
