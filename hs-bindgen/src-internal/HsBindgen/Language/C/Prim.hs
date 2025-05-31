-- | Primitive types in C
module HsBindgen.Language.C.Prim (
    -- * Definition
    PrimType(..)
  , PrimIntType(..)
  , PrimFloatType(..)
  , PrimSign(..)
  , PrimSignChar(..)
    -- * Pretty-printing
  , showsPrimType
  ) where

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data PrimType =
    -- | @[signed | unsigned] char@
    PrimChar PrimSignChar

    -- | An integral type, such as @int@ or @unsigned long int@.
  | PrimIntegral PrimIntType PrimSign

    -- | @ptrdiff_t@
  | PrimPtrDiff

    -- | @size_t@
  | PrimSize

    -- | A floating-point type, such as @float@ or @long double@.
  | PrimFloating PrimFloatType

    -- | @_Bool@
  | PrimBool
  deriving stock (Show, Eq, Generic)

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
  deriving stock (Show, Eq, Enum, Generic)

data PrimFloatType
    -- | @float@
  = PrimFloat

    -- | @double@
  | PrimDouble

    -- | @long double@
  | PrimLongDouble
  deriving stock (Show, Eq, Ord, Generic)

-- | Sign of a primitive type
data PrimSign = Signed | Unsigned
  deriving stock (Show, Eq, Ord, Generic)

-- | Sign for @char@
--
-- The C standard distinguishes between /three/ kinds of @char@: @char@, @signed
-- char@ and @unsigned char@. Unlike the other integer types, the interpretation
-- of @char@ as either @signed char@ or @unsigned char@ is implementation
-- defined (see also <https://eel.is/c++draft/basic#fundamental>).
--
-- Our general approach in @hs-bindgen@ is to generate machine code but with a
-- machine independent API. For example, we might know that @int@ is 32 bits on
-- a particular platform, and use this information to define 'sizeOf' in
-- 'Storable' instances, but still use 'CInt' in the type definition (rather
-- than 'Word32'). For this reason, /if/ the sign was compiler inferred, we
-- record this as a special case, so that we can generate 'CChar' instead of
-- 'CUChar' or 'CSChar'.
data PrimSignChar =
    -- ^ User explicitly specified sign
    PrimSignExplicit PrimSign

    -- ^ Sign was left implicit
    --
    -- In most cases we know the compiler-determined sign, but currently not in
    -- all cases. That's probably fixable but it's not trivial; at present we
    -- don't need the information and so we can leave this as a 'Maybe'.
  | PrimSignImplicit (Maybe PrimSign)
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

showsPrimType :: PrimType -> ShowS
showsPrimType (PrimChar (PrimSignImplicit _)) = showString "char"
showsPrimType (PrimChar (PrimSignExplicit s)) = showsPrimSign s . showString " char"
showsPrimType (PrimIntegral i s) = showsPrimSign s . showChar ' ' . showsPrimIntType i
showsPrimType (PrimFloating f) = showsPrimFloatType f
showsPrimType PrimPtrDiff = showString "ptrdiff_t"
showsPrimType PrimSize = showString "size_t"
showsPrimType PrimBool = showString "_Bool"

showsPrimIntType :: PrimIntType -> ShowS
showsPrimIntType PrimShort = showString "short"
showsPrimIntType PrimInt = showString "int"
showsPrimIntType PrimLong = showString "long"
showsPrimIntType PrimLongLong = showString "long long"

showsPrimFloatType :: PrimFloatType -> ShowS
showsPrimFloatType PrimFloat = showString "float"
showsPrimFloatType PrimDouble = showString "double"
showsPrimFloatType PrimLongDouble = showString "long double"

showsPrimSign :: PrimSign -> ShowS
showsPrimSign Signed = showString "signed"
showsPrimSign Unsigned = showString "unsigned"
