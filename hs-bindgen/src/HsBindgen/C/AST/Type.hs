-- | C types
--
-- This is re-exported in "HsBindgen.C.AST".
module HsBindgen.C.AST.Type (
    Type(..)
    -- * Primitive types
  , PrimType(..)
  , PrimIntType(..)
  , PrimFloatType(..)
  , PrimSign(..)
    -- * Structs
  , Struct(..)
  , StructField(..)
    -- * Enums
  , Enu(..)
  , EnumValue(..)
    -- * Typedefs
  , Typedef(..)
  ) where

import HsBindgen.Imports
import HsBindgen.C.AST.Name

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Type representing /usage/ of a type: field type, argument or result type etc.
data Type =
    TypePrim PrimType
  | TypeStruct Struct
  | TypePointer Type
  | TypeConstArray Natural Type
  | TypeElaborated Symbol
  -- todo | TypEnum Enum
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

-- | Reference into the type symbol table
--
-- TODO: for now this is simply the name of the C type.
type Symbol = CName

{-------------------------------------------------------------------------------
  Primitives types
-------------------------------------------------------------------------------}

-- | Primitive type
--
-- The interpretation of the primitive types is in many cases implementation
-- and/or machine dependent. In @hs-bindgen@ we are dealing with one specific
-- implementation (@libclang@), and we are generating code for one specific
-- machine (possibly cross platform). This means we have a choice; suppose we
-- see a field of type @int@ in a C struct:
--
-- 1. We could produce a field of type 'CInt' in the generated Haskell code
-- 2. We could query @libclang@ to what choice it makes for the selected
--    target platform, and use 'CShort' or 'CLong' (or something else) again.
--
-- Both options have advantages; most users will probably prefer (1), so that
-- we generate a /single/ API, independent of implementation details. However,
-- some users may prefer (2) in some cases, if they want to take advantage of
-- specific features of the target platform.
--
-- We don't force the decision here, but simply represent the C AST faithfully.
data PrimType =
    -- | @[signed | unsigned] char@
    --
    -- The C standard distinguishes between /three/ kinds of @char@: @char@,
    -- @signed char@ and @unsigned char@. Unlike the other integer types,
    -- the interpretation of @char@ as either @signed char@ or @unsigned char@
    -- is implementation defined.
    --
    -- See also <https://eel.is/c++draft/basic#fundamental>.
    PrimChar (Maybe PrimSign)

    -- | An integral type, such as @int@ or @unsigned long int@.
  | PrimIntegral PrimIntType

    -- | A floating-point type, such as @float@ or @long double@.
  | PrimFloating PrimFloatType

    -- | @void@
  | PrimVoid
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (PrettyVal)

-- | An integral type, such as @int@ or @unsigned long int@.
data PrimIntType
    -- | @[signed | unsigned] short [int]@
  = PrimShort PrimSign

    -- | @[signed | unsigned] int@
  | PrimInt PrimSign

    -- | @[signed | unsigned] long [int]@
  | PrimLong PrimSign

    -- | @[signed | unsigned] long long [int]@
  | PrimLongLong PrimSign
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (PrettyVal)

data PrimFloatType
    -- | @float@
  = PrimFloat

    -- | @double@
  | PrimDouble

    -- | @long double@
  | PrimLongDouble
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (PrettyVal)

-- | Sign of a primitive type
data PrimSign = Signed | Unsigned
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

-- | Definition of a struct
data Struct = Struct {
      structTag       :: Maybe CName
    , structSizeof    :: Int
    , structAlignment :: Int
    , structFields    :: [StructField]
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data StructField = StructField {
      fieldName   :: CName
    , fieldOffset :: Int -- ^ Offset in bits
    , fieldType   :: Type
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

data Enu = Enu {
      enumTag       :: Maybe CName
    , enumType      :: Type
    , enumSizeof    :: Int
    , enumAlignment :: Int
    , enumValues    :: [EnumValue]
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data EnumValue = EnumValue {
      valueName  :: CName
    , valueValue :: Integer
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

data Typedef = Typedef {
      typedefName :: CName
    , typedefType :: Type
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)
