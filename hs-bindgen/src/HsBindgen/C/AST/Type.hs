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
  , OpaqueStruct(..)
    -- * Enums
  , Enu(..)
  , EnumValue(..)
  , OpaqueEnum(..)
    -- * Typedefs
  , Typedef(..)
    -- * DeclPath
  , DeclPath(..)
  , DeclName(..)
  ) where

import HsBindgen.Clang.HighLevel.Types (SingleLoc)
import HsBindgen.Imports
import HsBindgen.C.AST.Name

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Type representing /usage/ of a type: field type, argument or result type etc.
data Type =
    TypePrim PrimType
  | TypeStruct DeclPath
  | TypeEnum CName
  | TypeTypedef CName
  | TypePointer Type
  | TypeConstArray Natural Type
  | TypeFun [Type] Type
  | TypeVoid

    -- | Arrays of unknown size
    --
    -- Arrays normally have a known size, but not always:
    --
    -- * Arrays of unknown size are allowed as function arguments; such arrays
    --   are interpreted as pointers.
    -- * Arrays of unknown size may be declared for externs; this is considered
    --   an incomplete type.
    -- * Structs may contain an array of undefined size as their last field,
    --   known as a "flexible array member" (FLAM).
    --
    -- We treat the FLAM case separately.
    --
    -- See <https://en.cppreference.com/w/c/language/array#Arrays_of_unknown_size>
  | TypeIncompleteArray Type
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

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
  | PrimIntegral PrimIntType PrimSign

    -- | @ptrdiff_t@
  | PrimPtrDiff

    -- | A floating-point type, such as @float@ or @long double@.
  | PrimFloating PrimFloatType

    -- | @_Bool@
  | PrimBool
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (PrettyVal)

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
  deriving stock (Show, Eq, Enum, Ord, Generic)
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
      structDeclPath  :: DeclPath
    , structSizeof    :: Int
    , structAlignment :: Int
    , structFields    :: [StructField]
    , structFlam      :: Maybe StructField -- ^ Note: type is the type of elements of flexible array.
    , structSourceLoc :: SingleLoc
    , structBitfields :: [(StructField, Int)] -- ^ we store all bitfields separately, even if they are interspersed with other fields in C definition.
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data StructField = StructField {
      fieldName      :: CName
    , fieldOffset    :: Int -- ^ Offset in bits
    , fieldType      :: Type
    , fieldSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

-- | Opaque structure
--
-- An /opaque structure/ is a structure declaration that specifies neither
-- members nor size.  Example:
--
-- > struct foo;
data OpaqueStruct = OpaqueStruct {
      opaqueStructTag       :: CName
    , opaqueStructSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

data Enu = Enu {
      enumTag       :: CName
    , enumType      :: Type
    , enumSizeof    :: Int
    , enumAlignment :: Int
    , enumValues    :: [EnumValue]
    , enumSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data EnumValue = EnumValue {
      valueName      :: CName
    , valueValue     :: Integer
    , valueSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

-- | Opaque enum
--
-- An /opaque enum/ is an enum declaration that specifies neither members nor
-- size.  Example:
--
-- > enum foo;
data OpaqueEnum = OpaqueEnum {
      opaqueEnumTag       :: CName
    , opaqueEnumSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

data Typedef = Typedef {
      typedefName      :: CName
    , typedefType      :: Type
    , typedefSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  DeclPath
-------------------------------------------------------------------------------}

-- | Declaration path
--
-- This type tracks how declarations are defined.  This information is used to
-- create Haskell names, and it is also used in test generation.
--
-- Syntax @struct {...}@ and @union {...}@ are /types/ that can be used in the
-- definition of a variable or field.  They may even be nested.  When in a
-- top-level declaration and given a name, like @struct foo {..}@ or
-- @union bar {..}@, they /also/ act as declarations in the global scope.  When
-- a @struct@ or @union@ is not given a name, the field name may be used in
-- creation of the corresponding Haskell name.
data DeclPath
    = DeclPathTop
    | DeclPathStruct DeclName DeclPath
    -- TODO | DeclPathUnion (Maybe CName) DeclPath
    | DeclPathField CName DeclPath
    -- TODO | DeclPathPtr Path
    -- TODO | DeclPathConstArray Natural Path
  deriving stock (Eq, Generic, Show)
  deriving anyclass (PrettyVal)

-- | Declaration name
data DeclName
    = -- No name specified (anonymous)
      DeclNameNone
    | -- Structure/union tag specified
      DeclNameTag CName
    | -- Typedef name specified
      DeclNameTypedef CName
  deriving stock (Eq, Generic, Show)
  deriving anyclass (PrettyVal)
