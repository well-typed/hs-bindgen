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
    -- * Unions
  , Union(..)
  , UnionField(..)
    -- * Enums
  , Enu(..)
  , EnumValue(..)
  , OpaqueEnum(..)
    -- * Typedefs
  , Typedef(..)
    -- * DeclPath
  , DeclPath(..)
  , DeclPathCtxt(..)
  , topLevel
  , declPathName
  ) where

import Clang.HighLevel.Types (SingleLoc)
import HsBindgen.ExtBindings
import HsBindgen.Imports
import HsBindgen.C.AST.Name

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Type representing /usage/ of a type: field type, argument or result type etc.
data Type =
    TypePrim PrimType
  | TypeStruct DeclPath
  | TypeUnion DeclPath
  | TypeEnum DeclPath
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
  | TypeExtBinding ExtIdentifier
  deriving stock (Show, Eq, Generic)
  deriving Repr via ReprShow Type

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

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

-- | Definition of a struct
data Struct = Struct {
      structDeclPath  :: DeclPath
    , structAliases   :: [CName]
    , structSizeof    :: Int
    , structAlignment :: Int
    , structFields    :: [StructField]
    , structFlam      :: Maybe StructField -- ^ Note: type is the type of elements of flexible array.
    , structSourceLoc :: SingleLoc
  }
  deriving stock (Show, Eq, Generic)

data StructField = StructField {
      fieldName      :: CName
    , fieldOffset    :: Int -- ^ Offset in bits
    , fieldWidth     :: Maybe Int
    , fieldType      :: Type
    , fieldSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)

-- | Opaque structure
--
-- An /opaque structure/ is a structure declaration that specifies neither
-- members nor size.  Example:
--
-- > struct foo;
data OpaqueStruct = OpaqueStruct {
      opaqueStructTag       :: CName
    , opaqueStructAliases   :: [CName]
    , opaqueStructSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

-- | Definition of an union
data Union = Union {
      unionDeclPath  :: DeclPath
    , unionAliases   :: [CName]
    , unionSizeof    :: Int
    , unionAlignment :: Int
    , unionFields    :: [UnionField]
    , unionSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)

data UnionField = UnionField {
      ufieldName      :: CName
    , ufieldType      :: Type
    , ufieldSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

data Enu = Enu {
      enumDeclPath  :: DeclPath
    , enumAliases   :: [CName]
    , enumType      :: Type
    , enumSizeof    :: Int
    , enumAlignment :: Int
    , enumValues    :: [EnumValue]
    , enumSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)

data EnumValue = EnumValue {
      valueName      :: CName
    , valueValue     :: Integer
    , valueSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)

-- | Opaque enum
--
-- An /opaque enum/ is an enum declaration that specifies neither members nor
-- size.  Example:
--
-- > enum foo;
data OpaqueEnum = OpaqueEnum {
      opaqueEnumTag       :: CName
    , opaqueEnumAliases   :: [CName]
    , opaqueEnumSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

data Typedef = Typedef {
      typedefName      :: CName
    , typedefType      :: Type
    , typedefSourceLoc :: SingleLoc
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  DeclPath
-------------------------------------------------------------------------------}

-- | Type declaration in context
--
-- C allows types to be declared in various contexts; we need to keep track of
-- these contexts for name mangling, external bindings generation, and test
-- generation.
data DeclPath =
    -- | Named type
    DeclPathName CName DeclPathCtxt

    -- | Anonymous type
    --
    -- TODO: Ideally we should be able to insist that we have a non-empty
    -- context here.
  | DeclPathAnon DeclPathCtxt
  deriving stock (Eq, Ord, Generic, Show)

data DeclPathCtxt =
    -- | Top-level declaration
    --
    -- Example:
    --
    -- > struct Foo { .. };
    DeclPathCtxtTop

    -- | Declaration inside a typedef
    --
    -- Example: @Foo_t@ is a typedef context for the declaration of @Foo@ in
    --
    -- > typedef struct Foo { .. } Foo_t;
    --
    -- The inner declaration might also be anonymous.
    --
    -- Since @typedef@ can only appear at the top-level, this constructor is
    -- not recursive.
  | DeclPathCtxtTypedef CName

    -- | Declaration inside pointer definition
    --
    -- Example: this is the declaration of a pointer to an anonymous struct:
    --
    -- > typedef struct { char a; int b; } *Foo;
  | DeclPathCtxtPtr DeclPathCtxt

    -- | Declaration as part of a field in a struct or union
    --
    -- The nature of the declared type (struct, union, enum) is unrelated to the
    -- nature of the outer type.
    --
    -- Example: this is the declaration of an anonymous struct inside a field:
    --
    -- > struct outer {
    -- >   struct { .. } field;
    -- > };
    --
    -- We record the name of the struct (@outer@), if available (that is, if not
    -- itself anonymous), as well as the name of the field (@field@); the field
    -- name alone is not unique.
  | DeclPathCtxtField (Maybe CName) CName DeclPathCtxt
  deriving stock (Eq, Ord, Generic, Show)

topLevel :: CName -> DeclPath
topLevel cname = DeclPathName cname DeclPathCtxtTop

-- | Name of the declared type, or 'Nothing' if anonymous
declPathName :: DeclPath -> Maybe CName
declPathName (DeclPathName name _ctxt) = Just name
declPathName (DeclPathAnon _ctxt) = Nothing
