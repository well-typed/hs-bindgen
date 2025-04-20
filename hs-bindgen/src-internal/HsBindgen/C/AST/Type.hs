-- | C types
--
-- This is re-exported in "HsBindgen.C.AST".
module HsBindgen.C.AST.Type (
    Type(..)
    -- * Primitive types
  , PrimType(..)
  , showsType
  , showsFunctionType
  , PrimIntType(..)
  , PrimFloatType(..)
  , PrimSign(..)
  , PrimSignChar(..)
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

import Data.Text qualified as T

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
  | TypeExtBinding ExtIdentifier Type
  deriving stock (Show, Eq, Generic)
  deriving Repr via ReprShow Type

-- | Show type in C syntax.
-- Used to generate userland-capi C-code.
--
-- >>> showsType (showString "x") (TypePrim PrimBool) ""
-- "_Bool x"
--
-- TODO: int (*baz2 (int arg1))[2][3] { ... }
--
--
showsType :: ShowS -- ^ variable name
  -> Type -> ShowS
showsType x (TypePrim p)            = showsPrimType p . showChar ' ' . x
showsType x (TypeStruct dp)         = showString "struct " . showDeclPath dp . showChar ' ' . x
showsType x (TypeUnion dp)          = showString "union " . showDeclPath dp . showChar ' ' . x
showsType x (TypeEnum dp)           = showString "enum " . showDeclPath dp . showChar ' ' . x
showsType x (TypeTypedef n)         = showString (cnameToString n) . showChar ' ' . x
showsType x (TypePointer t)         = showsType (showString "*" . x) t
showsType x (TypeConstArray n t)    = showsType (x . showChar '[' . shows n . showChar ']') t
showsType x (TypeFun args res)      = showsFunctionType (showParen True x) args res
showsType x TypeVoid                = showString "void " . x
showsType x (TypeIncompleteArray t) = showsType (x . showString "[]") t
showsType x (TypeExtBinding _ t)    = showsType x t

-- | by "construction" function arguments (where showsType is used), cannot be anonymous, so DeclPathAnon is shown as <anon>.
-- FWIW, that's similar to how libclang show types referencing anonymous structs.
showDeclPath :: DeclPath -> ShowS
showDeclPath (DeclPathName n _) = showString (T.unpack (getCName n))
showDeclPath (DeclPathAnon _)   = showString "<anon>"

showsFunctionType
  :: ShowS   -- ^ function name
  -> [Type]  -- ^ argument types
  -> Type    -- ^ return type
  -> ShowS
showsFunctionType n args res =
    showsType n res . showChar ' ' . showParen True signatureArgs
  where
    signatureArgs :: ShowS
    signatureArgs = case args of
        [] -> showString "void"
        p:ps -> foldr1 sep $ fmap showT $ (1, p) :| zip [2..] ps
      where
        sep a b = a . showString ", " . b

        showT :: (Int, Type) -> ShowS
        showT (i, p) = showsType (showString "arg" . shows i) p


cnameToString :: CName -> String
cnameToString = T.unpack . getCName

{- TODO: function pointers

static int arr[2][3];

int (*baz2 (int arg1))[2][3] {

        return &arr;
}

int main() {
  int (*(*tmp) (int arg1))[2][3] = baz2;
  int* (*ptr)(int);
  return 0;
}

-}

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
    PrimChar PrimSignChar

    -- | An integral type, such as @int@ or @unsigned long int@.
  | PrimIntegral PrimIntType PrimSign

    -- | @ptrdiff_t@
  | PrimPtrDiff

    -- | A floating-point type, such as @float@ or @long double@.
  | PrimFloating PrimFloatType

    -- | @_Bool@
  | PrimBool
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

showsPrimType :: PrimType -> ShowS
showsPrimType (PrimChar (PrimSignImplicit _)) = showString "char"
showsPrimType (PrimChar (PrimSignExplicit s)) = showsPrimSign s . showString " char"
showsPrimType (PrimIntegral i s) = showsPrimSign s . showChar ' ' . showsPrimIntType i
showsPrimType (PrimFloating f) = showsPrimFloatType f
showsPrimType PrimPtrDiff = showString "ptrdiff_t"
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
