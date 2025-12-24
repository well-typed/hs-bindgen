{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The final C AST after the frontend is done
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.AST.External qualified as C
module HsBindgen.Frontend.AST.External (
    -- * Top-level
    TranslationUnit(..)
    -- * Declarations
  , Decl(..)
  , Int.Availability(..)
  , DeclInfo(..)
  , C.DeclIdPair
  , Int.HeaderInfo(..)
  , FieldInfo(..)
  , DeclKind(..)
  , DeclSpec(..)
    -- ** Structs
  , Struct(..)
  , StructField(..)
    -- ** Unions
  , Union(..)
  , UnionField(..)
    -- ** Enums
  , Enum(..)
  , EnumConstant(..)
    -- ** Typedefs
  , Typedef(..)
  , TypedefRef(..)
    -- ** Comment
  , CommentRef(..)
    -- ** Macros
  , CheckedMacro(..)
  , CheckedMacroType(..)
  , Int.CheckedMacroExpr(..)
    -- ** Functions
  , Function(..)
  , Int.FunctionAttributes(..)
  , Int.FunctionPurity(..)
    -- * Types
  , Type
  , TypeF(TypePrim, TypeRef, TypeTypedef, TypeFun, TypeVoid,
          TypeConstArray, TypeIncompleteArray, TypeBlock, TypeQualified,
          TypeExtBinding, TypeComplex, TypePointers)
  , TypeQualifier(..)
  , ResolveBindingSpecs.ResolvedExtBinding(..)
  , isVoid
  , isErasedTypeConstQualified
  , isCanonicalTypeFunction
  , isCanonicalTypeStruct
  , isCanonicalTypeUnion
  , isCanonicalTypeComplex
  , ArrayClassification(..)
  , getArrayElementType
  , isCanonicalTypeArray
  , typeDeclIds
    -- ** Erasure
  , FullType
  , Full
  , ErasedType
  , Erased
  , CanonicalType
  , Canonical
  , GetErasedType(..)
  , GetCanonicalType(..)
  , eraseTypedef
    -- * Names
  , C.TagKind(..)
  , C.NameKind(..)
  , C.AnonId(..)
  , C.PrelimDeclId(..)
  , C.DeclId(..)
  ) where

import Prelude hiding (Enum)

import Clang.HighLevel.Documentation qualified as CDoc
import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal qualified as Int
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass qualified as ResolveBindingSpecs
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data TranslationUnit = TranslationUnit{
      unitDecls :: [Decl]

      -- | All transitive dependencies
      --
      -- We use this to declare TH dependent files.
    , unitDeps :: [SourcePath]
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

data Decl = Decl {
      declInfo :: DeclInfo
    , declKind :: DeclKind
    , declSpec :: DeclSpec
    }
  deriving stock (Show, Eq, Generic)

data DeclInfo = DeclInfo {
      declLoc        :: SingleLoc
    , declId         :: C.DeclIdPair
    , declHeaderInfo :: Maybe Int.HeaderInfo
    , declComment    :: Maybe (CDoc.Comment CommentRef)
    }
  deriving stock (Show, Eq, Generic)

data FieldInfo = FieldInfo {
      fieldLoc     :: SingleLoc
    , fieldName    :: C.ScopedNamePair
    , fieldComment :: Maybe (CDoc.Comment CommentRef)
    }
  deriving stock (Show, Eq, Generic)

data DeclKind =
    DeclStruct Struct
  | DeclUnion Union
  | DeclTypedef Typedef
  | DeclEnum Enum
    -- | Opaque type
    --
    -- When parsing, a C @struct@, @union@, or @enum@ may be opaque.  Users may
    -- specify any kind of type to be opaque using a prescriptive binding
    -- specification, however, including @typedef@ types.
  | DeclOpaque
  | DeclMacro CheckedMacro
  | DeclFunction Function
    -- | A global variable, whether it be declared @extern@, @static@ or neither.
  | DeclGlobal Type
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Information from the binding spec, minus naming information
-------------------------------------------------------------------------------}

-- | Binding specification for this declaration
--
-- Although we have interpreted /part/ of this binding specification during
-- name mangling, we leave the /full/ binding specification in the AST, because
-- we need it when we  /generate/ the output binding specification.
--
-- TODO: This is not quite right: we should distinguish between binding
-- specifications for different classes of things (declarations of types,
-- functions, etc.). When we do, we should not associate them with the top-level
-- 'Decl' but instead with specific 'DeclKind's. When we change this, this will
-- have consequences for "HsBindgen.Language.Haskell.Origin" also.
data DeclSpec = DeclSpec {
      declSpecC  :: Maybe BindingSpec.CTypeSpec
    , declSpecHs :: Maybe BindingSpec.HsTypeSpec
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

-- | Definition of a struct
data Struct = Struct {
      structNames     :: MangleNames.RecordNames
    , structSizeof    :: Int
    , structAlignment :: Int
    , structFields    :: [StructField]

      -- | Type of the elements of the FLAM, if any
    , structFlam :: Maybe StructField
  }
  deriving stock (Show, Eq, Generic)

data StructField = StructField {
      structFieldInfo    :: FieldInfo
    , structFieldType    :: Type
    , structFieldOffset  :: Int -- ^ Offset in bits
    , structFieldWidth   :: Maybe Int
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

-- | Definition of an union
data Union = Union {
      unionNames     :: MangleNames.NewtypeNames
    , unionSizeof    :: Int
    , unionAlignment :: Int
    , unionFields    :: [UnionField]
    }
  deriving stock (Show, Eq, Generic)

data UnionField = UnionField {
      unionFieldInfo :: FieldInfo
    , unionFieldType :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

data Enum = Enum {
      enumNames     :: MangleNames.NewtypeNames
    , enumType      :: Type
    , enumSizeof    :: Int
    , enumAlignment :: Int
    , enumConstants :: [EnumConstant]
    }
  deriving stock (Show, Eq, Generic)

data EnumConstant = EnumConstant {
      enumConstantInfo  :: FieldInfo
    , enumConstantValue :: Integer
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

data Typedef = Typedef {
      typedefNames :: MangleNames.NewtypeNames
    , typedefType  :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Functions (signatures)
-------------------------------------------------------------------------------}

data Function = Function {
      functionArgs    :: [(Maybe C.ScopedNamePair, Type)]
    , functionAttrs   :: Int.FunctionAttributes
    , functionRes     :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Comments
-------------------------------------------------------------------------------}

-- | Cross-reference in a Doxygen comment
--
-- The Haskell identifier might not be known (if the reference is to a
-- declaration not in the current translation unit).
data CommentRef = CommentRef Text (Maybe C.DeclIdPair)
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

data CheckedMacro =
    MacroType CheckedMacroType
  | MacroExpr Int.CheckedMacroExpr
  deriving stock (Show, Eq, Generic)

data CheckedMacroType = CheckedMacroType {
      macroTypeNames   :: MangleNames.NewtypeNames
    , macroType        :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | C types
--
-- For type /declarations/ see 'Decl'.
type Type = FullType

-- | C types in Trees That Shrink style
data TypeF tag =
    TypePrim C.PrimType
  | TypeRef C.DeclIdPair
  | TypeTypedef
    -- | NOTE: has a strictness annotation, which allows GHC to infer that
    -- pattern matches are redundant when @TypedefRefF tag ~ Void@.
    !(TypedefRefF tag)
    -- TODO: macros should get annotations with underlying types just like
    -- typedefs, so that we can erase the macro types and replace them with
    -- their underlying type. See issue #1200.
  | TypeUnsafePointer (TypeF tag)
  | TypeConstArray Natural (TypeF tag)
  | TypeFun [TypeF tag] (TypeF tag)
  | TypeVoid
  | TypeIncompleteArray (TypeF tag)
  | TypeBlock (TypeF tag)
  | TypeQualified
      -- | NOTE: has a strictness annotation, which allows GHC to infer that
      -- pattern matches are redundant when @TypeQualifierF tag ~ Void@.
      !(TypeQualifierF tag)
      (TypeF tag)
  | TypeExtBinding ResolveBindingSpecs.ResolvedExtBinding
  | TypeComplex C.PrimType
  deriving stock Generic

deriving stock instance (Show (TypedefRefF tag), Show (TypeQualifierF tag)) => Show (TypeF tag)
deriving stock instance (Eq (TypedefRefF tag), Eq (TypeQualifierF tag))     => Eq (TypeF tag)
deriving stock instance (Ord (TypedefRefF tag), Ord (TypeQualifierF tag))   => Ord (TypeF tag)

{-------------------------------------------------------------------------------
  Pattern synonyms for safe pointer handling
-------------------------------------------------------------------------------}

-- | Bidirectional pattern synonym for N layers of pointer indirection
--
-- This pattern can be used both for matching and construction.
--
-- Examples (matching):
--   * @TypePointers 1 inner@ matches @TypeUnsafePointer inner@
--   * @TypePointers 2 inner@ matches @TypeUnsafePointer (TypeUnsafePointer inner)@
--   * @TypePointers 3 inner@ matches @TypeUnsafePointer (TypeUnsafePointer (TypeUnsafePointer inner))@
--
-- Examples (construction):
--   * @TypePointers 1 someType@ creates @TypeUnsafePointer someType@
--   * @TypePointers 2 someType@ creates @TypeUnsafePointer (TypeUnsafePointer someType)@
--
-- The inner type can be anything (TypeFun, TypeRef, TypePrim, etc.).
--
pattern TypePointers :: Int -> TypeF tag -> TypeF tag
pattern TypePointers n inner <- (stripPointersF -> Just (n, inner))
  where
    TypePointers n inner = buildPointersF n inner

-- | Helper for TypePointers pattern synonym (matching direction)
--
-- Strips all pointer layers and returns the count and inner type.
-- Returns Nothing if there are no pointer layers.
stripPointersF :: TypeF tag -> Maybe (Int, TypeF tag)
stripPointersF = go 0
  where
    go :: Int -> TypeF tag -> Maybe (Int, TypeF tag)
    go !n (TypeUnsafePointer inner) = go (n + 1) inner
    go !n inner
      | n > 0     = Just (n, inner)
      | otherwise = Nothing

-- | Helper for TypePointers pattern synonym (construction direction)
--
-- Builds N layers of pointers around an inner type.
buildPointersF :: Int -> TypeF tag -> TypeF tag
buildPointersF n inner
  | n <= 0    = inner
  | otherwise = TypeUnsafePointer (buildPointersF (n - 1) inner)

-- | COMPLETE pragma to ensure exhaustiveness checking works
--
-- This tells GHC that pattern matching on these patterns (instead of the raw
-- TypeUnsafePointer) is complete and exhaustive.
{-# COMPLETE TypePrim, TypeRef, TypeTypedef, TypePointers, TypeFun, TypeVoid,
             TypeConstArray, TypeIncompleteArray, TypeBlock, TypeQualified,
             TypeExtBinding, TypeComplex #-}

data TypeQualifier = TypeQualifierConst
  deriving stock (Show, Eq, Ord, Generic)

data TypedefRef = TypedefRef {
      -- | Name of the referenced typedef declaration
      ref :: C.DeclIdPair

      -- | The underlying type of the referenced typedef declaration
      --
      -- NOTE: the underlying type can arbitrarily reference other types,
      -- including typedefs that we have not parsed. Use the underlying type with
      -- care!
    , underlying :: Type
    }
  deriving stock (Show, Eq, Ord, Generic)

isVoid :: Type -> Bool
isVoid TypeVoid = True
isVoid _        = False

-- | Is the erased type @const@-qualified?
isErasedTypeConstQualified :: GetErasedType t => t -> Bool
isErasedTypeConstQualified ty = case getErasedType ty of
    -- Types can be directly @const@-qualified,
    TypeQualified TypeQualifierConst _ -> True
    -- but arrays are also @const@-qualified if their element type is. Note that
    -- elements of arrays can themselves be arrays, hence we recurse into the
    -- array element type.
    TypeConstArray _ ty' -> isErasedTypeConstQualified ty'
    TypeIncompleteArray ty' -> isErasedTypeConstQualified ty'
    -- And otherwise, the type is not considered to be @const@-qualified.
    _ -> False

-- | Is the canonical type a function type?
isCanonicalTypeFunction :: GetCanonicalType t => t -> Bool
isCanonicalTypeFunction ty = case getCanonicalType ty of
    TypeFun{} -> True
    _         -> False

-- | Is the canonical type a struct type?
isCanonicalTypeStruct :: GetCanonicalType t => t -> Bool
isCanonicalTypeStruct ty = case getCanonicalType ty of
    TypeRef ref -> ref.cName.name.kind == C.NameKindTagged C.TagKindStruct
    _otherwise  -> False

-- | Is the canonical type a union type?
isCanonicalTypeUnion :: GetCanonicalType t => t -> Bool
isCanonicalTypeUnion ty = case getCanonicalType ty of
    TypeRef ref -> ref.cName.name.kind == C.NameKindTagged C.TagKindUnion
    _otherwise  -> False

-- | Is the canonical type a complex type?
isCanonicalTypeComplex :: GetCanonicalType t => t -> Bool
isCanonicalTypeComplex ty = case getCanonicalType ty of
    TypeComplex{} -> True
    _otherwise    -> False

-- | An array of known size or unknown size
data ArrayClassification t =
    -- | Array of known size
    ConstantArrayClassification
      -- | Array size
      Natural
      -- | Array element type
      t
    -- | Array of unkown size
  | IncompleteArrayClassification
      -- | Array element type
      t

getArrayElementType :: ArrayClassification t -> t
getArrayElementType (ConstantArrayClassification _ ty) = ty
getArrayElementType (IncompleteArrayClassification ty) = ty

-- | Is the canonical type an array type? If so, is it an array of known size or
-- unknown size? And what is the /full type/ of the array elements?
isCanonicalTypeArray :: FullType -> Maybe (ArrayClassification FullType)
isCanonicalTypeArray ty =
    case ty of
      TypePrim _pt          -> Nothing
      TypeRef _declId       -> Nothing
      TypeTypedef ref       -> isCanonicalTypeArray (eraseTypedef ref)
      TypePointers _n _t    -> Nothing
      TypeConstArray n t    -> Just (ConstantArrayClassification n t)
      TypeFun _args _res    -> Nothing
      TypeVoid              -> Nothing
      TypeIncompleteArray t -> Just (IncompleteArrayClassification t)
      TypeBlock _t          -> Nothing
      TypeQualified _q t    -> isCanonicalTypeArray t
      TypeExtBinding _reb   -> Nothing
      TypeComplex _pt       -> Nothing

-- | (Non-external) declarations referred to in this type
--
-- These are declarations that would appear in the generated Haskell type
-- whenever this type is used.
--
-- This does /not/ include any external declarations.
typeDeclIds :: Type -> [C.DeclIdPair]
typeDeclIds = \case
    -- Primitive types
    TypePrim _    -> []
    TypeVoid      -> []
    TypeComplex _ -> []

    -- Interesting cases
    TypeRef         declId  -> [declId]
    TypeTypedef     typedef -> [typedef.ref]
    TypeExtBinding  _ext    -> []

    -- Recurse
    TypePointers _      t -> typeDeclIds t
    TypeConstArray _    t -> typeDeclIds t
    TypeIncompleteArray t -> typeDeclIds t
    TypeBlock           t -> typeDeclIds t
    TypeQualified _     t -> typeDeclIds t
    TypeFun args res      -> concatMap typeDeclIds (args ++ [res])

{-------------------------------------------------------------------------------
  Types: Trees That Shrink

  Trees That Grow, but used as Trees That Shrink. Setting these type families to
  'Void' makes it impossible to construct or match on some of the constructors
  in the 'TypeF' datatype.
-------------------------------------------------------------------------------}

type family TypedefRefF tag :: Star
type family TypeQualifierF tag :: Star

-- | A full C type includes all C type constructs.
type FullType = TypeF Full
data Full
type instance TypedefRefF Full = TypedefRef
type instance TypeQualifierF Full = TypeQualifier

-- | An /erased/ C type is a C type without @typedef@s.
type ErasedType = TypeF Erased
data Erased
type instance TypedefRefF Erased = Void
type instance TypeQualifierF Erased = TypeQualifier

-- | A /canonical/ C type is a C type with all sugar removed, such as @typedef@s
-- and type qualifiers like @const@.
type CanonicalType = TypeF Canonical
data Canonical
type instance TypedefRefF Canonical = Void
type instance TypeQualifierF Canonical = Void

-- | Erase @typedef@s
--
-- Note: the algorithm to erase @typedef@s is simple. Replace any references to
-- @typedef@s we find by the definitions of these @typedef@s. The @typedef@
-- definitions that we inline this way can contain references to other
-- @typedef@s, but they can not construct infinitely long types, so this
-- algorithm will terminate sooner or later. In practice, @typedef@ "chains" are
-- probably not that long, so we do not expect this algorithm to have
-- problematic performance.
class GetErasedType t where
  -- | Obtain the /erased/ version of the given type
  getErasedType :: t -> ErasedType

instance GetErasedType  ErasedType where
  getErasedType = id

instance GetErasedType Type where
  getErasedType = mapTypeF fRef fQual
    where
      fRef = getErasedType . eraseTypedef
      fQual q t = TypeQualified q (getErasedType t)

-- | Note: canonicalise types.
--
-- The algorithm to canonicalise types performs the same @typedef@ erasure that
-- we perform in 'GetErasedType'. Along the way, any type qualifiers like
-- @const@ are also removed.
class GetCanonicalType t where
  -- | Obtain the /canonical/ version of the given type
  getCanonicalType :: t -> CanonicalType

instance GetCanonicalType CanonicalType where
  getCanonicalType = id

instance GetCanonicalType Type where
  getCanonicalType = mapTypeF fRef fQual
    where
      fRef = getCanonicalType . eraseTypedef
      fQual _ t = getCanonicalType t

-- | Erase one layer of a typedef, replacing it by its underlying type.
--
-- NOTE: the underlying type can arbitrarily reference other types, including
-- typedefs that we have not parsed. Use the underlying type with care!
eraseTypedef :: TypedefRef -> Type
eraseTypedef = (.underlying)

-- | Map 'TypeF's from one tag to another
mapTypeF ::
     forall tag tag'.
     -- | What to do when encountering a typedef reference.
     (TypedefRefF tag -> TypeF tag')
     -- | What to do when encountering a type qualifier.
  -> (TypeQualifierF tag -> TypeF tag -> TypeF tag')
  -> TypeF tag
  -> TypeF tag'
mapTypeF fRef fQual = go
  where
    go :: TypeF tag -> TypeF tag'
    go ty = case ty of
      TypePrim pt           -> TypePrim pt
      TypeRef declId        -> TypeRef declId
      TypeTypedef ref       -> fRef ref
      TypePointers n t      -> TypePointers n $ go t
      TypeConstArray n t    -> TypeConstArray n $ go t
      TypeFun args res      -> TypeFun (go <$> args) (go res)
      TypeVoid              -> TypeVoid
      TypeIncompleteArray t -> TypeIncompleteArray (go t)
      TypeBlock t           -> TypeBlock (go t)
      TypeQualified q t     -> fQual q t
      TypeExtBinding reb    -> TypeExtBinding reb
      TypeComplex pt        -> TypeComplex pt
