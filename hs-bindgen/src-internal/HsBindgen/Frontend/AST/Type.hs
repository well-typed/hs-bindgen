-- | AST: C types (at use sites)
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.AST.Type qualified as C
module HsBindgen.Frontend.AST.Type (
    -- * Types
    Type
  , TypeF(
       TypePrim
     , TypeRef
     , TypeMacro
     , TypeTypedef
     , TypeFun
     , TypeVoid
     , TypeConstArray
     , TypeIncompleteArray
     , TypeBlock
     , TypeQual
     , TypeExtBinding
     , TypeComplex
     , TypePointers
     )
  , TypeQual(..)
  , Ref (..)

    -- * Normal forms
  , Normalize(..)

    -- * Queries
  , ValOrRef(..)
  , depsOfType
  , hasUnsupportedType

    -- * Classification
  , isVoid
  , isCanonicalTypeComplex
  , isCanonicalTypeFunction
  , isCanonicalTypeStruct
  , isCanonicalTypeUnion
  , isCanonicalTypeArray
  , isErasedTypeConstQualified

    -- ** Arrays
  , ArrayClassification(..)
  , getArrayElementType
  ) where

import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | C types (use sites)
data TypeF tag p =
    -- | Primitive types
    TypePrim C.PrimType

    -- | Complex type (such as @float complex@)
  | TypeComplex C.PrimType

    -- | Reference to named type other than a macro type or typedef
    --
    -- TODO: enum should get annotations with underlying types just like
    -- typedefs, so that we can erase the enum types and replace them with
    -- their underlying type.
  | TypeRef (Id p)

    -- | Reference to a macro type
    --
    -- NOTE: has a strictness annotation, which allows GHC to infer that
    -- pattern matches are redundant when @TypeMacroRefF tag p ~ Void@.
  | TypeMacro !(TypeMacroRefF tag p)

    -- | Reference to typedef
    --
    -- NOTE: has a strictness annotation, which allows GHC to infer that
    -- pattern matches are redundant when @TypedefRefF tag p ~ Void@.
  | TypeTypedef !(TypedefRefF tag p)

    -- | Pointer
    --
    -- This is /one/ layer of indirection; see also 'TypePointers'
  | TypeUnsafePointer (TypeF tag p)

    -- | Array of constant size
  | TypeConstArray Natural (TypeF tag p)

    -- | Array of unknown size
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
  | TypeIncompleteArray (TypeF tag p)

    -- | Functions
  | TypeFun [TypeF tag p] (TypeF tag p)

    -- | Void
    --
    -- NOTE: @void@ has different meanings in C, depending on context.
  | TypeVoid

    -- | Blocks
    --
    -- Blocks are a clang-specific C extension.
    --
    -- See <https://clang.llvm.org/docs/BlockLanguageSpec.html>
  | TypeBlock (TypeF tag p)

    -- | Qualified type (such as @const@)
    --
    -- NOTE: has a strictness annotation, which allows GHC to infer that pattern
    -- matches are redundant when @TypeQualifierF tag p ~ Void@.
  | TypeQual !(TypeQualifierF tag p) (TypeF tag p)

    -- | Type with an external binding
    --
    -- NOTE: has a strictness annotation, which allows GHC to infer that pattern
    -- matches are redundant when @TypeExtBindingRefF tag p ~ Void@.
  | TypeExtBinding !(TypeExtBindingRefF tag p)
  deriving stock Generic

deriving stock instance ValidTypeTag tag p => Show (TypeF tag p)
deriving stock instance ValidTypeTag tag p => Eq   (TypeF tag p)
deriving stock instance ValidTypeTag tag p => Ord  (TypeF tag p)

-- | Map 'TypeF's from one tag to another
mapTypeF :: forall tag tag' p.
     -- | What to do when encountering a typedef reference.
     (TypedefRefF tag p -> TypeF tag' p)
     -- | What to do when encountering a type qualifier.
  -> (TypeQualifierF tag p -> TypeF tag p -> TypeF tag' p)
     -- | What to do when encountering an external binding reference.
  -> (TypeExtBindingRefF tag p -> TypeF tag' p)
     -- | What to do when encountering a macro type reference.
  -> (TypeMacroRefF tag p -> TypeF tag' p)
  -> TypeF tag  p
  -> TypeF tag' p
mapTypeF fTypedefRef fQual fExtBindingRef fMacroRef = go
  where
    go :: TypeF tag p -> TypeF tag' p
    go ty = case ty of
      TypePrim pt           -> TypePrim pt
      TypeRef declId        -> TypeRef declId
      TypeMacro ref         -> fMacroRef ref
      TypeTypedef ref       -> fTypedefRef ref
      TypePointers n t      -> TypePointers n $ go t
      TypeConstArray n t    -> TypeConstArray n $ go t
      TypeFun args res      -> TypeFun (go <$> args) (go res)
      TypeVoid              -> TypeVoid
      TypeIncompleteArray t -> TypeIncompleteArray (go t)
      TypeBlock t           -> TypeBlock (go t)
      TypeQual q t          -> fQual q t
      TypeExtBinding ref    -> fExtBindingRef ref
      TypeComplex pt        -> TypeComplex pt

{-------------------------------------------------------------------------------
  Qualifiers
-------------------------------------------------------------------------------}

data TypeQual =
    QualConst
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  References
-------------------------------------------------------------------------------}

-- |
--
-- For example, if we have this C code:
--
-- > typedef int T;
-- > extern T x;
--
-- The type of the global variable @x@ is roughly:
--
-- > Ref { name = "T", underlying = TypePrim int }
--
type TypedefRef p = Ref (Id p) p

-- |
--
-- For example, if we have this C code:
--
-- > struct S {};
-- > extern S x;
--
-- The type of the global variable @x@ is roughly:
--
-- > Ref { name = ResolvedBinding "S", underlying = TypeRef ("S", StructKind) }
--
type ExtBindingRef p = Ref (ExtBinding p) p

-- |
--
-- For example, if we have this C code:
--
-- > #define T int
-- > extern T x;
--
-- The type of the global variable @x@ is roughly:
--
-- > Ref { name = "T", underlying = TypePrim int }
--
type MacroRef p = Ref (MacroId p) p

-- | A reference (by name) to another type, annotated with an underlying type.
--
-- Reference types include:
--
-- * struct reference
-- * union reference
-- * enum reference
-- * typedef reference
-- * macro typedef reference
-- * external binding reference
--
-- See 'TypedefRef' and 'ExtBindingRef' for examples.
--
data Ref a p = Ref {
    -- | The reference type: a name
    --
    -- NOTE: has a strictness annotation, which allows GHC to infer that pattern
    -- matches are redundant when @a ~ Void@.
    name :: !a
    -- | The underlying type.
    --
    -- NOTE: the underlying type can arbitrarily reference other types,
    -- including references that we have not parsed, mangled, modified, resolved
    -- (binding specs), etc. Use the underlying type with care!
  , underlying :: Type p
  }
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Normal forms

  Trees That Grow, but used as Trees That Shrink. Setting these type families to
  'Void' makes it impossible to construct or match on some of the constructors
  in the 'TypeF' datatype.
-------------------------------------------------------------------------------}

data TypeTag =
    Full       -- ^ A full C type includes all C type constructs.
  | Erased     -- ^ No @typedef@s.
  | Canonical  -- ^ All sugar (typedefs and qualifiers like @const@) removed

-- | Normal forms of types
class ( IsPass p

      , Show (TypedefRefF tag p)
      , Eq   (TypedefRefF tag p)
      , Ord  (TypedefRefF tag p)

      , Show (TypeQualifierF tag p)
      , Eq   (TypeQualifierF tag p)
      , Ord  (TypeQualifierF tag p)

      , Show (TypeExtBindingRefF tag p)
      , Eq   (TypeExtBindingRefF tag p)
      , Ord  (TypeExtBindingRefF tag p)

      , Show (TypeMacroRefF tag p)
      , Eq   (TypeMacroRefF tag p)
      , Ord  (TypeMacroRefF tag p)
      ) => ValidTypeTag (tag :: TypeTag) (p :: Pass) where
  type family TypedefRefF     tag p :: Star
  type family TypeQualifierF  tag p :: Star
  type family TypeExtBindingRefF tag p :: Star
  type family TypeMacroRefF tag p :: Star

  getTypedefUnderlying :: TypedefRefF tag p -> TypeF tag p
  getMacroUnderlying :: TypeMacroRefF tag p -> TypeF tag p
  getExtBindingUnderlying :: TypeExtBindingRefF tag p -> TypeF tag p

instance IsPass p => ValidTypeTag Full p where
  type instance TypedefRefF        Full p = TypedefRef p
  type instance TypeQualifierF     Full p = TypeQual
  type instance TypeExtBindingRefF Full p = ExtBindingRef p
  type instance TypeMacroRefF      Full p = MacroRef p

  getTypedefUnderlying = (.underlying)
  getMacroUnderlying = (.underlying)
  getExtBindingUnderlying = (.underlying)

instance IsPass p => ValidTypeTag Erased p where
  type instance TypedefRefF        Erased p = Void
  type instance TypeQualifierF     Erased p = TypeQual
  type instance TypeExtBindingRefF Erased p = Void
  type instance TypeMacroRefF      Erased p = Void

  getTypedefUnderlying = absurd
  getMacroUnderlying = absurd
  getExtBindingUnderlying = absurd

instance IsPass p => ValidTypeTag Canonical p where
  type instance TypedefRefF        Canonical p = Void
  type instance TypeQualifierF     Canonical p = Void
  type instance TypeExtBindingRefF Canonical p = Void
  type instance TypeMacroRefF      Canonical p = Void

  getTypedefUnderlying = absurd
  getMacroUnderlying = absurd
  getExtBindingUnderlying = absurd

type Type          = FullType
type FullType      = TypeF Full
type ErasedType    = TypeF Erased
type CanonicalType = TypeF Canonical

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
pattern TypePointers :: Int -> TypeF tag p -> TypeF tag p
pattern TypePointers n inner <- (stripPointersF -> Just (n, inner))
  where
    TypePointers n inner = buildPointersF n inner

-- | Helper for TypePointers pattern synonym (matching direction)
--
-- Strips all pointer layers and returns the count and inner type.
-- Returns Nothing if there are no pointer layers.
stripPointersF :: TypeF tag p -> Maybe (Int, TypeF tag p)
stripPointersF = go 0
  where
    go :: Int -> TypeF tag p -> Maybe (Int, TypeF tag p)
    go !n (TypeUnsafePointer inner) = go (n + 1) inner
    go !n inner
      | n > 0     = Just (n, inner)
      | otherwise = Nothing

-- | Helper for TypePointers pattern synonym (construction direction)
--
-- Builds N layers of pointers around an inner type.
buildPointersF :: Int -> TypeF tag p -> TypeF tag p
buildPointersF n inner
  | n <= 0    = inner
  | otherwise = TypeUnsafePointer (buildPointersF (n - 1) inner)

-- | COMPLETE pragma to ensure exhaustiveness checking works
--
-- This tells GHC that pattern matching on these patterns (instead of the raw
-- TypeUnsafePointer) is complete and exhaustive.
{-# COMPLETE
       TypePrim
     , TypeRef
     , TypeMacro
     , TypeTypedef
     , TypePointers
     , TypeFun
     , TypeVoid
     , TypeConstArray
     , TypeIncompleteArray
     , TypeBlock
     , TypeQual
     , TypeExtBinding
     , TypeComplex
  #-}

{-------------------------------------------------------------------------------
  Computing normal forms
-------------------------------------------------------------------------------}

class Normalize tag tag' where
  normalize :: TypeF tag p -> TypeF tag' p

instance Normalize tag tag where
  normalize = id

-- | Erase @typedef@s
--
-- Note: the algorithm to erase @typedef@s is simple. Replace any references to
-- @typedef@s we find by the definitions of these @typedef@s. The @typedef@
-- definitions that we inline this way can contain references to other
-- @typedef@s, but they can not construct infinitely long types, so this
-- algorithm will terminate sooner or later. In practice, @typedef@ "chains" are
-- probably not that long, so we do not expect this algorithm to have
-- problematic performance.
instance Normalize Full Erased where
  normalize = mapTypeF fTypedefRef fQual fExtBindingRef fMacroRef
    where
      fTypedefRef :: TypedefRef p -> TypeF Erased p
      fTypedefRef ref = normalize ref.underlying

      fQual :: TypeQual -> TypeF Full p -> TypeF Erased p
      fQual qual typ = TypeQual qual $ normalize typ

      fExtBindingRef :: ExtBindingRef p -> TypeF Erased p
      fExtBindingRef ref = normalize ref.underlying

      fMacroRef :: MacroRef p -> TypeF Erased p
      fMacroRef ref = normalize ref.underlying

instance Normalize Erased Canonical where
  normalize = mapTypeF absurd fQual absurd absurd
    where
      fQual :: TypeQual -> TypeF Erased p -> TypeF Canonical p
      fQual _qual typ = normalize typ

instance Normalize Full Canonical where
  normalize = getCanonicalType . getErasedType

getCanonicalType :: Normalize tag Canonical => TypeF tag p -> CanonicalType p
getCanonicalType = normalize

getErasedType :: Normalize tag Erased => TypeF tag p -> ErasedType p
getErasedType = normalize

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

data ValOrRef = ByValue | ByRef
  deriving stock (Show, Eq, Ord)

-- | The declarations this type depends on (direct dependencies only)
--
-- We also report whether this dependence is through a pointer or not.
depsOfType :: forall p. IsPass p => Type p -> [(ValOrRef, Id p)]
depsOfType = \case
    -- Primitive types
    TypePrim _    -> []
    TypeVoid      -> []
    TypeComplex _ -> []

    -- Interesting cases
    TypeRef ref         -> [(ByValue, ref)]
    TypeMacro ref       -> [(ByValue, macroIdId (Proxy @p) ref.name)]
    TypeTypedef ref     -> [(ByValue, ref.name)]
    TypeUnsafePointer t -> first (const ByRef) <$> depsOfType t

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1467>
    -- We could in /principle/ use extBindingId here to implement this case
    -- properly. However, one of the use cases of 'depsOfType' is in a check
    -- whether a type is defined in the current module; 'extBindingId' currently
    -- omits the module, so this could result in potentially incorrect
    -- conclusions (if there happens to be a /another/ type of the same name).
    -- TypeExtBinding extBinding -> [(ByValue, extBindingId (Proxy @p) extBinding)]
    TypeExtBinding _extBinding -> []

    -- Recurse
    TypeConstArray _    t -> depsOfType t
    TypeIncompleteArray t -> depsOfType t
    TypeBlock           t -> depsOfType t
    TypeQual _          t -> depsOfType t
    TypeFun args res      -> concatMap depsOfType args <> depsOfType res

-- | Checks if a type is unsupported by Haskell's FFI
hasUnsupportedType :: forall tag p.
     (IsPass p, Normalize tag Canonical)
  => TypeF tag p -> Bool
hasUnsupportedType = aux . getCanonicalType
  where
    aux :: CanonicalType p -> Bool
    aux (TypeRef declId)      = auxRef (idNameKind (Proxy @p) declId)
    aux TypeComplex{}         = True
    aux TypeConstArray{}      = True
    aux TypeIncompleteArray{} = True
    aux TypePrim{}            = False
    aux TypePointers{}        = False
    aux TypeFun{}             = False
    aux TypeVoid              = False
    aux TypeBlock{}           = False

    auxRef :: C.NameKind -> Bool
    auxRef = \case
          C.NameKindOrdinary               -> False
          C.NameKindTagged C.TagKindStruct -> True
          C.NameKindTagged C.TagKindUnion  -> True
          C.NameKindTagged C.TagKindEnum   -> False

{-------------------------------------------------------------------------------
  Classification: simple classifiers
-------------------------------------------------------------------------------}

-- TODO: Should this be replaced by @isCanonicalTypeVoid@?
isVoid :: Type p -> Bool
isVoid TypeVoid = True
isVoid _        = False

-- | Is the canonical type a complex type?
isCanonicalTypeComplex :: Normalize tag Canonical => TypeF tag p -> Bool
isCanonicalTypeComplex ty =
    case getCanonicalType ty of
      TypeComplex{} -> True
      _otherwise    -> False

-- | Is the canonical type a function type?
isCanonicalTypeFunction :: Normalize tag Canonical => TypeF tag p -> Bool
isCanonicalTypeFunction ty =
    case getCanonicalType ty of
      TypeFun{} -> True
      _         -> False

-- | Is the canonical type a struct type?
isCanonicalTypeStruct :: forall tag p.
     (Normalize tag Canonical, IsPass p)
  => TypeF tag p -> Bool
isCanonicalTypeStruct ty =
    case getCanonicalType ty of
      TypeRef ref -> idNameKind (Proxy @p) ref == C.NameKindTagged C.TagKindStruct
      _otherwise  -> False

-- | Is the canonical type a union type?
isCanonicalTypeUnion :: forall tag p.
     (Normalize tag Canonical, IsPass p)
  => TypeF tag p -> Bool
isCanonicalTypeUnion ty =
    case getCanonicalType ty of
      TypeRef ref -> idNameKind (Proxy @p) ref == C.NameKindTagged C.TagKindUnion
      _otherwise  -> False

-- | Is the erased type @const@-qualified?
isErasedTypeConstQualified :: Normalize tag Erased => TypeF tag p -> Bool
isErasedTypeConstQualified ty =
    case getErasedType ty of
      -- Types can be directly @const@-qualified,
      TypeQual QualConst _ -> True

      -- but arrays are also @const@-qualified if their element type is. Note
      -- that elements of arrays can themselves be arrays, hence we recurse into
      -- the array element type.
      TypeConstArray _    ty' -> isErasedTypeConstQualified ty'
      TypeIncompleteArray ty' -> isErasedTypeConstQualified ty'

      -- And otherwise, the type is not considered to be @const@-qualified.
      _ -> False

{-------------------------------------------------------------------------------
  Classification: arrays
-------------------------------------------------------------------------------}

-- | An array of known size or unknown size
data ArrayClassification tag p =
    -- | Array of known size
    ConstantArrayClassification
      Natural        -- ^ Array size
      (TypeF tag p)  -- ^ Array element type

    -- | Array of unkown size
  | IncompleteArrayClassification
      (TypeF tag p)  -- ^ Array element type

getArrayElementType :: ArrayClassification tag p -> TypeF tag p
getArrayElementType (ConstantArrayClassification _ ty) = ty
getArrayElementType (IncompleteArrayClassification ty) = ty

-- | Is the canonical type an array type?
--
-- If so, is it an array of known size or unknown size? And what is the /full
-- type/ of the array elements?
isCanonicalTypeArray ::
     ValidTypeTag tag p
  => TypeF tag p -> Maybe (ArrayClassification tag p)
isCanonicalTypeArray ty =
    -- We do not use getCanonicalType here, because we might not want to
    -- canonicalize the array /element/ type.
    case ty of
      TypePrim _pt          -> Nothing
      TypeRef _declId       -> Nothing
      TypeMacro ref         -> isCanonicalTypeArray (getMacroUnderlying ref)
      TypeTypedef ref       -> isCanonicalTypeArray (getTypedefUnderlying ref)
      TypePointers _n _t    -> Nothing
      TypeConstArray n t    -> Just (ConstantArrayClassification n t)
      TypeFun _args _res    -> Nothing
      TypeVoid              -> Nothing
      TypeIncompleteArray t -> Just (IncompleteArrayClassification t)
      TypeBlock _t          -> Nothing
      TypeQual _q t         -> isCanonicalTypeArray t
      TypeExtBinding ref    -> isCanonicalTypeArray (getExtBindingUnderlying ref)
      TypeComplex _pt       -> Nothing
