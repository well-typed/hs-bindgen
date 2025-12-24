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
     , TypeTypedef
     , TypeFun
     , TypeVoid
     , TypeConstArray
     , TypeIncompleteArray
     , TypeBlock
     , TypeQualified
     , TypeExtBinding
     , TypeComplex
     , TypePointers
     )
  , TypedefRef(..)
  , TypeQualifier(..)

    -- * Normal forms
  , Normalize(..)

    -- * Simple queries
  , typeDeclIds
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
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass qualified as ResolveBindingSpecs
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | C types (use sites)
data TypeF tag p =
    TypePrim C.PrimType
  | TypeRef (Id p)
  | TypeTypedef
    -- | NOTE: has a strictness annotation, which allows GHC to infer that
    -- pattern matches are redundant when @TypedefRefF tag ~ Void@.
    !(TypedefRefF tag p)
    -- TODO: macros should get annotations with underlying types just like
    -- typedefs, so that we can erase the macro types and replace them with
    -- their underlying type. See issue #1200.
  | TypeUnsafePointer (TypeF tag p)
  | TypeConstArray Natural (TypeF tag p)
  | TypeFun [TypeF tag p] (TypeF tag p)
  | TypeVoid
  | TypeIncompleteArray (TypeF tag p)
  | TypeBlock (TypeF tag p)
  | TypeQualified
      -- | NOTE: has a strictness annotation, which allows GHC to infer that
      -- pattern matches are redundant when @TypeQualifierF tag ~ Void@.
      !(TypeQualifierF tag p)
      (TypeF tag p)
  | TypeExtBinding ResolveBindingSpecs.ResolvedExtBinding
  | TypeComplex C.PrimType
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
  -> TypeF tag  p
  -> TypeF tag' p
mapTypeF fRef fQual = go
  where
    go :: TypeF tag p -> TypeF tag' p
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

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

data TypedefRef p = TypedefRef {
      -- | Name of the referenced typedef declaration
      ref :: Id p

      -- | The underlying type of the referenced typedef declaration
      --
      -- NOTE: the underlying type can arbitrarily reference other types,
      -- including typedefs that we have not parsed. Use the underlying type
      -- with care!
    , underlying :: Type p
    }
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Qualifiers
-------------------------------------------------------------------------------}

data TypeQualifier = TypeQualifierConst
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
      ) => ValidTypeTag (tag :: TypeTag) (p :: Pass) where
  type family TypedefRefF    tag p :: Star
  type family TypeQualifierF tag p :: Star

  getUnderlying :: TypedefRefF tag p -> TypeF tag p

instance IsPass p => ValidTypeTag Full p where
  type instance TypedefRefF    Full p = TypedefRef p
  type instance TypeQualifierF Full p = TypeQualifier

  getUnderlying = (.underlying)

instance IsPass p => ValidTypeTag Erased p where
  type instance TypedefRefF    Erased p = Void
  type instance TypeQualifierF Erased p = TypeQualifier

  getUnderlying = absurd

instance IsPass p => ValidTypeTag Canonical p where
  type instance TypedefRefF    Canonical p = Void
  type instance TypeQualifierF Canonical p = Void

  getUnderlying = absurd

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
     , TypeTypedef
     , TypePointers
     , TypeFun
     , TypeVoid
     , TypeConstArray
     , TypeIncompleteArray
     , TypeBlock
     , TypeQualified
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
  normalize = mapTypeF fRef fQual
    where
      fRef :: TypedefRef p -> TypeF Erased p
      fRef ref = normalize ref.underlying

      fQual :: TypeQualifier -> TypeF Full p -> TypeF Erased p
      fQual qual typ = TypeQualified qual $ normalize typ

instance Normalize Erased Canonical where
  normalize = mapTypeF absurd fQual
    where
      fQual :: TypeQualifier -> TypeF Erased p -> TypeF Canonical p
      fQual _qual typ = normalize typ

instance Normalize Full Canonical where
  normalize = getCanonicalType . getErasedType

getCanonicalType :: Normalize tag Canonical => TypeF tag p -> CanonicalType p
getCanonicalType = normalize

getErasedType :: Normalize tag Erased => TypeF tag p -> ErasedType p
getErasedType = normalize

{-------------------------------------------------------------------------------
  Simple queries
-------------------------------------------------------------------------------}

-- | (Non-external) declarations referred to in this type
--
-- These are declarations that would appear in the generated Haskell type
-- whenever this type is used.
--
-- This does /not/ include any external declarations.
typeDeclIds :: Type p -> [Id p]
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
    aux TypeExtBinding{}      = False

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
      TypeQualified TypeQualifierConst _ -> True

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
      TypeTypedef ref       -> isCanonicalTypeArray (getUnderlying ref)
      TypePointers _n _t    -> Nothing
      TypeConstArray n t    -> Just (ConstantArrayClassification n t)
      TypeFun _args _res    -> Nothing
      TypeVoid              -> Nothing
      TypeIncompleteArray t -> Just (IncompleteArrayClassification t)
      TypeBlock _t          -> Nothing
      TypeQualified _q t    -> isCanonicalTypeArray t
      TypeExtBinding _reb   -> Nothing
      TypeComplex _pt       -> Nothing

