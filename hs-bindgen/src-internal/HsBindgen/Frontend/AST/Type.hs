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

import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass qualified as ResolveBindingSpecs
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | C types (use sites)
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

deriving stock instance ValidTypeTag tag => Show (TypeF tag)
deriving stock instance ValidTypeTag tag => Eq   (TypeF tag)
deriving stock instance ValidTypeTag tag => Ord  (TypeF tag)

-- | Map 'TypeF's from one tag to another
mapTypeF :: forall tag tag'.
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

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

data TypedefRef = TypedefRef {
      -- | Name of the referenced typedef declaration
      ref :: C.DeclIdPair

      -- | The underlying type of the referenced typedef declaration
      --
      -- NOTE: the underlying type can arbitrarily reference other types,
      -- including typedefs that we have not parsed. Use the underlying type
      -- with care!
    , underlying :: Type
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
class ( Show (TypedefRefF tag)
      , Eq   (TypedefRefF tag)
      , Ord  (TypedefRefF tag)

      , Show (TypeQualifierF tag)
      , Eq   (TypeQualifierF tag)
      , Ord  (TypeQualifierF tag)
      ) => ValidTypeTag (tag :: TypeTag) where
  type family TypedefRefF    tag :: Star
  type family TypeQualifierF tag :: Star

  getUnderlying :: TypedefRefF tag -> TypeF tag

instance ValidTypeTag Full where
  type instance TypedefRefF    Full = TypedefRef
  type instance TypeQualifierF Full = TypeQualifier

  getUnderlying = (.underlying)

instance ValidTypeTag Erased where
  type instance TypedefRefF    Erased = Void
  type instance TypeQualifierF Erased = TypeQualifier

  getUnderlying = absurd

instance ValidTypeTag Canonical where
  type instance TypedefRefF    Canonical = Void
  type instance TypeQualifierF Canonical = Void

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
  normalize :: TypeF tag -> TypeF tag'

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
      fRef :: TypedefRef -> TypeF Erased
      fRef ref = normalize ref.underlying

      fQual :: TypeQualifier -> TypeF Full -> TypeF Erased
      fQual qual typ = TypeQualified qual $ normalize typ

instance Normalize Erased Canonical where
  normalize = mapTypeF absurd fQual
    where
      fQual :: TypeQualifier -> TypeF Erased -> TypeF Canonical
      fQual _qual typ = normalize typ

instance Normalize Full Canonical where
  normalize = getCanonicalType . getErasedType

getCanonicalType :: Normalize tag Canonical => TypeF tag -> CanonicalType
getCanonicalType = normalize

getErasedType :: Normalize tag Erased => TypeF tag -> ErasedType
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

-- | Checks if a type is unsupported by Haskell's FFI
hasUnsupportedType :: Normalize tag Canonical => TypeF tag -> Bool
hasUnsupportedType = aux . getCanonicalType
  where
    aux :: CanonicalType -> Bool
    aux (TypeRef declId)      = auxRef declId
    aux TypeComplex{}         = True
    aux TypeConstArray{}      = True
    aux TypeIncompleteArray{} = True
    aux TypePrim{}            = False
    aux TypePointers{}        = False
    aux TypeFun{}             = False
    aux TypeVoid              = False
    aux TypeBlock{}           = False
    aux TypeExtBinding{}      = False

    auxRef :: C.DeclIdPair -> Bool
    auxRef declId =
        case declId.cName.name.kind of
          C.NameKindOrdinary               -> False
          C.NameKindTagged C.TagKindStruct -> True
          C.NameKindTagged C.TagKindUnion  -> True
          C.NameKindTagged C.TagKindEnum   -> False

{-------------------------------------------------------------------------------
  Classification: simple classifiers
-------------------------------------------------------------------------------}

-- TODO: Should this be replaced by @isCanonicalTypeVoid@?
isVoid :: Type -> Bool
isVoid TypeVoid = True
isVoid _        = False

-- | Is the canonical type a complex type?
isCanonicalTypeComplex :: Normalize tag Canonical => TypeF tag -> Bool
isCanonicalTypeComplex ty =
    case getCanonicalType ty of
      TypeComplex{} -> True
      _otherwise    -> False

-- | Is the canonical type a function type?
isCanonicalTypeFunction :: Normalize tag Canonical => TypeF tag -> Bool
isCanonicalTypeFunction ty =
    case getCanonicalType ty of
      TypeFun{} -> True
      _         -> False

-- | Is the canonical type a struct type?
isCanonicalTypeStruct :: Normalize tag Canonical => TypeF tag -> Bool
isCanonicalTypeStruct ty =
    case getCanonicalType ty of
      TypeRef ref -> ref.cName.name.kind == C.NameKindTagged C.TagKindStruct
      _otherwise  -> False

-- | Is the canonical type a union type?
isCanonicalTypeUnion :: Normalize tag Canonical => TypeF tag -> Bool
isCanonicalTypeUnion ty =
    case getCanonicalType ty of
      TypeRef ref -> ref.cName.name.kind == C.NameKindTagged C.TagKindUnion
      _otherwise  -> False

-- | Is the erased type @const@-qualified?
isErasedTypeConstQualified :: Normalize tag Erased => TypeF tag -> Bool
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
data ArrayClassification tag =
    -- | Array of known size
    ConstantArrayClassification
      Natural      -- ^ Array size
      (TypeF tag)  -- ^ Array element type

    -- | Array of unkown size
  | IncompleteArrayClassification
      (TypeF tag)  -- ^ Array element type

getArrayElementType :: ArrayClassification tag -> TypeF tag
getArrayElementType (ConstantArrayClassification _ ty) = ty
getArrayElementType (IncompleteArrayClassification ty) = ty

-- | Is the canonical type an array type?
--
-- If so, is it an array of known size or unknown size? And what is the /full
-- type/ of the array elements?
isCanonicalTypeArray ::
     ValidTypeTag tag
  => TypeF tag -> Maybe (ArrayClassification tag)
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

