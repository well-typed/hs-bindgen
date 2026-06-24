-- | C types (use sites)
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.C" should be used.
--
-- Within @HsBindgen.IR@, all modules aside from "HsBindgen.IR.C" should import
-- this module qualified for consistency.
--
-- > import HsBindgen.IR.C.Type qualified as C
module HsBindgen.IR.C.Type (
    -- * Definition
    Type
  , TypeF(
       TypePrim
     , TypeComplex
     , TypeRef
     , TypeEnum
     , TypeMacro
     , TypeTypedef
     , TypePointers
     , TypeConstArray
     , TypeIncompleteArray
     , TypeFun
     , TypeVoid
     , TypeBlock
     , TypeQual
     , TypeExtBinding
     )
  , TypeFunArg
  , TypeFunArgF(..)

    -- ** Qualifiers
  , TypeQual(..)

    -- ** References
  , Ref(..)
  , EnumRef
  , MacroRef(..)
  , TypedefRef

    -- * Normal forms
  , Normalize(..)
  , getCanonicalType

    -- * Queries
  , ValOrRef(..)
  , depsOfType
  , depsOfTypeFunArg
  , hasUnsupportedType
  , getAllFunTypeIndirections
  , getAllFunTypes
  , getFirstFunTypeIndirection

    -- * Classification
  , isVoid
  , isCanonicalTypeComplex
  , isCanonicalTypeFunction
  , isCanonicalTypeStruct
  , isCanonicalTypeUnion
  , isCanonicalTypeArray
  , isErasedTypeConstQualified
  ) where

import Data.Foldable qualified as Foldable
import Data.Set qualified as Set

import HsBindgen.Errors (panicPure)
import HsBindgen.Imports
import HsBindgen.IR.C.Naming qualified as C
import HsBindgen.IR.Pass.Ann
import HsBindgen.IR.Pass.Definition
import HsBindgen.IR.Pass.ExtBinding
import HsBindgen.IR.Pass.Id
import HsBindgen.IR.Pass.Macro
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Type = FullType

-- | C type (use site)
data TypeF (tag :: TypeTag) (p :: Pass) =
    -- | Primitive type
    TypePrim C.PrimType

    -- | Complex type (such as @float complex@)
  | TypeComplex C.PrimType

    -- | Reference to named type other than an @enum@, macro type or @typedef@
  | TypeRef (Id p)

    -- | Reference to an @enum@
    --
    -- NOTE: has a strictness annotation, which allows GHC to infer that
    -- pattern matches are redundant when @TypeEnumRefF tag p ~ Void@.
  | TypeEnum !(TypeEnumRefF tag p)

    -- | Reference to a macro type
    --
    -- NOTE: has a strictness annotation, which allows GHC to infer that
    -- pattern matches are redundant when @TypeMacroRefF tag p ~ Void@.
  | TypeMacro !(TypeMacroRefF tag p)

    -- | Reference to a @typedef@
    --
    -- NOTE: has a strictness annotation, which allows GHC to infer that
    -- pattern matches are redundant when @TypedefRefF tag p ~ Void@.
  | TypeTypedef !(TypedefRefF tag p)

    -- | Pointer
    --
    -- This is /one/ layer of indirection.  See also 'TypePointers'.
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
  | TypeFun [TypeFunArgF tag p] (TypeF tag p)

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

deriving stock instance ValidTypeTag tag p => Eq   (TypeF tag p)
deriving stock instance ValidTypeTag tag p => Ord  (TypeF tag p)
deriving stock instance ValidTypeTag tag p => Show (TypeF tag p)

type TypeFunArg = TypeFunArgF Full

-- | C types in function argument positions
--
-- Separate types are used to represent function arguments in declarations
-- ('HsBindgen.IR.C.Decl.FunctionArg') and function arguments in types.
--
-- * An argument in a declaration may have a name, while type arguments do not
--   have names.
-- * We translate declaration arguments to Haskell, while recursively
--   translating type arguments is not necessary.
--
-- Both of these types use the @TypeFunArg@ annotation, however.
data TypeFunArgF (tag :: TypeTag) (p :: Pass) = TypeFunArgF {
    typ :: TypeF tag p
  , ann :: Ann "TypeFunArg" p
  }
  deriving stock (Generic)

deriving stock instance ValidTypeTag tag p => Eq   (TypeFunArgF tag p)
deriving stock instance ValidTypeTag tag p => Show (TypeFunArgF tag p)
deriving stock instance ValidTypeTag tag p => Ord  (TypeFunArgF tag p)

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
  -- | What to do when encountering an enum reference.
  -> (TypeEnumRefF tag p -> TypeF tag' p)
  -> TypeF tag  p
  -> TypeF tag' p
mapTypeF fTypedefRef fQual fExtBindingRef fMacroRef fEnumRef = go
  where
    go :: TypeF tag p -> TypeF tag' p
    go ty = case ty of
      TypePrim pt           -> TypePrim pt
      TypeComplex pt        -> TypeComplex pt
      TypeRef declId        -> TypeRef declId
      TypeEnum ref          -> fEnumRef ref
      TypeMacro ref         -> fMacroRef ref
      TypeTypedef ref       -> fTypedefRef ref
      TypePointers n t      -> TypePointers n $ go t
      TypeConstArray n t    -> TypeConstArray n $ go t
      TypeIncompleteArray t -> TypeIncompleteArray (go t)
      TypeFun args res      -> TypeFun (fmap goTypeFunArg args) (go res)
      TypeVoid              -> TypeVoid
      TypeBlock t           -> TypeBlock (go t)
      TypeQual q t          -> fQual q t
      TypeExtBinding ref    -> fExtBindingRef ref

    goTypeFunArg :: TypeFunArgF tag p -> TypeFunArgF tag' p
    goTypeFunArg arg = TypeFunArgF {
          typ = go arg.typ
        , ann = arg.ann
        }

{-------------------------------------------------------------------------------
  Qualifiers
-------------------------------------------------------------------------------}

data TypeQual =
    QualConst
  deriving stock (Eq, Generic, Ord, Show)

{-------------------------------------------------------------------------------
  References
-------------------------------------------------------------------------------}

-- | A reference (by name) to another type, annotated with an underlying type
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
data Ref a (p :: Pass) = Ref {
    -- | The reference type: a name
    --
    -- NOTE: has a strictness annotation, which allows GHC to infer that pattern
    -- matches are redundant when @a ~ Void@.
    name :: !a
    -- | The underlying type.
    --
    -- NOTE: the underlying type can arbitrarily reference other types,
    -- including references that we have not parsed, mangled, modified, resolved
    -- (binding specs), etc.  Use the underlying type with care!
  , underlying :: Type p
  }
  deriving stock (Eq, Generic, Ord, Show)

-- | @enum@ reference
--
-- > enum E : u_int { e };
-- > extern enum E x;
--
-- The type of the global variable @x@ is roughly:
--
-- > Ref { ref = "E", underlying = TypePrim u_int }
type EnumRef p = Ref (Id p) p

-- | A reference to a macro use site.
--
-- Structurally similar to 'Ref' but the 'underlying' field is driven by the
-- 'MacroUnderlying' associated type family, so it can be a placeholder (@()@)
-- during the
-- 'HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.ReparseMacroExpansions'
-- pass and the real @'Type' p@ after the pass is finished.
--
-- For example, if we have this C code:
--
-- > #define T int
-- > extern T x;
--
-- The type of the global variable @x@ is roughly:
--
-- > MacroRef { name = macroIdOfT, underlying = TypePrim int }
data MacroRef p = MacroRef {
    name       :: !(MacroId p)
  , underlying :: !(MacroUnderlying p)
  }
  deriving stock (Generic)

deriving stock instance
     (Eq (MacroId p), Eq (MacroUnderlying p))
  => Eq (MacroRef p)
deriving stock instance
     (Ord (MacroId p), Ord (MacroUnderlying p))
  => Ord (MacroRef p)
deriving stock instance
     (Show (MacroId p), Show (MacroUnderlying p))
  => Show (MacroRef p)

-- | @typedef@ reference
--
-- For example, if we have this C code:
--
-- > typedef int T;
-- > extern T x;
--
-- The type of the global variable @x@ is roughly:
--
-- > Ref { name = "T", underlying = TypePrim int }
type TypedefRef p = Ref (Id p) p

-- | External binding reference
--
-- For example, if we have this C code:
--
-- > struct S {};
-- > extern S x;
--
-- The type of the global variable @x@ is roughly:
--
-- > Ref { name = ResolvedBinding "S", underlying = TypeRef ("S", StructKind) }
type ExtBindingRef p = Ref (ExtBinding p) p

{-------------------------------------------------------------------------------
  Normal forms

  Trees That Grow, but used as Trees That Shrink. Setting these type families to
  'Void' makes it impossible to construct or match on some of the constructors
  in the 'TypeF' datatype.
-------------------------------------------------------------------------------}

data TypeTag =
    -- | Full C type, includes all C type constructs
    Full
    -- | No @typedef@s, external binding specification references, macro-defined
    -- types, or @enum@s
  | Erased
    -- | All sugar (typedefs and qualifiers like @const@) removed
  | Canonical

type FullType      = TypeF Full
type ErasedType    = TypeF Erased
type CanonicalType = TypeF Canonical

-- | Normal forms of types
class ( PassAnn        p
      , PassExtBinding p
      , PassId         p
      , PassMacro      p

      , Show (TypeEnumRefF tag p)
      , Eq   (TypeEnumRefF tag p)
      , Ord  (TypeEnumRefF tag p)

      , Show (TypeMacroRefF tag p)
      , Eq   (TypeMacroRefF tag p)
      , Ord  (TypeMacroRefF tag p)

      , Show (TypedefRefF tag p)
      , Eq   (TypedefRefF tag p)
      , Ord  (TypedefRefF tag p)

      , Show (TypeQualifierF tag p)
      , Eq   (TypeQualifierF tag p)
      , Ord  (TypeQualifierF tag p)

      , Show (TypeExtBindingRefF tag p)
      , Eq   (TypeExtBindingRefF tag p)
      , Ord  (TypeExtBindingRefF tag p)
      ) => ValidTypeTag (tag :: TypeTag) (p :: Pass) where
  type family TypeEnumRefF       tag p :: Star
  type family TypeMacroRefF      tag p :: Star
  type family TypedefRefF        tag p :: Star
  type family TypeQualifierF     tag p :: Star
  type family TypeExtBindingRefF tag p :: Star

instance (
      PassAnn        p
    , PassExtBinding p
    , PassId         p
    , PassMacro      p
    ) => ValidTypeTag Full p where
  type instance TypeEnumRefF       Full p = EnumRef p
  type instance TypeMacroRefF      Full p = MacroRef p
  type instance TypedefRefF        Full p = TypedefRef p
  type instance TypeQualifierF     Full p = TypeQual
  type instance TypeExtBindingRefF Full p = ExtBindingRef p

instance (
      PassAnn        p
    , PassExtBinding p
    , PassId         p
    , PassMacro      p
    ) => ValidTypeTag Erased p where
  type instance TypeEnumRefF       Erased p = Void
  type instance TypeMacroRefF      Erased p = Void
  type instance TypedefRefF        Erased p = Void
  type instance TypeQualifierF     Erased p = TypeQual
  type instance TypeExtBindingRefF Erased p = Void

instance (
      PassAnn        p
    , PassExtBinding p
    , PassId         p
    , PassMacro      p
    ) => ValidTypeTag Canonical p where
  type instance TypeEnumRefF       Canonical p = Void
  type instance TypeMacroRefF      Canonical p = Void
  type instance TypedefRefF        Canonical p = Void
  type instance TypeQualifierF     Canonical p = Void
  type instance TypeExtBindingRefF Canonical p = Void

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

-- | Helper for 'TypePointers' pattern synonym (matching direction)
--
-- Strips all pointer layers and returns the count and inner type.  Returns
-- 'Nothing' if there are no pointer layers.
stripPointersF :: TypeF tag p -> Maybe (Int, TypeF tag p)
stripPointersF = go 0
  where
    go :: Int -> TypeF tag p -> Maybe (Int, TypeF tag p)
    go !n (TypeUnsafePointer inner) = go (n + 1) inner
    go !n inner
      | n > 0     = Just (n, inner)
      | otherwise = Nothing

-- | Helper for 'TypePointers' pattern synonym (construction direction)
--
-- Builds N layers of pointers around an inner type.
buildPointersF :: Int -> TypeF tag p -> TypeF tag p
buildPointersF n inner
  | n <= 0    = inner
  | otherwise = TypeUnsafePointer (buildPointersF (n - 1) inner)

-- | COMPLETE pragma to ensure exhaustiveness checking works
--
-- This tells GHC that pattern matching on these patterns (instead of the raw
-- @TypeUnsafePointer@) is complete and exhaustive.
{-# COMPLETE
       TypePrim
     , TypeComplex
     , TypeRef
     , TypeEnum
     , TypeMacro
     , TypeTypedef
     , TypePointers
     , TypeConstArray
     , TypeIncompleteArray
     , TypeFun
     , TypeVoid
     , TypeBlock
     , TypeQual
     , TypeExtBinding
  #-}

_completePragmaCoversAllCases :: TypeF tag p -> ()
_completePragmaCoversAllCases = \case
    TypePrim{}            -> ()
    TypeComplex{}         -> ()
    TypeRef{}             -> ()
    TypeEnum{}            -> ()
    TypeMacro{}           -> ()
    TypeTypedef{}         -> ()
    TypeUnsafePointer{}   -> ()
    TypeConstArray{}      -> ()
    TypeIncompleteArray{} -> ()
    TypeFun{}             -> ()
    TypeVoid{}            -> ()
    TypeBlock{}           -> ()
    TypeQual{}            -> ()
    TypeExtBinding{}      -> ()

{-------------------------------------------------------------------------------
  Computing normal forms
-------------------------------------------------------------------------------}

-- | Normal-form computation.
--
-- Normalization recurses into 'MacroRef.underlying', which only makes sense if
-- 'MacroUnderlying p ~ Type p'. During the
-- 'HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.ReparseMacroExpansions'
-- pass, the 'MacroUnderlying' is temporarily set to @()@.
class Normalize tag tag' where
  normalize :: (
      MacroUnderlying p ~ Type p
    ) => TypeF tag p -> TypeF tag' p

instance Normalize tag tag where
  normalize = id

-- | Erase @typedef@s
--
-- The algorithm to erase @typedef@s is simple.  Replace any references to
-- @typedef@s we find by the definitions of these @typedef@s.  The @typedef@
-- definitions that we inline this way can contain references to other
-- @typedef@s, but they can not construct infinitely long types, so this
-- algorithm will terminate sooner or later.  In practice, @typedef@ \"chains\"
-- are probably not that long, so we do not expect this algorithm to have
-- problematic performance.
instance Normalize Full Erased where
  normalize :: forall p.
       MacroUnderlying p ~ Type p
    => TypeF Full p
    -> TypeF Erased p
  normalize = mapTypeF fTypedefRef fQual fExtBindingRef fMacroRef fEnumRef
    where
      fTypedefRef :: TypedefRef p -> TypeF Erased p
      fTypedefRef ref = normalize ref.underlying

      fQual :: TypeQual -> TypeF Full p -> TypeF Erased p
      fQual qual typ = TypeQual qual $ normalize typ

      fExtBindingRef :: ExtBindingRef p -> TypeF Erased p
      fExtBindingRef ref = normalize ref.underlying

      fMacroRef :: MacroRef p -> TypeF Erased p
      fMacroRef ref = normalize ref.underlying

      fEnumRef :: EnumRef p -> TypeF Erased p
      fEnumRef ref = normalize ref.underlying

instance Normalize Erased Canonical where
  normalize = mapTypeF absurd fQual absurd absurd absurd
    where
      fQual ::
           MacroUnderlying p ~ Type p
        => TypeQual
        -> TypeF Erased p
        -> TypeF Canonical p
      fQual _qual typ = normalize typ

instance Normalize Full Canonical where
  normalize = getCanonicalType . getErasedType

getCanonicalType ::
     (Normalize tag Canonical, MacroUnderlying p ~ Type p)
  => TypeF tag p
  -> CanonicalType p
getCanonicalType = normalize

getErasedType ::
     (Normalize tag Erased, MacroUnderlying p ~ Type p)
  => TypeF tag p
  -> ErasedType p
getErasedType = normalize

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

data ValOrRef = ByValue | ByRef
  deriving stock (Eq, Ord, Show)

-- | The declarations this type depends on (direct dependencies only)
--
-- We also report whether this dependence is through a pointer or not.
depsOfType :: forall p.
     PassMacro p
  => Type p
  -> [(ValOrRef, Id p)]
depsOfType = \case
    -- Primitive types
    TypePrim _    -> []
    TypeComplex _ -> []
    TypeVoid      -> []

    -- Interesting cases
    TypeRef ref         -> [(ByValue, ref)]
    TypeEnum ref        -> [(ByValue, ref.name)]
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
    TypeFun args res      -> concatMap depsOfTypeFunArg args <> depsOfType res

depsOfTypeFunArg ::
     PassMacro p
  => TypeFunArgF Full p
  -> [(ValOrRef, Id p)]
depsOfTypeFunArg arg = depsOfType arg.typ

-- | Checks if a type is unsupported by Haskell's FFI
hasUnsupportedType :: forall tag p.
     (Normalize tag Canonical, PassId p, MacroUnderlying p ~ Type p)
  => TypeF tag p
  -> Bool
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

    -- 'Normalize Full Erased' erases @typedef@s, @enum@s, and type macros.
    auxRef :: C.NameKind -> Bool
    auxRef = \case
      C.NameKindOrdinary               -> panicPure "Unexpected NameKindOrdinary"
      C.NameKindTagged C.TagKindStruct -> True
      C.NameKindTagged C.TagKindUnion  -> True
      C.NameKindTagged C.TagKindEnum   -> panicPure "Unexpected TagKindEnum"
      C.NameKindMacro                  -> panicPure "Unexpected NameKindMacro"

-- | Recursively accumulate all function types
getAllFunTypes ::
     forall p. (PassMacro p, PassId p, PassExtBinding p, PassAnn p)
  => Type p
  -> Set ([TypeFunArg p], Type p)
getAllFunTypes =
      Set.map (\(_, args, res) -> (args, res))
    . getFunTypes

-- | Recursively accumulate all function type indirections
getAllFunTypeIndirections ::
     forall p. (PassMacro p, PassId p, PassExtBinding p, PassAnn p)
  => Type p
  -> Set ([TypeFunArg p], Type p)
getAllFunTypeIndirections =
      Set.map (\(_, args, res) -> (args, res))
    . Set.filter (\(numIndirections, _, _) -> numIndirections > 0)
    . getFunTypes

-- | Internal function: accumulate function types recursively together with the
-- number of indirections to get to those function types.
getFunTypes ::
     forall p. (PassMacro p, PassId p, PassExtBinding p, PassAnn p)
  => Type p
  -> Set (Int, [TypeFunArg p], Type p)
getFunTypes = go 0 Set.empty
  where
    go ::
         Int
      -> Set (Int, [TypeFunArg p], Type p)
      -> Type p
      -> Set (Int, [TypeFunArg p], Type p)
    go numIndirections acc = \case
      -- interesting case
      TypeFun args res ->
          let acc' = Set.insert (numIndirections, args, res) acc
          in  Foldable.foldl' (go (numIndirections + 1)) acc' (fmap (.typ) args ++ [res])

      -- recursive cases
      TypePointers n t       -> go (numIndirections + n) acc t
      TypeConstArray _n t    -> go (numIndirections + 1) acc t
      TypeIncompleteArray  t -> go (numIndirections + 1) acc t
      TypeBlock t            -> go (numIndirections + 1) acc t
      TypeQual _q t          -> go (numIndirections + 1) acc t

      -- primitive cases
      TypePrim{} -> acc
      TypeComplex{} -> acc
      TypeVoid{} -> acc

      -- sugar cases
      TypeRef{} -> acc
      TypeEnum{} -> acc
      TypeMacro{} -> acc
      TypeTypedef{} -> acc
      TypeExtBinding{} -> acc

-- | Get the first function type indirection
getFirstFunTypeIndirection ::
     forall p.
     Type p
  -> Maybe ( [TypeFunArg p]
           , Type p
           , Type p -> Type p -- ^ zipper: how to reconstruct the original type
           )
getFirstFunTypeIndirection = go 0 id
  where
    go ::
         Int
      -> (Type p -> Type p)
      -> Type p
      -> Maybe ( [TypeFunArg p]
               , Type p
               , Type p -> Type p
               )
    go numIndirections reconstruct = \case
      -- interesting case
      TypeFun args res
        | numIndirections <= 0 -> Nothing
        | otherwise ->  Just (args, res, reconstruct)

      -- recursive cases
      TypePointers n t        -> go (numIndirections + n) (reconstruct . TypePointers n) t
      TypeConstArray n t      -> go (numIndirections + 1) (reconstruct . TypeConstArray n) t
      TypeIncompleteArray  t  -> go (numIndirections + 1) (reconstruct . TypeIncompleteArray) t
      TypeBlock t             -> go (numIndirections + 1) (reconstruct . TypeBlock) t
      TypeQual q t            -> go (numIndirections + 1) (reconstruct . TypeQual q) t

      -- primitive cases
      TypePrim{} -> Nothing
      TypeComplex{} -> Nothing
      TypeVoid{} -> Nothing

      -- sugar cases
      TypeRef{} -> Nothing
      TypeEnum{} -> Nothing
      TypeMacro{} -> Nothing
      TypeTypedef{} -> Nothing
      TypeExtBinding{} -> Nothing

{-------------------------------------------------------------------------------
  Classification: simple classifiers
-------------------------------------------------------------------------------}

isVoid :: Type p -> Bool
isVoid TypeVoid = True
isVoid _        = False

-- | Is the canonical type a complex type?
isCanonicalTypeComplex ::
     (Normalize tag Canonical, MacroUnderlying p ~ Type p)
  => TypeF tag p
  -> Bool
isCanonicalTypeComplex ty =
    case getCanonicalType ty of
      TypeComplex{} -> True
      _otherwise    -> False

-- | Is the canonical type a function type?
isCanonicalTypeFunction ::
     (Normalize tag Canonical, MacroUnderlying p ~ Type p)
  => TypeF tag p
  -> Bool
isCanonicalTypeFunction ty =
    case getCanonicalType ty of
      TypeFun{} -> True
      _         -> False

-- | Is the canonical type a struct type?
isCanonicalTypeStruct :: forall tag p.
     (Normalize tag Canonical, PassId p, MacroUnderlying p ~ Type p)
  => TypeF tag p -> Bool
isCanonicalTypeStruct ty =
    case getCanonicalType ty of
      TypeRef ref ->
        idNameKind (Proxy @p) ref == C.NameKindTagged C.TagKindStruct
      _otherwise  -> False

-- | Is the canonical type a union type?
isCanonicalTypeUnion :: forall tag p.
     (Normalize tag Canonical, PassId p, MacroUnderlying p ~ Type p)
  => TypeF tag p -> Bool
isCanonicalTypeUnion ty =
    case getCanonicalType ty of
      TypeRef ref ->
        idNameKind (Proxy @p) ref == C.NameKindTagged C.TagKindUnion
      _otherwise  -> False

-- | Is the canonical type an array type?
isCanonicalTypeArray ::
     (Normalize tag Canonical, MacroUnderlying p ~ Type p)
  => TypeF tag p
  -> Bool
isCanonicalTypeArray ty =
    case getCanonicalType ty of
      TypeConstArray{}   -> True
      TypeIncompleteArray{} -> True
      _                     -> False

-- | Is the erased type @const@-qualified?
isErasedTypeConstQualified ::
     (Normalize tag Erased, MacroUnderlying p ~ Type p)
  => TypeF tag p
  -> Bool
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
  CoercePass instances
-------------------------------------------------------------------------------}

-- NOTE: types do not contain 'ScopedName's, so unlike most other 'CoercePass'
-- instances this one carries no @ScopedName p ~ ScopedName p'@ constraint. This
-- lets us reindex types across passes that change 'ScopedName' (e.g.
-- 'HsBindgen.Frontend.Pass.MangleNames.IsPass.CreateNames').
instance (
      CoercePassId p p'
    , CoercePassMacroId p p'
    , CoercePassMacroUnderlying p p'
    , CoercePassAnn "TypeFunArg" p p'
    , ExtBinding p ~ ExtBinding p'
    ) => CoercePass Type p p' where
  coercePass = \case
      TypePrim prim           -> TypePrim prim
      TypeRef uid             -> TypeRef (goId uid)
      TypeEnum ref            -> TypeEnum (Ref (goId ref.name) (coercePass ref.underlying))
      TypeMacro ref           -> TypeMacro (MacroRef (goMacroId ref.name) (goMacroUnderlying ref.underlying))
      TypeTypedef ref         -> TypeTypedef (Ref (goId ref.name) (coercePass ref.underlying))
      TypePointers n typ      -> TypePointers n (coercePass typ)
      TypeFun args res        -> TypeFun (map coercePass args) (coercePass res)
      TypeVoid                -> TypeVoid
      TypeConstArray n typ    -> TypeConstArray n (coercePass typ)
      TypeIncompleteArray typ -> TypeIncompleteArray (coercePass typ)
      TypeExtBinding ref      -> TypeExtBinding (Ref ref.name (coercePass ref.underlying))
      TypeBlock typ           -> TypeBlock (coercePass typ)
      TypeQual qual typ       -> TypeQual qual (coercePass typ)
      TypeComplex prim        -> TypeComplex prim
    where
      goId :: Id p -> Id p'
      goId = coercePassId (Proxy @'(p, p'))

      goMacroId :: MacroId p -> MacroId p'
      goMacroId = coercePassMacroId (Proxy @'(p, p'))

      goMacroUnderlying :: MacroUnderlying p -> MacroUnderlying p'
      goMacroUnderlying = coercePassMacroUnderlying (Proxy @'(p, p'))

-- NOTE: see the 'CoercePass Type' instance for why there is no
-- @ScopedName p ~ ScopedName p'@ constraint here.
instance (
      CoercePassId p p'
    , CoercePassMacroId p p'
    , CoercePassMacroUnderlying p p'
    , CoercePassAnn "TypeFunArg" p p'
    , ExtBinding p ~ ExtBinding p'
    ) => CoercePass TypeFunArg p p' where
  coercePass arg = TypeFunArgF {
        typ = coercePass arg.typ
      , ann = coercePassAnn (Proxy @'("TypeFunArg", p, p')) arg.ann
      }
