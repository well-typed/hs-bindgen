{-# LANGUAGE EmptyCase #-}

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
  , TypeF(..)
  , TypeQualifier(..)
  , ResolveBindingSpecs.ResolvedExtBinding(..)
  , isVoid
  , isErasedTypeConstQualified
  , isCanonicalTypeFunction
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
  , C.Name(..)
  , C.TagKind(..)
  , C.NameKind(..)
  , C.QualName(..)
  , C.qualNameText
  , C.parseQualName
  , C.AnonId(..)
  , C.NameOrigin(..)
  , C.QualPrelimDeclId(..)
  , C.QualDeclId(..)
  , C.qualDeclIdText
  , Int.NamePair(..)
  , Int.nameHs
  , Int.RecordNames(..)
  , Int.NewtypeNames(..)
  ) where

import Prelude hiding (Enum)

import Clang.HighLevel.Documentation qualified as CDoc
import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal qualified as Int
import HsBindgen.Frontend.Naming qualified as C
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
    , declId         :: Int.NamePair
    , declOrigin     :: C.NameOrigin
    , declAliases    :: [C.Name]
    , declHeaderInfo :: Maybe Int.HeaderInfo
    , declComment    :: Maybe (CDoc.Comment CommentRef)
    }
  deriving stock (Show, Eq, Generic)

data FieldInfo = FieldInfo {
      fieldLoc     :: SingleLoc
    , fieldName    :: Int.NamePair
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
  | DeclOpaque C.NameKind
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
newtype DeclSpec = DeclSpec BindingSpec.CTypeSpec
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

-- | Definition of a struct
data Struct = Struct {
      structNames     :: Int.RecordNames
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
      unionNames     :: Int.NewtypeNames
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
      enumNames     :: Int.NewtypeNames
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
      typedefNames   :: Int.NewtypeNames
    , typedefType    :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Functions (signatures)
-------------------------------------------------------------------------------}

data Function = Function {
      functionArgs    :: [(Maybe Int.NamePair, Type)]
    , functionAttrs   :: Int.FunctionAttributes
    , functionRes     :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Comments
-------------------------------------------------------------------------------}

-- | Needed for cross referencing identifiers when translating to Haddocks.
-- When parsing a referencing command, e.g. \\ref, we need an identifier that
-- passes through all the name mangling passes so that in the end we have
-- access to the right name to reference.
--
newtype CommentRef = ById Int.NamePair
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

data CheckedMacro =
    MacroType CheckedMacroType
  | MacroExpr Int.CheckedMacroExpr
  deriving stock (Show, Eq, Generic)

data CheckedMacroType = CheckedMacroType {
      macroTypeNames   :: Int.NewtypeNames
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
  | TypeStruct Int.NamePair C.NameOrigin
  | TypeUnion Int.NamePair C.NameOrigin
  | TypeEnum Int.NamePair C.NameOrigin
  | TypeTypedef
    -- | NOTE: has a strictness annotation, which allows GHC to infer that
    -- pattern matches are redundant when @TypedefRefF tag ~ Void@.
    !(TypedefRefF tag)
    -- TODO: macros should get annotations with underlying types just like
    -- typedefs, so that we can erase the macro types and replace them with
    -- their underlying type. See issue #1200.
  | TypeMacroTypedef Int.NamePair C.NameOrigin
  | TypePointer (TypeF tag)
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

deriving stock instance (Show (TypedefRefF tag), Show (TypeQualifierF tag))  => Show (TypeF tag)
deriving stock instance (Eq (TypedefRefF tag), Eq (TypeQualifierF tag))  => Eq (TypeF tag)

data TypeQualifier = TypeQualifierConst
  deriving stock (Show, Eq, Generic)

data TypedefRef =
    TypedefRegular
      -- | Name of the referenced typedef declaration
      Int.NamePair
      -- | The underlying type of the referenced typedef declaration
      --
      -- NOTE: the underlying type can arbitrarily reference other types,
      -- including typedefs that we have not parsed. Use the underlying type with
      -- care!
      Type
  | TypedefSquashed C.Name Type
  deriving stock (Show, Eq, Generic)

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
    _ -> False

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
eraseTypedef = \case
      TypedefRegular _ t' -> t'
      TypedefSquashed _ t' -> t'

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
      TypePrim pt -> TypePrim pt
      TypeStruct np no -> TypeStruct np no
      TypeUnion np no -> TypeUnion np no
      TypeEnum np no -> TypeEnum np no
      TypeTypedef ref -> fRef ref
      TypeMacroTypedef np no -> TypeMacroTypedef np no
      TypePointer t -> TypePointer $ go t
      TypeConstArray n t -> TypeConstArray n $ go t
      TypeFun args res -> TypeFun (go <$> args) (go res)
      TypeVoid -> TypeVoid
      TypeIncompleteArray t -> TypeIncompleteArray (go t)
      TypeBlock t -> TypeBlock (go t)
      TypeQualified q t -> fQual q t
      TypeExtBinding reb -> TypeExtBinding reb
      TypeComplex pt -> TypeComplex pt
