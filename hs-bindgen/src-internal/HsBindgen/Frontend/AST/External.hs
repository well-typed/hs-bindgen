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
  , Type(..)
  , ResolveBindingSpecs.ResolvedExtBinding(..)
  , isVoid
    -- * Names
  , C.Name(..)
  , C.TypeNamespace(..)
  , C.TagKind(..)
  , C.NameKind(..)
  , C.nameKindTypeNamespace
  , C.QualName(..)
  , C.qualNameText
  , C.parseQualName
  , C.AnonId(..)
  , C.NameOrigin(..)
  , C.QualPrelimDeclId(..)
  , C.QualDeclId(..)
  , C.qualDeclIdText
  , NamePair(..)
  , nameHs
  , RecordNames(..)
  , NewtypeNames(..)
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
import HsBindgen.Language.Haskell qualified as Hs

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
    , declId         :: NamePair
    , declOrigin     :: C.NameOrigin
    , declAliases    :: [C.Name]
    , declHeaderInfo :: Maybe Int.HeaderInfo
    , declComment    :: Maybe (CDoc.Comment CommentRef)
    }
  deriving stock (Show, Eq, Generic)

data FieldInfo = FieldInfo {
      fieldLoc     :: SingleLoc
    , fieldName    :: NamePair
    , fieldComment :: Maybe (CDoc.Comment CommentRef)
    }
  deriving stock (Show, Eq, Generic)

data DeclKind =
    DeclStruct Struct
  | DeclStructOpaque
  | DeclUnion Union
  | DeclUnionOpaque
  | DeclTypedef Typedef
  | DeclEnum Enum
  | DeclEnumOpaque
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
-- have consequences for "Hs.Origin" also.
newtype DeclSpec = DeclSpec BindingSpec.TypeSpec
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

-- | Definition of a struct
data Struct = Struct {
      structNames     :: RecordNames
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
      unionNames     :: NewtypeNames
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
      enumNames     :: NewtypeNames
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
      typedefNames   :: NewtypeNames
    , typedefType    :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Functions (signatures)
-------------------------------------------------------------------------------}

data Function = Function {
      functionArgs    :: [(Maybe NamePair, Type)]
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
newtype CommentRef = ById NamePair
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

data CheckedMacro =
    MacroType CheckedMacroType
  | MacroExpr Int.CheckedMacroExpr
  deriving stock (Show, Eq, Generic)

data CheckedMacroType = CheckedMacroType {
      macroTypeNames   :: NewtypeNames
    , macroType        :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Type use
--
-- For type /declarations/ see 'Decl'.
data Type =
    TypePrim C.PrimType
  | TypeStruct NamePair C.NameOrigin
  | TypeUnion NamePair C.NameOrigin
  | TypeEnum NamePair C.NameOrigin
  | TypeTypedef TypedefRef
  | TypeMacroTypedef NamePair C.NameOrigin
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
  | TypeBlock Type
  | TypeConst Type
  | TypeExtBinding ResolveBindingSpecs.ResolvedExtBinding
  | TypeComplex C.PrimType
  deriving stock (Show, Eq, Generic)

data TypedefRef =
    TypedefRegular NamePair
  | TypedefSquashed C.Name Type
  deriving stock (Show, Eq, Generic)

isVoid :: Type -> Bool
isVoid TypeVoid = True
isVoid _        = False

{-------------------------------------------------------------------------------
  Identifiers
-------------------------------------------------------------------------------}

-- | Pair of a C name and the corresponding Haskell name
--
-- Invariant: the 'Hs.Identifier' must satisfy the rules for legal Haskell
-- names, for its intended use (constructor, variable, ..).
data NamePair = NamePair {
      nameC       :: C.Name
    , nameHsIdent :: Hs.Identifier
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Extract namespaced Haskell name
--
-- The invariant on 'NamePair' justifies this otherwise unsafe operation.
nameHs :: NamePair -> Hs.Name ns
nameHs NamePair{nameHsIdent = Hs.Identifier name} = Hs.Name name

{-------------------------------------------------------------------------------
  Additional names

  This is in addition to the 'NamePair's already embedded in the AST.
-------------------------------------------------------------------------------}

-- | Names for a Haskell record type
data RecordNames = RecordNames {
      recordConstr :: Hs.Name Hs.NsConstr
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Names for a Haskell newtype
data NewtypeNames = NewtypeNames {
      newtypeConstr :: Hs.Name Hs.NsConstr
    , newtypeField  :: Hs.Name Hs.NsVar
    }
  deriving stock (Show, Eq, Ord, Generic)
