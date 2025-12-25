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
import HsBindgen.Frontend.AST.Type
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
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
  | DeclGlobal (Type Final)
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
    , structFieldType    :: Type Final
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
    , unionFieldType :: Type Final
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

data Enum = Enum {
      enumNames     :: MangleNames.NewtypeNames
    , enumType      :: Type Final
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
    , typedefType  :: Type Final
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Functions (signatures)
-------------------------------------------------------------------------------}

data Function = Function {
      functionArgs    :: [(Maybe C.ScopedNamePair, Type Final)]
    , functionAttrs   :: Int.FunctionAttributes
    , functionRes     :: Type Final
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
    , macroType        :: Type Final
    }
  deriving stock (Show, Eq, Generic)
