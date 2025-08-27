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
  , DeclInfo(..)
  , DeclKind(..)
  , MangleNames.DeclSpec(..)
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
  , Reference(..)
    -- ** Macros
  , CheckedMacro(..)
  , CheckedMacroType(..)
  , Int.CheckedMacroExpr(..)
  , Macro.MExpr(..)
  , Macro.MFun(..)
  , Macro.MTerm(..)
  , Macro.Pass(Ps)
  , Macro.XVar(..)
    -- ** Functions
  , Function(..)
  , Int.FunctionAttributes(..)
  , Int.FunctionPurity(..)
    -- * Types
  , Type(..)
  , ResolveBindingSpec.ResolvedExtBinding(..)
  , isVoid
  , TypeQualifier(..)
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
  , MangleNames.NamePair(..)
  , MangleNames.nameHs
  , MangleNames.RecordNames(..)
  , MangleNames.NewtypeNames(..)
  ) where

import Prelude hiding (Enum)

import Clang.HighLevel.Documentation
import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.Frontend.AST.Internal qualified as Int
import HsBindgen.Frontend.Macro.AST.Syntax qualified as Macro
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass qualified as ResolveBindingSpec
import HsBindgen.Frontend.RootHeader
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
    , declSpec :: MangleNames.DeclSpec
    }
  deriving stock (Show, Eq, Generic)

data DeclInfo = DeclInfo{
      declLoc     :: SingleLoc
    , declId      :: MangleNames.NamePair
    , declOrigin  :: C.NameOrigin
    , declAliases :: [C.Name]
    , declHeader  :: HashIncludeArg
    , declComment :: Maybe (Comment Reference)
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
  | DeclConst Type
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
      structFieldLoc     :: SingleLoc
    , structFieldName    :: MangleNames.NamePair
    , structFieldType    :: Type
    , structFieldOffset  :: Int -- ^ Offset in bits
    , structFieldWidth   :: Maybe Int
    , structFieldComment :: Maybe (Comment Reference)
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
      unionFieldLoc     :: SingleLoc
    , unionFieldName    :: MangleNames.NamePair
    , unionFieldType    :: Type
    , unionFieldComment :: Maybe (Comment Reference)
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
      enumConstantLoc     :: SingleLoc
    , enumConstantName    :: MangleNames.NamePair
    , enumConstantValue   :: Integer
    , enumConstantComment :: Maybe (Comment Reference)
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

data Typedef = Typedef {
      typedefNames   :: MangleNames.NewtypeNames
    , typedefType    :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Functions (signatures)
-------------------------------------------------------------------------------}

data Function = Function {
      functionArgs    :: [(Maybe MangleNames.NamePair, Type)]
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
newtype Reference = ById MangleNames.NamePair
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

-- | Type use
--
-- For type /declarations/ see 'Decl'.
data Type =
    TypePrim C.PrimType
  | TypeStruct MangleNames.NamePair C.NameOrigin
  | TypeUnion MangleNames.NamePair C.NameOrigin
  | TypeEnum MangleNames.NamePair C.NameOrigin
  | TypeTypedef TypedefRef
  | TypeMacroTypedef MangleNames.NamePair C.NameOrigin
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
  | TypeExtBinding ResolveBindingSpec.ResolvedExtBinding
  deriving stock (Show, Eq, Generic)
  deriving Repr via ReprShow Type

data TypedefRef =
    TypedefRegular MangleNames.NamePair
  | TypedefSquashed C.Name Type
  deriving stock (Show, Eq, Generic)
  deriving Repr via ReprShow TypedefRef

isVoid :: Type -> Bool
isVoid TypeVoid = True
isVoid _        = False

-- | A type qualifier
--
-- <https://en.cppreference.com/w/c/language/const.html>
data TypeQualifier =
    -- | No type qualifier
    TypeQualifierNone
    -- | @const@ type qualifier
    --
    -- <https://en.cppreference.com/w/c/language/const.html>
  | TypeQualifierConst
  deriving stock (Show, Eq, Generic)
