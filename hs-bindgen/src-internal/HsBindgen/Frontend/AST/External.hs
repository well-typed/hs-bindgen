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
  , NameMangler.DeclSpec(..)
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
    -- * Types
  , Type(..)
  , isVoid
    -- * Names
  , CName(..)
  , HsIdentifier(..)
  , NameMangler.NamePair(..)
  , NameMangler.nameHs
  , NameMangler.RecordNames(..)
  , NameMangler.NewtypeNames(..)
  ) where

import Prelude hiding (Enum)

import Clang.CNameSpelling (CNameSpelling)
import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal qualified as Int
import HsBindgen.Frontend.Macros.AST.Syntax qualified as Macro
import HsBindgen.Frontend.Pass.NameMangler.IsPass qualified as NameMangler
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Language.Haskell

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
    , declSpec :: NameMangler.DeclSpec
    }
  deriving stock (Show, Eq, Generic)

data DeclInfo = DeclInfo{
      declLoc :: SingleLoc
    , declId  :: NameMangler.NamePair
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
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

-- | Definition of a struct
data Struct = Struct {
      structNames     :: NameMangler.RecordNames
    , structSizeof    :: Int
    , structAlignment :: Int
    , structFields    :: [StructField]

      -- | Type of the elements of the FLAM, if any
    , structFlam :: Maybe StructField
  }
  deriving stock (Show, Eq, Generic)

data StructField = StructField {
      structFieldLoc    :: SingleLoc
    , structFieldName   :: NameMangler.NamePair
    , structFieldType   :: Type
    , structFieldOffset :: Int -- ^ Offset in bits
    , structFieldWidth  :: Maybe Int
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Unions
-------------------------------------------------------------------------------}

-- | Definition of an union
data Union = Union {
      unionNames     :: NameMangler.NewtypeNames
    , unionSizeof    :: Int
    , unionAlignment :: Int
    , unionFields    :: [UnionField]
    }
  deriving stock (Show, Eq, Generic)

data UnionField = UnionField {
      unionFieldLoc  :: SingleLoc
    , unionFieldName :: NameMangler.NamePair
    , unionFieldType :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

data Enum = Enum {
      enumNames     :: NameMangler.NewtypeNames
    , enumType      :: Type
    , enumSizeof    :: Int
    , enumAlignment :: Int
    , enumConstants :: [EnumConstant]
    }
  deriving stock (Show, Eq, Generic)

data EnumConstant = EnumConstant {
      enumConstantLoc   :: SingleLoc
    , enumConstantName  :: NameMangler.NamePair
    , enumConstantValue :: Integer
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

data Typedef = Typedef {
      typedefNames :: NameMangler.NewtypeNames
    , typedefType  :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Functions (signatures)
-------------------------------------------------------------------------------}

data Function = Function {
      functionArgs   :: [Type]
    , functionRes    :: Type
    , functionHeader :: CHeaderIncludePath
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

data CheckedMacro =
    MacroType CheckedMacroType
  | MacroExpr Int.CheckedMacroExpr
  deriving stock (Show, Eq, Generic)

data CheckedMacroType = CheckedMacroType{
      macroTypeNames :: NameMangler.NewtypeNames
    , macroType      :: Type
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Type use
--
-- For type /declarations/ see 'Decl'.
data Type =
    TypePrim PrimType
  | TypeStruct NameMangler.NamePair
  | TypeUnion NameMangler.NamePair
  | TypeEnum NameMangler.NamePair
  | TypeTypedef TypedefRef
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
  | TypeExtBinding CNameSpelling ExtHsRef BindingSpec.TypeSpec
  deriving stock (Show, Eq, Generic)
  deriving Repr via ReprShow Type

data TypedefRef =
    TypedefRegular NameMangler.NamePair
  | TypedefSquashed CName Type
  deriving stock (Show, Eq, Generic)
  deriving Repr via ReprShow TypedefRef

isVoid :: Type -> Bool
isVoid TypeVoid = True
isVoid _        = False
