-- | The final C AST after the frontend is done
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.AST.External qualified as C
module HsBindgen.Frontend.AST.External (
    -- * Top-level
    TranslationUnit(..)
  , emptyTranslationUnit
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
    -- * Names
  , AnonId(..)
  , NameOrigin(..)
  , MangleNames.NamePair(..)
  , MangleNames.nameHs
  , MangleNames.RecordNames(..)
  , MangleNames.NewtypeNames(..)
  ) where

import Prelude hiding (Enum)

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.Frontend.AST.Internal qualified as Int
import HsBindgen.Frontend.Macros.AST.Syntax qualified as Macro
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass qualified as ResolveBindingSpec
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

emptyTranslationUnit :: TranslationUnit
emptyTranslationUnit = TranslationUnit{
      unitDecls = []
    , unitDeps  = []
    }

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
    , declOrigin  :: NameOrigin
    , declAliases :: [C.Name]
    , declHeader  :: CHeaderIncludePath
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
  | DeclExtern Type
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
      structFieldLoc    :: SingleLoc
    , structFieldName   :: MangleNames.NamePair
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
      unionNames     :: MangleNames.NewtypeNames
    , unionSizeof    :: Int
    , unionAlignment :: Int
    , unionFields    :: [UnionField]
    }
  deriving stock (Show, Eq, Generic)

data UnionField = UnionField {
      unionFieldLoc  :: SingleLoc
    , unionFieldName :: MangleNames.NamePair
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
      enumConstantLoc   :: SingleLoc
    , enumConstantName  :: MangleNames.NamePair
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
      functionArgs :: [Type]
    , functionAttrs :: Int.FunctionAttributes
    , functionRes  :: Type
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
      macroTypeNames :: MangleNames.NewtypeNames
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
    TypePrim C.PrimType
  | TypeStruct MangleNames.NamePair NameOrigin
  | TypeUnion MangleNames.NamePair NameOrigin
  | TypeEnum MangleNames.NamePair NameOrigin
  | TypeTypedef TypedefRef
  | TypeMacroTypedef MangleNames.NamePair NameOrigin
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
