-- | Internal AST as it is constructed step by step in the frontend
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.AST.Internal (ValidPass)
-- > import HsBindgen.Frontend.AST.Internal qualified as C
module HsBindgen.Frontend.AST.Internal (
    TranslationUnit(..)
    -- * Declarations
  , Decl(..)
  , DeclInfo(..)
  , DeclKind(..)
  , Struct(..)
  , StructField(..)
  , Union(..)
  , UnionField(..)
  , Typedef(..)
  , Enum(..)
  , EnumConstant(..)
  , Function(..)
    -- ** Macros
  , CheckedMacro(..)
  , CheckedMacroType(..)
  , CheckedMacroExpr(..)
    -- * Types (at use sites)
  , Type(..)
    -- * Naming
  , AnonId(..)
  , NameOrigin(..)
    -- * Show
  , ValidPass
    -- * Helper functions
  , declNsPrelimDeclId
  , declQualPrelimDeclId
  , declQualName
  , declKindNameKind
  ) where

import Prelude hiding (Enum)

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.C.Tc.Macro.Type qualified as Macro
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Macros.AST.Syntax qualified as Macro
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Declarations

  NOTE: Struct and union fields, as well as enum constants, have their /own/
  'SingleLoc' (in addition to the 'SingleLoc' of the enclosing declaration).
-------------------------------------------------------------------------------}

data TranslationUnit p = TranslationUnit{
      -- | Declarations in the unit
      --
      -- Declarations from all headers that we have processed. Passes may remove
      -- some declarations. For example,
      --
      -- * The 'Parse' pass filters out declarations not matching the selection
      --   predicate (without program slicing).
      --
      -- * If program slicing is enabled, the 'Slice' pass filters selected
      --   declarations and their transitive dependencies.
      --
      -- * The 'ResolveBindingSpec' pass removes declarations for which we have
      --   existing external bindings, as well as declarations omitted by a
      --   prescriptive binding specification.
      unitDecls :: [Decl p]

      -- | Include graph
      --
      -- This is used to declare TH dependencies.
      --
      -- It can also be useful for users to see this graph, as it may provide
      -- insight into the binding generation process. For example, suppose we
      -- have a large library (say Gtk), with a few main entry points (for which
      -- we should generate separate Haskell modules) and a core of "common"
      -- definitions; it may be quite useful to look at the include graph to
      -- figure out what this set of "core" headers is.
    , unitIncludeGraph :: IncludeGraph

      -- | Pass-specific annotation
    , unitAnn :: Ann "TranslationUnit" p
    }

data Decl p = Decl {
      declInfo :: DeclInfo p
    , declKind :: DeclKind p
    , declAnn  :: Ann "Decl" p
    }

data DeclInfo p = DeclInfo{
      declLoc     :: SingleLoc
    , declId      :: Id p
    , declAliases :: [C.Name]

      -- | User-specified header that provides this declaration
      --
      -- Note that the declaration may not be in this header directly, but in
      -- one of its (transitive) includes.
      --
      -- If the user specifies /multiple/ headers, all of which directly or
      -- indirectly contain the same declaration, then either
      --
      -- 1. the C headers will need to explicitly check
      --    (if this is already defined, do nothing), or
      -- 2. This is an error in the C code.
      --
      -- This means that there is always a /single/ header to choose here.
    , declHeader :: CHeaderIncludePath
    }

data DeclKind p =
    DeclStruct (Struct p)
  | DeclStructOpaque
  | DeclUnion (Union p)
  | DeclUnionOpaque
  | DeclTypedef (Typedef p)
  | DeclEnum (Enum p)
  | DeclEnumOpaque
  | DeclMacro (MacroBody p)
  | DeclFunction (Function p)
  | DeclExtern (Type p)
  | DeclConst (Type p)

data Struct p = Struct {
      structSizeof    :: Int
    , structAlignment :: Int
    , structFields    :: [StructField p]
    , structAnn       :: Ann "Struct" p
    }

data StructField p = StructField {
      structFieldLoc    :: SingleLoc
    , structFieldName   :: FieldName p
    , structFieldType   :: Type p
    , structFieldOffset :: Int     -- ^ Offset in bits
    , structFieldWidth  :: Maybe Int
    , structFieldAnn    :: Ann "StructField" p
    }

data Union p = Union {
      unionSizeof    :: Int
    , unionAlignment :: Int
    , unionFields    :: [UnionField p]
    , unionAnn       :: Ann "Union" p
    }

data UnionField p = UnionField {
      unionFieldLoc   :: SingleLoc
    , unionFieldName  :: FieldName p
    , unionFieldType  :: Type p
    , unionFieldAnn   :: Ann "UnionField" p
    }

data Typedef p = Typedef {
      typedefType :: Type p
    , typedefAnn  :: Ann "Typedef" p
    }

data Enum p = Enum {
      enumType      :: Type p
    , enumSizeof    :: Int
    , enumAlignment :: Int
    , enumConstants :: [EnumConstant p]
    , enumAnn       :: Ann "Enum" p
    }

data EnumConstant p = EnumConstant {
      enumConstantLoc   :: SingleLoc
    , enumConstantName  :: FieldName p
    , enumConstantValue :: Integer
    }

data Function p = Function {
      functionArgs :: [Type p]
    , functionRes  :: Type p
    , functionAnn  :: Ann "Function" p
    }

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

data CheckedMacro p =
    MacroType (CheckedMacroType p)
  | MacroExpr CheckedMacroExpr

data CheckedMacroType p = CheckedMacroType{
      macroType    :: Type p
    , macroTypeAnn :: Ann "CheckedMacroType" p
    }

-- | Checked expression (function) macro
--
-- TODO: This is wrong, it does not allow name mangling to do its job. To fix
-- that we'd have to change 'Macro.MExpr'.
data CheckedMacroExpr = CheckedMacroExpr{
      macroExprArgs :: [C.Name]
    , macroExprBody :: Macro.MExpr Macro.Ps
    , macroExprType :: Macro.Quant (Macro.Type Macro.Ty)
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Types (at use sites)
-------------------------------------------------------------------------------}

data Type p =
    TypePrim C.PrimType
  | TypeStruct (Id p)
  | TypeUnion (Id p)
  | TypeEnum (Id p)
  | TypeTypedef (TypedefRef p)

    -- | Macro-defined type
    --
    -- These behave very similar to 'TypeTypedef'.
  | TypeMacroTypedef (Id p)

  | TypePointer (Type p)
  | TypeFun [Type p] (Type p)
  | TypeVoid
  | TypeConstArray Natural (Type p)
  | TypeExtBinding (ExtBinding p)

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
  | TypeIncompleteArray (Type p)

    -- | Block type
    --
    -- Blocks are a clang-specific C extension.
    --
    -- See <https://clang.llvm.org/docs/BlockLanguageSpec.html>
  | TypeBlock (Type p)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

class    ( Show (Ann ix p)
         , Eq   (Ann ix p)
         ) => ValidAnn (ix :: Symbol) (p :: Pass)
instance ( Show (Ann ix p)
         , Eq   (Ann ix p)
         ) => ValidAnn (ix :: Symbol) (p :: Pass)

-- | Valid pass
--
-- A pass is valid if the various type family instances satisfy constraints that
-- we need, primarily for debugging and testing.
--
-- This is intentionally /not/ defined as a class alias, so that we get an error
-- the moment we declare a pass to be 'ValidPass', rather than at use sites.
class ( IsPass p

        -- Identifiers
        --
        -- We often store identifiers in maps etc., so we insist on 'Ord'

      , Show (Id p)
      , Show (FieldName p)

      , Ord (Id p)
      , Ord (FieldName p)

        -- Other constructs

      , Show (ExtBinding p)
      , Show (MacroBody  p)
      , Show (TypedefRef p)

      , Eq (ExtBinding p)
      , Eq (MacroBody  p)
      , Eq (TypedefRef p)

        -- Annotations

      , ValidAnn "CheckedMacroType" p
      , ValidAnn "Decl"             p
      , ValidAnn "Enum"             p
      , ValidAnn "Function"         p
      , ValidAnn "Struct"           p
      , ValidAnn "StructField"      p
      , ValidAnn "TranslationUnit"  p
      , ValidAnn "Typedef"          p
      , ValidAnn "Union"            p
      , ValidAnn "UnionField"       p
      ) => ValidPass p where

deriving stock instance ValidPass p => Show (CheckedMacro     p)
deriving stock instance ValidPass p => Show (CheckedMacroType p)
deriving stock instance ValidPass p => Show (Decl             p)
deriving stock instance ValidPass p => Show (DeclInfo         p)
deriving stock instance ValidPass p => Show (DeclKind         p)
deriving stock instance ValidPass p => Show (Enum             p)
deriving stock instance ValidPass p => Show (EnumConstant     p)
deriving stock instance ValidPass p => Show (Function         p)
deriving stock instance ValidPass p => Show (Struct           p)
deriving stock instance ValidPass p => Show (StructField      p)
deriving stock instance ValidPass p => Show (TranslationUnit  p)
deriving stock instance ValidPass p => Show (Type             p)
deriving stock instance ValidPass p => Show (Typedef          p)
deriving stock instance ValidPass p => Show (Union            p)
deriving stock instance ValidPass p => Show (UnionField       p)

deriving stock instance ValidPass p => Eq (CheckedMacro     p)
deriving stock instance ValidPass p => Eq (CheckedMacroType p)
deriving stock instance ValidPass p => Eq (Decl             p)
deriving stock instance ValidPass p => Eq (DeclInfo         p)
deriving stock instance ValidPass p => Eq (DeclKind         p)
deriving stock instance ValidPass p => Eq (Enum             p)
deriving stock instance ValidPass p => Eq (EnumConstant     p)
deriving stock instance ValidPass p => Eq (Function         p)
deriving stock instance ValidPass p => Eq (Struct           p)
deriving stock instance ValidPass p => Eq (StructField      p)
deriving stock instance ValidPass p => Eq (TranslationUnit  p)
deriving stock instance ValidPass p => Eq (Type             p)
deriving stock instance ValidPass p => Eq (Typedef          p)
deriving stock instance ValidPass p => Eq (Union            p)
deriving stock instance ValidPass p => Eq (UnionField       p)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance PrettyForTrace (Located (Id p)) => PrettyForTrace (DeclInfo p) where
  prettyForTrace DeclInfo{declId, declLoc} =
    prettyForTrace $ Located declLoc declId

{-------------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------------}

declNsPrelimDeclId :: Id p ~ PrelimDeclId => Decl p -> NsPrelimDeclId
declNsPrelimDeclId Decl{declInfo = DeclInfo{declId}, declKind} =
    nsPrelimDeclId declId $ C.nameKindTypeNamespace (declKindNameKind declKind)

declQualPrelimDeclId :: Id p ~ PrelimDeclId => Decl p -> QualPrelimDeclId
declQualPrelimDeclId Decl{declInfo = DeclInfo{declId}, declKind} =
    qualPrelimDeclId declId (declKindNameKind declKind)

declQualName :: Id p ~ DeclId => Decl p -> C.QualName
declQualName Decl{declInfo = DeclInfo{declId}, declKind} =
    C.QualName (declIdName declId) (declKindNameKind declKind)

declKindNameKind :: DeclKind p -> C.NameKind
declKindNameKind = \case
    DeclStruct{}       -> C.NameKindStruct
    DeclStructOpaque{} -> C.NameKindStruct
    DeclUnion{}        -> C.NameKindUnion
    DeclUnionOpaque{}  -> C.NameKindUnion
    DeclEnum{}         -> C.NameKindEnum
    DeclEnumOpaque{}   -> C.NameKindEnum
    _otherwise         -> C.NameKindOrdinary
