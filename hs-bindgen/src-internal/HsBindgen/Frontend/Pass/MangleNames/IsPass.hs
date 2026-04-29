module HsBindgen.Frontend.Pass.MangleNames.IsPass (
    MangleNames
    -- * Additional names
  , StructNames(..)
  , NewtypeNames(..)
  , UnionFieldNames(..)
  , TypedefNames(..)
    -- * Trace messages
  , MangleNamesMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Mangle names pass
type MangleNames :: Pass
data MangleNames a

type family AnnMangleNames ix where
  AnnMangleNames "TranslationUnit"  = DeclMeta
  AnnMangleNames "Decl"             = PrescriptiveDeclSpec
  AnnMangleNames "Struct"           = StructNames
  AnnMangleNames "Union"            = NewtypeNames
  AnnMangleNames "UnionField"       = UnionFieldNames
  AnnMangleNames "Enum"             = NewtypeNames
  AnnMangleNames "Typedef"          = TypedefNames
  AnnMangleNames "CheckedMacroType" = NewtypeNames
  AnnMangleNames _                  = NoAnn

instance IsPass MangleNames where
  type Id          MangleNames = DeclIdPair
  type ScopedName  MangleNames = ScopedNamePair
  type MacroBody   MangleNames = CheckedMacro MangleNames
  type ExtBinding  MangleNames = ResolvedExtBinding
  type Ann ix      MangleNames = AnnMangleNames ix
  type Msg         MangleNames = WithLocationInfo MangleNamesMsg
  type MacroId     MangleNames = Id MangleNames
  type CommentDecl MangleNames = Maybe (C.Comment MangleNames)

  idNameKind     _ namePair   = namePair.cName.name.kind
  idSourceName   _ namePair   = declIdSourceName namePair.cName
  idLocationInfo _ namePair   = declIdLocationInfo namePair.cName
  extBindingId   _ extBinding = extDeclIdPair extBinding
  macroIdId _ = id

{-------------------------------------------------------------------------------
  Additional names required for Haskell code generation
-------------------------------------------------------------------------------}

data StructNames = StructNames {
      constr  :: Hs.Name Hs.NsConstr
      -- TODO https://github.com/well-typed/hs-bindgen/issues/1925
      --
      -- Tie generation of names to the generation of the associated code.
      --
      -- | Name of the auxiliary @typedef@ we generate for @struct@s with
      --   flexible array members (FLAMs).
    , flamAux :: Maybe (Hs.Name Hs.NsTypeConstr)
    }
  deriving stock (Show, Eq, Ord, Generic)

data NewtypeNames = NewtypeNames {
      dataConstr :: Hs.Name Hs.NsConstr
    , field      :: Hs.Name Hs.NsVar
    }
  deriving stock (Show, Eq, Ord, Generic)

data UnionFieldNames = UnionFieldNames {
      getter  :: Hs.Name Hs.NsVar
    , setter  :: Hs.Name Hs.NsVar
    }
  deriving stock (Show, Eq, Ord, Generic)

data TypedefNames = TypedefNames {
      orig :: NewtypeNames
      -- TODO https://github.com/well-typed/hs-bindgen/issues/1925
      --
      -- Tie generation of names to the generation of the associated code.
      --
      -- | Names of the auxiliary type definition that we generate for function
      --   pointers.
    , aux  :: Maybe (Hs.Name Hs.NsTypeConstr, NewtypeNames)
    }
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data MangleNamesMsg =
    MangleNamesAssignedName Hs.SomeName
  deriving stock (Show)

instance PrettyForTrace MangleNamesMsg where
  prettyForTrace = \case
      MangleNamesAssignedName newName -> PP.hsep [
          "Assigned name"
        , PP.text newName.text
        ]

instance IsTrace Level MangleNamesMsg where
  getDefaultLogLevel = \case
      MangleNamesAssignedName{} -> Info

  getSource  = const HsBindgen
  getTraceId = \case
    MangleNamesAssignedName{} -> "mangle-names-assigned-name"
