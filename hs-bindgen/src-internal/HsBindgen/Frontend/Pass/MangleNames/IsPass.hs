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

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Mangle names pass
type MangleNames :: Pass
data MangleNames a

type family AnnMangleNames ix where
  AnnMangleNames "Decl"                 = PrescriptiveDeclSpec
  AnnMangleNames "Struct"               = StructNames
  AnnMangleNames "Union"                = NewtypeNames
  AnnMangleNames "UnionField"           = UnionFieldNames
  AnnMangleNames "Enum"                 = NewtypeNames
  AnnMangleNames "Typedef"              = TypedefNames
  AnnMangleNames "TypecheckedMacroType" = NewtypeNames
  AnnMangleNames _                      = NoAnn

instance IsPass MangleNames

instance PassId MangleNames where
  type Id MangleNames = C.DeclIdPair

  idNameKind     _ namePair = namePair.cName.name.kind
  idSourceName   _ namePair = C.declIdSourceName namePair.cName
  idLocationInfo _ namePair = C.declIdLocationInfo namePair.cName

instance PassScopedName MangleNames where
  type ScopedName MangleNames = C.ScopedNamePair

instance PassMacro MangleNames where
  type MacroId         MangleNames = Id MangleNames
  type MacroBody       MangleNames = TypecheckedMacro MangleNames
  type MacroUnderlying MangleNames = C.Type MangleNames

  macroIdId _ = id

instance PassExtBinding MangleNames where
  type ExtBinding MangleNames = BindingSpec.ResolvedExtBinding

  extBindingId _ extBinding = BindingSpec.extDeclIdPair extBinding

instance PassCommentDecl MangleNames where
  type CommentDecl MangleNames = Maybe (C.Comment MangleNames)

instance PassAnn MangleNames where
  type Ann ix MangleNames = AnnMangleNames ix

instance PassMsg MangleNames where
  type Msg MangleNames = C.WithLocationInfo MangleNamesMsg

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
