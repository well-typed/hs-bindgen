module HsBindgen.Frontend.Pass.MangleNames.IsPass (
    MangleNames
    -- * Additional names
  , StructNames(..)
  , FlamNames(..)
  , NewtypeNames(..)
  , TypedefNames(..)
    -- * Trace messages
  , MangleNamesMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.MangleNames.Names
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation
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
  AnnMangleNames "Enum"                 = NewtypeNames
  AnnMangleNames "Flam"                 = FlamNames
  AnnMangleNames "Struct"               = StructNames
  AnnMangleNames "Typedef"              = TypedefNames
  AnnMangleNames "TypecheckedMacroType" = NewtypeNames
  AnnMangleNames "Union"                = NewtypeNames
  AnnMangleNames _                      = NoAnn

instance IsPass MangleNames

instance PassId MangleNames where
  type Id MangleNames = DeclIdPair

  idNameKind     _ namePair = namePair.cName.name.kind
  idSourceName   _ namePair = C.declIdSourceName namePair.cName
  idLocationInfo _ namePair = C.declIdLocationInfo namePair.cName

instance PassScopedName MangleNames where
  type ScopedName MangleNames = ScopedNamePair

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

-- | The auxiliary type-constructor name for a @struct@ with a flexible array
-- member (FLAM) no longer lives here: it is bundled with the FLAM field in the
-- 'C.Flam' constructor (as 'FlamNames'), so that the name is minted exactly when
-- the FLAM is present (see <https://github.com/well-typed/hs-bindgen/issues/1925>).
data StructNames = StructNames {
      constr :: Hs.Name Hs.NsConstr
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Names required to generate the auxiliary type of a struct with a flexible
-- array member (FLAM)
--
-- Bundled with the FLAM field in the 'C.Flam' constructor, so that these names
-- exist exactly when there is a FLAM to generate code for.
data FlamNames = FlamNames {
      -- | Name of the auxiliary type we generate for the @struct@
      aux :: Hs.Name Hs.NsTypeConstr
    }
  deriving stock (Show, Eq, Ord, Generic)

data NewtypeNames = NewtypeNames {
      dataConstr :: Hs.Name Hs.NsConstr
    , field      :: Hs.Name Hs.NsVar
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
    MangleNamesAssignedName       Hs.SomeName
  | MangleNamesReusedAssignedName Hs.SomeName
  | MangleNamesNameMap            NameMap
  | MangleNamesNameRegistry       NameRegistry
  deriving stock (Show)

instance PrettyForTrace MangleNamesMsg where
  prettyForTrace = \case
      MangleNamesAssignedName newName -> PP.hsep [
          "Assigned name"
        , PP.text newName.text
        ]
      MangleNamesReusedAssignedName newName -> PP.hsep [
          "Reused assigned name"
        , PP.text newName.text
        ]
      MangleNamesNameMap nm -> PP.hsep [
          "The name map is:"
        , PP.show nm
        ]
      MangleNamesNameRegistry rg -> PP.hsep [
          "The name registry is:"
        , PP.show rg
        ]

instance IsTrace Level MangleNamesMsg where
  getDefaultLogLevel = \case
      MangleNamesAssignedName{}       -> Info
      MangleNamesReusedAssignedName{} -> Info
      MangleNamesNameMap{}            -> Debug
      MangleNamesNameRegistry{}       -> Debug

  getSource  = const HsBindgen
  getTraceId = \case
    MangleNamesAssignedName{}       -> "mangle-names-assigned-name"
    MangleNamesReusedAssignedName{} -> "mangle-names-reused-assigned-name"
    MangleNamesNameMap{}            -> "mangle-names-name-map"
    MangleNamesNameRegistry{}       -> "mangle-names-name-registry"
