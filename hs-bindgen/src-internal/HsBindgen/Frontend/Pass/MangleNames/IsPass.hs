module HsBindgen.Frontend.Pass.MangleNames.IsPass (
    MangleNames
    -- * Intermediate pass (local to name mangling)
  , CreateNames
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
  AnnMangleNames "Struct"               = StructNames
  AnnMangleNames "Flam"                 = FlamNames
  AnnMangleNames "Union"                = NewtypeNames
  AnnMangleNames "Enum"                 = NewtypeNames
  AnnMangleNames "Typedef"              = TypedefNames
  AnnMangleNames "TypecheckedMacroType" = NewtypeNames
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
  Intermediate pass: CreateNames

  This pass is local to name mangling: it is /not/ exposed as a frontend
  artefact and not selectable via the CLI. It is the result of the first of the
  three name-mangling traversals ("create names"): it has all within-declaration
  names minted (as annotations and 'ScopedNamePair' slots), but all references
  (the 'Id' slots) are still unresolved 'DeclId's. The final 'resolveNames'
  traversal rewrites these into 'DeclIdPair's, producing 'MangleNames'.

  Note that @'Ann' ix 'CreateNames'@ and @'Ann' ix 'MangleNames'@ reduce to the
  same bundle types, and 'ScopedName' is 'ScopedNamePair' in both. The
  annotations and scoped names can therefore be carried across directly during
  'resolveNames'.
-------------------------------------------------------------------------------}

-- | Create names pass (local to name mangling)
type CreateNames :: Pass
data CreateNames a

type family AnnCreateNames ix where
  AnnCreateNames "Decl"                 = PrescriptiveDeclSpec
  AnnCreateNames "Struct"               = StructNames
  AnnCreateNames "Flam"                 = FlamNames
  AnnCreateNames "Union"                = NewtypeNames
  AnnCreateNames "Enum"                 = NewtypeNames
  AnnCreateNames "Typedef"              = TypedefNames
  AnnCreateNames "TypecheckedMacroType" = NewtypeNames
  AnnCreateNames _                      = NoAnn

instance IsPass CreateNames

instance PassId CreateNames where
  type Id CreateNames = C.DeclId

instance PassScopedName CreateNames where
  type ScopedName CreateNames = ScopedNamePair

instance PassMacro CreateNames where
  type MacroId         CreateNames = C.DeclId
  type MacroBody       CreateNames = TypecheckedMacro CreateNames
  type MacroUnderlying CreateNames = C.Type CreateNames

  macroIdId _ = id

instance PassExtBinding CreateNames where
  type ExtBinding CreateNames = BindingSpec.ResolvedExtBinding

  extBindingId _ = (.cName)

instance PassCommentDecl CreateNames where
  type CommentDecl CreateNames = Maybe (C.Comment CreateNames)

instance PassAnn CreateNames where
  type Ann ix CreateNames = AnnCreateNames ix

instance PassMsg CreateNames where
  type Msg CreateNames = C.WithLocationInfo MangleNamesMsg

{-------------------------------------------------------------------------------
  CoercePass: ResolveBindingSpecs → CreateNames

  Used by the "create names" traversal to reindex the parts of a declaration
  that do not change representation (types, comments, enclosing references). The
  'Id' (and hence 'MacroId') and 'ExtBinding' representations agree between the
  two passes, so these coercions are pure reindexing.
-------------------------------------------------------------------------------}

instance CoercePassId               ResolveBindingSpecs CreateNames
instance CoercePassMacroId          ResolveBindingSpecs CreateNames
instance CoercePassAnn "TypeFunArg" ResolveBindingSpecs CreateNames

instance CoercePassMacroUnderlying ResolveBindingSpecs CreateNames where
  coercePassMacroUnderlying _ = coercePass

instance CoercePassCommentDecl ResolveBindingSpecs CreateNames where
  coercePassCommentDecl _ = fmap coercePass

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
