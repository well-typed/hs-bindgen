module HsBindgen.Frontend.Pass.MangleNames.IsPass (
    MangleNames
    -- * Annotations
  , NamePair(..)
  , nameHs
  , RecordNames(..)
  , NewtypeNames(..)
  , DeclSpec(..)
    -- * Trace messages
  , Msg(..)
  ) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass (ResolvedExtBinding)
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Mangle names pass
type MangleNames :: Pass
data MangleNames a deriving anyclass (ValidPass)

type family AnnMangleNames ix where
  AnnMangleNames "Decl"             = DeclSpec
  AnnMangleNames "TranslationUnit"  = DeclMeta
  AnnMangleNames "Struct"           = RecordNames
  AnnMangleNames "Union"            = NewtypeNames
  AnnMangleNames "Enum"             = NewtypeNames
  AnnMangleNames "Typedef"          = NewtypeNames
  AnnMangleNames "CheckedMacroType" = NewtypeNames
  AnnMangleNames _                  = NoAnn

instance IsPass MangleNames where
  type Id         MangleNames = (NamePair, NameOrigin)
  type FieldName  MangleNames = NamePair
  type TypedefRef MangleNames = RenamedTypedefRef MangleNames
  type MacroBody  MangleNames = CheckedMacro MangleNames
  type ExtBinding MangleNames = ResolvedExtBinding
  type Ann ix     MangleNames = AnnMangleNames ix
  data Msg        MangleNames =
      CouldNotMangle Text
    | MissingDeclaration C.QualName
    deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Identifiers
-------------------------------------------------------------------------------}

-- | Pair of a C name and the corresponding Haskell name
--
-- Invariant: the 'HsIdentifier' must satisfy the rules for legal Haskell names,
-- for its intended use (constructor, variable, ..).
data NamePair = NamePair {
      nameC       :: C.Name
    , nameHsIdent :: HsIdentifier
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Extract namespaced Haskell name
--
-- The invariant on 'NamePair' justifies this otherwise unsafe operation.
nameHs :: NamePair -> HsName ns
nameHs NamePair{nameHsIdent = HsIdentifier name} = HsName name

{-------------------------------------------------------------------------------
  Additional names

  This is in addition to the 'NamePair's already embedded in the AST.
-------------------------------------------------------------------------------}

-- | Names for a Haskell record type
data RecordNames = RecordNames {
      recordConstr :: HsName NsConstr
    }
  deriving stock (Show, Eq, Generic)

-- | Names for a Haskell newtype
data NewtypeNames = NewtypeNames {
      newtypeConstr :: HsName NsConstr
    , newtypeField  :: HsName NsVar
    }
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
  Trace messages
-------------------------------------------------------------------------------}


instance PrettyForTrace (Msg MangleNames) where
  prettyForTrace (CouldNotMangle name) =
    "Could not mangle C name: " >< textToCtxDoc name
  prettyForTrace (MissingDeclaration cQualName) = hcat [
      "Missing declaration: '"
    , prettyForTrace cQualName
    , "'; did you select the declaration?"
    ]

instance HasDefaultLogLevel (Msg MangleNames) where
  getDefaultLogLevel = const Error
