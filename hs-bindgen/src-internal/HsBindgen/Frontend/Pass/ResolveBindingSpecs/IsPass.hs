module HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (
    ResolveBindingSpecs
  , ResolvedExtBinding(..)
  , ResolveBindingSpecsMsg(..)
  ) where

import Text.SimplePrettyPrint ((<+>))

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (DeclMeta)
import HsBindgen.Frontend.Pass.Parse.IsPass (OrigTypedefRef)
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Resolve binding specifications
--
-- For every C name, we resolve:
--
-- * External binding specification, which is used to specify existing bindings
--   that should be used, external from the module being generated.  Matching
--   types are replaced with external references, and matching declarations are
--   removed from the AST.
-- * Prescriptive binding specification, which is used to configure how bindings
--   are generated.  This information is added to the AST as annotations.
type ResolveBindingSpecs :: Pass
data ResolveBindingSpecs a deriving anyclass (ValidPass)

type family AnnResolveBindingSpecs ix where
  AnnResolveBindingSpecs "TranslationUnit" = DeclMeta
  AnnResolveBindingSpecs "Decl"            =
    (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
  AnnResolveBindingSpecs _                 = NoAnn

instance IsPass ResolveBindingSpecs where
  type Id           ResolveBindingSpecs = C.DeclId
  type FieldName    ResolveBindingSpecs = C.Name
  type ArgumentName ResolveBindingSpecs = Maybe C.Name
  type TypedefRef   ResolveBindingSpecs = OrigTypedefRef ResolveBindingSpecs
  type MacroBody    ResolveBindingSpecs = CheckedMacro ResolveBindingSpecs
  type ExtBinding   ResolveBindingSpecs = ResolvedExtBinding
  type Ann ix       ResolveBindingSpecs = AnnResolveBindingSpecs ix
  type Msg          ResolveBindingSpecs = ResolveBindingSpecsMsg

data ResolvedExtBinding = ResolvedExtBinding{
      -- | Name of the C declaration for which we are using this binding
      extCName :: C.QualName

      -- | The Haskell type which will be used
    , extHsRef :: Hs.ExtRef

      -- | Additional information about the C type
    , extCSpec :: BindingSpec.CTypeSpec

      -- | Additional information about the Haskell type
    , extHsSpec :: Maybe BindingSpec.HsTypeSpec
    }
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ResolveBindingSpecsMsg =
    ResolveBindingSpecsModuleMismatch       Hs.ModuleName Hs.ModuleName
  | ResolveBindingSpecsExtHsRefNoIdentifier C.QualName
  | ResolveBindingSpecsOmittedTypeUse       C.QualName
  | ResolveBindingSpecsTypeNotUsed          C.QualName
  | ResolveBindingSpecsExtDecl              C.QualName
  | ResolveBindingSpecsExtType              C.QualName C.QualName
  | ResolveBindingSpecsPrescriptiveRequire  C.QualName
  | ResolveBindingSpecsPrescriptiveOmit     C.QualName
  deriving stock (Show)

instance PrettyForTrace ResolveBindingSpecsMsg where
  prettyForTrace = \case
      ResolveBindingSpecsModuleMismatch hsModuleName pSpecHsModuleName ->
        "prescriptive binding specification for module"
          <+> prettyForTrace pSpecHsModuleName
          <+> "cannot be used to generate"
          <+> prettyForTrace hsModuleName
      ResolveBindingSpecsExtHsRefNoIdentifier cQualName ->
        "Haskell identifier not specified in binding specification:"
          <+> prettyForTrace cQualName
      ResolveBindingSpecsOmittedTypeUse cQualName ->
        "type omitted by binding specification used:"
          <+> prettyForTrace cQualName
      ResolveBindingSpecsTypeNotUsed cQualName ->
        "binding specification for type not used:"
          <+> prettyForTrace cQualName
      ResolveBindingSpecsExtDecl cQualName ->
        "declaration with external binding dropped:"
          <+> prettyForTrace cQualName
      ResolveBindingSpecsExtType ctx cQualName ->
        "within declaration"
          <+> prettyForTrace ctx
          <+> "type replaced with external binding:"
          <+> prettyForTrace cQualName
      ResolveBindingSpecsPrescriptiveRequire cQualName ->
        "prescriptive binding specification found:"
          <+> prettyForTrace cQualName
      ResolveBindingSpecsPrescriptiveOmit cQualName ->
        "declaration omitted by prescriptive binding specification:"
          <+> prettyForTrace cQualName

instance IsTrace Level ResolveBindingSpecsMsg where
  getDefaultLogLevel = \case
    ResolveBindingSpecsModuleMismatch{}       -> Error
    ResolveBindingSpecsExtHsRefNoIdentifier{} -> Error
    ResolveBindingSpecsOmittedTypeUse{}       -> Error
    ResolveBindingSpecsTypeNotUsed{}          -> Error
    ResolveBindingSpecsExtDecl{}              -> Info
    ResolveBindingSpecsExtType{}              -> Info
    ResolveBindingSpecsPrescriptiveRequire{}  -> Info
    ResolveBindingSpecsPrescriptiveOmit{}     -> Info
  getSource          = const HsBindgen
  getTraceId         = const "resolve-binding-specs"
