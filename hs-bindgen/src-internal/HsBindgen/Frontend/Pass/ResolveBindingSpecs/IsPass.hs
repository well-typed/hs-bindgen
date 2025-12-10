module HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (
    ResolveBindingSpecs
  , ResolvedExtBinding(..)
  , ResolveBindingSpecsMsg(..)
  ) where

import Text.SimplePrettyPrint ((<+>))

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (DeclMeta)
import HsBindgen.Frontend.Pass.NameAnon.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
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
  type Id           ResolveBindingSpecs = C.DeclId ResolveBindingSpecs
  type FieldName    ResolveBindingSpecs = C.ScopedName
  type ArgumentName ResolveBindingSpecs = Maybe C.ScopedName
  type MacroBody    ResolveBindingSpecs = CheckedMacro ResolveBindingSpecs
  type ExtBinding   ResolveBindingSpecs = ResolvedExtBinding
  type Ann ix       ResolveBindingSpecs = AnnResolveBindingSpecs ix
  type Msg          ResolveBindingSpecs = ResolveBindingSpecsMsg

data ResolvedExtBinding = ResolvedExtBinding{
      -- | Name of the C declaration for which we are using this binding
      extCName :: C.DeclName

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
  | ResolveBindingSpecsExtHsRefNoIdentifier C.DeclName
  | ResolveBindingSpecsOmittedType          C.DeclName
  | ResolveBindingSpecsTypeNotUsed          C.DeclName
  | ResolveBindingSpecsExtDecl              C.DeclName
  | ResolveBindingSpecsExtType              C.DeclName C.DeclName
  | ResolveBindingSpecsPrescriptiveRequire  C.DeclName
  | ResolveBindingSpecsPrescriptiveOmit     C.DeclName
  deriving stock (Show)

instance PrettyForTrace ResolveBindingSpecsMsg where
  prettyForTrace = \case
      ResolveBindingSpecsModuleMismatch hsModuleName pSpecHsModuleName ->
        "Prescriptive binding specification for module"
          <+> prettyForTrace pSpecHsModuleName
          <+> "cannot be used to generate"
          <+> prettyForTrace hsModuleName
      ResolveBindingSpecsExtHsRefNoIdentifier cDeclName ->
        "Haskell identifier not specified in binding specification:"
          <+> prettyForTrace cDeclName
      ResolveBindingSpecsOmittedType cDeclName ->
        "Type omitted by binding specification used:"
          <+> prettyForTrace cDeclName
      ResolveBindingSpecsTypeNotUsed cDeclName ->
        "Binding specification for type not used:"
          <+> prettyForTrace cDeclName
      ResolveBindingSpecsExtDecl cDeclName ->
        "Declaration with external binding dropped:"
          <+> prettyForTrace cDeclName
      ResolveBindingSpecsExtType ctx cDeclName ->
        "Within declaration"
          <+> prettyForTrace ctx
          <+> "type replaced with external binding:"
          <+> prettyForTrace cDeclName
      ResolveBindingSpecsPrescriptiveRequire cDeclName ->
        "Prescriptive binding specification found:"
          <+> prettyForTrace cDeclName
      ResolveBindingSpecsPrescriptiveOmit cDeclName ->
        "Declaration omitted by prescriptive binding specification:"
          <+> prettyForTrace cDeclName

instance IsTrace Level ResolveBindingSpecsMsg where
  getDefaultLogLevel = \case
    ResolveBindingSpecsModuleMismatch{}       -> Error
    ResolveBindingSpecsExtHsRefNoIdentifier{} -> Error
    ResolveBindingSpecsOmittedType{}          -> Info
    ResolveBindingSpecsTypeNotUsed{}          -> Error
    ResolveBindingSpecsExtDecl{}              -> Info
    ResolveBindingSpecsExtType{}              -> Info
    ResolveBindingSpecsPrescriptiveRequire{}  -> Info
    ResolveBindingSpecsPrescriptiveOmit{}     -> Info
  getSource          = const HsBindgen
  getTraceId         = const "resolve-binding-specs"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassHaskellId NameAnon ResolveBindingSpecs where
  coercePassHaskellId _ = id

instance CoercePassId NameAnon ResolveBindingSpecs where
  coercePassId _ = coercePass
