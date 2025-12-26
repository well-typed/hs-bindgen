{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoRecordWildCards #-}

module HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (
    ResolveBindingSpecs
  , PrescriptiveDeclSpec(..)
  , ResolvedExtBinding(..)
  , ResolveBindingSpecsMsg(..)
  , extDeclIdPair
  ) where

import Text.SimplePrettyPrint ((<+>))

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass (DeclMeta)
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
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
data ResolveBindingSpecs a

type family AnnResolveBindingSpecs ix where
  AnnResolveBindingSpecs "TranslationUnit" = DeclMeta
  AnnResolveBindingSpecs "Decl"            = PrescriptiveDeclSpec
  AnnResolveBindingSpecs _                 = NoAnn

instance IsPass ResolveBindingSpecs where
  type MacroBody  ResolveBindingSpecs = CheckedMacro ResolveBindingSpecs
  type ExtBinding ResolveBindingSpecs = ResolvedExtBinding
  type Ann ix     ResolveBindingSpecs = AnnResolveBindingSpecs ix
  type Msg        ResolveBindingSpecs = ResolveBindingSpecsMsg

  extBindingId _ = (.cName)

-- | Prescriptive binding specification for declaration
--
-- Although we have interpreted /part/ of this binding specification during
-- name mangling, we leave the /full/ binding specification in the AST, because
-- we need it when we  /generate/ the output binding specification.
--
-- TODO: This is not quite right: we should distinguish between binding
-- specifications for different classes of things (declarations of types,
-- functions, etc.). When we do, we should not associate them with the top-level
-- 'Decl' but instead with specific 'DeclKind's. When we change this, this will
-- have consequences for "HsBindgen.Language.Haskell.Origin" also.
data PrescriptiveDeclSpec = PrescriptiveDeclSpec {
      cSpec  :: Maybe BindingSpec.CTypeSpec
    , hsSpec :: Maybe BindingSpec.HsTypeSpec
    }
  deriving stock (Show, Eq, Generic)

data ResolvedExtBinding = ResolvedExtBinding{
      -- | C declaration for which we are using this binding
      cName :: DeclId

      -- | The Haskell type which will be used
    , hsName :: Hs.ExtRef

      -- | Additional information about the C type
    , cSpec :: BindingSpec.CTypeSpec

      -- | Additional information about the Haskell type
    , hsSpec :: BindingSpec.HsTypeSpec
    }
  deriving stock (Show, Eq, Ord, Generic)

extDeclIdPair :: ResolvedExtBinding -> DeclIdPair
extDeclIdPair ext = DeclIdPair{
      cName  = ext.cName
    , hsName = ext.hsName.extRefIdentifier
    }

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ResolveBindingSpecsMsg =
    ResolveBindingSpecsModuleMismatch       Hs.ModuleName Hs.ModuleName
  | ResolveBindingSpecsExtHsRefNoIdentifier DeclId
  | ResolveBindingSpecsNoHsTypeSpec         DeclId
  | ResolveBindingSpecsNoHsTypeRep          DeclId
  | ResolveBindingSpecsOmittedType          DeclId
  | ResolveBindingSpecsTypeNotUsed          DeclId
  | ResolveBindingSpecsExtDecl              DeclId
  | ResolveBindingSpecsExtType              DeclId DeclId
  | ResolveBindingSpecsPrescriptiveRequire  DeclId
  | ResolveBindingSpecsPrescriptiveOmit     DeclId
  deriving stock (Show)

instance PrettyForTrace ResolveBindingSpecsMsg where
  prettyForTrace = \case
      ResolveBindingSpecsModuleMismatch hsModuleName pSpecHsModuleName ->
        "Prescriptive binding specification for module"
          <+> prettyForTrace pSpecHsModuleName
          <+> "cannot be used to generate"
          <+> prettyForTrace hsModuleName
      ResolveBindingSpecsExtHsRefNoIdentifier cDeclId ->
        "Haskell identifier not specified in binding specification:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsNoHsTypeSpec cDeclId ->
        "Haskell type spec not specified in binding specification:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsNoHsTypeRep cDeclId ->
        "Haskell type rep not specified in binding specification:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsOmittedType cDeclId ->
        "Type omitted by binding specification used:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsTypeNotUsed cDeclId ->
        "Binding specification for type not used:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsExtDecl cDeclId ->
        "Declaration with external binding dropped:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsExtType ctx cDeclId ->
        "Within declaration"
          <+> prettyForTrace ctx
          <+> "type replaced with external binding:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsPrescriptiveRequire cDeclId ->
        "Prescriptive binding specification found:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsPrescriptiveOmit cDeclId ->
        "Declaration omitted by prescriptive binding specification:"
          <+> prettyForTrace cDeclId

instance IsTrace Level ResolveBindingSpecsMsg where
  getDefaultLogLevel = \case
    ResolveBindingSpecsModuleMismatch{}       -> Error
    ResolveBindingSpecsExtHsRefNoIdentifier{} -> Error
    ResolveBindingSpecsNoHsTypeSpec{}         -> Error
    ResolveBindingSpecsNoHsTypeRep{}          -> Error
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

instance CoercePassId HandleMacros ResolveBindingSpecs
