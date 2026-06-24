module HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (
    ResolveBindingSpecs
  , PrescriptiveDeclSpec(..)
  , ResolveBindingSpecsMsg(..)
  ) where

import Text.SimplePrettyPrint ((<+>))

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
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
  AnnResolveBindingSpecs "Decl" = PrescriptiveDeclSpec
  AnnResolveBindingSpecs _      = NoAnn

instance IsPass ResolveBindingSpecs

instance PassId ResolveBindingSpecs

instance PassScopedName ResolveBindingSpecs

instance PassTypes ResolveBindingSpecs

instance PassMacro ResolveBindingSpecs where
  type MacroId         ResolveBindingSpecs = Id ResolveBindingSpecs
  type MacroBody       ResolveBindingSpecs = TypecheckedMacro ResolveBindingSpecs
  type MacroUnderlying ResolveBindingSpecs = C.Type ResolveBindingSpecs

  macroIdId _ = id

instance PassExtBinding ResolveBindingSpecs where
  type ExtBinding ResolveBindingSpecs = BindingSpec.ResolvedExtBinding

  extBindingId _ = (.cName)

instance PassCommentDecl ResolveBindingSpecs where
  type CommentDecl ResolveBindingSpecs = Maybe (C.Comment ResolveBindingSpecs)

instance PassAnn ResolveBindingSpecs where
  type Ann ix ResolveBindingSpecs = AnnResolveBindingSpecs ix

instance PassMsg ResolveBindingSpecs where
  type Msg ResolveBindingSpecs = ResolveBindingSpecsMsg

-- | Prescriptive binding specification for declaration
--
-- Although we have interpreted /part/ of this binding specification during
-- name mangling, we leave the /full/ binding specification in the AST, because
-- we need it when we /generate/ the output binding specification.
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1770>
-- If we have binding specs for different kinds of things (types, functions, ..)
-- we may want to have different types for those kinds of specs, and then
-- associate the appropriate type with specific 'HsBindgen.Frontend.AST.Decl.DeclKind's (rather than this
-- "catch all" type with the top-level 'HsBindgen.Frontend.AST.Decl.Decl').
data PrescriptiveDeclSpec = PrescriptiveDeclSpec {
      cSpec  :: Maybe BindingSpec.CTypeSpec
    , hsSpec :: Maybe BindingSpec.HsTypeSpec
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ResolveBindingSpecsMsg =
    ResolveBindingSpecsModuleMismatch       Hs.ModuleName Hs.ModuleName
  | ResolveBindingSpecsEnumTypeMismatch     C.DeclId
  | ResolveBindingSpecsExtHsRefNoIdentifier C.DeclId
  | ResolveBindingSpecsNoHsTypeSpec         C.DeclId
  | ResolveBindingSpecsOmittedType          C.DeclId
  | ResolveBindingSpecsTypeNotUsed          C.DeclId
  | ResolveBindingSpecsExtDecl              C.DeclId
  | ResolveBindingSpecsExtType              C.DeclId C.DeclId
  | ResolveBindingSpecsPreRequire           C.DeclId
  | ResolveBindingSpecsPreOmit              C.DeclId
  | ResolveBindingSpecsPreEmptyData         C.DeclId
  | ResolveBindingSpecsPreEmptyDataInvalid  C.DeclId
  deriving stock (Show)

instance PrettyForTrace ResolveBindingSpecsMsg where
  prettyForTrace = \case
      ResolveBindingSpecsModuleMismatch hsModuleName pSpecHsModuleName ->
        "Prescriptive binding specification for module"
          <+> prettyForTrace pSpecHsModuleName
          <+> "cannot be used to generate"
          <+> prettyForTrace hsModuleName
      ResolveBindingSpecsEnumTypeMismatch cDeclId ->
        "C enum specification for non-enum type:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsExtHsRefNoIdentifier cDeclId ->
        "Haskell identifier not specified in binding specification:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsNoHsTypeSpec cDeclId ->
        "Haskell type spec not specified in binding specification:"
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
      ResolveBindingSpecsPreRequire cDeclId ->
        "Prescriptive binding specification found:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsPreOmit cDeclId ->
        "Declaration omitted by prescriptive binding specification:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsPreEmptyData cDeclId ->
        "Declaration opaqued by prescriptive binding specification:"
          <+> prettyForTrace cDeclId
      ResolveBindingSpecsPreEmptyDataInvalid cDeclId ->
        "Declaration opaqued by prescriptive binding specification invalid for kind:"
          <+> prettyForTrace cDeclId

instance IsTrace Level ResolveBindingSpecsMsg where
  getDefaultLogLevel = \case
    ResolveBindingSpecsModuleMismatch{}       -> Warning
    ResolveBindingSpecsEnumTypeMismatch{}     -> Warning
    ResolveBindingSpecsExtHsRefNoIdentifier{} -> Warning
    ResolveBindingSpecsNoHsTypeSpec{}         -> Warning
    ResolveBindingSpecsOmittedType{}          -> Info
    ResolveBindingSpecsTypeNotUsed{}          -> Warning
    ResolveBindingSpecsExtDecl{}              -> Info
    ResolveBindingSpecsExtType{}              -> Info
    ResolveBindingSpecsPreRequire{}           -> Info
    ResolveBindingSpecsPreOmit{}              -> Info
    ResolveBindingSpecsPreEmptyData{}         -> Info
    ResolveBindingSpecsPreEmptyDataInvalid{}  -> Warning
  getSource          = const HsBindgen
  getTraceId         = const "resolve-binding-specs"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId               ReparseMacroExpansions ResolveBindingSpecs
instance CoercePassMacroId          ReparseMacroExpansions ResolveBindingSpecs
instance CoercePassAnn "TypeFunArg" ReparseMacroExpansions ResolveBindingSpecs

instance CoercePassCommentDecl ReparseMacroExpansions ResolveBindingSpecs where
  coercePassCommentDecl _ = fmap coercePass
