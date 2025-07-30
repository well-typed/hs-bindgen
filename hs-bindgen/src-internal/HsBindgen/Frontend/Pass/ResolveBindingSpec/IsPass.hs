module HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass (
    ResolveBindingSpec
  , ResolvedExtBinding(..)
  , ResolveBindingSpecMsg(..)
  ) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta)
import HsBindgen.Imports
import HsBindgen.Language.Haskell (ExtHsRef)
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((<+>))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Resolve binding specification
--
-- For every C name, we resolve:
--
-- * External binding specification, which is used to specify existing bindings
--   that should be used, external from the module being generated.  Matching
--   types are replaced with external references, and matching declarations are
--   removed from the AST.
-- * Prescriptive binding specification, which is used to configure how bindings
--   are generated.  This information is added to the AST as annotations.
type ResolveBindingSpec :: Pass
data ResolveBindingSpec a deriving anyclass (ValidPass)

type family AnnResolveBindingSpec ix where
  AnnResolveBindingSpec "Decl"            = BindingSpec.TypeSpec
  AnnResolveBindingSpec "TranslationUnit" = DeclMeta
  AnnResolveBindingSpec _                 = NoAnn

instance IsPass ResolveBindingSpec where
  type Id         ResolveBindingSpec = C.DeclId
  type FieldName  ResolveBindingSpec = C.Name
  type TypedefRef ResolveBindingSpec = C.Name
  type MacroBody  ResolveBindingSpec = CheckedMacro ResolveBindingSpec
  type ExtBinding ResolveBindingSpec = ResolvedExtBinding
  type Ann ix     ResolveBindingSpec = AnnResolveBindingSpec ix
  type Msg        ResolveBindingSpec = ResolveBindingSpecMsg

data ResolvedExtBinding = ResolvedExtBinding{
      -- | Name of the C declaration for which we are using this binding
      extCName :: C.QualName

      -- | The Haskell type which will be used
    , extHsRef :: ExtHsRef

      -- | Additional information about the Haskell type
    , extHsSpec :: BindingSpec.TypeSpec
    }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ResolveBindingSpecMsg =
    ResolveBindingSpecExtHsRefNoModule     C.QualName
  | ResolveBindingSpecExtHsRefNoIdentifier C.QualName
  | ResolveBindingSpecOmittedTypeUse       C.QualName
  | ResolveBindingSpecTypeNotUsed          C.QualName
  deriving stock (Show, Eq)

instance PrettyForTrace ResolveBindingSpecMsg where
  prettyForTrace = \case
      ResolveBindingSpecExtHsRefNoModule cQualName ->
        "Haskell module not specified in binding specification:"
          <+> prettyForTrace cQualName
      ResolveBindingSpecExtHsRefNoIdentifier cQualName ->
        "Haskell identifier not specified in binding specification:"
          <+> prettyForTrace cQualName
      ResolveBindingSpecOmittedTypeUse cQualName ->
        "type omitted by binding specification used:"
          <+> prettyForTrace cQualName
      ResolveBindingSpecTypeNotUsed cQualName ->
        "binding specification for type not used:"
          <+> prettyForTrace cQualName

instance HasDefaultLogLevel ResolveBindingSpecMsg where
  getDefaultLogLevel = const Error

instance HasSource ResolveBindingSpecMsg where
  getSource = const HsBindgen
