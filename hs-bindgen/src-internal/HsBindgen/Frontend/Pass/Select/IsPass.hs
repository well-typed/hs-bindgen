module HsBindgen.Frontend.Pass.Select.IsPass (
    Select
    -- * Configuration
  , ProgramSlicing(..)
  , SelectConfig(..)
    -- * Trace messages
  , SelectReason(..)
  , UnavailabilityReason(..)
  , SelectStatus(..)
  , SelectMsg(..)
  ) where

import Data.Default (Default (def))
import Text.SimplePrettyPrint ((<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Select :: Pass
data Select a deriving anyclass ValidPass

type family AnnSelect ix where
  AnnSelect "TranslationUnit" = DeclMeta
  AnnSelect "Decl"            = BindingSpec.CTypeSpec
  AnnSelect _                 = NoAnn

instance IsPass Select where
  type Id           Select = C.DeclId
  type FieldName    Select = C.Name
  type ArgumentName Select = Maybe C.Name
  type TypedefRef   Select = OrigTypedefRef Select
  -- NOTE Using @CheckedMacro Select@ is incompatible with 'CoercePass'
  type MacroBody    Select = CheckedMacro ResolveBindingSpecs
  type ExtBinding   Select = ResolvedExtBinding
  type Ann ix       Select = AnnSelect ix
  type Config       Select = SelectConfig
  type Msg          Select = SelectMsg

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Select transitive dependencies?
data ProgramSlicing =
    -- | Select declarations using the select predicate /and/ their transitive
    -- dependencies.
    EnableProgramSlicing
  | DisableProgramSlicing
  deriving stock (Show, Eq)

instance Default ProgramSlicing where
  def :: ProgramSlicing
  def = DisableProgramSlicing

data SelectConfig = SelectConfig {
      selectConfigProgramSlicing :: ProgramSlicing
    , selectConfigParsePredicate :: Boolean ParsePredicate
    , selectConfigPredicate      :: Boolean SelectPredicate
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data SelectReason =
    -- | The user actively selects the declarations.
    SelectionRoot
    -- | The user has activated program slicing, and the declaration is a
    -- transitive dependency of a selection root.
  | TransitiveDependency
  deriving stock (Show, Eq)

instance PrettyForTrace SelectReason where
  prettyForTrace = \case
    SelectionRoot        -> "direct select predicate match"
    TransitiveDependency -> "transitive dependency"

data SelectStatus =
    NotSelected
  | Selected SelectReason
  deriving stock (Show)

data UnavailabilityReason =
    UnavailableParseNotAttempted  C.QualPrelimDeclId
  | UnavailableParseFailed        C.QualPrelimDeclId
  | UnavailableHandleMacrosFailed C.QualPrelimDeclId
  deriving stock (Show, Eq, Ord)


instance PrettyForTrace UnavailabilityReason where
  prettyForTrace r = "unavailable" <+> case r of
    UnavailableParseNotAttempted x -> PP.hcat [
        "(parse of transitive dependency "
      , prettyForTrace x
      , " not attempted: (!) adjust parse predicate)"
      ]
    UnavailableParseFailed x -> PP.hcat [
        "(parse of transitive dependency "
      , prettyForTrace x
      , " failed)"
      ]
    UnavailableHandleMacrosFailed x -> PP.hcat [
        "(macro parsing or type-checking of transitive dependency "
      , prettyForTrace x
      , " failed)"
      ]

-- | Select trace messages
data SelectMsg =
    -- | Information about selection status; issued for all available
    --declarations.
    SelectStatusInfo SelectStatus (C.Decl Select)
    -- | The user has selected a declaration that is available but at least one
    -- of its transitive dependencies is _unavailable_.
  | TransitiveDependencyOfDeclarationUnavailable SelectReason UnavailabilityReason (C.Decl Select)
    -- | A declaration itself is unavailable.
  | SelectDeclarationUnavailable C.QualPrelimDeclId
    -- | The user has selected a deprecated declaration. Maybe they want to
    -- de-select deprecated declaration?
  | SelectDeprecated (C.Decl Select)
    -- | Delayed parse message for actually selected declarations.
  | SelectParseSuccess (AttachedParseMsg DelayedParseMsg)
    -- | Delayed parse message for declarations the user directly wants to
    -- select (i.e., not a transitive dependency), but we have not attempted to
    -- parse.
  | SelectParseNotAttempted (AttachedParseMsg ParseNotAttemptedReason)
    -- | Delayed parse message for declarations the user directly wants to
    -- select (i.e., not a transitive dependency), but we have failed to parse.
  | SelectParseFailure (AttachedParseMsg DelayedParseMsg)
    -- | Inform the user that no declarations matched the select predicate.
  | SelectNoDeclarationsMatched
  deriving stock (Show)

instance PrettyForTrace SelectMsg where
  prettyForTrace = \case
    SelectStatusInfo NotSelected x ->
      prettyForTrace x >< "not selected"
    SelectStatusInfo (Selected r) x ->
      prettyForTrace x >< " selected (" >< prettyForTrace r >< ")"
    TransitiveDependencyOfDeclarationUnavailable s u x -> PP.hcat [
        prettyForTrace x
      , " selected ("
      , prettyForTrace s
      , ") but "
      , prettyForTrace u
      ]
    SelectDeclarationUnavailable i ->
        "Tried to select an unavailable declaration: " >< prettyForTrace i
    SelectDeprecated x -> PP.hcat [
        "Selected a deprecated declaration: "
      , prettyForTrace x
      , "; you may want to de-select it"
      ]
    SelectParseSuccess x ->
      "During parse:" <+> prettyForTrace x
    SelectParseNotAttempted x -> PP.vcat [
      "Could not select declaration:" <+> prettyForTrace x
      , "Consider changing the parse predicate"
      ]
    SelectParseFailure x ->
      "Could not select declaration; parse failure:" <+> prettyForTrace x
    SelectNoDeclarationsMatched ->
      "No declarations matched the select predicate"

instance IsTrace Level SelectMsg where
  getDefaultLogLevel = \case
    SelectStatusInfo{}                             -> Info
    TransitiveDependencyOfDeclarationUnavailable{} -> Warning
    SelectDeclarationUnavailable{}                 -> Error
    SelectDeprecated{}                             -> Notice
    SelectParseSuccess x                           -> getDefaultLogLevel x
    SelectParseNotAttempted{}                      -> Warning
    SelectParseFailure x                           -> getDefaultLogLevel x
    SelectNoDeclarationsMatched                    -> Warning
  getSource  = const HsBindgen
  getTraceId = \case
    SelectStatusInfo{}                             -> "select"
    TransitiveDependencyOfDeclarationUnavailable{} -> "select"
    SelectDeclarationUnavailable{}                 -> "select"
    SelectDeprecated{}                             -> "select"
    SelectParseSuccess x                           -> "select-" <> getTraceId x
    SelectParseNotAttempted{}                      -> "select-parse"
    SelectParseFailure x                           -> "select-" <> getTraceId x
    SelectNoDeclarationsMatched                    -> "select"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePass TypedefRefWrapper ResolveBindingSpecs Select where
  coercePass (TypedefRefWrapper ref) = TypedefRefWrapper (coercePass ref)
