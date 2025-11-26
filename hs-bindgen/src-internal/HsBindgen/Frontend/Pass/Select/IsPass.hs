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
import Text.SimplePrettyPrint (CtxDoc, (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.Error (HandleMacrosParseMsg)
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
  AnnSelect "Decl"            =
    (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
  AnnSelect _                 = NoAnn

instance IsPass Select where
  type Id           Select = C.DeclId Select
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

-- | The order is important, the most "natural" cause of unavailability comes
-- first.
--
-- For example, if something fails to parse, but was not selected, we should say
-- that it was not selected, rather than it failed to parse (why would we parse
-- it, if it was not selected).
data UnavailabilityReason =
    UnavailableParseNotAttempted
  | UnavailableNotSelected
  | UnavailableParseFailed
  | UnavailableHandleMacrosFailed
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace UnavailabilityReason where
  prettyForTrace r = case r of
    UnavailableParseNotAttempted ->
      "parse of transitive dependency not attempted: (!) adjust parse predicate"
    UnavailableParseFailed ->
      "parse of transitive dependency failed"
    UnavailableHandleMacrosFailed ->
      "macro parsing or type-checking of transitive dependency failed"
    UnavailableNotSelected ->
      "transitive dependency not selected"

-- | Select trace messages
data SelectMsg =
    -- | Information about selection status; issued for all available
    --declarations.
    SelectStatusInfo SelectStatus (C.Decl Select)
    -- | The user has selected a declaration that is available but at least one
    -- of its transitive dependencies is _unavailable_.
  | TransitiveDependencyOfDeclarationUnavailable
      SelectReason
      (C.QualPrelimDeclId, UnavailabilityReason)
      (C.Decl Select)
    -- | The user has selected a deprecated declaration. Maybe they want to
    -- de-select deprecated declaration?
  | SelectDeprecated (C.Decl Select)
    -- | Delayed parse message for actually selected declarations.
  | SelectParseSuccess (AttachedParseMsg DelayedParseMsg)
    -- | Delayed parse message for declarations the user wants to select
    -- directly, but we have not attempted to parse.
  | SelectParseNotAttempted ParseNotAttempted
    -- | Delayed parse message for declarations the user wants to select
    -- directly, but we have failed to parse.
  | SelectParseFailure ParseFailure
    -- | Delayed handle macros message for macros the user wants to select
    -- | directly, but we have failed to parse.
  | SelectMacroFailure HandleMacrosParseMsg
    -- | Inform the user that no declarations matched the select predicate.
  | SelectNoDeclarationsMatched
  deriving stock (Show)

instance PrettyForTrace SelectMsg where
  prettyForTrace = \case
    SelectStatusInfo NotSelected x ->
      prettyForTrace x >< "not selected"
    SelectStatusInfo (Selected r) x ->
      prettyForTrace x >< " selected (" >< prettyForTrace r >< ")"
    TransitiveDependencyOfDeclarationUnavailable s (i, u) x -> PP.hcat [
        prettyForTrace x
      , " selected ("
      , prettyForTrace s
      , ") but depends on "
      , prettyForTrace i
      , ", which is unavailable: "
      , prettyForTrace u
      ]
    SelectDeprecated x -> PP.hang
        "Selected a deprecated declaration: " 2 $ PP.vcat [
          prettyForTrace x
        , "You may want to de-select it"
        ]
    SelectParseSuccess x -> PP.hang "During parse:" 2 (prettyForTrace x)
    SelectParseNotAttempted x -> hangReason "parse not attempted" [
        prettyForTrace x
      , "Consider changing the parse predicate"
      ]
    SelectParseFailure x -> hangReason "parse failure" [
        prettyForTrace x
      ]
    SelectMacroFailure x -> hangReason "macro parse failure" [
        prettyForTrace x
      ]
    SelectNoDeclarationsMatched ->
      "No declarations matched the select predicate"
    where
      hangReason :: CtxDoc -> [CtxDoc] -> CtxDoc
      hangReason x xs =
        let header = "Could not select declaration (" >< x >< "):"
        in  PP.hang header 2 $ PP.vcat xs

instance IsTrace Level SelectMsg where
  getDefaultLogLevel = \case
    SelectStatusInfo{}                             -> Info
    TransitiveDependencyOfDeclarationUnavailable{} -> Warning
    SelectDeprecated{}                             -> Notice
    SelectParseSuccess x                           -> getDefaultLogLevel x
    SelectParseNotAttempted{}                      -> Warning
    SelectParseFailure x                           -> getDefaultLogLevel x
    SelectMacroFailure x                           -> getDefaultLogLevel x
    SelectNoDeclarationsMatched                    -> Warning
  getSource  = const HsBindgen
  getTraceId = \case
    SelectStatusInfo{}                             -> "select"
    TransitiveDependencyOfDeclarationUnavailable{} -> "select"
    SelectDeprecated{}                             -> "select"
    SelectParseSuccess x                           -> "select-" <> getTraceId x
    SelectParseNotAttempted{}                      -> "select-parse"
    SelectParseFailure x                           -> "select-" <> getTraceId x
    SelectMacroFailure x                           -> "select-" <> getTraceId x
    SelectNoDeclarationsMatched                    -> "select"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId ResolveBindingSpecs Select where
  coercePassId _ = coercePass

instance CoercePassTypedefRef ResolveBindingSpecs Select where
  coercePassTypedefRef _ = coercePass
