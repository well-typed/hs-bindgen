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
import Text.SimplePrettyPrint (CtxDoc, (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types (SingleLoc)

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
    UnavailableParseNotAttempted
  | UnavailableParseFailed
  deriving stock (Show)

instance PrettyForTrace UnavailabilityReason where
  prettyForTrace r = "unavailable" <+> case r of
    UnavailableParseNotAttempted ->
      "(parse of one or more transitive dependencies not attempted; (!) adjust parse predicate)"
    UnavailableParseFailed ->
      "(parse of one or more transitive dependencies failed)"

-- | Select trace messages
data SelectMsg
  = -- | Information about selection status; issued for all available
    --declarations.
    SelectStatusInfo SelectStatus (C.Decl Select)
  | -- | The user has selected a declaration that is available but at least one
    --   of its transitive dependencies is _unavailable_.
    TransitiveDependencyOfDeclarationUnavailable SelectReason UnavailabilityReason (C.Decl Select)
  | -- | A transitive dependency itself is unavailable. The declaration may be
    --   unavailable because we did not attempt to parse it, or it has failed to
    --   parse.
    TransitiveDependencyUnavailable C.QualPrelimDeclId
  | -- | The user has selected a deprecated declaration. Maybe they want to
    -- de-select deprecated declaration?
    SelectDeprecated (C.Decl Select)
  | -- | Delayed parse message for actually selected declarations.
    SelectParseSuccess AttachedParseMsg
  | -- | Delayed parse message for declarations the user directly wants to
    -- select (i.e., not a transitive dependency), but we have not attempted to
    -- parse.
    SelectParseNotAttempted C.QualPrelimDeclId SingleLoc ParseNotAttemptedReason
  | -- | Delayed parse message for declarations the user directly wants to
    -- select (i.e., not a transitive dependency), but we have failed to parse.
    SelectParseFailure AttachedParseMsg
  deriving stock (Show)

instance PrettyForTrace SelectMsg where
  prettyForTrace = \case
    SelectStatusInfo NotSelected x ->
      prettyForTrace x >< "not selected"
    SelectStatusInfo (Selected r) x ->
      prettyForTrace x >< "selected (" >< prettyForTrace r >< ")"
    TransitiveDependencyOfDeclarationUnavailable s u x -> PP.hcat [
        prettyForTrace x
      , " selected ("
      , prettyForTrace s
      , ") but "
      , prettyForTrace u
      ]
    TransitiveDependencyUnavailable i ->
        "Unavailable transitive dependency: " >< prettyForTrace i
    SelectDeprecated x -> PP.hcat [
        "Selected a deprecated declaration: "
      , prettyForTrace x
      , "; you may want to de-select it"
      ]
    SelectParseSuccess x ->
      "During parse:" <+> prettyForTrace x
    SelectParseNotAttempted n l r -> PP.vcat [
      "Could not select declaration:" <+> prettyInfo n l
      , "Parse not attempted:" <+> prettyForTrace r
      , "Consider changing the parse predicate"
      ]
    SelectParseFailure x ->
      "Could not select declaration; parse failure:" <+> prettyForTrace x
    where
      prettyInfo :: C.QualPrelimDeclId -> SingleLoc -> CtxDoc
      prettyInfo n l = PP.hsep [
          prettyForTrace n
        , "at"
        , PP.showToCtxDoc l
        ] >< ":"

instance IsTrace Level SelectMsg where
  getDefaultLogLevel = \case
    SelectStatusInfo{}                             -> Info
    TransitiveDependencyOfDeclarationUnavailable{} -> Warning
    TransitiveDependencyUnavailable{}              -> Warning
    SelectDeprecated{}                             -> Notice
    SelectParseSuccess x                           -> getDefaultLogLevel x
    SelectParseNotAttempted{}                      -> Warning
    SelectParseFailure x                           -> getDefaultLogLevel x
  getSource  = const HsBindgen
  getTraceId = \case
    SelectStatusInfo{}                             -> "select"
    TransitiveDependencyOfDeclarationUnavailable{} -> "select"
    TransitiveDependencyUnavailable{}              -> "select"
    SelectDeprecated{}                             -> "select"
    SelectParseSuccess x                           -> "select-" <> getTraceId x
    SelectParseNotAttempted{}                      -> "select-parse"
    SelectParseFailure x                           -> "select-" <> getTraceId x

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePass TypedefRefWrapper ResolveBindingSpecs Select where
  coercePass (TypedefRefWrapper ref) = TypedefRefWrapper (coercePass ref)
