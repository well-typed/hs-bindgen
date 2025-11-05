module HsBindgen.Frontend.Pass.Select.IsPass (
    Select
    -- * Configuration
  , ProgramSlicing(..)
  , SelectConfig(..)
    -- * Trace messages
  , SelectReason(..)
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
    -- | Select declarations using the selection predicate /and/ their
    -- transitive dependencies.
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
    SelectionRoot        -> "selection root; direct select predicate match"
    TransitiveDependency -> "transitive dependency"

data SelectStatus =
    NotSelected
  | Selected SelectReason
  deriving stock (Show)

-- | Select trace messages
data SelectMsg =
    SelectSelectStatus SelectStatus (C.DeclInfo Select)
  | SelectUnavailableDeclaration C.QualPrelimDeclId
    -- | Inform the user that they select a deprecated declaration. Maybe they
    -- want to de-select deprecated declaration?
  | SelectDeprecated (C.DeclInfo Select)
    -- | Delayed parse message for actually selected declarations.
  | SelectParseSuccess AttachedParseMsg
    -- | Delayred parse message for declarations the user wants to select, but
    -- we have not attempted to parse.
  | SelectParseNotAttempted C.QualPrelimDeclId SingleLoc ParseNotAttemptedReason
    -- | Delayed parse message for declarations the user wants to select, but
    -- we have failed to parse.
  | SelectParseFailure AttachedParseMsg
  deriving stock (Show)

instance PrettyForTrace SelectMsg where
  prettyForTrace = \case
    SelectSelectStatus NotSelected info ->
      prettyForTrace info >< " not selected"
    SelectSelectStatus (Selected reason) info ->
      prettyForTrace info >< " selected (" >< prettyForTrace reason >< ")"
    SelectUnavailableDeclaration i -> PP.vcat [
        prettyForTrace i >< " selected but unavailable"
      , "This may be a bug and is an indication that declarations have been removed after parse."
      ]
    SelectDeprecated info -> PP.hcat [
        "Selected a deprecated declaration: "
      , prettyForTrace info
      , "; you may want to de-select it"
      ]
    SelectParseSuccess x ->
      "During parse:" <+> prettyForTrace x
    SelectParseNotAttempted n l r -> PP.vcat [
      "Failed to select declaration:" <+> prettyInfo n l
      , "Parse not attempted:" <+> prettyForTrace r
      ]
    SelectParseFailure x ->
      "Failed to select declaration; during parse:" <+> prettyForTrace x
    where
      prettyInfo :: C.QualPrelimDeclId -> SingleLoc -> CtxDoc
      prettyInfo n l = PP.hsep [
          prettyForTrace n
        , "at"
        , PP.showToCtxDoc l
        ] >< ":"

instance IsTrace Level SelectMsg where
  getDefaultLogLevel = \case
    SelectSelectStatus{}           -> Info
    SelectUnavailableDeclaration{} -> Warning
    SelectDeprecated{}             -> Notice
    SelectParseSuccess x           -> getDefaultLogLevel x
    SelectParseNotAttempted{}      -> Warning
    SelectParseFailure x           -> getDefaultLogLevel x
  getSource  = const HsBindgen
  getTraceId = \case
    SelectSelectStatus{}           -> "select"
    SelectUnavailableDeclaration{} -> "select"
    SelectDeprecated{}             -> "select"
    SelectParseSuccess x           -> "select-" <> getTraceId x
    SelectParseNotAttempted{}      -> "select-parse"
    SelectParseFailure x           -> "select-" <> getTraceId x

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePass TypedefRefWrapper ResolveBindingSpecs Select where
  coercePass (TypedefRefWrapper ref) = TypedefRefWrapper (coercePass ref)
