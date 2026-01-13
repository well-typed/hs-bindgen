module HsBindgen.Frontend.Pass.Select.IsPass (
    Select
    -- * Configuration
  , ProgramSlicing(..)
  , SelectConfig(..)
    -- * Trace messages
  , SelectReason(..)
  , SelectStatus(..)
  , TransitiveDependencyMissing(..)
  , SelectMsg(..)
  ) where

import Data.Default (Default (def))
import Text.SimplePrettyPrint (CtxDoc, (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types

import HsBindgen.Frontend.Analysis.DeclIndex (Unusable (..))
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.Error
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Select :: Pass
data Select a

type family AnnSelect ix where
  AnnSelect "TranslationUnit" = DeclMeta
  AnnSelect "Decl"            = PrescriptiveDeclSpec
  AnnSelect _                 = NoAnn

instance IsPass Select where
  type MacroBody  Select = CheckedMacro Select
  type ExtBinding Select = ResolvedExtBinding
  type Ann ix     Select = AnnSelect ix
  type Msg        Select = WithLocationInfo SelectMsg
  type MacroId    Select = Id Select

  extBindingId _ = (.cName)
  macroIdId _ = id

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
      programSlicing  :: ProgramSlicing
    , parsePredicate  :: Boolean ParsePredicate
    , selectPredicate :: Boolean SelectPredicate
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

data TransitiveDependencyMissing =
    -- | Transitive dependency is 'Unusable'.
    TransitiveDependencyUnusable DeclId Unusable [SingleLoc]
    -- | Transitive dependency is not selected.
  | TransitiveDependencyNotSelected DeclId [SingleLoc]
  deriving stock (Show)

instance PrettyForTrace TransitiveDependencyMissing where
  prettyForTrace = \case
      TransitiveDependencyUnusable i r ls ->
        let intro = "Transitive dependency unusable:"
        in  PP.hang intro 2 $ prettyForTrace $ WithLocationInfo{
                loc = declIdLocationInfo i ls
              , msg = r
              }
      TransitiveDependencyNotSelected i ls ->
        let intro = "Transitive dependency not selected:"
        in  PP.hang intro 2 $ PP.vcat [
                prettyForTrace $ declIdLocationInfo i ls
              , "Consider adjusting the select predicate"
              ]

-- | Select trace messages
data SelectMsg =
    -- | Information about selection status; issued for all available
    --declarations.
    SelectStatusInfo SelectStatus
    -- | The user has selected a declaration that is available but has missing
    -- transitive dependencies.
  | TransitiveDependenciesMissing SelectReason [TransitiveDependencyMissing]
    -- | The user has selected a deprecated declaration. Maybe they want to
    -- de-select the deprecated declaration?
  | SelectDeprecated SelectReason
    -- | Delayed parse message for actually selected declarations.
  | SelectParseSuccess DelayedParseMsg
    -- | Delayed parse message for declarations the user wants to select
    -- directly, but we have not attempted to parse.
  | SelectParseNotAttempted ParseNotAttempted
    -- | Delayed parse message for declarations the user wants to select
    -- directly, but we have failed to parse.
  | SelectParseFailure ParseFailure
    -- | Delayed construct translation unit message for conflicting declarations
    -- the user wants to select directly.
  | SelectConflict
    -- | Delayed handle macros message for macros the user wants to select
    -- directly, but we have failed to parse.
  | SelectMacroFailure HandleMacrosError
    -- | Inform the user that no declarations matched the select predicate.
  | SelectNoDeclarationsMatched
  deriving stock (Show)

instance PrettyForTrace SelectMsg where
  prettyForTrace = \case
      SelectStatusInfo NotSelected ->
        "Not selected"
      SelectStatusInfo (Selected r) ->
        "Selected (" >< prettyForTrace r >< ")"
      TransitiveDependenciesMissing s xs ->
        couldNotSelectWithReason s $
          PP.vcat $ map prettyForTrace xs
      SelectDeprecated r ->
        appendSelectReason r "Selected a deprecated declaration"
      SelectParseSuccess x ->
        PP.hang "During parse:" 2 (prettyForTrace x)
      SelectParseNotAttempted x ->
        couldNotSelect $ PP.vcat [
            prettyForTrace x
          , "Consider changing the parse predicate"
          ]
      SelectParseFailure x ->
        couldNotSelect $ prettyForTrace x
      SelectConflict ->
        couldNotSelect $ "conflicting declarations"
      SelectMacroFailure x ->
        couldNotSelect $ prettyForTrace x
      SelectNoDeclarationsMatched ->
        "No declarations matched the select predicate"
    where
      couldNotSelect :: CtxDoc -> CtxDoc
      couldNotSelect x = PP.hang "Could not select declaration:" 2 x

      appendSelectReason :: SelectReason -> CtxDoc -> CtxDoc
      appendSelectReason r x = x <+> "(" >< prettyForTrace r >< ")"

      couldNotSelectWithReason :: SelectReason -> CtxDoc -> CtxDoc
      couldNotSelectWithReason r x =
        let intro = appendSelectReason r "Could not select declaration" >< ":"
        in  PP.hang intro 2 x

instance IsTrace Level SelectMsg where
  getDefaultLogLevel = \case
    SelectStatusInfo{}              -> Info
    TransitiveDependenciesMissing{} -> Warning
    SelectDeprecated{}              -> Notice
    SelectParseSuccess x            -> getDefaultLogLevel x
    SelectParseNotAttempted{}       -> Warning
    SelectParseFailure x            -> getDefaultLogLevel x
    SelectConflict{}                -> Warning
    SelectMacroFailure x            -> getDefaultLogLevel x
    SelectNoDeclarationsMatched     -> Warning
  getSource  = const HsBindgen
  getTraceId = \case
    SelectStatusInfo{}              -> "select"
    TransitiveDependenciesMissing{} -> "select"
    SelectDeprecated{}              -> "select"
    SelectParseSuccess x            -> "select-" <> getTraceId x
    SelectParseNotAttempted{}       -> "select-parse"
    SelectParseFailure x            -> "select-" <> getTraceId x
    SelectConflict{}                -> "select"
    SelectMacroFailure x            -> "select-" <> getTraceId x
    SelectNoDeclarationsMatched     -> "select"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId ResolveBindingSpecs Select
instance CoercePassMacroId ResolveBindingSpecs Select

instance CoercePassMacroBody ResolveBindingSpecs Select where
  coercePassMacroBody _ = coercePass
