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

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Analysis.DeclIndex (Squashed (..), UnusableEntry,
                                              UnusableReason (..))
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg (DelayedPrepareReparseMsg)
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.Msg (DelayedReparseMacroExpansionsMsg)
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Select :: Pass
data Select a

type family AnnSelect ix where
  AnnSelect "Decl"                 = PrescriptiveDeclSpec
  AnnSelect "Enum"                 = NewtypeNames
  AnnSelect "Flam"                 = FlamNames
  AnnSelect "Struct"               = StructNames
  AnnSelect "Typedef"              = TypedefNames
  AnnSelect "TypecheckedMacroType" = NewtypeNames
  AnnSelect "TypeFunArg"           = AdjustedFrom Select
  AnnSelect "Union"                = NewtypeNames
  AnnSelect _                      = NoAnn

instance IsPass Select

instance PassId Select where
  type Id Select = DeclIdPair

  idNameKind     _ namePair = namePair.cName.name.kind
  idSourceName   _ namePair = C.declIdSourceName namePair.cName
  idLocationInfo _ namePair = C.declIdLocationInfo namePair.cName

instance PassScopedName Select where
  type ScopedName Select = ScopedNamePair

instance PassMacro Select where
  type MacroId Select = Id Select
  type MacroBody       Select = TypecheckedMacro Select
  type MacroUnderlying Select = C.Type Select

  macroIdId _ = id

instance PassExtBinding Select where
  type ExtBinding Select = BindingSpec.ResolvedExtBinding

  extBindingId _ extBinding = BindingSpec.extDeclIdPair extBinding

instance PassCommentDecl Select where
  type CommentDecl Select = Maybe (C.Comment Select)

instance PassAnn Select where
  type Ann ix Select = AnnSelect ix

instance PassMsg Select where
  type Msg Select = C.WithLocationInfo SelectMsg

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Select transitive dependencies?
data ProgramSlicing =
    -- | Select declarations using the selection predicate /and/ their transitive
    -- dependencies.
    EnableProgramSlicing
  | DisableProgramSlicing
  deriving stock (Show, Eq)

instance Default ProgramSlicing where
  def :: ProgramSlicing
  def = DisableProgramSlicing

data SelectConfig = SelectConfig {
      programSlicing     :: ProgramSlicing
    , selectionPredicate :: Boolean SelectionPredicate
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
    SelectionRoot        -> "direct selection predicate match"
    TransitiveDependency -> "transitive dependency"

data SelectStatus =
    NotSelected
  | Selected SelectReason
  deriving stock (Show)

data TransitiveDependencyMissing =
    -- | Transitive dependency is 'Unusable'.
    TransitiveDependencyUnusable C.DeclId UnusableEntry
    -- | Transitive dependency is not selected.
  | TransitiveDependencyNotSelected C.DeclId [SingleLoc]
  deriving stock (Show)

instance PrettyForTrace TransitiveDependencyMissing where
  prettyForTrace = \case
      TransitiveDependencyUnusable i u ->
        let intro = "Transitive dependency unusable:"
        in  PP.hang intro 2 $ prettyForTrace $ C.WithLocationInfo{
                loc = C.declIdLocationInfo i $
                  C.declLocsToList $ DeclIndex.unusableToLoc u
              , msg = u
              }
      TransitiveDependencyNotSelected i ls ->
        let intro = "Transitive dependency not selected:"
        in  PP.hang intro 2 $ PP.vcat [
                prettyForTrace $ C.declIdLocationInfo i ls
              , "Adjust the selection predicate or enable program slicing"
              ]

-- | Select trace messages
data SelectMsg =
    -- | Information about selection status; issued for all available
    -- declarations.
    SelectStatusInfo SelectStatus
    -- | The user has selected a declaration that is available but has missing
    -- transitive dependencies.
  | TransitiveDependenciesMissing SelectReason [TransitiveDependencyMissing]
    -- | The user has selected a deprecated declaration. Maybe they want to
    -- de-select the deprecated declaration?
  | SelectDeprecated SelectReason
    -- | Delayed parse message.
  | SelectDelayedParseMsg DelayedParseMsg
    -- | A directly or transitively selected declaration is unusable.
  | SelectUnusable UnusableReason
  | SelectConflict
  | SelectMangleNamesSquashed Squashed
    -- | Delayed @PrepareReparse@ message
  | SelectDelayedPrepareReparseMsg DelayedPrepareReparseMsg
    -- | Delayed @ReparseMacroExpansions@ message
  | SelectDelayedReparseMacroExpansionsMsg DelayedReparseMacroExpansionsMsg
    -- | Inform the user that no declarations matched the selection predicate.
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
        withSelectReason r "Selected a deprecated declaration"
      SelectDelayedParseMsg x ->
        during x $ prettyForTrace x
      SelectUnusable reason -> case reason of
        UnusableUnavailable ->
          couldNotSelect $ PP.hang "Parse not attempted: " 2
            "Declaration is 'unavailable' on this platform"
        UnusableOmitted ->
          couldNotSelect $
            "Declaration omitted by prescriptive binding specifications"
        UnusableParseFailure x ->
          couldNotSelect $ prettyForTrace x
        UnusableMangleNamesFailure x ->
          couldNotSelect $ prettyForTrace x
        UnusableMacroTypecheckFailure x ->
          couldNotSelect $ prettyForTrace x
        UnusableMacroResolutionFailure x ->
          couldNotSelect $ prettyForTrace x
      SelectConflict ->
        couldNotSelect "Conflicting declarations"
      SelectMangleNamesSquashed x -> PP.hsep [
          "Squashed typedef to"
        , prettyForTrace x.targetNameC
        ]
      SelectDelayedPrepareReparseMsg x ->
        during x $ prettyForTrace x
      SelectDelayedReparseMacroExpansionsMsg x ->
        during x $ prettyForTrace x
      SelectNoDeclarationsMatched ->
        "No declarations matched the selection predicate"
    where
      during :: IsTrace l e => e -> CtxDoc -> CtxDoc
      during x = PP.hang (PP.string ("During " <> (getTraceId x).id <> ":")) 2

      couldNotSelectStr :: CtxDoc
      couldNotSelectStr = "Could not select declaration"

      couldNotSelect :: CtxDoc -> CtxDoc
      couldNotSelect x = PP.hang (couldNotSelectStr >< ":") 2 x

      withSelectReason :: SelectReason -> CtxDoc -> CtxDoc
      withSelectReason r x = x <+> "(" >< prettyForTrace r >< ")"

      couldNotSelectWithReason :: SelectReason -> CtxDoc -> CtxDoc
      couldNotSelectWithReason r x =
        let intro = withSelectReason r couldNotSelectStr >< ":"
        in  PP.hang intro 2 x

instance IsTrace Level SelectMsg where
  getDefaultLogLevel = \case
    SelectStatusInfo{}                       -> Info
    TransitiveDependenciesMissing{}          -> Warning
    SelectDeprecated{}                       -> Notice
    SelectDelayedParseMsg x                  -> getDefaultLogLevel x
    SelectUnusable r -> case r of
      UnusableUnavailable              -> Warning
      UnusableOmitted                  -> Info
      UnusableParseFailure x           -> getDefaultLogLevel x
      UnusableMangleNamesFailure x     -> getDefaultLogLevel x
      UnusableMacroTypecheckFailure x  -> getDefaultLogLevel x
      UnusableMacroResolutionFailure x -> getDefaultLogLevel x
    SelectConflict{}                         -> Warning
    SelectMangleNamesSquashed{}              -> Notice
    SelectDelayedPrepareReparseMsg x         -> getDefaultLogLevel x
    SelectDelayedReparseMacroExpansionsMsg x -> getDefaultLogLevel x
    SelectNoDeclarationsMatched              -> Warning
  getSource  = const HsBindgen
  getTraceId = \case
    SelectStatusInfo{}                       -> "select"
    TransitiveDependenciesMissing{}          -> "select"
    SelectDeprecated{}                       -> "select"
    SelectDelayedParseMsg x                  -> "select-" <> getTraceId x
    SelectUnusable r -> case r of
      UnusableUnavailable              -> "select-parse"
      UnusableOmitted                  -> "select-omitted"
      UnusableParseFailure           x -> "select-" <> getTraceId x
      UnusableMangleNamesFailure     x -> "select-" <> getTraceId x
      UnusableMacroTypecheckFailure  x -> "select-" <> getTraceId x
      UnusableMacroResolutionFailure x -> "select-" <> getTraceId x
    SelectConflict{}                         -> "select"
    SelectMangleNamesSquashed{}              -> "select-mangle-names-squashed"
    SelectDelayedPrepareReparseMsg x         -> "select-" <> getTraceId x
    SelectDelayedReparseMacroExpansionsMsg x -> "select-" <> getTraceId x
    SelectNoDeclarationsMatched              -> "select"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId        AdjustTypes Select
instance CoercePassMacroId   AdjustTypes Select
instance CoercePassMacroUnderlying AdjustTypes Select where
  coercePassMacroUnderlying _ = coercePass

instance CoercePassMacroBody AdjustTypes Select where
  coercePassMacroBody _ = coercePassParam

instance CoercePassAnn "TypeFunArg" AdjustTypes Select where
  coercePassAnn _ = \case
      AdjustedFromArray ty    -> AdjustedFromArray (coercePass ty)
      AdjustedFromFunction ty -> AdjustedFromFunction (coercePass ty)
      NotAdjusted             -> NotAdjusted

instance CoercePassCommentDecl AdjustTypes Select where
  coercePassCommentDecl _ = fmap coercePass
