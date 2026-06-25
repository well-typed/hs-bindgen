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
import HsBindgen.Frontend.Analysis.DeclIndex (Squashed (..), Unusable (..))
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.MangleNames.Error (MangleNamesError)
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg (DelayedPrepareReparseMsg)
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation
import HsBindgen.Macro.Error
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Select :: Pass
data Select a

type family AnnSelect ix where
  AnnSelect "Decl"                 = PrescriptiveDeclSpec
  AnnSelect "Struct"               = StructNames
  AnnSelect "Union"                = NewtypeNames
  AnnSelect "Enum"                 = NewtypeNames
  AnnSelect "Typedef"              = TypedefNames
  AnnSelect "TypecheckedMacroType" = NewtypeNames
  AnnSelect "TypeFunArg"           = AdjustedFrom Select
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
    TransitiveDependencyUnusable C.DeclId Unusable [SingleLoc]
    -- | Transitive dependency is not selected.
  | TransitiveDependencyNotSelected C.DeclId [SingleLoc]
  deriving stock (Show)

instance PrettyForTrace TransitiveDependencyMissing where
  prettyForTrace = \case
      TransitiveDependencyUnusable i r ls ->
        let intro = "Transitive dependency unusable:"
        in  PP.hang intro 2 $ prettyForTrace $ C.WithLocationInfo{
                loc = C.declIdLocationInfo i ls
              , msg = r
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
    -- | Delayed parse message for declarations the user wants to select
    -- directly, but we have not attempted to parse.
  | SelectParseNotAttempted ParseNotAttempted
    -- | Delayed parse message for declarations the user wants to select
    -- directly, but we have failed to parse.
  | SelectParseFailure DelayedParseMsg
    -- | Delayed construct translation unit message for conflicting declarations
    -- the user wants to select directly.
  | SelectConflict
  | SelectMangleNamesFailure MangleNamesError
  | SelectMangleNamesSquashed Squashed
    -- | Delayed handle macros message for macros the user wants to select
    -- directly, but we have failed to parse.
  | SelectMacroTypecheckFailure MacroTypecheckError
    -- | Delayed macro name-resolution failure for macros the user wants to
    -- select directly.
  | SelectMacroResolutionFailure MacroResolutionError
    -- | Delayed @PrepareReparse@ message
  | SelectDelayedPrepareReparseMsg DelayedPrepareReparseMsg
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
        PP.hang "During parse:" 2 (prettyForTrace x)
      SelectParseNotAttempted x ->
        couldNotSelect $ prettyForTrace x
      SelectParseFailure x ->
        couldNotSelect $ prettyForTrace x
      SelectConflict ->
        couldNotSelect "Conflicting declarations"
      SelectMangleNamesFailure x ->
        couldNotSelect $ prettyForTrace x
      SelectMangleNamesSquashed x -> PP.hsep [
          "Squashed typedef to"
        , prettyForTrace x.targetNameC
        ]
      SelectMacroTypecheckFailure x ->
        couldNotSelect $ prettyForTrace x
      SelectMacroResolutionFailure x ->
        couldNotSelect $ prettyForTrace x
      SelectDelayedPrepareReparseMsg x ->
        PP.hang "During prepare-reparse:" 2 (prettyForTrace x)
      SelectNoDeclarationsMatched ->
        "No declarations matched the selection predicate"
    where
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
    SelectStatusInfo{}               -> Info
    TransitiveDependenciesMissing{}  -> Warning
    SelectDeprecated{}               -> Notice
    SelectDelayedParseMsg x          -> getDefaultLogLevel x
    SelectParseNotAttempted{}        -> Warning
    SelectParseFailure x             -> getDefaultLogLevel x
    SelectConflict{}                 -> Warning
    SelectMangleNamesFailure{}       -> Warning
    SelectMangleNamesSquashed{}      -> Notice
    SelectMacroTypecheckFailure x    -> getDefaultLogLevel x
    SelectMacroResolutionFailure x   -> getDefaultLogLevel x
    SelectDelayedPrepareReparseMsg x -> getDefaultLogLevel x
    SelectNoDeclarationsMatched      -> Warning
  getSource  = const HsBindgen
  getTraceId = \case
    SelectStatusInfo{}               -> "select"
    TransitiveDependenciesMissing{}  -> "select"
    SelectDeprecated{}               -> "select"
    SelectDelayedParseMsg x          -> "select-" <> getTraceId x
    SelectParseNotAttempted{}        -> "select-parse"
    SelectParseFailure x             -> "select-" <> getTraceId x
    SelectConflict{}                 -> "select"
    SelectMangleNamesFailure{}       -> "select-mangle-names-failure"
    SelectMangleNamesSquashed{}      -> "select-mangle-names-squashed"
    SelectMacroTypecheckFailure x    -> "select-" <> getTraceId x
    SelectMacroResolutionFailure x   -> "select-" <> getTraceId x
    SelectDelayedPrepareReparseMsg x -> "select-" <> getTraceId x
    SelectNoDeclarationsMatched      -> "select"

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
