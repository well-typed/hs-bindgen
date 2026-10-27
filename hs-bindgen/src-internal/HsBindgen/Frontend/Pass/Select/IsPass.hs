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
import HsBindgen.Frontend.DeclDB (Deactivation (..), Inactive,
                                  InactiveReason (..), SquashTarget (..),
                                  UnusableReason (..))
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.DelayedMsg
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass.PassBy (PassArgBy,
                                                             PassResBy)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.IR.Translation
import HsBindgen.Macro.Error (MacroLangFailure)
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
  AnnSelect "FunctionArg"          = PassArgBy Select
  AnnSelect "FunctionRes"          = PassResBy Select
  AnnSelect "IndirectField"        = IndirectFieldNames Select
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

instance PassTypes Select where
  type Types Select   = TranslatedTypes Select
  type TypeFunArg Select = TranslatedTypeFunArg Select
  type AnonRef Select = TranslatedAnonRef Select

  cType _ translatedTypes = translatedTypes.c
  cTypeFunArg _ translatedTypeFunArg = translatedTypeFunArg.c
  anonRefTypes _ = translatedAnonRefType

instance PassMacro Select where
  type MacroId Select = Id Select
  type MacroBody       Select = TypecheckedMacro Select
  type MacroUnderlying Select = C.Type Select

  macroIdId _ = id

instance PassExtBinding Select where
  type ExtBinding Select = BindingSpec.ResolvedExtBinding

  extBindingId _ extBinding = extDeclIdPair extBinding

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
    -- | Transitive dependency is unusable
    TransitiveDependencyUnusable C.DeclId Inactive
    -- | Transitive dependency is not selected.
  | TransitiveDependencyNotSelected C.DeclId [SingleLoc C.DeclPath]
  deriving stock (Show)

instance PrettyForTrace TransitiveDependencyMissing where
  prettyForTrace = \case
      TransitiveDependencyUnusable i u ->
        let intro = "Transitive dependency unusable:"
        in  PP.hang intro 2 $ prettyForTrace $ C.WithLocationInfo{
                loc = C.declIdLocationInfo i $
                  C.declLocsToList $ DeclDB.inactiveToLoc u
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
    -- | Delayed message of an earlier pass
  | SelectDelayedMsg DelayedMsg
    -- | A directly or transitively selected declaration is unusable.
  | SelectUnusable InactiveReason
  | SelectConflict
  | SelectMangleNamesSquashed SquashTarget
    -- | Inform the user that no declarations matched the selection predicate.
  | SelectNoDeclarationsMatched
    -- | Summary of the number of selected macros that hs-bindgen failed to
    -- translate.
  | SelectMacrosDropped Int
    -- | A selected macro falls back to its raw token spellings
  | SelectMacroFallback MacroLangFailure
    -- | Summary of the number of selected macros that fall back
  | SelectMacrosFallback Int
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
      SelectDelayedMsg x ->
        during x $ prettyForTrace x
      SelectUnusable reason -> case reason of
        NotAttempted x ->
          couldNotSelect $ PP.hang "Parse not attempted: " 2 $ prettyForTrace x
        Deactivated Omitted ->
          couldNotSelect $
            "Declaration omitted by prescriptive binding specifications"
        ParseFailed x ->
          couldNotSelect $ prettyForTrace x
        Deactivated (Unusable (UnusableMangleNames x)) ->
          couldNotSelect $ prettyForTrace x
        Deactivated (Unusable (UnusableMacro x)) ->
          couldNotSelect $ prettyForTrace x
      SelectConflict ->
        couldNotSelect "Conflicting declarations"
      SelectMangleNamesSquashed target -> PP.hsep [
          "Squashed typedef to"
        , prettyForTrace target.c
        ]
      SelectNoDeclarationsMatched ->
        "No declarations matched the selection predicate"
      SelectMacrosDropped n ->
        PP.string $
          show n
            <> (if n == 1 then " macro failed to translate; "
                          else " macros failed to translate; ")
            <> "use --log-enable-macro-warnings for details"
      SelectMacroFallback x ->
        PP.hang "Falling back to raw tokens:" 2 $ prettyForTrace x
      SelectMacrosFallback n ->
        PP.string $
          show n
            <> (if n == 1 then " macro falls back to raw tokens; "
                          else " macros fall back to raw tokens; ")
            <> "use --log-enable-macro-warnings for details"
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
    SelectDelayedMsg x                       -> getDefaultLogLevel x
    SelectUnusable r -> case r of
      NotAttempted{}           -> Warning
      Deactivated Omitted      -> Info
      ParseFailed x            -> getDefaultLogLevel x
      Deactivated (Unusable x) -> getDefaultLogLevel x
    SelectConflict{}                         -> Warning
    SelectMangleNamesSquashed{}              -> Notice
    SelectNoDeclarationsMatched              -> Warning
    SelectMacrosDropped{}                    -> Notice
    SelectMacroFallback{}                    -> Info
    SelectMacrosFallback{}                   -> Notice
  getSource  = const HsBindgen
  getTraceId = \case
    SelectStatusInfo{}                       -> "select"
    TransitiveDependenciesMissing{}          -> "select"
    SelectDeprecated{}                       -> "select"
    SelectDelayedMsg x                       -> "select-" <> getTraceId x
    SelectUnusable r -> case r of
      NotAttempted{}           -> "select-parse"
      Deactivated Omitted      -> "select-omitted"
      ParseFailed x            -> "select-" <> getTraceId x
      Deactivated (Unusable x) -> "select-" <> getTraceId x
    SelectConflict{}                         -> "select"
    SelectMangleNamesSquashed{}              -> "select-mangle-names-squashed"
    SelectNoDeclarationsMatched              -> "select"
    SelectMacrosDropped{}                    -> "select-dropped-macros"
    SelectMacroFallback{}                    -> "select-macro-fallback"
    SelectMacrosFallback{}                   -> "select-fallback-macros"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassAnonRef TranslateTypes Select where
  coercePassAnonRef _ = coercePass

instance CoercePassId      TranslateTypes Select

instance CoercePassTypes TranslateTypes Select where
  coercePassTypes _ = coercePass

instance CoercePassTypeFunArg TranslateTypes Select where
  coercePassTypeFunArg _ = coercePass

instance CoercePassMacroId TranslateTypes Select

instance CoercePassMacroUnderlying TranslateTypes Select where
  coercePassMacroUnderlying _ = coercePass

instance CoercePassMacroBody TranslateTypes Select where
  coercePassMacroBody _ = coercePassParam

instance CoercePassAnn "FunctionArg" TranslateTypes Select where
  coercePassAnn _ = coercePass

instance CoercePassAnn "FunctionRes" TranslateTypes Select where
  coercePassAnn _ = coercePass

instance CoercePassAnn "IndirectField" TranslateTypes Select where
  coercePassAnn _ = \case
      IndirectFieldNames name -> IndirectFieldNames name

instance CoercePassAnn "TypeFunArg" TranslateTypes Select where
  coercePassAnn _ = coercePass

instance CoercePassCommentDecl TranslateTypes Select where
  coercePassCommentDecl _ = fmap coercePass
