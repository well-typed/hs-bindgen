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

import Clang.HighLevel.Types

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Analysis.DeclIndex (Unusable (..))
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.Conflict
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.HandleMacros.Error (FailedMacro)
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Language.C qualified as C
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
  type FieldName    Select = C.ScopedName
  type ArgumentName Select = Maybe C.ScopedName
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

-- | Select trace messages
data SelectMsg =
    -- | Information about selection status; issued for all available
    --declarations.
    SelectStatusInfo (C.Decl Select) SelectStatus
    -- | The user has selected a declaration that is available but at least one
    -- of its transitive dependencies is 'Unusable'.
  | TransitiveDependencyOfDeclarationUnusable
      (C.Decl Select)
      SelectReason
      C.PrelimDeclId
      Unusable
      [SingleLoc]
    -- | The user has selected a declaration that is available but at least one
    -- of its transitive dependencies is not selected.
  | TransitiveDependencyOfDeclarationNotSelected
      (C.Decl Select)
      SelectReason
      C.PrelimDeclId
      [SingleLoc]
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
    -- | Delayed construct translation unit message for conflicting declarations
    -- the user wants to select directly.
  | SelectConflict ConflictingDeclarations
    -- | Delayed handle macros message for macros the user wants to select
    -- directly, but we have failed to parse.
  | SelectMacroFailure FailedMacro
    -- | Inform the user that no declarations matched the select predicate.
  | SelectNoDeclarationsMatched
  deriving stock (Show)

instance PrettyForTrace SelectMsg where
  prettyForTrace = \case
    SelectStatusInfo x NotSelected ->
      prettyForTrace x >< " not selected"
    SelectStatusInfo x (Selected r) ->
      prettyForTrace x >< " selected (" >< prettyForTrace r >< ")"
    TransitiveDependencyOfDeclarationUnusable x s i r ls ->
      let intro = PP.hcat [
              prettyForTrace x
            , " (" >< prettyForTrace s >< ")"
            , " because a transitive dependency is unusable:"
            ]
      in  hangWith $ PP.hang intro 2 $ prettyDep i ls >< ": " >< prettyForTrace r
    TransitiveDependencyOfDeclarationNotSelected x s i ls ->
      let intro = PP.hcat [
              prettyForTrace x
            , " (" >< prettyForTrace s >< ")"
            , " because a transitive dependency is not selected:"
            ]
          outro = PP.vcat [
              prettyDep i ls
            , "Consider adjusting the select predicate"
            ]
      in  hangWith $ PP.hang intro 2 outro
    SelectDeprecated x -> PP.hang
        "Selected a deprecated declaration: " 2 $ PP.vcat [
          prettyForTrace x
        , "You may want to de-select it"
        ]
    SelectParseSuccess x -> PP.hang "During parse:" 2 (prettyForTrace x)
    SelectParseNotAttempted x -> hangWith $ PP.vcat [
        prettyForTrace x
      , "Consider changing the parse predicate"
      ]
    SelectParseFailure x -> hangWith $ prettyForTrace x
    SelectConflict     x -> hangWith $ prettyForTrace x
    SelectMacroFailure x -> hangWith $ prettyForTrace x
    SelectNoDeclarationsMatched ->
      "No declarations matched the select predicate"
    where
      hangWith :: CtxDoc -> CtxDoc
      hangWith x = PP.hang "Could not select declaration:" 2 x

      prettyDep :: C.PrelimDeclId -> [SingleLoc] -> CtxDoc
      prettyDep i = \case
        []  -> prettyForTrace i >< " (no source location available)"
        [l] -> prettyForTrace (C.Located l i)
        ls  -> prettyForTrace i <+> PP.hlist '(' ')' (map PP.showToCtxDoc ls)

instance IsTrace Level SelectMsg where
  getDefaultLogLevel = \case
    SelectStatusInfo{}                             -> Info
    TransitiveDependencyOfDeclarationUnusable{}    -> Warning
    TransitiveDependencyOfDeclarationNotSelected{} -> Warning
    SelectDeprecated{}                             -> Notice
    SelectParseSuccess x                           -> getDefaultLogLevel x
    SelectParseNotAttempted{}                      -> Warning
    SelectParseFailure x                           -> getDefaultLogLevel x
    SelectConflict{}                               -> Warning
    SelectMacroFailure x                           -> getDefaultLogLevel x
    SelectNoDeclarationsMatched                    -> Warning
  getSource  = const HsBindgen
  getTraceId = \case
    SelectStatusInfo{}                             -> "select"
    TransitiveDependencyOfDeclarationUnusable{}    -> "select"
    TransitiveDependencyOfDeclarationNotSelected{} -> "select"
    SelectDeprecated{}                             -> "select"
    SelectParseSuccess x                           -> "select-" <> getTraceId x
    SelectParseNotAttempted{}                      -> "select-parse"
    SelectParseFailure x                           -> "select-" <> getTraceId x
    SelectConflict{}                               -> "select"
    SelectMacroFailure x                           -> "select-" <> getTraceId x
    SelectNoDeclarationsMatched                    -> "select"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId ResolveBindingSpecs Select where
  coercePassId _ = coercePass

instance CoercePassHaskellId ResolveBindingSpecs Select where
  coercePassHaskellId _ = id
