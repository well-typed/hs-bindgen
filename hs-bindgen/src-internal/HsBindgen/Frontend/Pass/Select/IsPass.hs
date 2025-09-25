module HsBindgen.Frontend.Pass.Select.IsPass (
    Select
  , SelectDeclMeta(..)
    -- * Configuration
  , ProgramSlicing(..)
  , SelectConfig(..)
    -- * Trace messages
  , SelectReason(..)
  , SelectMsg(..)
  ) where

import Data.Default (Default (def))
import Text.SimplePrettyPrint (CtxDoc, (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph
import HsBindgen.Frontend.AST.Coerce (CoercePass (coercePass))
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.NonParsedDecls
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (ResolveBindingSpecs,
                                                           ResolvedExtBinding)
import HsBindgen.Frontend.Predicate
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Select :: Pass
data Select a deriving anyclass ValidPass

type family AnnSelect ix where
  AnnSelect "TranslationUnit" = SelectDeclMeta
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
  Information about the declarations
-------------------------------------------------------------------------------}

-- TODO https://github.com/well-typed/hs-bindgen/issues/1038: Remove and thread
-- through parse messages until the end. However, we have trouble mangling names
-- due to (expected) missing declarations.
data SelectDeclMeta = SelectDeclMeta {
      selectDeclIndex     :: DeclIndex
    , selectDeclUseDecl   :: UseDeclGraph
    , selectDeclDeclUse   :: DeclUseGraph
    , selectDeclNonParsed :: NonParsedDecls
    }
  deriving stock (Show)

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
    SelectionRoot        -> "selection root"
    TransitiveDependency -> "transitive dependency"

-- | Select trace messages
data SelectMsg =
    SelectNotSelected (C.DeclInfo Select)
  | SelectSelected SelectReason (C.DeclInfo Select)
    -- | Delayed parse message for actually selected declarations.
  | SelectParse  (ParseMsgKey Select) DelayedParseMsg
    -- | Delayed parse message for declarations that the user wanted to select,
    -- but we failed to parse.
  | SelectFailed (ParseMsgKey Select) DelayedParseMsg
  -- TODO https://github.com/well-typed/hs-bindgen/issues/1037: Introduce
  -- `SelectedButSkipped`.
  deriving stock (Show)

instance PrettyForTrace SelectMsg where
  prettyForTrace = \case
    SelectNotSelected info ->
      prettyForTrace info >< " not selected"
    SelectSelected reason info ->
      prettyForTrace info >< " selected (" >< prettyForTrace reason >< ")"
    SelectParse  k x -> "During parse:" <+> prettyDelayedParseMsg k x
    SelectFailed k x -> "Failed to select declaration declaration:" <+> prettyDelayedParseMsg k x
    where
      prettyDelayedParseMsg :: ParseMsgKey Select -> DelayedParseMsg -> CtxDoc
      prettyDelayedParseMsg k v = PP.hcat [
          prettyForTrace k
        , ":"
        , prettyForTrace v
        ]

instance IsTrace Level SelectMsg where
  getDefaultLogLevel = \case
    SelectNotSelected{} -> Info
    SelectSelected{}    -> Info
    SelectParse  _ x    -> getDefaultLogLevel x
    SelectFailed _ x    -> getDefaultLogLevel x
  getSource  = const HsBindgen
  getTraceId = \case
    SelectParse  _ x -> "select-parse-"   <> getTraceId x
    SelectFailed _ x -> "select-missed-"  <> getTraceId x
    _else            -> "select"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePass TypedefRefWrapper ResolveBindingSpecs Select where
  coercePass (TypedefRefWrapper ref) = TypedefRefWrapper (coercePass ref)
