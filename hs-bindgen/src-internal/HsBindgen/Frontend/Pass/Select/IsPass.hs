module HsBindgen.Frontend.Pass.Select.IsPass (
    Select
  , SelectDeclMeta (..)
    -- * Configuration
  , ProgramSlicing (..)
  , SelectConfig (..)
    -- * Trace messages
  , SelectMsg (..)
  ) where

import Data.Default (Default (def))

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.Analysis.DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.NonParsedDecls
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass (ResolveBindingSpec,
                                                          ResolvedExtBinding)
import HsBindgen.Frontend.Predicate
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((><))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Select :: Pass
data Select a deriving anyclass ValidPass

type family AnnSelect ix where
  AnnSelect "TranslationUnit" = SelectDeclMeta
  AnnSelect "Decl"            = BindingSpec.TypeSpec
  AnnSelect _                 = NoAnn

instance IsPass Select where
  type Id           Select = C.DeclId
  type FieldName    Select = C.Name
  type ArgumentName Select = Maybe C.Name
  type TypedefRef   Select = C.Name
  -- NOTE Using @CheckedMacro Select@ is incompatible with 'CoercePass'
  type MacroBody    Select = CheckedMacro ResolveBindingSpec
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

data ProgramSlicing =
  -- | Enable program slicing: Select declarations using the selection predicate
  -- /and/ their transitive dependencies.
  EnableProgramSlicing
  | DisableProgramSlicing
  deriving stock (Show, Eq)

instance Default ProgramSlicing where
  def :: ProgramSlicing
  def = DisableProgramSlicing

data SelectConfig = SelectConfig {
      selectConfigProgramSlicing :: ProgramSlicing
    , selectConfigPredicate      :: SelectPredicate
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | Select trace messages
data SelectMsg =
    SelectTransitiveDependencyUnavailable C.NsPrelimDeclId
  | SelectExcluded (C.DeclInfo Select)
  | SelectSelected (C.DeclInfo Select)
  | SelectedButFailed ParseMsg
  -- TODO https://github.com/well-typed/hs-bindgen/issues/1037: Introduce
  -- `SelectedButSkipped`.
  deriving stock (Show)

instance PrettyForTrace SelectMsg where
  prettyForTrace = \case
    SelectTransitiveDependencyUnavailable qualId ->
      "Program slicing: Transitive dependency unavailable: " >< prettyForTrace qualId
    SelectExcluded info     -> prettyForTrace info >< " excluded"
    SelectSelected info     -> "Selected " >< prettyForTrace info
    SelectedButFailed  x -> "Missed declaration: " >< prettyForTrace x

instance IsTrace Level SelectMsg where
  getDefaultLogLevel = \case
    SelectTransitiveDependencyUnavailable{} -> Error
    SelectExcluded{}                        -> Info
    SelectSelected{}                        -> Info
    SelectedButFailed  x                 -> getDefaultLogLevel x
  getSource  = const HsBindgen
  getTraceId = \case
    SelectedButFailed  x -> "select-miss-" <> getTraceId x
    _else                   -> "select"
