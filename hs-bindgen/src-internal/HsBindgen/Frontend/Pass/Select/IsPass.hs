module HsBindgen.Frontend.Pass.Select.IsPass (
    Select
    -- * Configuration
  , ProgramSlicing (..)
  , SelectConfig (..)
    -- * Trace messages
  , SelectMsg (..)
  , SelectReason (..)
  ) where

import Data.Default (Default (def))
import Data.Set (Set)
import Data.Set qualified as Set

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.C.Predicate (SelectPredicate)
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass (
    ResolveBindingSpec, ResolvedExtBinding)
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta)
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((><))
import Text.SimplePrettyPrint qualified as PP

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Select :: Pass
data Select a deriving anyclass ValidPass

type family AnnSelect ix where
  AnnSelect "Decl"            = BindingSpec.TypeSpec
  AnnSelect "TranslationUnit" = DeclMeta
  AnnSelect _                 = NoAnn

instance IsPass Select where
  type Id         Select = C.DeclId
  type FieldName  Select = C.Name
  type TypedefRef Select = C.Name
  -- NOTE Using @CheckedMacro Select@ is incompatible with 'CoercePass'
  type MacroBody  Select = CheckedMacro ResolveBindingSpec
  type ExtBinding Select = ResolvedExtBinding
  type Ann ix     Select = AnnSelect ix
  type Config     Select = SelectConfig
  type Msg        Select = SelectMsg

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
  | SelectSelected SelectReason
  deriving stock (Show, Eq)

data SelectReason =
    TransitiveDependencyOf {
      selectedDecl           :: C.NsPrelimDeclId
      -- NOTE: The inverse dependencies form tree. For now, we just flatten the
      -- tree to list all inverse dependencies.
    , transitiveDependencyOf :: Set C.NsPrelimDeclId
    }
  deriving stock (Show, Eq)

instance PrettyForTrace SelectReason where
  prettyForTrace (TransitiveDependencyOf sel deps) =
    PP.hang
      ("Selected " >< prettyForTrace sel)
      2
      (PP.hangs' "because it is a transitive dependency of" 2 $
         map prettyForTrace $ Set.toList deps)

instance HasDefaultLogLevel SelectReason where
  getDefaultLogLevel _ = Info

instance PrettyForTrace SelectMsg where
  prettyForTrace = \case
    SelectTransitiveDependencyUnavailable qualId ->
      "Program slicing: Transitive dependency unavailable: " >< prettyForTrace qualId
    SelectExcluded info   -> prettyForTrace info >< " excluded"
    SelectSelected reason -> prettyForTrace reason

instance HasDefaultLogLevel SelectMsg where
  getDefaultLogLevel = \case
    SelectTransitiveDependencyUnavailable{} -> Error
    SelectExcluded{}                        -> Info
    SelectSelected reason                   -> getDefaultLogLevel reason

instance HasSource SelectMsg where
  getSource = const HsBindgen
