module HsBindgen.Frontend.Pass.Select.IsPass (
    Select
    -- * Configuration
  , ProgramSlicing (..)
  , SelectConfig (..)
    -- * Trace messages
  , SelectMsg (..)
  ) where

import Data.Default (Default (def))

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Internal (CheckedMacro, ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass (ResolveBindingSpec,
                                                          ResolvedExtBinding)
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta)
import HsBindgen.Frontend.Predicate (SelectPredicate)
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((><))

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
  | SelectSelected (C.DeclInfo Select)
  deriving stock (Show, Eq)

instance PrettyForTrace SelectMsg where
  prettyForTrace = \case
    SelectTransitiveDependencyUnavailable qualId ->
      "Program slicing: Transitive dependency unavailable: " >< prettyForTrace qualId
    SelectExcluded info -> prettyForTrace info >< " excluded"
    SelectSelected info -> "Selected " >< prettyForTrace info

instance IsTrace Level SelectMsg where
  getDefaultLogLevel = \case
    SelectTransitiveDependencyUnavailable{} -> Error
    SelectExcluded{}                        -> Info
    SelectSelected{}                        -> Info
  getSource  = const HsBindgen
  getTraceId = const "select"
