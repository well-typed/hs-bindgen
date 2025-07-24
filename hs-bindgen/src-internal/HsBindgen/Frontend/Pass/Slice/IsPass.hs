module HsBindgen.Frontend.Pass.Slice.IsPass (
    Slice
    -- * Configuration
  , ProgramSlicing (..)
  , SliceConfig (..)
    -- * Trace messages
  , SliceMsg (..)
  , SelectReason (..)
  ) where

import Data.Default (Default (def))
import Data.Set (Set)
import Data.Set qualified as Set

import HsBindgen.C.Predicate
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Sort.IsPass (Sort)
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((><))
import Text.SimplePrettyPrint qualified as PP

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Slice :: Pass
data Slice a deriving anyclass ValidPass

type family AnnSlice ix where
  AnnSlice ix = Ann ix Sort

instance IsPass Slice where
  type Id         Slice = Id         Sort
  type FieldName  Slice = FieldName  Sort
  type TypedefRef Slice = TypedefRef Sort
  type MacroBody  Slice = MacroBody  Sort
  type ExtBinding Slice = ExtBinding Sort
  type Ann ix     Slice = AnnSlice ix
  type Config     Slice = SliceConfig
  type Msg        Slice = SliceMsg

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

data SliceConfig = SliceConfig {
      sliceConfigProgramSlicing :: ProgramSlicing
    , sliceConfigPredicate      :: Predicate
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | Slice trace messages
data SliceMsg =
    SliceTransitiveDependencyUnavailable NsPrelimDeclId
  | SliceSkipped (C.DeclInfo Slice)
  | SliceSelected SelectReason
  deriving stock (Show, Eq)

data SelectReason =
    TransitiveDependencyOf {
      selectedDecl           :: NsPrelimDeclId
      -- NOTE: The inverse dependencies form tree. For now, we just flatten the
      -- tree to list all inverse dependencies.
    , transitiveDependencyOf :: Set NsPrelimDeclId
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

instance PrettyForTrace SliceMsg where
  prettyForTrace = \case
    SliceTransitiveDependencyUnavailable qualId ->
      "Program slicing: Transitive dependency unavailable: " >< prettyForTrace qualId
    SliceSkipped  info -> prettyForTrace info >< " not selected"
    SliceSelected reason -> prettyForTrace reason

instance HasDefaultLogLevel SliceMsg where
  getDefaultLogLevel = \case
    SliceTransitiveDependencyUnavailable{} -> Error
    SliceSkipped{}                         -> Info
    SliceSelected reason                   -> getDefaultLogLevel reason

instance HasSource SliceMsg where
  getSource = const HsBindgen
