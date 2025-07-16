module HsBindgen.Frontend.Pass.Slice.IsPass (
    Slice
    -- * Configuration
  , ProgramSlicing (..)
  , SliceConfig (..)
    -- * Trace messages
  , Msg (..)
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

  -- | Slice trace messages
  data Msg        Slice =
      TransitiveDependencyUnavailable NsPrelimDeclId
    | Skipped (C.DeclInfo Slice)
    | Selected SelectReason
    deriving stock (Show, Eq)

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

instance PrettyForTrace (Msg Slice) where
  prettyForTrace = \case
    TransitiveDependencyUnavailable qualId ->
      "Program slicing: Transitive dependency unavailable: " >< prettyForTrace qualId
    Skipped  info -> prettyForTrace info >< " not selected"
    Selected reason -> prettyForTrace reason

instance HasDefaultLogLevel (Msg Slice) where
  getDefaultLogLevel = \case
    TransitiveDependencyUnavailable _ -> Error
    Skipped{}       -> Info
    Selected reason -> getDefaultLogLevel reason

instance HasSource (Msg Slice) where
  getSource = const HsBindgen
