module HsBindgen.Frontend.Pass.Slice.IsPass (
    Slice
  , ProgramSlicing (..)
  , SliceConfig (..)
  , Msg (..)
  ) where

import Data.Default (Default (def))

import HsBindgen.C.Predicate
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Type.DeclId
import HsBindgen.Frontend.Pass.Sort.IsPass (Sort)
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint

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
  data Msg        Slice = TransitiveDependencyUnavailable QualDeclId
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

instance PrettyForTrace (Msg Slice) where
  prettyForTrace (TransitiveDependencyUnavailable qualId) =
    "Program slicing: Transitive dependency unavailable: " >< prettyForTrace qualId

instance HasDefaultLogLevel (Msg Slice) where
  getDefaultLogLevel = const Error

instance HasSource (Msg Slice) where
  getSource = const HsBindgen
