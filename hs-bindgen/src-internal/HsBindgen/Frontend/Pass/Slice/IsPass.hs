module HsBindgen.Frontend.Pass.Slice.IsPass (
    Slice
  , ProgramSlicing (..)
  , SliceConfig (..)
  , SliceMsg (..)
  ) where

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

data SliceConfig = SliceConfig {
      sliceConfigProgramSlicing :: ProgramSlicing
    , sliceConfigPredicate      :: Predicate
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data SliceMsg = TransitiveDependencyUnavailable QualDeclId
  deriving stock (Show, Eq)

instance PrettyForTrace SliceMsg where
  prettyForTrace (TransitiveDependencyUnavailable qualId) =
    "Program slicing: Transitive dependency unavailable: " >< prettyForTrace qualId

instance HasDefaultLogLevel SliceMsg where
  getDefaultLogLevel = const Error

instance HasSource SliceMsg where
  getSource = const HsBindgen
