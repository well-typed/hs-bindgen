-- | Main entry point for using @hs-bindgen@ in Template-Haskell mode.
--
-- Intended for unqualified import.

-- NOTE: Client code should /NOT/ have to import from @clang@.

module HsBindgen.TH (
    TH.withHsBindgen
  , TH.hashInclude

    -- * Configuration
  , TH.Config
  , Config.Config_(..)
  , Config.ConfigTH(..)

    -- ** Clang arguments
  , ClangArgs.ClangArgsConfig(..)
  , ClangArgs.Target(..)
  , ClangArgs.TargetEnv(..)
  , ClangArgs.CStandard(..)
  , ClangArgs.Gnu(..)
  , ClangArgs.BuiltinIncDirConfig(..)
  , TH.IncludeDir(..)

    -- ** Binding specifications
  , BindingSpec.BindingSpecConfig(..)
  , BindingSpec.EnableStdlibBindingSpec(..)
  , BindingSpec.BindingSpecCompatibility(..)

    -- ** Predicates and program slicing
  , Predicate.Boolean(..)
  , Predicate.HeaderPathPredicate(..)
  , Predicate.Regex

    -- *** Parse predicates
  , Predicate.ParsePredicate(..)

    -- *** Select predicates and program slicing
  , Predicate.DeclPredicate(..)
  , Predicate.SelectPredicate(..)
  , Select.ProgramSlicing(..)

    -- ** Haddocks
  , Haddock.PathStyle(..)

    -- ** Safety
  , Safety.Safety(..)

   -- * Re-exports
  , Default.Default(..)
  ) where

import Data.Default qualified as Default

import HsBindgen.Backend.Hs.Haddock.Config qualified as Haddock
import HsBindgen.Backend.SHs.AST qualified as Safety
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config qualified as Config
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Frontend.Pass.Select.IsPass qualified as Select
import HsBindgen.Frontend.Predicate qualified as Predicate
import HsBindgen.TH.Internal qualified as TH
