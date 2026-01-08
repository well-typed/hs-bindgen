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

     -- ** Binding categories
  , Category.ByCategory(..)
  , Category.Choice(..)
  , Category.RenameTerm(..)
  , Category.useSafeCategory
  , Category.useUnsafeCategory
  , Category.useFunPtrCategory

    -- ** Haddocks
  , Haddock.PathStyle(..)

    -- ** Tracer
  , Tracer.Verbosity(..)
  , Tracer.Level(..)
  , TraceMsg.CustomLogLevelSetting(..)

   -- * Re-exports
  , Default.Default(..)
  ) where

import Data.Default qualified as Default

import HsBindgen.Backend.Category qualified as Category
import HsBindgen.Backend.Hs.Haddock.Config qualified as Haddock
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config qualified as Config
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Frontend.Pass.Select.IsPass qualified as Select
import HsBindgen.Frontend.Predicate qualified as Predicate
import HsBindgen.TH.Internal qualified as TH
import HsBindgen.TraceMsg as TraceMsg
import HsBindgen.Util.Tracer as Tracer
