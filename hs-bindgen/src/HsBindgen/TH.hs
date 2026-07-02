-- | Main entry point for using @hs-bindgen@ in Template-Haskell mode.
--
-- Intended for unqualified import.

-- NOTE: Client code should /NOT/ have to import from @clang@.

module HsBindgen.TH (
    withHsBindgen
  , TH.withHsBindgenMacroLang
  , TH.hashInclude

    -- * Configuration
  , TH.Config
  , Config.Config_(..)
  , Config.ConfigTH(..)

    -- ** Clang arguments
  , ClangArgs.ClangArgsConfig(..)
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

    -- *** Selection predicates and program slicing
  , Predicate.SelectionPredicate(..)
  , Predicate.DeclPredicate(..)
  , Select.ProgramSlicing(..)

    -- ** Fields
  , Config.FieldNamingStrategy(..)

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
  , TraceMsg.CustomLogLevelSetting(..)
  , Tracer.Level(..)
  , Tracer.TraceId(..)

    -- * Re-exports
  , Default.Default(..)

    -- * Deriving
    --
    -- Ensure constructors are in scope when using @deriving via@.
  , Deriving.EquivStorable(..)
  , Deriving.SizedByteArray(..)
  ) where

import Data.Default qualified as Default
import Language.Haskell.TH qualified as TH

import HsBindgen.Runtime.Internal.Deriving qualified as Deriving

import HsBindgen.Backend.Category qualified as Category
import HsBindgen.Backend.Hs.Haddock.Config qualified as Haddock
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config qualified as Config
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Frontend.Pass.Select.IsPass qualified as Select
import HsBindgen.Frontend.Predicate qualified as Predicate
import HsBindgen.Macro
import HsBindgen.TH.Internal qualified as TH
import HsBindgen.TraceMsg as TraceMsg
import HsBindgen.Util.Tracer as Tracer

-- | Generate bindings for given C headers at compile-time using the default C
-- macro language.
--
-- Use 'TH.withHsBindgenMacroLang' to supply a different macro-language backend.
withHsBindgen ::
     TH.Config
  -> Config.ConfigTH
  -> TH.BindgenM
  -> TH.Q [TH.Dec]
withHsBindgen = TH.withHsBindgenMacroLang (pure . cExpr)
