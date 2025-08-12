module HsBindgen.Config
  ( Config (..)
  , ConfigMsg (..)
  , checkConfig
  ) where

import Data.Default (Default)
import GHC.Generics (Generic)

import Clang.Args
import HsBindgen.Backend.Artefact.PP.Render
import HsBindgen.Backend.Artefact.PP.Translation
import HsBindgen.Backend.Hs.Translation
import HsBindgen.Backend.UniqueId
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing)
import HsBindgen.Frontend.Predicate (ParsePredicate, SelectPredicate)
import HsBindgen.Util.Tracer

-- | Configuration of @hs-bindgen@.
--
-- 'Config' determines the "how", not the "what". For example, it should state how
-- we process a header file, but not state which headers we want to process.
--
-- 'Config' should contain user-provided data, not @hs-bindgen@-provided data.
data Config = Config {
      -- Translation
      configClangArgs       :: ClangArgs
    , configTranslation     :: TranslationOpts
    , configParsePredicate  :: ParsePredicate
    , configSelectPredicate :: SelectPredicate
    , configProgramSlicing  :: ProgramSlicing
      -- Pretty printing
    , configHsModuleOpts    :: HsModuleOpts
    , configHsRenderOpts    :: HsRenderOpts
    }
  deriving stock (Show, Generic)

instance Default Config

checkConfig :: Tracer IO ConfigMsg -> Config -> IO ()
checkConfig tracer config =
    checkUniqueId (contramap ConfigUniqueId tracer) uniqueId
  where
    uniqueId :: UniqueId
    uniqueId = translationUniqueId $ configTranslation config

data ConfigMsg = ConfigUniqueId UniqueIdMsg
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (PrettyForTrace, HasDefaultLogLevel, HasSource)
