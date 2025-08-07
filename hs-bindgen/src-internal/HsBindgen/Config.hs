module HsBindgen.Config
  ( Config (..)
  ) where

import Data.Default (Default)
import GHC.Generics (Generic)

import Clang.Args
import HsBindgen.Backend.Artefact.PP.Render
import HsBindgen.Backend.Artefact.PP.Translation
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing)
import HsBindgen.Frontend.Predicate (ParsePredicate, SelectPredicate)
import HsBindgen.Hs.Translation

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
