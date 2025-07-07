module HsBindgen.Config
  ( Config (..)
  ) where

import Data.Default (Default (def))

import Clang.Args
import HsBindgen.Backend.PP.Render
import HsBindgen.Backend.PP.Translation
import HsBindgen.C.Predicate
import HsBindgen.Frontend.Pass.Slice.IsPass
import HsBindgen.Hs.Translation

-- | Configuration of @hs-bindgen@.
--
-- 'Config' determines the "how", not the "what". For example, it should state how
-- we process a header file, but not state which headers we want to process.
--
-- 'Config' should contain user-provided data, not @hs-bindgen@-provided data.
data Config = Config {
      -- Translation
      configClangArgs      :: ClangArgs
    , configTranslation    :: TranslationOpts
    , configPredicate      :: Predicate
    , configProgramSlicing :: ProgramSlicing
      -- Pretty printing
    , configHsModuleOpts   :: HsModuleOpts
    , configHsRenderOpts   :: HsRenderOpts
    }
  deriving stock (Show)

instance Default Config where
  def :: Config
  def = Config {
      -- Translation
      configClangArgs      = def
    , configTranslation    = def
    , configPredicate      = SelectFromMainFiles
    , configProgramSlicing = DisableProgramSlicing
      -- Pretty printing
    , configHsModuleOpts   = HsModuleOpts { hsModuleOptsName = "Generated" }
    , configHsRenderOpts   = HsRenderOpts { hsLineLength     = 120 }
    }
