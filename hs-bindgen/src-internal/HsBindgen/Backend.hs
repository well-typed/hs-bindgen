module HsBindgen.Backend
  ( backend
  , BackendArtefact(..)
  ) where

import HsBindgen.Backend.Artefact.HsModule.Translation
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Translation qualified as Hs
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Simplify qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Cache
import HsBindgen.Config
import HsBindgen.Frontend
import HsBindgen.Language.Haskell
import HsBindgen.Util.Tracer (nullTracer)

-- | The backend translates the parsed C declarations in to Haskell
-- declarations.
--
-- The backend is pure and should not emit warnings or errors.
backend :: BackendConfig -> FrontendArtefact -> IO BackendArtefact
backend BackendConfig{..} FrontendArtefact{..} = do
    -- 1. Reified C declarations to @Hs@ declarations.
    backendHsDecls <- cache $
      Hs.generateDeclarations
        backendTranslationOpts
        backendHaddockConfig
        moduleBaseName <$>
        frontendCDecls

    -- 2. @Hs@ declarations to simple @Hs@ declarations.
    sHsDecls <- cache $ SHs.translateDecls <$> backendHsDecls

    -- 3. Simplify.
    backendFinalDecls <- cache $ SHs.simplifySHs <$> sHsDecls

    -- 4. Translate to modules.
    backendFinalModuleSafe <- cache $
      translateModuleSingle SHs.Safe moduleBaseName <$> backendFinalDecls
    backendFinalModuleUnsafe <- cache $
      translateModuleSingle SHs.Unsafe moduleBaseName <$> backendFinalDecls
    backendFinalModules <- cache $
      translateModuleMultiple moduleBaseName <$> backendFinalDecls

    pure $ BackendArtefact {
      backendFinalModuleBaseName = moduleBaseName
    , ..
    }
  where
    moduleBaseName = hsModuleOptsBaseName backendHsModuleOpts

    -- TODO https://github.com/well-typed/hs-bindgen/issues/1119.
    cache :: IO a -> IO (IO a)
    cache = cacheWith nullTracer Nothing

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

data BackendArtefact = BackendArtefact {
    backendHsDecls             :: IO (SHs.ByCategory [Hs.Decl])
  , backendFinalDecls          :: IO (SHs.ByCategory ([UserlandCapiWrapper], [SHs.SDecl]))
  , backendFinalModuleBaseName :: HsModuleName
  , backendFinalModuleSafe     :: IO HsModule
  , backendFinalModuleUnsafe   :: IO HsModule
  , backendFinalModules        :: IO (SHs.ByCategory HsModule)
  }
