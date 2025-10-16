module HsBindgen.Backend
  ( backend
  , BackendArtefact(..)
  , BackendMsg(..)
  ) where

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Translation qualified as Hs
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Simplify qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Cache
import HsBindgen.Config.Internal
import HsBindgen.Frontend
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

-- | The backend translates the parsed C declarations in to Haskell
-- declarations.
--
-- The backend is pure and should not emit warnings or errors.
backend :: Tracer IO BackendMsg -> BackendConfig -> FrontendArtefact -> IO BackendArtefact
backend tracer BackendConfig{..} FrontendArtefact{..} = do
    -- 1. Reified C declarations to @Hs@ declarations.
    backendHsDecls <- cache $
      Hs.generateDeclarations
        backendTranslationOpts
        backendHaddockConfig
        moduleBaseName <$> frontendIndex
                       <*> frontendCDecls

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

    cache :: IO a -> IO (IO a)
    cache = cacheWith (contramap BackendCache tracer) Nothing

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

data BackendArtefact = BackendArtefact {
    backendHsDecls             :: IO (SHs.ByCategory [Hs.Decl])
  , backendFinalDecls          :: IO (SHs.ByCategory ([UserlandCapiWrapper], [SHs.SDecl]))
  , backendFinalModuleBaseName :: Hs.ModuleName
  , backendFinalModuleSafe     :: IO HsModule
  , backendFinalModuleUnsafe   :: IO HsModule
  , backendFinalModules        :: IO (SHs.ByCategory HsModule)
  }

{-------------------------------------------------------------------------------
  Trace
-------------------------------------------------------------------------------}

-- | Frontend trace messages
--
-- Most passes in the frontend have their own set of trace messages.
data BackendMsg = BackendCache CacheMsg
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace SafeLevel)
