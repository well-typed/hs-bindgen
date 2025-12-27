module HsBindgen.Backend
  ( backend
  , BackendArtefact(..)
  , BackendMsg(..)
  ) where

import HsBindgen.Backend.Category
import HsBindgen.Backend.Category.ApplyChoice
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Translation qualified as Hs
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Simplify qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Boot
import HsBindgen.Cache
import HsBindgen.Config.Internal
import HsBindgen.Frontend
import HsBindgen.Imports
import HsBindgen.Util.Tracer

-- | The backend translates the parsed C declarations in to Haskell
-- declarations.
--
-- The backend is pure and should not emit warnings or errors.
backend ::
     Tracer BackendMsg
  -> BackendConfig
  -> BootArtefact
  -> FrontendArtefact
  -> IO BackendArtefact
backend tracer config BootArtefact{..} FrontendArtefact{..} = do
    -- 1. Reified C declarations to @Hs@ declarations.
    backendHsDeclsAll <- cache $
      Hs.generateDeclarations config.translation config.haddock moduleBaseName
        <$> frontendIndex
        <*> frontendCDecls

    -- 2. Apply binding category choice.
    backendHsDecls <- cache $ do
      decls <- backendHsDeclsAll
      pure $ applyBindingCategoryChoice config.categoryChoice decls

    -- 3. @Hs@ declarations to simple @Hs@ declarations.
    sHsDecls <- cache $ SHs.translateDecls <$> backendHsDecls

    -- 4. Simplify.
    backendFinalDecls <- cache $ SHs.simplifySHs <$> sHsDecls

    pure $ BackendArtefact {
      backendFinalModuleBaseName = moduleBaseName
    , ..
    }
  where
    moduleBaseName = bootBaseModule

    cache :: Cached a -> IO (Cached a)
    cache = cacheWith (contramap BackendCache tracer) Nothing

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

data BackendArtefact = BackendArtefact {
    backendHsDecls             :: Cached (ByCategory_ [Hs.Decl])
  , backendFinalDecls          :: Cached (ByCategory_ ([CWrapper], [SHs.SDecl]))
  , backendFinalModuleBaseName :: BaseModuleName
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
