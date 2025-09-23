module HsBindgen.Backend
  ( backend
  , BackendArtefact(..)
  , BackendMsg(..)
  , RunArtefactMsg(..)
  ) where

import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

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
import HsBindgen.Imports
import HsBindgen.Language.Haskell
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

    cache :: IO a -> IO (IO a)
    cache = cacheWith (contramap BackendCache tracer) Nothing

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

{-------------------------------------------------------------------------------
  Trace
-------------------------------------------------------------------------------}

-- | Frontend trace messages
--
-- Most passes in the frontend have their own set of trace messages.
data BackendMsg =
    BackendCache       CacheMsg
  | BackendRunArtefact RunArtefactMsg
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace SafeLevel)

data RunArtefactMsg = RunArtefactWriteFile String FilePath
  deriving stock (Show, Generic)

instance PrettyForTrace RunArtefactMsg where
  prettyForTrace = \case
    RunArtefactWriteFile what path ->
      "Writing" <+> PP.showToCtxDoc what <+> "to file" <+> PP.showToCtxDoc path

instance IsTrace SafeLevel RunArtefactMsg where
  getDefaultLogLevel = \case
    RunArtefactWriteFile _ _ -> SafeInfo
  getSource = const HsBindgen
  getTraceId = \case
    RunArtefactWriteFile _ _ -> "run-artefact-write-file"
