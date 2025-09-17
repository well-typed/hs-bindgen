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
import HsBindgen.Config
import HsBindgen.Frontend
import HsBindgen.Language.Haskell

-- | The backend translates the parsed C declarations in to Haskell
-- declarations.
--
-- The backend is pure and should not emit warnings or errors.
backend :: BackendConfig -> FrontendArtefact -> BackendArtefact
backend BackendConfig{..} FrontendArtefact{..} =
  let -- 1. Reified C declarations to @Hs@ declarations.
      hsDecls :: SHs.ByCategory [Hs.Decl]
      hsDecls = Hs.generateDeclarations
                  backendTranslationOpts
                  backendHaddockConfig
                  moduleBaseName
                  frontendCDecls

      -- 2. @Hs@ declarations to simple @Hs@ declarations.
      sHsDecls :: SHs.ByCategory ([UserlandCapiWrapper], [SHs.SDecl])
      sHsDecls = SHs.translateDecls hsDecls

      -- 3. Simplify
      finalDecls :: SHs.ByCategory ([UserlandCapiWrapper], [SHs.SDecl])
      finalDecls = SHs.simplifySHs sHsDecls
  in BackendArtefact {
    backendHsDecls             = hsDecls
  , backendFinalModuleBaseName = moduleBaseName
  , backendFinalDecls          = finalDecls
  }
  where
    moduleBaseName = hsModuleOptsBaseName backendHsModuleOpts

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

data BackendArtefact = BackendArtefact {
    backendHsDecls             :: SHs.ByCategory [Hs.Decl]
  , backendFinalDecls          :: SHs.ByCategory ([UserlandCapiWrapper], [SHs.SDecl])
  , backendFinalModuleBaseName :: HsModuleName
  }
