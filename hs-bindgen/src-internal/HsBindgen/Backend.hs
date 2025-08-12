module HsBindgen.Backend
  ( backend
  , BackendArtefact(..)
  ) where

import HsBindgen.Backend.Artefact.PP.Translation
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Translation qualified as Hs
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Simplify qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Config
import HsBindgen.Frontend

-- | The backend translates the parsed C declarations in to Haskell
-- declarations.
--
-- The backend is pure and should not emit warnings or errors.
backend :: Config -> FrontendArtefact -> BackendArtefact
backend Config{..} FrontendArtefact{..} =
  let -- 1. Reified C declarations to @Hs@ declarations.
      hsDecls :: [Hs.Decl]
      hsDecls = Hs.generateDeclarations configTranslation moduleName frontendCDecls

      -- 2. @Hs@ declarations to simple @Hs@ declarations.
      sHsDecls :: [SHs.SDecl]
      sHsDecls = SHs.translateDecls hsDecls

      -- 3. Simplify
      finalDecls :: [SHs.SDecl]
      finalDecls = SHs.simplifySHs sHsDecls
  in BackendArtefact {
    backendHsDecls    = hsDecls
  , backendFinalDecls = finalDecls
  }
  where
    moduleName = hsModuleOptsName configHsModuleOpts

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

data BackendArtefact = BackendArtefact {
    backendHsDecls    :: [Hs.Decl]
  , backendFinalDecls :: [SHs.SDecl]
  }
