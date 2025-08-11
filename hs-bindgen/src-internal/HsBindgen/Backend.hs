module HsBindgen.Backend
  ( backend
  , BackendArtefact(..)
  ) where

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Translation qualified as Hs
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Simplify qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Config
import HsBindgen.Frontend
import HsBindgen.ModuleUnique

backend :: ModuleUnique -> Config -> FrontendArtefact -> BackendArtefact
backend moduleUnique Config{..} FrontendArtefact{..} =
  BackendArtefact {
    backendHsDecls  = hsDecls
  , backendSHsDecls = sHsDecls
  }
  where
    hsDecls :: [Hs.Decl]
    hsDecls = Hs.generateDeclarations configTranslation moduleUnique frontendCDecls

    sHsDecls :: [SHs.SDecl]
    sHsDecls = SHs.simplifySHs $ SHs.translateDecls hsDecls

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

data BackendArtefact = BackendArtefact {
    backendHsDecls  ::  [Hs.Decl]
  , backendSHsDecls :: [SHs.SDecl]
  }
