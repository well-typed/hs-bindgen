module HsBindgen.Backend
  ( runBackend
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
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Type
import HsBindgen.Util.Tracer

-- | The backend translates the parsed C declarations in to Haskell
-- declarations.
--
-- The backend is pure and should not emit warnings or errors.
runBackend ::
     forall l. HasMacroTypes l
  => Tracer BackendMsg
  -> BindgenConfig
  -> BootArtefact l
  -> FrontendArtefact l
  -> IO (BackendArtefact l)
runBackend tracer config boot frontend = do
    -- 1. Reified C declarations to @Hs@ declarations.
    backendHsDeclsAll <- cache "hsDeclsAll" $ do
      final     <- frontend.final
      macroLang <- boot.macroLang
      sizeofs   <- boot.sizeofs
      let declIndex :: DeclIndex l
          declIndex = final.meta.declIndex

          cDecls :: [C.Decl l Final]
          cDecls = final.decls
      pure $ Hs.generateDeclarations
        macroLang
        config.backend.uniqueId
        config.backend.haddock
        boot.baseModule
        declIndex
        sizeofs
        cDecls

    -- 2. Apply binding category choice.
    backendHsDecls <- cache "hsDecls" $ do
      decls <- backendHsDeclsAll
      pure $ applyBindingCategoryChoice config.backend.categoryChoice decls

    -- 3. @Hs@ declarations to simple @Hs@ declarations.
    sHsDecls <- cache "sHsDecls" $ do
      macroLang <- boot.macroLang
      SHs.translateDecls macroLang <$> backendHsDecls

    -- 4. Simplify.
    backendFinalDecls <- cache "finalDecls" $ SHs.simplifySHs <$> sHsDecls

    pure $ BackendArtefact {
        hsDecls             = backendHsDecls
      , finalDecls          = backendFinalDecls
      }
  where
    cache :: String -> Cached a -> IO (Cached a)
    cache = cacheWith (contramap BackendCache tracer) . Just

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

data BackendArtefact l = BackendArtefact {
      hsDecls    :: Cached (ByCategory_ [Hs.Decl l])
    , finalDecls :: Cached (ByCategory_ ([CWrapper], [SHs.SDecl]))
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
