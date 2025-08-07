{-# LANGUAGE UndecidableSuperClasses #-}

module HsBindgen
  ( Artefact (..)
  , writeIncludeGraph
  , writeUseDeclGraph
  , writeBindings
  , hsBindgen
  ) where

import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths
import Generics.SOP (I (..), NP (..))
import HsBindgen.Backend.Artefact.PP.Render qualified as PP
import HsBindgen.Backend.Artefact.PP.Translation qualified as PP
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Translation qualified as Hs
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Backend.SHs.Simplify qualified as SHs
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.BindingSpec
import HsBindgen.Clang
import HsBindgen.Config
import HsBindgen.Frontend hiding (frontend)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.AST.Finalize
import HsBindgen.Frontend.AST.Internal hiding (Type)
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.HandleTypedefs
import HsBindgen.Frontend.Pass.MangleNames
import HsBindgen.Frontend.Pass.NameAnon
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.ResolveBindingSpec
import HsBindgen.Frontend.Pass.Select
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.Sort
import HsBindgen.Frontend.Pass.Sort.IsPass (DeclMeta (declDeclUse, declIndex, declUseDecl))
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.ModuleUnique
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Build artefacts
-------------------------------------------------------------------------------}

data Artefact (a :: Star) where
  -- Frontend.
  IncludeGraph :: Artefact IncludeGraph.IncludeGraph
  DeclIndex    :: Artefact DeclIndex.DeclIndex
  UseDeclGraph :: Artefact UseDeclGraph.UseDeclGraph
  DeclUseGraph :: Artefact DeclUseGraph.DeclUseGraph
  ReifiedC     :: Artefact [C.Decl]
  -- Backend.
  Hs           :: Artefact [Hs.Decl]
  SHs          :: Artefact [SHs.SDecl]
  Module       :: Artefact String
  -- Writers.
  Write :: Artefacts as -> (NP I as -> IO b) -> Artefact b

type Artefacts as = NP Artefact as

-- Writers.
writeIncludeGraph :: Maybe FilePath -> Artefact ()
writeIncludeGraph mfile =
  Write
    (IncludeGraph :* Nil)
    (\(I includeGraph :* Nil) -> write mfile (IncludeGraph.dumpMermaid includeGraph))

writeUseDeclGraph :: Maybe FilePath -> Artefact ()
writeUseDeclGraph mfile =
  Write
    (DeclIndex :* UseDeclGraph :* Nil)
    (\(I index :* I useDeclGraph :* Nil) ->
       write mfile (UseDeclGraph.dumpMermaid index useDeclGraph))

writeBindings :: Maybe FilePath -> Artefact ()
writeBindings mfile =
  Write
    (Module :* Nil)
    (\(I module_ :* Nil) -> write mfile module_)

-- TODO: WriteBindingSpecs (but then we need an HList).

-- NOTE: Instead of providing the tracer, we should probably provide the
-- complete tracer configuration. Then we can abort before running the backend.

-- NOTE: Instead of providing the resolved binding specifications, we should
-- provide the binding specification configuration and resolve binding
-- specifications here.

-- NOTE: `checkInputs` should also be performed here.
hsBindgen ::
     Tracer IO FrontendMsg
  -> ModuleUnique
  -> Config
  -> ExternalBindingSpec
  -> PrescriptiveBindingSpec
  -> [HashIncludeArg]
  -> Artefacts as
  -> IO (Maybe (NP I as))
hsBindgen tracer moduleUnique config extSpec pSpec headers artefacts = do
    mFrontedArtefact <- frontend tracer config extSpec pSpec headers
    case mFrontedArtefact of
      Nothing -> pure Nothing
      Just frontendArtefact ->
        Just <$> backend moduleUnique config frontendArtefact artefacts

{-------------------------------------------------------------------------------
  Frontend
-------------------------------------------------------------------------------}

data FrontendArtefact = FrontendArtefact {
    frontendIncludeGraph :: IncludeGraph.IncludeGraph
  , frontendIndex        :: DeclIndex.DeclIndex
  , frontendUseDeclGraph :: UseDeclGraph.UseDeclGraph
  , frontendDeclUseGraph :: DeclUseGraph.DeclUseGraph
  , frontendCDecls       :: [C.Decl]
  }

frontend ::
     Tracer IO FrontendMsg
  -> Config
  -> ExternalBindingSpec
  -> PrescriptiveBindingSpec
  -> [HashIncludeArg]
  -> IO (Maybe FrontendArtefact)
frontend tracer Config{..} extSpec pSpec headers = do
    -- Frontend: Impure parse pass
    mParseResult <-
      withClang (contramap FrontendClang tracer) setup $ \unit -> Just <$> do
        (includeGraph, isMainHeader, isInMainHeaderDir, getMainHeader) <-
          processIncludes rootHeader unit
        reifiedUnit <- parseDecls
          (contramap FrontendParse tracer)
          rootHeader
          configParsePredicate
          includeGraph
          isMainHeader
          isInMainHeaderDir
          getMainHeader
          unit
        pure (reifiedUnit, isMainHeader, isInMainHeaderDir)

    case mParseResult of
      Nothing -> pure Nothing
      Just (afterParse, isMainHeader, isInMainHeaderDir) -> do
        -- Frontend: Pure passes.
        let (afterSort, msgsSort) =
              sortDecls afterParse
            (afterHandleMacros, msgsHandleMacros) =
              handleMacros afterSort
            (afterNameAnon, msgsNameAnon) =
              nameAnon afterHandleMacros
            (afterResolveBindingSpec, msgsResolveBindingSpecs) =
              resolveBindingSpec extSpec pSpec afterNameAnon
            (afterSelect, msgsSelect) =
              selectDecls isMainHeader isInMainHeaderDir selectConfig afterResolveBindingSpec
            (afterHandleTypedefs, msgsHandleTypedefs) =
              handleTypedefs afterSelect
            (afterMangleNames, msgsMangleNames) =
              mangleNames afterHandleTypedefs

        -- TODO https://github.com/well-typed/hs-bindgen/issues/967: By emitting
        -- all traces in one place, we lose the callstack and timestamp
        -- information of the individual traces.

        -- TODO: Emitting traces forces all passes.
        forM_ msgsSort                $ traceWith tracer . FrontendSort
        forM_ msgsHandleMacros        $ traceWith tracer . FrontendHandleMacros
        forM_ msgsNameAnon            $ traceWith tracer . FrontendNameAnon
        forM_ msgsResolveBindingSpecs $ traceWith tracer . FrontendResolveBindingSpecs
        forM_ msgsSelect              $ traceWith tracer . FrontendSelect
        forM_ msgsHandleTypedefs      $ traceWith tracer . FrontendHandleTypedefs
        forM_ msgsMangleNames         $ traceWith tracer . FrontendMangleNames

        let -- Graphs.
            frontendIncludeGraph :: IncludeGraph.IncludeGraph
            frontendIncludeGraph = unitIncludeGraph afterParse
            frontendIndex        :: DeclIndex.DeclIndex
            frontendIndex        = declIndex $ unitAnn afterSort
            frontendUseDeclGraph :: UseDeclGraph.UseDeclGraph
            frontendUseDeclGraph = declUseDecl $ unitAnn afterSort
            frontendDeclUseGraph :: DeclUseGraph.DeclUseGraph
            frontendDeclUseGraph = declDeclUse $ unitAnn afterSort
            -- Declarations.
            frontendCDecls           :: [C.Decl]
            frontendCDecls           = C.unitDecls $ finalize afterMangleNames

        pure $ Just FrontendArtefact{..}
  where
    rootHeader :: RootHeader
    rootHeader = fromMainFiles headers

    setup :: ClangSetup
    setup = (defaultClangSetup configClangArgs $ ClangInputMemory hFilePath hContent) {
          clangFlags = bitfieldEnum [
              CXTranslationUnit_SkipFunctionBodies
            , CXTranslationUnit_DetailedPreprocessingRecord
            , CXTranslationUnit_IncludeAttributedTypes
            , CXTranslationUnit_VisitImplicitAttributes
            ]
        }

    hFilePath :: FilePath
    hFilePath = getSourcePath name

    hContent :: String
    hContent = content rootHeader

    selectConfig :: SelectConfig
    selectConfig = SelectConfig configProgramSlicing configSelectPredicate

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

backend :: ModuleUnique -> Config -> FrontendArtefact -> Artefacts as -> IO (NP I as)
backend moduleUnique Config{..} FrontendArtefact{..} = runArtefacts
  where
    runArtefacts :: Artefacts as -> IO (NP I as)
    runArtefacts Nil       = return Nil
    runArtefacts (a :* as) = (:*) . I <$> runArtefact a <*> runArtefacts as

    runArtefact :: Artefact a -> IO a
    runArtefact = \case
      -- Frontend.
      IncludeGraph -> pure frontendIncludeGraph
      DeclIndex    -> pure frontendIndex
      UseDeclGraph -> pure frontendUseDeclGraph
      DeclUseGraph -> pure frontendDeclUseGraph
      ReifiedC     -> pure frontendCDecls
      -- Backend.
      Hs     -> pure hsDecls
      SHs    -> pure sHsDecls
      Module -> pure $ render $ translate sHsDecls
      -- Writer.
      (Write as' f) -> runArtefacts as' >>= f

    hsDecls :: [Hs.Decl]
    hsDecls = Hs.generateDeclarations configTranslation moduleUnique frontendCDecls

    sHsDecls :: [SHs.SDecl]
    sHsDecls = SHs.simplifySHs $ SHs.translateDecls hsDecls

    translate :: [SHs.SDecl] -> PP.HsModule
    translate = PP.translateModule configHsModuleOpts

    render :: PP.HsModule -> String
    render = PP.render configHsRenderOpts

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

write :: Maybe FilePath -> String -> IO ()
write mfile str =
    let out = case mfile of
          Nothing -> putStr
          Just file -> writeFile file
    in  out str
