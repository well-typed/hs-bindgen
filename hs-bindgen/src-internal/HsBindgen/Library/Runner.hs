-- | Library runner: orchestrate the full library-mode pipeline
--
-- Processes all library headers in a single frontend run, plans the module
-- layout, then generates one Haskell module per header. Cross-module type
-- references are resolved through unqualified imports computed from the
-- @UseDeclGraph@ and placement map.
module HsBindgen.Library.Runner (
    runLibrary
  , LibraryModuleOutput(..)
  , LibraryMsg(..)
  ) where

import Data.Bifunctor (bimap)
import Data.Foldable (forM_)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Directory (canonicalizePath)
import Text.SimplePrettyPrint (hsep, string)

import Clang.Paths (RealPath, getRealPath)

import HsBindgen.Backend.Category (ByCategory_ (..), Category (..))
import HsBindgen.Backend.Category.ApplyChoice (applyBindingCategoryChoice)
import HsBindgen.Backend.Hs.Translation qualified as Hs
import HsBindgen.Backend.HsModule.Render (render, renderHsBoot)
import HsBindgen.Backend.HsModule.Translation (ImportListItem (..),
                                               ImportSource (..),
                                               translateModuleMultiple)
import HsBindgen.Backend.HsModule.Translation.Doxygen (computeExportTags,
                                                       resolveExports)
import HsBindgen.Backend.SHs.Simplify (simplifySHs)
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.Boot (BootArtefact (..))
import HsBindgen.Cache (getCached)
import HsBindgen.Config.Internal (BackendConfig (..), BindgenConfig (..),
                                  FrontendConfig (..))
import HsBindgen.Config.Prelims (BaseModuleName (..), ModuleRenderConfig,
                                 fromBaseModuleName)
import HsBindgen.Frontend (FrontendArtefact (..), FrontendMsg (..))
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.DeclDB
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.Pass.AdjustTypes (adjustTypes)
import HsBindgen.Frontend.Pass.Final (Final)
import HsBindgen.Frontend.Pass.MangleNames (mangleNames)
import HsBindgen.Frontend.Pass.Select (selectDecls)
import HsBindgen.Frontend.Pass.Select.IsPass (SelectConfig (..))
import HsBindgen.Frontend.Pass.TranslateTypes (translateTypes)
import HsBindgen.Frontend.Predicate (mkIsInMainHeaderDir, mkIsMainHeader)
import HsBindgen.Frontend.TranslationUnit (TranslationUnit (..))
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Translation (DeclIdPair (..))
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Library
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Util.Tracer

import Doxygen.Parser (Doxygen)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | Trace messages emitted by the library runner.
data LibraryMsg =
    -- | A planned module produced no declarations after selection and
    -- placement filtering. The header exists but nothing from it survived.
    LibraryEmptyModule Hs.ModuleName RealPath
  deriving stock (Show)

instance PrettyForTrace LibraryMsg where
  prettyForTrace = \case
    LibraryEmptyModule modName path -> hsep [
        "Library module"
      , string (Hs.moduleNameToString modName)
      , "for header"
      , string (getRealPath path)
      , "has no declarations after filtering"
      ]

instance IsTrace Level LibraryMsg where
  getDefaultLogLevel = \case
    LibraryEmptyModule{} -> Notice
  getSource = const HsBindgen
  getTraceId = const "library"

{-------------------------------------------------------------------------------
  Output
-------------------------------------------------------------------------------}

-- | Output for a single library module after processing.
data LibraryModuleOutput = LibraryModuleOutput {
      moduleName :: Hs.ModuleName
    , headerPath :: RealPath
      -- | Rendered module contents, indexed by binding category. 'Nothing'
      -- for categories that produced no declarations. Each 'Just' pair is
      -- (module source, optional boot file source).
    , rendered   :: ByCategory_ (Maybe (String, Maybe String))
    }

{-------------------------------------------------------------------------------
  Runner
-------------------------------------------------------------------------------}

-- | Run the full library-mode pipeline.
--
-- The caller is responsible for running @runFrontend@ and @runBoot@ and
-- passing their results here. The runner forces the @resolveBindingSpecs@
-- result from the frontend artefact, then takes over from @mangleNames@
-- through the backend.
runLibrary ::
     forall l. Macro.HasTypes l
  => Tracer LibraryMsg
  -> Tracer FrontendMsg
  -> BindgenConfig
  -> BootArtefact l
  -> FrontendArtefact l
  -> ModuleRenderConfig
  -> [FilePath]
  -> Hs.ModuleName
  -> IO (Either PlanError [LibraryModuleOutput])
runLibrary
  tracer
  tracerFrontend
  config
  boot
  frontend
  mrc
  libraryDirs
  baseModule
  = do
    canonicalLibraryDirs <- mapM canonicalizePath libraryDirs

    -- The caller ran runFrontend; we force the resolveBindingSpecs result
    -- to get the usage decl graph.
    tuResolveBindingSpecs <- getCached frontend.resolveBindingSpecs
    let declDB       = tuResolveBindingSpecs.db
        useDeclGraph = UseDeclGraph.fromDeclUseGraph declDB.declUseGraph

    case planLibrary canonicalLibraryDirs baseModule useDeclGraph (DeclDB.active AnyOrder declDB) of
      Left err -> pure (Left err)
      Right plan -> do
        -- mangleNames with the placement map from the plan.
        --
        -- The TU still contains vendor/system declarations that are not in
        -- the placement map. The fallback to baseModule is a totality guard;
        -- it does not matter what module those declarations get because
        -- selectDecls and the placement lookup both filter them out.
        let moduleOf declId =
              Map.findWithDefault baseModule declId plan.placement

            -- Two declarations are placed together only when both appear in the
            -- placement map and map to the same module. Vendor/system declarations
            -- (absent from the map) are never considered co-placed: returning True
            -- for two Nothing lookups would suppress typedef-suffixing between
            -- unrelated vendor types.
            placedTogether a b =
              case (Map.lookup a plan.placement, Map.lookup b plan.placement) of
                (Just ma, Just mb) -> ma == mb
                _                  -> False

            (tuMangleNames, mangleMsgs) =
              mangleNames moduleOf placedTogether fns tuResolveBindingSpecs

        forM_ mangleMsgs $
          traceWith (contramap FrontendMangleNames tracerFrontend)

        -- adjustTypes, translateTypes, selectDecls on the mangled TU.
        let tuAdjustTypes = adjustTypes tuMangleNames
            (tuTranslateTypes, translateMsgs) = translateTypes tuAdjustTypes

        forM_ translateMsgs $
          traceWith (contramap FrontendTranslateTypes tracerFrontend)

        let libraryHeaders    = Set.fromList
                              [ m.headerPath
                              | m <- plan.modules
                              ]
            isMainHeader      = mkIsMainHeader libraryHeaders
            isInMainHeaderDir = mkIsInMainHeaderDir libraryHeaders
            selectCfg = SelectConfig {
                programSlicing     = config.frontend.programSlicing
              , selectionPredicate = config.frontend.selectionPredicate
              }
            -- selectDecls keeps only declarations from library headers (and
            -- their transitive dependencies). Vendor/system declarations that
            -- mangleNames processed with the baseModule fallback are dropped
            -- here.
            --
            -- Note that selection and placement are independent: isMainHeader
            -- and selectionPredicate control which declarations are roots for
            -- program slicing, while the placement map covers all library
            -- headers. If a user excludes header H via selectionPredicate,
            -- H's declarations can still be pulled in as transitive
            -- dependencies of another library header's roots, and they land
            -- in H's module via the placement map. However, if a
            -- third-party/vendor (i.e. outside the --library dirs) declaration
            -- exists in the TU, it will be dropped when constructing
            -- declsByModule.
            (tuFinal, selectMsgs) =
              selectDecls isMainHeader isInMainHeaderDir selectCfg tuTranslateTypes

            finalDecls = DeclDB.active (DeclUseGraph.outputOrder tuFinal.includeGraph) tuFinal.db

        forM_ selectMsgs $
          traceWith (contramap FrontendSelect tracerFrontend)

        -- Group declarations by module for per-module backend runs. The
        -- placement lookup silently drops any declaration not in the
        -- placement map (the 'Just' pattern). After selectDecls, the only
        -- declarations dropped here are transitive dependencies from
        -- non-library headers that program slicing pulled in.
        let declsByModule :: Map.Map Hs.ModuleName [C.Decl l Final]
            declsByModule = Map.fromListWith (++)
              [ (modName, [decl])
              | decl <- finalDecls
              , Just modName <- [Map.lookup decl.id.cName plan.placement]
              ]

        -- Per-module backend.
        macroLang <- getCached boot.macroLang
        rootDirs  <- getCached boot.rootDirectives
        doxygen   <- getCached frontend.doxygen

        let outputs =
              [ let moduleDecls =
                      Map.findWithDefault [] libMod.moduleName declsByModule
                in processModule
                     macroLang rootDirs doxygen tuFinal.db libMod moduleDecls
              | libMod <- plan.modules
              ]

        -- Trace any modules that ended up empty.
        sequence_
          [ traceWith tracer $ withCallStack $
              LibraryEmptyModule libMod.moduleName libMod.headerPath
          | libMod <- plan.modules
          , not (Map.member libMod.moduleName declsByModule)
          ]

        pure (Right outputs)
  where
    processModule ::
         Macro.Lang l
      -> [C.RootDirective C.HashIncludeArg]
      -> Doxygen
      -> DeclDB l Final
      -> LibraryModule
      -> [C.Decl l Final]
      -> LibraryModuleOutput
    processModule macroLang rootDirs doxygen declIndex libMod moduleDecls =
      let
        libBaseModule = BaseModuleName libMod.moduleName.text

        hsDecls = Hs.generateDeclarations
          macroLang
          config.backend.uniqueId
          libBaseModule
          config.backend.hsBootPolicy
          declIndex
          moduleDecls

        filteredDecls = applyBindingCategoryChoice
          config.backend.categoryChoice hsDecls

        sHsDecls   = SHs.translateDecls macroLang libMod.moduleName filteredDecls
        finalDecls = simplifySHs sHsDecls

        -- Cross-module imports from the plan.
        -- SCC peers get SOURCE imports (breaks cycles), other
        -- dependencies get normal imports.
        crossModuleImports =
          [ QualifiedImportListItem SourceImport
              (fromBaseModuleName (BaseModuleName peer.text) (Just CType))
              Nothing
          | peer <- Set.toList libMod.sccPeers
          ]
          ++
          [ QualifiedImportListItem NormalImport
              (fromBaseModuleName (BaseModuleName dep.text) (Just CType))
              Nothing
          | dep <- Set.toList (libMod.reaches `Set.difference` libMod.sccPeers)
          ]

        exportTags = computeExportTags doxygen moduleDecls

        modulesByCategory = translateModuleMultiple
          fns
          mrc
          config.backend.hsBootPolicy
          rootDirs
          libBaseModule
          crossModuleImports
          (resolveExports exportTags)
          finalDecls

        renderedModules = fmap (fmap (bimap render (fmap renderHsBoot)))
          modulesByCategory

      in LibraryModuleOutput {
          moduleName = libMod.moduleName
        , headerPath = libMod.headerPath
        , rendered   = renderedModules
        }

    fns = config.frontend.fieldNamingStrategy
