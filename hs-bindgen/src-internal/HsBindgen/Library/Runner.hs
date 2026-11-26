-- | Library runner: orchestrate the full library-mode pipeline
--
-- Processes all library headers in a single frontend run. Places each
-- declaration in the header unit of its header, or in the unit of the vendor
-- declarations, runs the frontend passes from name mangling to selection,
-- plans the header units on the result (see "HsBindgen.Library"), then
-- generates the files of each unit.
module HsBindgen.Library.Runner (
    runLibrary
  , LibraryResult(..)
  , LibraryUnitOutput(..)
  , GeneratedFile(..)
  , resultFiles
  , LibraryMsg(..)
  ) where

import Control.Monad (forM, guard, unless, when)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.Foldable (forM_)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Optics.Core (over, view)
import System.Directory (canonicalizePath)
import Text.SimplePrettyPrint (hsep, nest, string, text, vcat, ($$))

import Clang.Paths (RealPath, getRealPath)

import HsBindgen.ArtefactM (FileContent (..))
import HsBindgen.Backend.Category (Category (..), HsBootPolicy (..),
                                   TermCategory (..), lensForCategory,
                                   mapWithCategory_)
import HsBindgen.Backend.Category.ApplyChoice (applyBindingCategoryChoice)
import HsBindgen.Backend.Hs.AST qualified as HsAST
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Translation qualified as Hs
import HsBindgen.Backend.Hs.Translation.ToFromFunPtr qualified as ToFromFunPtr
import HsBindgen.Backend.HsModule.Render (render, renderHsBoot)
import HsBindgen.Backend.HsModule.Translation (ImportListItem (..),
                                               ImportSource (..),
                                               translateModuleMultiple,
                                               withModuleDoc)
import HsBindgen.Backend.HsModule.Translation.Doxygen (computeExportTags,
                                                       resolveExports)
import HsBindgen.Backend.SHs.Simplify (simplifySHs)
import HsBindgen.Backend.SHs.Translation qualified as SHs
import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.BindingSpec.Private.Common (Format)
import HsBindgen.Boot (BootArtefact (..))
import HsBindgen.Cache (getCached)
import HsBindgen.Config.ClangArgs (ClangArgsConfig (..))
import HsBindgen.Config.Internal (BackendConfig (..), BindgenConfig (..),
                                  BootConfig (..), FrontendConfig (..))
import HsBindgen.Config.Prelims (BaseModuleName (..), ModuleRenderConfig,
                                 fromBaseModuleName)
import HsBindgen.Frontend (FrontendArtefact (..), FrontendMsg (..),
                           ParseInfo (..))
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.DeclDB
import HsBindgen.Frontend.DeclDB qualified as DeclDB
import HsBindgen.Frontend.Pass.Final (Final)
import HsBindgen.Frontend.Pass.FunPtrInstances qualified as FunPtrInstances
import HsBindgen.Frontend.Pass.MangleNames (mangleNames)
import HsBindgen.Frontend.Pass.Select (selectDecls)
import HsBindgen.Frontend.Pass.Select.IsPass (SelectConfig (..))
import HsBindgen.Frontend.Pass.TranslateTypes (translateTypes)
import HsBindgen.Frontend.ProcessIncludes (GetMainHeaders)
import HsBindgen.Frontend.SortMsgs (sortMsgs)
import HsBindgen.Frontend.TranslationUnit (TranslationUnit (..))
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.FunPtrInstance (FunPtrInstance)
import HsBindgen.IR.Translation (DeclIdPair (..))
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Library qualified as Library
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Util.Tracer

import Doxygen.Parser (Doxygen)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | Trace messages emitted by the library runner.
data LibraryMsg =
    -- | A library header whose header unit writes no file: the selection
    -- kept none of its declarations, or they only have bindings in categories
    -- the user excluded.
    LibraryEmptyUnit Hs.ModuleName RealPath
    -- | Some types modules form an import cycle, so they get boot files.
    -- Emitted once per library run.
  | LibraryAutoBootFiles
    -- | The vendor module holds this many declarations (see
    -- "HsBindgen.Library"). Emitted once per library run, when the module is
    -- written.
  | LibraryVendorModule Hs.ModuleName Int
    -- | The declarations of the vendor module, at a lower level than
    -- 'LibraryVendorModule', since there can be hundreds of them
  | LibraryVendorDecls Hs.ModuleName (NonEmpty C.DeclId)
    -- | The files the run writes differ from the files the plan predicted.
    -- Carries the planned paths and the written paths. This is a bug in
    -- @hs-bindgen@: the plan decides output path collisions and the dry-run
    -- listing.
  | LibraryOutputMismatch [FilePath] [FilePath]
  deriving stock (Show)

instance PrettyForTrace LibraryMsg where
  prettyForTrace = \case
    LibraryEmptyUnit modName path -> hsep [
        "Library module"
      , string (Hs.moduleNameToString modName)
      , "for header"
      , string (getRealPath path)
      , "has no declarations after filtering"
      ]
    LibraryAutoBootFiles -> hsep [
        "Some types modules form an import cycle."
      , "Generating boot files for them automatically"
      ]
    LibraryVendorModule modName count -> hsep [
        "Library module"
      , string (Hs.moduleNameToString modName)
      , "holds"
      , string (show count)
      , if count == 1 then "declaration" else "declarations"
      , "from outside the --library directories"
      ]
    LibraryVendorDecls modName declIds ->
      hsep [
          "Library module"
        , string (Hs.moduleNameToString modName)
        , "holds:"
        ]
      $$ nest 2 (vcat (map (text . C.renderDeclId) (NonEmpty.toList declIds)))
    LibraryOutputMismatch planned written ->
      "The files written differ from the library plan"
      $$ nest 2 (vcat [
          title $$ nest 2 (vcat (map string paths))
        | (title, paths) <- [
              ("planned, not written:", planned List.\\ written)
            , ("written, not planned:", written List.\\ planned)
            ]
        , not (null paths)
        ])

instance IsTrace Level LibraryMsg where
  getDefaultLogLevel = \case
    LibraryEmptyUnit{}              -> Notice
    LibraryAutoBootFiles            -> Notice
    LibraryVendorModule{}           -> Notice
    LibraryVendorDecls{}            -> Info
    LibraryOutputMismatch{}         -> Bug
  getSource = const HsBindgen
  getTraceId = const "library"

{-------------------------------------------------------------------------------
  Output
-------------------------------------------------------------------------------}

-- | Everything a library run generates
data LibraryResult = LibraryResult {
      -- | The generated files of each header unit
      units          :: [LibraryUnitOutput]
      -- | The global callback module, paired with its planned output file
    , globalCallback :: Maybe GeneratedFile
    }

-- | The generated files of one header unit (see "HsBindgen.Library")
data LibraryUnitOutput = LibraryUnitOutput {
      header :: Library.UnitHeader
      -- | Each planned output file paired with its rendered content
    , files  :: [GeneratedFile]
    }

-- | A planned output file paired with its rendered content
data GeneratedFile = GeneratedFile {
      file    :: Library.OutputFile
    , content :: FileContent
    }

-- | Every generated file in a library result: the files of each unit, then the
-- global callback module
resultFiles :: LibraryResult -> [GeneratedFile]
resultFiles result =
    concatMap (.files) result.units
    ++ maybeToList result.globalCallback

{-------------------------------------------------------------------------------
  Runner
-------------------------------------------------------------------------------}

-- | Run the full library-mode pipeline.
--
-- The caller is responsible for running @runFrontend@ and @runBoot@ and
-- passing their results here. The runner forces the @resolveBindingSpecs@
-- result from the frontend artefact, then takes over from @mangleNames@
-- through the backend.
--
-- The caller passes the same library directories to @runFrontend@ and here.
-- The frontend uses them to run Doxygen on the library headers. Without them,
-- only the declarations of the main headers get their Doxygen comments.
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
  -> Maybe Format
  -> IO (Either Library.PlanError LibraryResult)
runLibrary
  tracer
  tracerFrontend
  config
  boot
  frontend
  mrc
  libraryDirs
  baseModule
  mBindingSpecFormat
  = do
    canonicalLibraryDirs <- mapM canonicalizePath libraryDirs
    canonicalIncludeDirs <-
      mapM canonicalizePath config.boot.clangArgs.extraIncludeDirs

    tuResolveBindingSpecs <- getCached frontend.resolveBindingSpecs

    -- The include order sorts trace messages so that their order matches the
    -- order runFrontend uses. Without it, traces from the runner appear in
    -- processing order, which can differ from include order.
    parseMeta <- getCached frontend.parseMeta
    let includeOrder = IncludeGraph.toIncludeOrder parseMeta.includeGraph

        -- Sorted trace emission, matching runFrontend's traceSorted.
        traceSorted ::
             Tracer (C.WithLocationInfo msg)
          -> [WithCallStack (C.WithLocationInfo msg)]
          -> IO ()
        traceSorted tracer' msgs = do
          let (bugMsgs, sorted) = sortMsgs includeOrder msgs
          forM_ bugMsgs $ traceWith (contramap FrontendSortMsgs tracerFrontend)
          forM_ sorted  $ traceWith tracer'

    case Library.placeDecls canonicalLibraryDirs baseModule (DeclDB.active AnyOrder tuResolveBindingSpecs.db) of
      Left err        -> pure (Left err)
      Right placement -> do
        -- mangleNames with the placement.
        --
        -- The TU still contains every vendor declaration, also those that the
        -- selection drops later. For name mangling they all share the vendor
        -- module.
        --
        -- Two units with the same module name do share it here, so their names
        -- can still clash (known limitation, see 'Library.Placement').
        let (tuMangleNames, mangleMsgs) =
              mangleNames (Library.lookupModule placement) fns tuResolveBindingSpecs

        traceSorted (contramap FrontendMangleNames tracerFrontend) mangleMsgs

        -- translateTypes, selectDecls on the mangled TU.
        --
        -- Selection uses the same header predicates as single-header mode:
        -- the CLI headers are the main headers, and isLibraryHeader checks
        -- whether a header is under a --library directory. The user opts
        -- into whole-library selection with --select-from-library-headers.
        macroLang <- getCached boot.macroLang
        headerPreds <- getCached frontend.headerPredicates
        let tuTranslateTypes = translateTypes macroLang tuMangleNames

            selectCfg = SelectConfig {
                programSlicing     = config.frontend.programSlicing
              , selectionPredicate = config.frontend.selectionPredicate
              }
            -- Selection and placement are independent: the header predicates
            -- and selectionPredicate control which declarations are roots for
            -- program slicing, while the placement covers all library
            -- headers. If a user excludes header H via selectionPredicate,
            -- H's declarations can still be pulled in as transitive
            -- dependencies of another library header's roots, and they land
            -- in H's unit. Every vendor declaration that the selection keeps
            -- lands in the vendor module, as in single-header mode, which
            -- generates whatever the selection keeps.
            (tuSelect, selectMsgs) =
              selectDecls headerPreds selectCfg tuTranslateTypes

        traceSorted (contramap FrontendSelect tracerFrontend) selectMsgs

        -- The ToFunPtr and FromFunPtr instances of the whole library. The plan
        -- decides the module of each.
        let (funPtrResult, funPtrMsgs) = FunPtrInstances.funPtrInstances tuSelect
            tuFinal = funPtrResult.unit
            insts   = funPtrResult.instances

        traceSorted (contramap FrontendFunPtrInstances tracerFrontend) funPtrMsgs

        let planConfig = Library.PlanConfig {
                baseModule        = baseModule
              , categoryChoice    = config.backend.categoryChoice
              , bindingSpecFormat = mBindingSpecFormat
              }

        -- Planning rejects name and path collisions before the backend runs,
        -- so a rejected run writes nothing.
        case Library.planLibrary planConfig placement tuFinal insts of
          Left err   -> pure (Left err)
          Right plan ->
            Right <$> generate canonicalIncludeDirs placement tuFinal insts plan
  where
    fns = config.frontend.fieldNamingStrategy

    -- | Run the backend on the plan.
    generate ::
         [FilePath] -- ^ Include directories of the run (canonicalized)
      -> Library.Placement
      -> TranslationUnit l Final
      -> [FunPtrInstance] -- ^ In output order
      -> Library.Plan l
      -> IO LibraryResult
    generate includeDirs placement tuFinal insts plan = do
        -- Inform the user when cycles are present and boot files will be
        -- generated for the affected modules.
        unless (all (Set.null . (.sourcePeers)) plan.units) $
          traceWith tracer $ withCallStack LibraryAutoBootFiles

        -- Report what the vendor module holds: the count at the default
        -- verbosity, the declarations at a lower level.
        sequence_
          [ do traceWith tracer $ withCallStack $
                 LibraryVendorModule unit.moduleName (length decls)
               traceWith tracer $ withCallStack $
                 LibraryVendorDecls unit.moduleName (fmap (.id.cName) decls)
          | unit@Library.HeaderUnit{header = Library.VendorHeaders} <- plan.units
          , Just decls <- [NonEmpty.nonEmpty unit.decls]
          ]

        -- Trace the headers whose units write nothing.
        let plannedHeaders = Set.fromList (map (.header) plan.units)
        sequence_
          [ traceWith tracer $ withCallStack $
              LibraryEmptyUnit (Library.unitModuleName placement.vendorModule unitHeader) header
          | unitHeader@(Library.LibraryHeader header _)
              <- Set.toList (Library.unitHeaders placement)
          , not (Set.member unitHeader plannedHeaders)
          ]

        macroLang <- getCached boot.macroLang
        rootDirs  <- getCached boot.rootDirectives
        doxygen   <- getCached frontend.doxygen

        -- The format and the parse metadata, when the run writes binding
        -- specifications
        mSpecInputs <- forM mBindingSpecFormat $ \format -> do
          parseMeta <- getCached frontend.parseMeta
          pure (format, parseMeta)

        -- The instance map covers every active declaration, and so the
        -- declarations of every unit.
        let activeDecls =
              DeclDB.active
                (DeclUseGraph.outputOrder tuFinal.includeGraph)
                tuFinal.db

            -- TODO <https://github.com/well-typed/hs-bindgen/issues/1447>
            -- An instance resolution pass in the frontend would record the
            -- instances on the C declarations, so this extra translation would
            -- go away.
            (_allDecls, fullInstanceMap) =
              Hs.generateDeclarations
                config.backend.uniqueId
                (BaseModuleName baseModule.text)
                DoNotGenerateHsBoot
                mempty
                insts
                activeDecls

            -- Encode the binding specification of a unit. A unit gets one if
            -- it has at least one CType declaration or squashed typedef;
            -- omitted types alone do not get one.
            encodeBindingSpec ::
                 Library.HeaderUnit l -> [HsAST.Decl l] -> Maybe ByteString
            encodeBindingSpec unit ctypeDecls = do
              (format, parseMeta) <- mSpecInputs
              guard $ not (null ctypeDecls && Map.null unit.squashedIn)
              pure $ BindingSpec.genBindingSpec
                format
                unit.moduleName
                tuFinal.includeGraph
                tuFinal.db
                (specHeaders parseMeta.getMainHeaders)
                (Map.toList unit.omitted)
                (Map.toList unit.squashedIn)
                ctypeDecls

            specHeaders :: GetMainHeaders -> GetMainHeaders
            specHeaders getMainHeaders header =
              case Library.headerIncludeArgs includeDirs header of
                name : names -> Right (name NonEmpty.:| names)
                []           -> getMainHeaders header

            unitOutputs =
              [ processUnit
                  macroLang rootDirs doxygen fullInstanceMap
                            encodeBindingSpec unit
              | unit <- plan.units
              ]

        -- Render the global callback module when the plan has one. This module
        -- collects ToFunPtr/FromFunPtr instances for raw function-pointer
        -- types that would otherwise be duplicated across per-header callback
        -- sub-modules.
        let globalCallbackFile :: Maybe GeneratedFile
            globalCallbackFile = do
              (_callbackModName, callbackInsts) <- plan.globalCallback
              outputFile <- Library.globalCallbackOutput plan
              let globalBaseName = BaseModuleName baseModule.text

                  -- Put the callback declarations in the CCallback slot
                  -- so they land in a Callback sub-module.
                  globalCallbackDecls =
                    over (lensForCategory (CTerm CCallback))
                         (++ concatMap ToFromFunPtr.forInstance
                               (NonEmpty.toList callbackInsts))
                         mempty

                  -- Boot policy is irrelevant: hsBootDeclsFor returns
                  -- Nothing for CTerm categories, so CCallback never
                  -- gets a boot file regardless of this value.
                  globalBootPolicy = DoNotGenerateHsBoot

                  globalSHsDecls = SHs.translateDecls
                    macroLang baseModule globalCallbackDecls
                  globalFinalDecls = simplifySHs globalSHsDecls

                  globalModules = translateModuleMultiple
                    fns mrc globalBootPolicy rootDirs globalBaseName
                    [] -- no cross-module imports
                    (const []) -- no export tags
                    globalFinalDecls

              case view (lensForCategory (CTerm CCallback)) globalModules of
                Just (m, _) -> Just GeneratedFile {
                    file    = outputFile
                  , content = StringContent (render m)
                  }
                Nothing     -> Nothing

            result = LibraryResult {
                units          = unitOutputs
              , globalCallback = globalCallbackFile
              }

        -- The plan decides output path collisions and the dry-run listing,
        -- so it must predict exactly the files the run generates. Compare
        -- the generated file paths with the planned ones.
        let plannedPaths = List.sort
              [ file.path | file <- Library.planOutputs plan ]
            generatedPaths = List.sort
              [ gf.file.path | gf <- resultFiles result ]

        when (plannedPaths /= generatedPaths) $
          traceWith tracer $ withCallStack $
            LibraryOutputMismatch plannedPaths generatedPaths

        pure result

    -- | Run the backend pipeline for a single header unit.
    --
    -- The plan decides the boot policy of each unit: 'GenerateHsBoot' for a
    -- unit in an import cycle, so that its peers' @SOURCE@ imports find its
    -- boot file, 'DoNotGenerateHsBoot' otherwise. The binding specification
    -- comes from the unit's types, after the user's category choice.
    --
    -- Each rendered output is paired with its planned 'Library.OutputFile' by
    -- matching origin keys. A planned file whose origin has no content is
    -- dropped; 'generate' detects this through the mismatch check. Content
    -- whose origin has no planned file is silently discarded.
    processUnit ::
         Macro.Lang l
      -> [C.RootDirective C.HashIncludeArg]
      -> Doxygen
      -> Hs.InstanceMap
      -> (Library.HeaderUnit l -> [HsAST.Decl l] -> Maybe ByteString)
         -- ^ Encode the binding specification of a unit, given its types
      -> Library.HeaderUnit l
      -> LibraryUnitOutput
    processUnit
      macroLang rootDirs doxygen instanceMap encodeBindingSpec unit =
      let
        unitBaseModule = BaseModuleName unit.moduleName.text

        -- The instance map this run returns is discarded: 'generate' already
        -- built the map for all active declarations and passes it in.
        (hsDecls, _) =
          Hs.generateDeclarations
            config.backend.uniqueId
            unitBaseModule
            unit.hsBootPolicy
            instanceMap
            unit.instances
            unit.decls

        filteredDecls =
          applyBindingCategoryChoice config.backend.categoryChoice hsDecls

        sHsDecls   = SHs.translateDecls macroLang unit.moduleName filteredDecls
        finalDecls = simplifySHs sHsDecls

        crossModuleImports =
          [ QualifiedImportListItem SourceImport
              (fromBaseModuleName (BaseModuleName peer.text) (Just CType))
              Nothing
          | peer <- Set.toList unit.sourcePeers
          ]

        exportTags = computeExportTags doxygen unit.decls

        modulesByCategory = documentVendorModule $ translateModuleMultiple
          fns
          mrc
          unit.hsBootPolicy
          rootDirs
          unitBaseModule
          crossModuleImports
          (resolveExports exportTags)
          finalDecls

        -- The types module of the vendor declarations explains what they are
        documentVendorModule = case unit.header of
          Library.LibraryHeader _ _ -> id
          Library.VendorHeaders   ->
            over (lensForCategory CType)
              (fmap (first (withModuleDoc vendorModuleDoc)))

        renderedModules =
          fmap ((bimap render (renderHsBoot <$>)) <$>) modulesByCategory

        -- CType declarations for binding spec generation, after applying
        -- the user's binding category choice.
        ctypeDecls = view (lensForCategory CType) filteredDecls

        -- Build a map from each output origin to its rendered content.
        -- Module files and boot files come from the rendered categories;
        -- binding spec files come from the encoding.
        contentByOrigin :: Map.Map Library.OutputOrigin FileContent
        contentByOrigin = Map.fromList $ concat $
            Foldable.toList (mapWithCategory_ (categoryContent unit.header) renderedModules)
            ++ [ [ ( Library.BindingSpecFile unit.header
                   , ByteStringContent bs
                   )
                 ]
               | Just bs <- [encodeBindingSpec unit ctypeDecls]
               ]

        -- Pair each planned output file with its rendered content, keeping
        -- only files whose origin has content.
        pairedFiles :: [GeneratedFile]
        pairedFiles =
          [ GeneratedFile { file = outputFile, content = c }
          | outputFile <- NonEmpty.toList unit.outputs
          , Just c <- [Map.lookup outputFile.origin contentByOrigin]
          ]

      in LibraryUnitOutput {
             header = unit.header
           , files  = pairedFiles
           }

    -- | The rendered content for one binding category of a unit
    --
    -- Returns entries for the module file and, when present, the boot file.
    categoryContent ::
         Library.UnitHeader
      -> Category
      -> Maybe (String, Maybe String)
      -> [(Library.OutputOrigin, FileContent)]
    categoryContent _      _   Nothing                      = []
    categoryContent header cat (Just (source, mBootSource)) =
        (Library.HeaderModule header cat, StringContent source)
          : [ (Library.HeaderBootFile header, StringContent bootSource)
            | Just bootSource <- [mBootSource]
            ]

-- | The Haddock of the vendor module (see "HsBindgen.Library")
--
-- Its declarations keep the usual Haddocks, as in single-header mode.
vendorModuleDoc :: HsDoc.Comment
vendorModuleDoc = mconcat [
      HsDoc.title [
          HsDoc.TextContent "Copies of declarations from outside the"
        , HsDoc.monospace "--library"
        , HsDoc.TextContent "directories"
        ]
    , HsDoc.paragraph [
          HsDoc.TextContent
            "The selection keeps them, usually because program slicing pulls in what the library uses."
        ]
    , HsDoc.paragraph [
          HsDoc.TextContent
            "To share them between libraries, generate their bindings once, and pass that binding specification with"
        , HsDoc.monospace "--external-binding-spec"
        , HsDoc.TextContent "to each library run."
        ]
    ]
