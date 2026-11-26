module HsBindgen.Frontend (
    runFrontend
  , FrontendArtefact (..)
  , FrontendMsg(..)
  , ParseInfo(..)
  ) where


import Prelude hiding (zip)

import Control.Exception (catch)
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as Map
import System.Directory (canonicalizePath)

import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Boot
import HsBindgen.Cache
import HsBindgen.Clang
import HsBindgen.Config.Internal
import HsBindgen.Doxygen
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UnnamedIdUsage (UnnamedIdUsageAnalysis)
import HsBindgen.Frontend.Analysis.UnnamedIdUsage qualified as UnnamedIdUsageAnalysis
import HsBindgen.Frontend.Pass.ApplyMacroFallback
import HsBindgen.Frontend.Pass.ApplyMacroFallback.IsPass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments
import HsBindgen.Frontend.Pass.EnrichComments.IsPass
import HsBindgen.Frontend.Pass.FillUnnamedIds
import HsBindgen.Frontend.Pass.FillUnnamedIds.IsPass
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.FunPtrInstances qualified as FunPtrInstances
import HsBindgen.Frontend.Pass.FunPtrInstances.IsPass
import HsBindgen.Frontend.Pass.MangleNames
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad.Decl qualified as ParseDecl
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.PrepareReparse
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
import HsBindgen.Frontend.Pass.ReparseMacroExpansions
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.SimplifyAST
import HsBindgen.Frontend.Pass.SimplifyAST.IsPass
import HsBindgen.Frontend.Pass.TranslateTypes
import HsBindgen.Frontend.Pass.TranslateTypes.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader (RootHeader, RootHeaderMsg)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Frontend.SortMsgs
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Macro.Type qualified as Macro
import HsBindgen.Macro.UniqueExpansion qualified as UniqueExpansion
import HsBindgen.Util.Tracer

import Doxygen.Parser (Doxygen, DoxygenException (..), Result (..),
                       emptyDoxygen, parse)

-- | Frontend
--
-- = Overview of passes
--
-- See the documentation of 'HsBindgen.IR.Pass.IsPass'.
--
-- == 1. "HsBindgen.Frontend.Pass.Parse"
--
-- "HsBindgen.Frontend.Pass.Parse" traverses the @libclang@ AST, getting all
-- information that we need from @libclang@ and constructing a pure Haskell
-- representation (see "HsBindgen.IR.C.Decl"). It runs in 'IO', to
-- interface with @libclang@.
--
-- Constraints:
--
-- * Must be first, to get the declarations from @libclang@
--
-- == 2. "HsBindgen.Frontend.Pass.SimplifyAST"
--
-- "HsBindgen.Frontend.Pass.SimplifyAST" simplifies the AST by converting
-- untagged enums without use sites into pattern synonym declarations. For
-- example, @enum { FOO, BAR }@ is converted into separate pattern synonym
-- declarations that will later be rendered as Haskell pattern synonyms.
--
-- Constraints:
--
-- * Must run before "HsBindgen.Frontend.Pass.FillUnnamedIds" because
--   FillUnnamedIds will delete unnamed declarations without use sites (it
--   needs a use site to determine a name)."
--
-- == 3. "HsBindgen.Frontend.Pass.FillUnnamedIds"
--
-- "HsBindgen.Frontend.Pass.FillUnnamedIds" assigns names to all unnamed
-- declarations, replacing 'HsBindgen.IR.C.Naming.PrelimDeclId' by
-- 'HsBindgen.IR.C.Naming.DeclId', which is used from this point forward.
--
-- Constraints:
--
-- * Must be before "HsBindgen.Frontend.Pass.ConstructTranslationUnit" so that
--   'HsBindgen.IR.C.Naming.DeclId' can be used as the key of the
--   'HsBindgen.Frontend.DeclDB.DeclDB' and the vertex of its dependency graph
-- * Must be before "HsBindgen.Frontend.Pass.ResolveBindingSpecs" so that
--   binding specifications can use the assigned names
--
-- == 4. "HsBindgen.Frontend.Pass.EnrichComments"
--
-- "HsBindgen.Frontend.Pass.EnrichComments" enriches parsed declarations with
-- doxygen comments by looking up each declaration in the 'Doxygen' state.
--
-- Constraints:
--
-- * Must be after "HsBindgen.Frontend.Pass.FillUnnamedIds" so that
--   'HsBindgen.IR.C.Naming.DeclId' is available for building
--   doxygen-qualified names
--
-- == 5. "HsBindgen.Frontend.Pass.ConstructTranslationUnit"
--
-- "HsBindgen.Frontend.Pass.ConstructTranslationUnit" constructs the
-- declaration database, 'HsBindgen.Frontend.DeclDB.DeclDB': an entry for every
-- parse result, and the dependency graph. From here on, each pass advances the
-- database.
--
-- Constraints:
--
-- * Must be before the rest of the passes because they read and advance the
--   database
--
-- == 6. "HsBindgen.Frontend.Pass.TypecheckMacros"
--
-- "HsBindgen.Frontend.Pass.TypecheckMacros" collects known types and typechecks
-- all macros. Note that macros may neither refer to nor introduce new unnamed
-- declarations, so running "HsBindgen.Frontend.Pass.FillUnnamedIds" before
-- "HsBindgen.Frontend.Pass.TypecheckMacros" is fine.
--
-- Constraints:
--
-- * Must be before "HsBindgen.Frontend.Pass.ApplyMacroFallback", since
-- "HsBindgen.Frontend.Pass.TypecheckMacros" is the last stage of the macro
-- language that can give up on a macro.
-- * Must be before "HsBindgen.Frontend.Pass.ReparseMacroExpansions", because
--   "HsBindgen.Frontend.Pass.TypecheckMacros" typechecks macro-defined types
--   that are required to parse declarations with macro expansions.
--
-- == 7. "HsBindgen.Frontend.Pass.ApplyMacroFallback"
--
-- "HsBindgen.Frontend.Pass.ApplyMacroFallback" decides whether to drop a macro
-- that the macro language did not handle or to fall back to generating a
-- binding which contains the raw tokens of the macro.
--
-- Constraints:
--
-- * Must be before "HsBindgen.Frontend.Pass.Select", which deselects the
--   dependents of the macros registered as unusable here.
--
-- == 8. "HsBindgen.Frontend.Pass.PrepareReparse"
--
-- @PrepareReparse@ prepares declarations that need to be reparsed because they
-- contain macro invocations. These declarations carry raw tokens intended to be
-- reparsed later, and the main goal of @PrepareReparse@ is to preprocess the
-- tokens to expand select macro invocations. If we leave macro invocations
-- unexpanded, then declarations that contain them will very likely fail to be
-- reparsed. In particular this is because the reparser expects its input to be
-- preprocessed. However, we do not expand a macro invocation if its definition
-- is identified (through parsing and typechecking) as a macro-defined type.
-- Such definitions are instead treated by the reparser as if they are
-- references to actual typedefs.
--
-- See issue #1225 for examples of how /not/ preprocessing caused problems in
-- the past:
--
-- <https://github.com/well-typed/hs-bindgen/issues/1225>
--
-- Constraints:
--
-- * Must be after @TypecheckMacros@, because @PrepareReparse@ needs to know
--   which macro definitions where identified as macro-defined types before we
--   can decide whether to expand macro invocations.
--
-- * Must be before @ReparseMacroExpansions@, because @ReparseMacroExpansions@
--   expects its reparser input to be preprocessed, which is what
--   @PrepareReparse@ takes care of.
--
-- == 9. "HsBindgen.Frontend.Pass.ReparseMacroExpansions"
--
-- "HsBindgen.Frontend.Pass.ReparseMacroExpansions" reparses declarations that
-- contain macro expansions.
--
-- == 10. "HsBindgen.Frontend.Pass.ResolveBindingSpecs"
--
-- "HsBindgen.Frontend.Pass.ResolveBindingSpecs" has two responsibilities:
--
-- * It matches declarations/uses with external binding specifications,
--   replacing matching declarations by 'HsBindgen.Frontend.DeclDB.External'
--   entries and uses by external references.
-- * It matches declarations with prescriptive binding specifications, either
--   omitting them or annotating the declarations with specifications to be used
--   in later passes.
--
-- Constraints:
--
-- * Must be before "HsBindgen.Frontend.Pass.Select" because prescriptive
--   binding specs may omit declarations and external binding specs may replace
--   declarations, and program slicing must take this into account
-- * Must be before "HsBindgen.Frontend.Pass.MangleNames" because prescriptive
--   binding specs may configure typedef squashing, which happens in
--   MangleNames.
-- * Must be before "HsBindgen.Frontend.Pass.MangleNames" because prescriptive
--   binding specs may specify arbitrary names
--
-- == 11. "HsBindgen.Frontend.Pass.MangleNames"
--
-- "HsBindgen.Frontend.Pass.MangleNames" assigns Haskell names for types,
-- constructors, fields, etc. It also deals with name clashes that can arise
-- from typedefs, squashing "unneeded" typedefs into
-- 'HsBindgen.Frontend.DeclDB.Squashed' entries. Declarations it cannot name
-- become unusable.
--
-- == 12. "HsBindgen.Frontend.Pass.TranslateTypes"
--
-- "HsBindgen.Frontend.Pass.TranslateTypes" translates C types (use sites) to
-- Haskell types.
--
-- == 13. "HsBindgen.Frontend.Pass.Select"
--
-- "HsBindgen.Frontend.Pass.Select" deselects declarations
-- ('HsBindgen.Frontend.DeclDB.Deselected') using predicates and program
-- slicing. It also emits delayed trace messages for declarations that are
-- selected.
--
-- Constraints:
--
-- * Must be after every pass that can make a declaration unusable
--   ('HsBindgen.Frontend.DeclDB.getUnusable'; e.g., it contains unsupported
--   types such as @long double@, or the name mangler was unable to find a
--   suitable name, etc.), so that the 'Select' pass can make sure that the
--   unusable declaration /and all declarations that use it/, directly or
--   transitively, will not be selected.
--
-- == 14. "HsBindgen.Frontend.Pass.FunPtrInstances"
--
-- "HsBindgen.Frontend.Pass.FunPtrInstances" decides which function types get
-- @ToFunPtr@ and @FromFunPtr@ instances. It returns them next to the
-- translation unit ('HsBindgen.Frontend.Pass.FunPtrInstances.Result').
--
-- Constraints:
--
-- * Must be after "HsBindgen.Frontend.Pass.Select": nested function types get
--   instances only if one of their types is selected, and messages concern
--   selected declarations only. Nothing depends on the instances, so no
--   declaration needs to fail here.
--
-- = Doxygen
--
-- Doxygen reads the main headers. It also reads every header the run reaches
-- under one of the library directories, so in library mode a header gets its
-- comments even when an umbrella header includes it. Single-header mode passes
-- no library directories, and Doxygen reads only the main headers.
runFrontend ::
     forall l. (Macro.HasTypes l, HasCallStack)
  => Tracer FrontendMsg
  -> FrontendConfig
  -> [FilePath]
     -- ^ Library directories, as given with @--library@. Empty in
     -- single-header mode.
  -> BootArtefact l
  -> IO (FrontendArtefact l)
runFrontend tracer config libraryDirs boot = do
    rootHeaderC <- cache "rootHeader" $ do
      (msgs, rootHeader) <- RootHeader.fromRootDirectives <$> boot.rootDirectives
      liftIO $ mapM_ (traceWith tracerRootHeader . withCallStack) msgs
      pure rootHeader

    parsePass <- cache "parse" $ do
      setup     <- getSetup =<< rootHeaderC
      macroLang <- boot.macroLang
      liftIO $ withClang (contramap FrontendClang tracer) setup $ \unit -> do
        (includeGraph, isMainHeader, isInMainHeaderDir, getMainHeadersAndInclude, mainHeaderPaths) <-
          processIncludes unit
        canonicalLibraryDirs <- mapM canonicalizePath libraryDirs
        let isLibraryHeader path =
              isJust (findLibraryDir canonicalLibraryDirs path)
        -- Run doxygen on the resolved main header paths, and on the library
        -- headers that the run reaches, to extract structured comments.  The
        -- paths come from clang's own include resolution (via
        -- processIncludes), so no separate path search is needed.
        let libraryHeaderPaths = [
                path
              | path <- IncludeGraph.toSortedList includeGraph
              , isLibraryHeader path
              , not (isMainHeader path)
              ]
            resolvedPaths =
              map getRealPath (mainHeaderPaths ++ libraryHeaderPaths)
            emptyResult   = Result {
                doxygen = emptyDoxygen, warnings = [], doxygenVersion = "unknown"
              }
        doxyResult <- case NE.nonEmpty resolvedPaths of
          Nothing -> pure emptyResult
          Just paths -> parse config.doxygenConfig paths
            `catch` \(e :: DoxygenException) -> do
              traceWith tracer $ withCallStack $ FrontendDoxygen $ DoxygenWarning e
              pure emptyResult

        -- Emit structured warnings for unsupported content
        forM_ doxyResult.warnings $ \w ->
          traceWith tracer $ withCallStack $ FrontendDoxygen $ DoxygenUnsupported w

        let parseEnv :: ParseDecl.Env
            parseEnv = ParseDecl.Env{
                unit                     = unit
              , getMainHeadersAndInclude = getMainHeadersAndInclude
              , emptyMacros              = config.emptyMacros
              , tracer                   = contramap FrontendParse tracer
              }
        (parseResults, macroAnalysis) <- parseDecls macroLang parseEnv

        let decls :: [C.Decl l Parse]
            decls = mapMaybe getParseResultMaybeDecl parseResults
            usageAnalysis = UnnamedIdUsageAnalysis.fromDecls decls

        pure $ ParsePassResult {
            results           = parseResults
          , doxygen           = doxyResult.doxygen
          , includeGraph      = includeGraph
          , headerPredicates  = HeaderPredicates {
                isMainHeader      = isMainHeader
              , isInMainHeaderDir = isInMainHeaderDir
              , isLibraryHeader   = isLibraryHeader
              }
          , getMainHeaders    = toGetMainHeaders getMainHeadersAndInclude
          , usageAnalysis     = usageAnalysis
          , macroAnalysis     = macroAnalysis
          }

    parseMeta <- cache "parseMeta" $ do
      afterParse <- parsePass
      pure ParseInfo {
        includeGraph   = afterParse.includeGraph
      , getMainHeaders = afterParse.getMainHeaders
      }

    includeOrderC <- cache "includeOrder" $
      IncludeGraph.toIncludeOrder . (.includeGraph) <$> parseMeta

    let -- Sorted, so that their order does not depend on the processing order
        traceSorted ::
             Tracer (C.WithLocationInfo msg)
          -> [WithCallStack (C.WithLocationInfo msg)]
          -> Cached ()
        traceSorted tracer' msgs = do
          order <- includeOrderC
          let (bugMsgs, sorted) = sortMsgs order msgs
          forM_ bugMsgs $ traceWith (contramap FrontendSortMsgs tracer)
          forM_ sorted  $ traceWith tracer'

    simplifyASTPass <- cache "simplifyAST" $ do
      afterParse <- parsePass
      let (afterSimplifyAST, msgsSimplifyAST) =
            simplifyAST afterParse.usageAnalysis afterParse.results
      forM_ msgsSimplifyAST $ traceWith (contramap FrontendSimplifyAST tracer)
      pure afterSimplifyAST

    fillUnnamedIdsPass <- cache "fillUnnamedIds" $ do
      afterParse       <- parsePass
      afterSimplifyAST <- simplifyASTPass
      let (afterFillUnnamedIds, msgsFillUnnamedIds) =
            fillUnnamedIds afterParse.usageAnalysis afterSimplifyAST
      forM_ msgsFillUnnamedIds $ traceWith (contramap FrontendFillUnnamedIds tracer)
      pure afterFillUnnamedIds

    enrichCommentsPass <- cache "enrichComments" $ do
      afterParse         <- parsePass
      afterFillUnnamedIds <- fillUnnamedIdsPass
      pure $ enrichComments afterParse.doxygen afterFillUnnamedIds

    constructTranslationUnitPass <- cache "constructTranslationUnit" $ do
      macroLang  <- boot.macroLang
      afterParse <- parsePass
      afterEnrichComments <- enrichCommentsPass
      let afterConstructTranslationUnit =
            constructTranslationUnit
              macroLang
              afterParse.macroAnalysis
              afterParse.headerPredicates.isMainHeader
              afterEnrichComments
              afterParse.includeGraph
      pure afterConstructTranslationUnit

    typecheckMacrosPass <- cache "typecheckMacros" $ do
      macroLang                     <- boot.macroLang
      afterConstructTranslationUnit <- constructTranslationUnitPass
      pure $ typecheckMacros macroLang afterConstructTranslationUnit

    applyMacroFallbackPass <- cache "applyMacroFallback" $ do
      afterParse                   <- parsePass
      (afterTypecheckMacros, _, _) <- typecheckMacrosPass
      pure $
        applyMacroFallback
          afterParse.headerPredicates
          config.macroFallback
          afterTypecheckMacros

    prepareReparsePass <- cache "prepareReparse" $ do
      afterApplyMacroFallback <- applyMacroFallbackPass
      clangExe <- boot.clangExe
      rootHeader <- rootHeaderC
      setup <- getSetup rootHeader
      liftIO $ prepareReparse
                  (contramap FrontendPrepareReparse tracer)
                  clangExe
                  setup
                  rootHeader
                  afterApplyMacroFallback

    reparseMacroExpansionsPass <- cache "reparseMacroExpansions" $ do
      (_, knownTypes, knownMacros) <- typecheckMacrosPass
      afterPrepareReparse <- prepareReparsePass
      cStd <- boot.cStandard
      let knownTypes' =
            Map.map
              (coercePass . coercePass @C.Type @TypecheckMacros @ApplyMacroFallback)
              knownTypes
      pure $ reparseMacroExpansions cStd knownTypes' knownMacros afterPrepareReparse

    resolveBindingSpecsPass <- cache "resolveBindingSpecs" $ do
      afterReparseMacroExpansions <- reparseMacroExpansionsPass
      extSpecs <- boot.externalBindingSpecs
      pSpec    <- boot.prescriptiveBindingSpec
      let moduleName = Hs.ModuleName boot.baseModule.text -- do not import Backend
          (afterResolveBindingSpecs, msgsResolveBindingSpecs) =
            resolveBindingSpecs moduleName extSpecs pSpec afterReparseMacroExpansions
      traceSorted (contramap FrontendResolveBindingSpecs tracer) msgsResolveBindingSpecs
      pure afterResolveBindingSpecs

    mangleNamesPass <- cache "mangleNames" $ do
      afterResolveBindingSpecs <- resolveBindingSpecsPass
      let moduleName = Hs.ModuleName boot.baseModule.text
          (afterMangleNames, msgsMangleNames) =
            mangleNames (const (Just moduleName)) config.fieldNamingStrategy afterResolveBindingSpecs
      traceSorted (contramap FrontendMangleNames tracer) msgsMangleNames
      pure afterMangleNames

    translateTypesPass <- cache "translateTypes" $ do
      afterMangleNamesPass <- mangleNamesPass
      macroLang            <- boot.macroLang
      pure $ translateTypes macroLang afterMangleNamesPass

    selectPass <- cache "select" $ do
      afterParse <- parsePass
      afterTranslateTypesPass <- translateTypesPass
      let (afterSelect, msgsSelect) =
            selectDecls
              afterParse.headerPredicates
              selectConfig
              afterTranslateTypesPass
      traceSorted (contramap FrontendSelect tracer) msgsSelect
      pure afterSelect

    funPtrInstancesPass <- cache "funPtrInstances" $ do
      afterSelect <- selectPass
      let (afterFunPtrInstances, msgsFunPtrInstances) =
            FunPtrInstances.funPtrInstances afterSelect
      traceSorted
        (contramap FrontendFunPtrInstances tracer)
        msgsFunPtrInstances
      pure afterFunPtrInstances

    finalPass <- cache "final" $ (.unit) <$> funPtrInstancesPass

    pure FrontendArtefact{
        parseMeta                = parseMeta
      , headerPredicates         = (.headerPredicates) <$> parsePass
      , parse                    = (.results) <$> parsePass
      , doxygen                  = (.doxygen) <$> parsePass
      , simplifyAST              = simplifyASTPass
      , fillUnnamedIds           = fillUnnamedIdsPass
      , enrichComments           = enrichCommentsPass
      , constructTranslationUnit = constructTranslationUnitPass
      , typecheckMacros          = (\(x,_,_) -> x) <$> typecheckMacrosPass
      , applyMacroFallback       = applyMacroFallbackPass
      , reparseMacroExpansions   = reparseMacroExpansionsPass
      , resolveBindingSpecs      = resolveBindingSpecsPass
      , mangleNames              = mangleNamesPass
      , translateTypes           = translateTypesPass
      , select                   = selectPass
      , funPtrInstances          = funPtrInstancesPass
      , final                    = finalPass
      }
  where
    getSetup :: RootHeader -> Cached ClangSetup
    getSetup rootHeader = do
      clangArgs <- boot.clangArgs
      let hContent = RootHeader.content rootHeader
          setup = defaultClangSetup clangArgs $ ClangInputMemory hFilePath hContent
      pure $ setup {
          flags = bitfieldEnum [
              CXTranslationUnit_DetailedPreprocessingRecord
            , CXTranslationUnit_IncludeAttributedTypes
            , CXTranslationUnit_VisitImplicitAttributes
            ]
        }

    hFilePath :: FilePath
    hFilePath = getSourcePath RootHeader.name

    selectConfig :: SelectConfig
    selectConfig = SelectConfig{
          programSlicing     = config.programSlicing
        , selectionPredicate = config.selectionPredicate
        }

    cache :: String -> Cached a -> IO (Cached a)
    cache = cacheWith (contramap (FrontendCache . SafeTrace) tracer) . Just

    tracerRootHeader :: Tracer RootHeaderMsg
    tracerRootHeader = contramap FrontendRootHeader tracer

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data FrontendArtefact l = FrontendArtefact {
      parseMeta                :: Cached ParseInfo
    , headerPredicates         :: Cached HeaderPredicates
    , parse                    :: Cached [ParseResult       l Parse]
    , doxygen                  :: Cached Doxygen
    , simplifyAST              :: Cached [ParseResult       l SimplifyAST]
    , fillUnnamedIds           :: Cached [ParseResult       l FillUnnamedIds]
    , enrichComments           :: Cached [ParseResult       l EnrichComments]
    , constructTranslationUnit :: Cached (C.TranslationUnit l ConstructTranslationUnit)
    , typecheckMacros          :: Cached (C.TranslationUnit l TypecheckMacros)
    , applyMacroFallback       :: Cached (C.TranslationUnit l ApplyMacroFallback)
    , reparseMacroExpansions   :: Cached (C.TranslationUnit l ReparseMacroExpansions)
    , resolveBindingSpecs      :: Cached (C.TranslationUnit l ResolveBindingSpecs)
    , mangleNames              :: Cached (C.TranslationUnit l MangleNames)
    , translateTypes           :: Cached (C.TranslationUnit l TranslateTypes)
    , select                   :: Cached (C.TranslationUnit l Select)
    , funPtrInstances          :: Cached (FunPtrInstances.Result l)
    , final                    :: Cached (C.TranslationUnit l Final)
    }

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

-- | Frontend trace messages
--
-- Most passes in the frontend have their own set of trace messages.
data FrontendMsg =
    FrontendClang                     ClangMsg
  | FrontendParse                    (Msg Parse)
  | FrontendSimplifyAST              (Msg SimplifyAST)
  | FrontendFillUnnamedIds           (Msg FillUnnamedIds)
  | FrontendPrepareReparse           (Msg PrepareReparse)
  | FrontendReparseMacroExpansions   (Msg ReparseMacroExpansions)
  | FrontendResolveBindingSpecs      (Msg ResolveBindingSpecs)
  | FrontendMangleNames              (Msg MangleNames)
  | FrontendSelect                   (Msg Select)
  | FrontendFunPtrInstances          (Msg FunPtrInstances)
  | FrontendSortMsgs                  SortMsgsMsg
  | FrontendCache                    (SafeTrace CacheMsg)
  | FrontendDoxygen                   DoxygenMsg
  | FrontendRootHeader                RootHeaderMsg
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Information useful for inspection as well as peripheral tasks.
--
-- Excluded from the parse pass result because there is no 'Show' instance.
data ParseInfo = ParseInfo {
      includeGraph      :: IncludeGraph
    , getMainHeaders    :: GetMainHeaders
    }

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

data ParsePassResult l = ParsePassResult {
      results           :: [ParseResult l Parse]
    , doxygen           :: Doxygen
    , includeGraph      :: IncludeGraph
    , headerPredicates  :: HeaderPredicates
    , getMainHeaders    :: GetMainHeaders
    , usageAnalysis     :: UnnamedIdUsageAnalysis
    , macroAnalysis     :: UniqueExpansion.Analysis
    }
