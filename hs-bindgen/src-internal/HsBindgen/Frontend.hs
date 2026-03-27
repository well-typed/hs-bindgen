module HsBindgen.Frontend (
    runFrontend
  , FrontendArtefact (..)
  , FrontendMsg(..)
  , ParseInfo(..)
  ) where


import Control.Exception (catch)
import Data.List.NonEmpty qualified as NE

import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Backend.Category (Category (..))
import HsBindgen.Boot
import HsBindgen.Cache
import HsBindgen.Clang
import HsBindgen.Config.Internal
import HsBindgen.Doxygen (DoxygenMsg (DoxygenUnsupported, DoxygenWarning))
import HsBindgen.Frontend.Analysis.AnonUsage (AnonUsageAnalysis)
import HsBindgen.Frontend.Analysis.AnonUsage qualified as AnonUsageAnalysis
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.AdjustTypes (adjustTypes)
import HsBindgen.Frontend.Pass.AdjustTypes.IsPass (AdjustTypes)
import HsBindgen.Frontend.Pass.AssignAnonIds
import HsBindgen.Frontend.Pass.AssignAnonIds.IsPass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.EnrichComments (enrichComments)
import HsBindgen.Frontend.Pass.EnrichComments.IsPass (EnrichComments)
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.MangleNames
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad.Decl qualified as ParseDecl
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.ReparseMacroExpansions
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.SimplifyAST
import HsBindgen.Frontend.Pass.SimplifyAST.IsPass
import HsBindgen.Frontend.Pass.TypecheckMacros
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer

import Doxygen.Parser (Doxygen, DoxygenException (..), Result (..),
                       emptyDoxygen, parse)

-- | Frontend
--
-- = Overview of passes
--
-- See the documentation of 'HsBindgen.Frontend.Pass.IsPass'.
--
-- == 1. "HsBindgen.Frontend.Pass.Parse"
--
-- "HsBindgen.Frontend.Pass.Parse" traverses the @libclang@ AST, getting all
-- information that we need from @libclang@ and constructing a pure Haskell
-- representation (see "HsBindgen.Frontend.AST.Decl").  It is the only pass
-- that runs in 'IO', to interface with @libclang@.
--
-- Constraints:
--
-- * Must be first, to get the declarations from @libclang@
--
-- == 2. "HsBindgen.Frontend.Pass.SimplifyAST"
--
-- "HsBindgen.Frontend.Pass.SimplifyAST" simplifies the AST by converting
-- anonymous enums without use sites into pattern synonym declarations. For
-- example, @enum { FOO, BAR }@ is converted into separate pattern synonym
-- declarations that will later be rendered as Haskell pattern synonyms.
--
-- Constraints:
--
-- * Must run before "HsBindgen.Frontend.Pass.AssignAnonIds" because
--   AssignAnonIds will delete anonymous declarations without use sites (it
--   needs a use site to determine a name)."
--
-- == 3. "HsBindgen.Frontend.Pass.AssignAnonIds"
--
-- "HsBindgen.Frontend.Pass.AssignAnonIds" assigns names to all anonymous
-- declarations, replacing 'HsBindgen.Frontend.Naming.PrelimDeclId' by
-- 'HsBindgen.Frontend.Naming.DeclId', which is used from this point forward.
--
-- Constraints:
--
-- * Must be before "HsBindgen.Frontend.Pass.ConstructTranslationUnit" so that
--   'HsBindgen.Frontend.Naming.DeclId' can be used as the key for the
--   'DeclIndex.DeclIndex', 'UseDeclGraph.UseDeclGraph', and
--   'DeclUseGraph.DeclUseGraph'
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
-- * Must be after "HsBindgen.Frontend.Pass.AssignAnonIds" so that
--   'HsBindgen.Frontend.Naming.DeclId' is available for the 'DeclIndex' and
--   for building doxygen-qualified names
--
-- == 5. "HsBindgen.Frontend.Pass.ConstructTranslationUnit"
--
-- "HsBindgen.Frontend.Pass.ConstructTranslationUnit" constructs a list of
-- sorted declarations as well as 'DeclIndex.DeclIndex',
-- 'UseDeclGraph.UseDeclGraph', and 'DeclUseGraph.DeclUseGraph'.
--
-- Constraints:
--
-- * Must be before the rest of the passes because they use these structures and
--   depend on the ordering of declarations
--
-- == 6. "HsBindgen.Frontend.Pass.TypecheckMacros"
--
-- "HsBindgen.Frontend.Pass.TypecheckMacros" collects known types and typechecks
-- all macros. Note that macros may neither refer to nor introduce new anonymous
-- declarations, so running "HsBindgen.Frontend.Pass.AssignAnonIds" before
-- "HsBindgen.Frontend.Pass.TypecheckMacros" is fine.
--
-- * Must be before "HsBindgen.Frontend.Pass.ReparseMacroExpansions", because
--   "HsBindgen.Frontend.Pass.TypecheckMacros" typechecks macro-defined types
--   that are required to parse declarations with macro expansions.
--
-- == 7. "HsBindgen.Frontend.Pass.ReparseMacroExpansions"
--
-- "HsBindgen.Frontend.Pass.ReparseMacroExpansions" reparses declarations that
-- contain macro expansions.
--
-- Constraints:
--
-- * Must be before "HsBindgen.Frontend.Pass.AdjustTypes", because
--   "HsBindgen.Frontend.Pass.ReparseMacroExpansions" reparses declarations
--   referencing macro-defined types that may have to be adjusted.
--
-- == 8. "HsBindgen.Frontend.Pass.ResolveBindingSpecs"
--
-- "HsBindgen.Frontend.Pass.ResolveBindingSpecs" has two responsibilities:
--
-- * It matches declarations/uses with external binding specifications, removing
--   matching declarations and replacing uses with external references.
-- * It matches declarations with prescriptive binding specifications, either
--   omitting them or annotating the declarations with specifications to be used
--   in later passes.
--
-- Constraints:
--
-- * Must be before "HsBindgen.Frontend.Pass.Select" because prescriptive
--   binding specs may omit declarations and external binding specs may remove
--   declarations, and program slicing must take this into account
-- * Must be before "HsBindgen.Frontend.Pass.MangleNames" because prescriptive
--   binding specs may configure typedef squashing, which happens in
--   MangleNames.
-- * Must be before "HsBindgen.Frontend.Pass.MangleNames" because prescriptive
--   binding specs may specify arbitrary names
--
-- == 9. "HsBindgen.Frontend.Pass.MangleNames"
--
-- "HsBindgen.Frontend.Pass.MangleNames" assigns Haskell names for types,
-- constructors, fields, etc. It also deals with name clashes that can arise
-- from typedefs, squashing "unneeded" typedefs.
--
-- == 10. "HsBindgen.Frontend.Pass.AdjustTypes"
--
-- "HsBindgen.Frontend.Pass.AdjustTypes" adjusts types in declarations. For
-- example, if a function argument is a function type, then it is adjusted to a
-- function /pointer/ type.
--
-- == 11. "HsBindgen.Frontend.Pass.Select"
--
-- "HsBindgen.Frontend.Pass.Select" filters the declarations using predicates
-- and program slicing. It also emits delayed trace messages for declarations
-- that are selected.
--
-- Constraints:
--
-- * The 'Select' pass must come last so that if a declaration is
--   'HsBindgen.Frontend.Analysis.DeclIndex.Unusable' for whatever reason (e.g.,
--   it contains unsupported types such as @long double@, or the name mangler
--   was unable to find a suitable name, etc.), the 'Select' pass can make sure
--   that the unusable declaration /and all of its dependencies/ will not be
--   selected.
runFrontend ::
     HasCallStack
  => Tracer FrontendMsg
  -> FrontendConfig
  -> BootArtefact
  -> IO FrontendArtefact
runFrontend tracer config boot = do
    parsePass <- cache "parse" $ do
      setup      <- getSetup
      cStd       <- boot.cStandard
      liftIO $ withClang (contramap FrontendClang tracer) setup $ \unit -> do
        (includeGraph, isMainHeader, isInMainHeaderDir, getMainHeadersAndInclude, mainHeaderPaths) <-
          processIncludes unit
        -- Run doxygen on the resolved main header paths to extract
        -- structured comments.  The paths come from clang's own include
        -- resolution (via processIncludes), so no separate path search
        -- is needed.
        let resolvedPaths = map getSourcePath mainHeaderPaths
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
              , cStandard                = cStd
              , getMainHeadersAndInclude = getMainHeadersAndInclude
              , tracer                   = contramap FrontendParse tracer
              }
        parseResults <- parseDecls parseEnv

        let decls :: [C.Decl Parse]
            decls = mapMaybe getParseResultMaybeDecl parseResults
            usageAnalysis = AnonUsageAnalysis.fromDecls decls

        pure $ ParsePassResult {
            results           = parseResults
          , doxygen           = doxyResult.doxygen
          , includeGraph      = includeGraph
          , isMainHeader      = isMainHeader
          , isInMainHeaderDir = isInMainHeaderDir
          , getMainHeaders    = toGetMainHeaders getMainHeadersAndInclude
          , usageAnalysis     = usageAnalysis
          }

    parseMeta <- cache "parseMeta" $ do
      afterParse <- parsePass
      pure ParseInfo {
        includeGraph   = afterParse.includeGraph
      , getMainHeaders = afterParse.getMainHeaders
      }

    simplifyASTPass <- cache "simplifyAST" $ do
      afterParse <- parsePass
      let (afterSimplifyAST, msgsSimplifyAST) =
            simplifyAST afterParse.usageAnalysis afterParse.results
      forM_ msgsSimplifyAST $ traceWith tracer . extendCallStackMsg FrontendSimplifyAST
      pure afterSimplifyAST

    assignAnonIdsPass <- cache "assignAnonIds" $ do
      afterParse <- parsePass
      afterSimplifyAST <- simplifyASTPass
      let (afterAssignAnonIds, msgsAssignAnonIds) =
            assignAnonIds afterParse.usageAnalysis afterSimplifyAST
      forM_ msgsAssignAnonIds $ traceWith tracer . extendCallStackMsg FrontendAssignAnonIds
      pure afterAssignAnonIds

    enrichCommentsPass <- cache "enrichComments" $ do
      afterParse <- parsePass
      afterAssignAnonIds <- assignAnonIdsPass
      pure $ enrichComments afterParse.doxygen afterAssignAnonIds

    constructTranslationUnitPass <- cache "constructTranslationUnit" $ do
      afterParse <- parsePass
      afterEnrichComments <- enrichCommentsPass
      let afterConstructTranslationUnit =
            constructTranslationUnit
              afterEnrichComments
              afterParse.includeGraph
      pure afterConstructTranslationUnit

    typecheckMacrosPass <- cache "typecheckMacros" $ do
      afterConstructTranslationUnit <- constructTranslationUnitPass
      pure $ typecheckMacros afterConstructTranslationUnit

    reparseMacroExpansionsPass <- cache "reparseMacroExpansions" $ do
      (afterTypecheckMacros, knownTypes, knownMacroTypes) <- typecheckMacrosPass
      cStd <- boot.cStandard
      pure $ reparseMacroExpansions cStd knownTypes knownMacroTypes afterTypecheckMacros

    resolveBindingSpecsPass <- cache "resolveBindingSpecs" $ do
      afterReparseMacroExpansions <- reparseMacroExpansionsPass
      extSpecs <- boot.externalBindingSpecs
      pSpec    <- boot.prescriptiveBindingSpec
      let (afterResolveBindingSpecs, msgsResolveBindingSpecs) =
            resolveBindingSpecs
              (fromBaseModuleName boot.baseModule (Just CType))
              extSpecs
              pSpec
              afterReparseMacroExpansions
      forM_ msgsResolveBindingSpecs $ traceWith tracer . extendCallStackMsg FrontendResolveBindingSpecs
      pure afterResolveBindingSpecs

    mangleNamesPass <- cache "mangleNames" $ do
      afterResolveBindingSpecs <- resolveBindingSpecsPass
      let (afterMangleNames, msgsMangleNames) =
            mangleNames config.fieldNamingStrategy afterResolveBindingSpecs
      forM_ msgsMangleNames $ traceWith tracer . extendCallStackMsg FrontendMangleNames
      pure afterMangleNames

    adjustTypesPass <- cache "AdjustTypes" $ do
      afterMangleNamesPass <- mangleNamesPass
      pure $ adjustTypes afterMangleNamesPass

    selectPass <- cache "select" $ do
      afterParse <- parsePass
      afterAdjustTypesPass <- adjustTypesPass
      let (afterSelect, msgsSelect) =
            selectDecls
              afterParse.isMainHeader
              afterParse.isInMainHeaderDir
              selectConfig
              afterAdjustTypesPass
      forM_ msgsSelect $ traceWith tracer . extendCallStackMsg FrontendSelect
      pure afterSelect

    finalPass <- cache "Final" $ selectPass

    pure FrontendArtefact{
        parseMeta                = parseMeta
      , parse                    = (.results) <$> parsePass
      , doxygen                  = (.doxygen) <$> parsePass
      , simplifyAST              = simplifyASTPass
      , assignAnonIds            = assignAnonIdsPass
      , enrichComments           = enrichCommentsPass
      , constructTranslationUnit = constructTranslationUnitPass
      , typecheckMacros          = (\(x,_,_) -> x) <$> typecheckMacrosPass
      , reparseMacroExpansions   = reparseMacroExpansionsPass
      , resolveBindingSpecs      = resolveBindingSpecsPass
      , mangleNames              = mangleNamesPass
      , adjustTypes              = adjustTypesPass
      , select                   = selectPass
      , final                    = finalPass
      }
  where
    getRootHeader :: Cached RootHeader
    getRootHeader = RootHeader.fromMainFiles <$> boot.hashIncludeArgs

    getSetup :: Cached ClangSetup
    getSetup = do
      clangArgs <- boot.clangArgs
      hContent <- RootHeader.content <$> getRootHeader
      let setup = defaultClangSetup clangArgs $ ClangInputMemory hFilePath hContent
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
          programSlicing  = config.programSlicing
        , selectPredicate = config.selectPredicate
        }

    cache :: String -> Cached a -> IO (Cached a)
    cache = cacheWith (contramap (FrontendCache . SafeTrace) tracer) . Just

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data FrontendArtefact = FrontendArtefact {
      parseMeta                :: Cached ParseInfo

    , parse                    :: Cached [ParseResult Parse]
    , doxygen                  :: Cached Doxygen
    , simplifyAST              :: Cached [ParseResult SimplifyAST]
    , assignAnonIds            :: Cached [ParseResult AssignAnonIds]
    , enrichComments           :: Cached [ParseResult EnrichComments]
    , constructTranslationUnit :: Cached (C.TranslationUnit ConstructTranslationUnit)
    , typecheckMacros          :: Cached (C.TranslationUnit TypecheckMacros)
    , reparseMacroExpansions   :: Cached (C.TranslationUnit ReparseMacroExpansions)
    , resolveBindingSpecs      :: Cached (C.TranslationUnit ResolveBindingSpecs)
    , mangleNames              :: Cached (C.TranslationUnit MangleNames)
    , adjustTypes              :: Cached (C.TranslationUnit AdjustTypes)
    , select                   :: Cached (C.TranslationUnit Select)
    , final                    :: Cached (C.TranslationUnit Final)
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
  | FrontendAssignAnonIds            (Msg AssignAnonIds)
  | FrontendResolveBindingSpecs      (Msg ResolveBindingSpecs)
  | FrontendMangleNames              (Msg MangleNames)
  | FrontendSelect                   (Msg Select)
  | FrontendCache                    (SafeTrace CacheMsg)
  | FrontendDoxygen                   DoxygenMsg
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

data ParsePassResult = ParsePassResult {
      results           :: [ParseResult Parse]
    , doxygen           :: Doxygen
    , includeGraph      :: IncludeGraph
    , isMainHeader      :: IsMainHeader
    , isInMainHeaderDir :: IsInMainHeaderDir
    , getMainHeaders    :: GetMainHeaders
    , usageAnalysis     :: AnonUsageAnalysis
    }
