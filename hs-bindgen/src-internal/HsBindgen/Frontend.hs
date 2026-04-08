module HsBindgen.Frontend (
    runFrontend
  , FrontendArtefact (..)
  , FrontendMsg(..)
  , ParseInfo(..)
  ) where


import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Backend.Category (Category (..))
import HsBindgen.Boot
import HsBindgen.Cache
import HsBindgen.Clang
import HsBindgen.Config.Internal
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
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.MangleNames
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad.Decl qualified as ParseDecl
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.ResolveBindingSpecs
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Pass.SimplifyAST
import HsBindgen.Frontend.Pass.SimplifyAST.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer

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
-- anonymous enums into pattern synonym declarations. For example, @enum { FOO, BAR }@
-- is converted into separate pattern synonym declarations that will later be rendered as
-- Haskell pattern synonyms.
--
-- Constraints:
--
-- * Must be before "HsBindgen.Frontend.Pass.AssignAnonIds" so that
--   unused anonymous declarations are not filtered
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
-- == 4. "HsBindgen.Frontend.Pass.ConstructTranslationUnit"
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
-- == 5. "HsBindgen.Frontend.Pass.HandleMacros"
--
-- "HsBindgen.Frontend.Pass.HandleMacros" typechecks macros and reparses
-- declarations with macros.  In order to construct the correct scope at any
-- point, ordering is critical.  Note that macros may neither refer to nor
-- introduce new anonymous declarations, so running
-- "HsBindgen.Frontend.Pass.AssignAnonIds" before
-- "HsBindgen.Frontend.Pass.HandleMacros" is fine.
--
-- == 6. "HsBindgen.Frontend.Pass.ResolveBindingSpecs"
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
-- == 7. "HsBindgen.Frontend.Pass.MangleNames"
--
-- "HsBindgen.Frontend.Pass.MangleNames" assigns Haskell names for types,
-- constructors, fields, etc. It also deals with name clashes that can arise
-- from typedefs, squashing "unneeded" typedefs.
--
-- == 8. "HsBindgen.Frontend.Pass.AdjustTypes"
--
-- "HsBindgen.Frontend.Pass.AdjustTypes" adjusts types in declarations. For
-- example, if a function argument is a function type, then it is adjusted to a
-- function /pointer/ type.
--
-- Constraints:
--
-- * Must be after "HsBindgen.Frontend.Pass.HandleMacros", because
--   "HsBindgen.Frontend.Pass.HandleMacros" parses and inserts macro-defined
--   types that may have to be adjusted.
--
-- == 9. "HsBindgen.Frontend.Pass.Select"
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
      setup <- getSetup
      rootHeader <- getRootHeader
      liftIO $ withClang (contramap FrontendClang tracer) setup $ \unit -> do
        (includeGraph, isMainHeader, isInMainHeaderDir, getMainHeadersAndInclude) <-
          processIncludes unit
        let parseEnv :: ParseDecl.Env
            parseEnv = ParseDecl.Env{
                unit                     = unit
              , rootHeader               = rootHeader
              , isMainHeader             = isMainHeader
              , isInMainHeaderDir        = isInMainHeaderDir
              , getMainHeadersAndInclude = getMainHeadersAndInclude
              , tracer                   = contramap FrontendParse tracer
              }
        parseResults <- parseDecls parseEnv unit

        let decls :: [C.Decl Parse]
            decls = mapMaybe getParseResultMaybeDecl parseResults
            usageAnalysis = AnonUsageAnalysis.fromDecls decls

        pure $ ParsePassResult {
            results           = parseResults
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

    constructTranslationUnitPass <- cache "constructTranslationUnit" $ do
      afterParse <- parsePass
      afterAssignAnonIds <- assignAnonIdsPass
      let afterConstructTranslationUnit =
            constructTranslationUnit afterAssignAnonIds afterParse.includeGraph
      pure afterConstructTranslationUnit

    handleMacrosPass <- cache "handleMacros" $ do
      afterConstructTranslationUnit <- constructTranslationUnitPass
      std <- boot.cStandard
      let (afterHandleMacros, msgsHandleMacros) =
            handleMacros std afterConstructTranslationUnit
      forM_ msgsHandleMacros $ traceWith tracer . extendCallStackMsg FrontendHandleMacros
      pure afterHandleMacros

    resolveBindingSpecsPass <- cache "resolveBindingSpecs" $ do
      afterHandleMacros <- handleMacrosPass
      extSpecs <- boot.externalBindingSpecs
      pSpec    <- boot.prescriptiveBindingSpec
      let (afterResolveBindingSpecs, msgsResolveBindingSpecs) =
            resolveBindingSpecs
              (fromBaseModuleName boot.baseModule (Just CType))
              extSpecs
              pSpec
              afterHandleMacros
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
      let (afterAdjustTypes, msgsAdjustTypes) =
            adjustTypes afterMangleNamesPass
      forM_ msgsAdjustTypes $ traceWith tracer . extendCallStackMsg FrontendAdjustTypes
      pure afterAdjustTypes

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
      , simplifyAST              = simplifyASTPass
      , assignAnonIds            = assignAnonIdsPass
      , constructTranslationUnit = constructTranslationUnitPass
      , handleMacros             = handleMacrosPass
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
    , simplifyAST              :: Cached [ParseResult SimplifyAST]
    , assignAnonIds            :: Cached [ParseResult AssignAnonIds]
    , constructTranslationUnit :: Cached (C.TranslationUnit ConstructTranslationUnit)
    , handleMacros             :: Cached (C.TranslationUnit HandleMacros)
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
  | FrontendConstructTranslationUnit (Msg ConstructTranslationUnit)
  | FrontendHandleMacros             (Msg HandleMacros)
  | FrontendResolveBindingSpecs      (Msg ResolveBindingSpecs)
  | FrontendMangleNames              (Msg MangleNames)
  | FrontendSelect                   (Msg Select)
  | FrontendAdjustTypes              (Msg AdjustTypes)
  | FrontendCache                    (SafeTrace CacheMsg)
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
    , includeGraph      :: IncludeGraph
    , isMainHeader      :: IsMainHeader
    , isInMainHeaderDir :: IsInMainHeaderDir
    , getMainHeaders    :: GetMainHeaders
    , usageAnalysis     :: AnonUsageAnalysis
    }
