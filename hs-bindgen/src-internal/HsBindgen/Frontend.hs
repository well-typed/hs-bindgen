module HsBindgen.Frontend (
    runFrontend
  , FrontendArtefact (..)
  , FrontendMsg(..)
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Backend.Category (Category (..))
import HsBindgen.Boot
import HsBindgen.Cache
import HsBindgen.Clang
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Analysis.AnonUsage qualified as AnonUsageAnalysis
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
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
import HsBindgen.Frontend.Pass.Parse.Decl.Monad qualified as ParseDecl
import HsBindgen.Frontend.Pass.Parse.IsPass
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
import HsBindgen.Language.Haskell qualified as Hs
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
-- == 8. "HsBindgen.Frontend.Pass.Select"
--
-- "HsBindgen.Frontend.Pass.Select" filters the declarations using predicates
-- and program slicing.  It also emits delayed trace messages for declarations
-- that are selected.
runFrontend ::
     Tracer FrontendMsg
  -> FrontendConfig
  -> BootArtefact
  -> IO FrontendArtefact
runFrontend tracer config boot = do
    parsePass <- cache "parse" $ fmap (fromMaybe emptyParseResult) $ do
      setup <- getSetup
      rootHeader <- getRootHeader
      liftIO $ withClang (contramap FrontendClang tracer) setup $ \unit -> Just <$> do
        (includeGraph, isMainHeader, isInMainHeaderDir, getMainHeadersAndInclude) <-
          processIncludes unit
        let parseEnv :: ParseDecl.Env
            parseEnv = ParseDecl.Env{
                unit                     = unit
              , rootHeader               = rootHeader
              , isMainHeader             = isMainHeader
              , isInMainHeaderDir        = isInMainHeaderDir
              , getMainHeadersAndInclude = getMainHeadersAndInclude
              , predicate                = config.parsePredicate
              , tracer                   = contramap FrontendParse tracer
              }
        parseResults <- parseDecls parseEnv unit

        let decls :: [C.Decl Parse]
            decls = mapMaybe getParseResultMaybeDecl parseResults
            usageAnalysis = AnonUsageAnalysis.fromDecls decls

        pure
          ( parseResults
          , includeGraph
          , isMainHeader
          , isInMainHeaderDir
          , toGetMainHeaders getMainHeadersAndInclude
          , usageAnalysis
          )

    simplifyASTPass <- cache "simplifyAST" $ do
      (afterParse, _, _, _, _, ua) <- parsePass
      let (afterSimplifyAST, msgsSimplifyAST) = simplifyAST ua afterParse
      forM_ msgsSimplifyAST $ traceWith tracer . FrontendSimplifyAST
      pure afterSimplifyAST

    assignAnonIdsPass <- cache "assignAnonIds" $ do
      (_, _, _, _, _, ua) <- parsePass
      afterSimplifyAST <- simplifyASTPass
      let (afterAssignAnonIds, msgsAssignAnonIds) = assignAnonIds ua afterSimplifyAST
      forM_ msgsAssignAnonIds $ traceWith tracer . FrontendAssignAnonIds
      pure afterAssignAnonIds

    constructTranslationUnitPass <- cache "constructTranslationUnit" $ do
      (_, includeGraph, _, _, _, _) <- parsePass
      afterAssignAnonIds <- assignAnonIdsPass
      let afterConstructTranslationUnit =
            constructTranslationUnit afterAssignAnonIds includeGraph
      pure afterConstructTranslationUnit

    handleMacrosPass <- cache "handleMacros" $ do
      afterConstructTranslationUnit <- constructTranslationUnitPass
      let (afterHandleMacros, msgsHandleMacros) =
            handleMacros boot.cStandard afterConstructTranslationUnit
      forM_ msgsHandleMacros $ traceWith tracer . FrontendHandleMacros
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
      forM_ msgsResolveBindingSpecs $ traceWith tracer . FrontendResolveBindingSpecs
      pure afterResolveBindingSpecs

    mangleNamesPass <- cache "mangleNames" $ do
      afterResolveBindingSpecs <- resolveBindingSpecsPass
      let (afterMangleNames, msgsMangleNames) = mangleNames afterResolveBindingSpecs
      forM_ msgsMangleNames $ traceWith tracer . FrontendMangleNames
      pure afterMangleNames

    selectPass <- cache "select" $ do
      (_, _, isMainHeader, isInMainHeaderDir, _, _) <- parsePass
      afterMangleNamesPass <- mangleNamesPass
      let (afterSelect, msgsSelect) =
            selectDecls
              isMainHeader
              isInMainHeaderDir
              selectConfig
              afterMangleNamesPass
      forM_ msgsSelect $ traceWith tracer . FrontendSelect
      pure afterSelect

    -- Unit.
    getCTranslationUnit <- cache "getCTranslationUnit" $ do
      afterMangleNames <- selectPass
      pure $ afterMangleNames

    -- Include graph predicate.
    getIncludeGraphP <- cache "getIncludeGraphP" $ do
      (_, _, isMainHeader, isInMainHeaderDir, _, _) <- parsePass
      pure $ \path ->
        matchParse isMainHeader isInMainHeaderDir path config.parsePredicate
        && path /= RootHeader.name

    -- Graphs.
    frontendIncludeGraph <- cache "frontendIncludeGraph" $ do
      includeGraphP <- getIncludeGraphP
      (_, includeGraph, _, _, _, _) <- parsePass
      pure (includeGraphP, includeGraph)
    frontendGetMainHeaders <- cache "frontendGetMainHeaders" $ do
      (_, _, _, _, getMainHeaders, _) <- parsePass
      pure getMainHeaders
    frontendIndex <- cache "frontendIndex" $ do
      (.ann.declIndex) <$> constructTranslationUnitPass
    frontendUseDeclGraph <- cache "frontendUseDeclGraph" $ do
      (.ann.useDeclGraph) <$> constructTranslationUnitPass
    frontendDeclUseGraph <- cache "frontendDeclUseGraph" $ do
      (.ann.declUseGraph) <$> constructTranslationUnitPass

    -- Omitted types
    frontendOmitTypes <- cache "frontendOmitTypes" $
      Map.toList . DeclIndex.getOmitted . view ( #ann % #declIndex ) <$>
        resolveBindingSpecsPass

    -- Declarations.
    frontendCDecls <- cache "frontendDecls" $
      (.decls) <$> getCTranslationUnit

    -- Squashed types
    --
    -- TODO https://github.com/well-typed/hs-bindgen/issues/1549: `getSquashed`
    -- should probably be changed or removed (or at least not used when
    -- generating binding specifications).
    frontendSquashedTypes <- cache "frontendSquashedTypes" $ do
      decls <- frontendCDecls
      let translatedDeclIds = Set.fromList $ map (.info.id.cName) decls
      declIndex <- view ( #ann % #declIndex ) <$> selectPass
      pure $ Map.toList $ DeclIndex.getSquashed declIndex translatedDeclIds

    -- Dependencies.
    frontendDependencies <- cache "frontendDependencies" $ do
      IncludeGraph.toSortedList . (.includeGraph) <$> getCTranslationUnit

    pure FrontendArtefact{
        includeGraph   = frontendIncludeGraph
      , getMainHeaders = frontendGetMainHeaders
      , index          = frontendIndex
      , useDeclGraph   = frontendUseDeclGraph
      , declUseGraph   = frontendDeclUseGraph
      , omitTypes      = frontendOmitTypes
      , cDecls         = frontendCDecls
      , squashedTypes  = frontendSquashedTypes
      , dependencies   = frontendDependencies
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
        , parsePredicate  = config.parsePredicate
        , selectPredicate = config.selectPredicate
        }

    emptyParseResult :: (
        [ParseResult Parse]
      , IncludeGraph
      , IsMainHeader
      , IsInMainHeaderDir
      , GetMainHeaders
      , AnonUsageAnalysis.AnonUsageAnalysis
      )
    emptyParseResult =
      ( []
      , IncludeGraph.empty
      , const False
      , const False
      , const (Left "empty")
      , AnonUsageAnalysis.AnonUsageAnalysis { map = Map.empty }
      )

    cache :: String -> Cached a -> IO (Cached a)
    cache = cacheWith (contramap (FrontendCache . SafeTrace) tracer) . Just

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data FrontendArtefact = FrontendArtefact {
      includeGraph   :: Cached (IncludeGraph.Predicate, IncludeGraph.IncludeGraph)
    , getMainHeaders :: Cached GetMainHeaders
    , index          :: Cached DeclIndex.DeclIndex
    , useDeclGraph   :: Cached UseDeclGraph.UseDeclGraph
    , declUseGraph   :: Cached DeclUseGraph.DeclUseGraph
    , omitTypes      :: Cached [(DeclId, SourcePath)]
    , cDecls         :: Cached [C.Decl Final]
    , squashedTypes  :: Cached [(DeclId, (SourcePath, Hs.Identifier))]
    , dependencies   :: Cached [SourcePath]
    }

{-------------------------------------------------------------------------------
  Trace
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
  | FrontendCache                    (SafeTrace CacheMsg)
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
