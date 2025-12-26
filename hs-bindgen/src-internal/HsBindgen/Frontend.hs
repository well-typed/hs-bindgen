{-# LANGUAGE OverloadedLabels #-}

module HsBindgen.Frontend
  ( frontend
  , FrontendArtefact (..)
  , FrontendMsg(..)
  ) where

import Data.Map.Strict qualified as Map
import Optics.Core (view, (%))

import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Backend.Category (Category (..))
import HsBindgen.Boot
import HsBindgen.Cache
import HsBindgen.Clang
import HsBindgen.Config.Internal
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
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.ResolveBindingSpecs
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select
import HsBindgen.Frontend.Pass.Select.IsPass
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
-- == 2. "HsBindgen.Frontend.Pass.AssignAnonIds"
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
-- == 3. "HsBindgen.Frontend.Pass.ConstructTranslationUnit"
--
-- "HsBindgen.Frontend.Pass.ConstructTranslationUnit" constructs a list of
-- sorted declarations as well as the 'DeclIndex.DeclIndex',
-- 'UseDeclGraph.UseDeclGraph', and 'DeclUseGraph.DeclUseGraph'.
--
-- Constraints:
--
-- * Must be before the rest of the passes because they use these structures and
--   depend on the ordering of the declarations
--
-- == 4. "HsBindgen.Frontend.Pass.HandleMacros"
--
-- "HsBindgen.Frontend.Pass.HandleMacros" typechecks macros and reparses
-- declarations with macros.  In order to construct the correct scope at any
-- point, ordering is critical.  Note that macros may neither refer to nor
-- introduce new anonymous declarations, so running
-- "HsBindgen.Frontend.Pass.AssignAnonIds" before
-- "HsBindgen.Frontend.Pass.HandleMacros" is fine.
--
-- == 5. "HsBindgen.Frontend.Pass.ResolveBindingSpecs"
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
-- * Must be before "HsBindgen.Frontend.Pass.HandleTypedefs" because
--   prescriptive binding specs may configure squashing
-- * Must be before "HsBindgen.Frontend.Pass.MangleNames" because prescriptive
--   binding specs may specify arbitrary names
--
-- == 6. "HsBindgen.Frontend.Pass.Select"
--
-- "HsBindgen.Frontend.Pass.Select" filters the declarations using predicates
-- and program slicing.  It also emits delayed trace messages for declarations
-- that are selected.
--
-- Constraints:
--
-- * Must be before "HsBindgen.Frontend.Pass.HandleTypedefs" so that predicates
--   can refer to @typedef@s that are later squashed, which is especially
--   important when program slicing is enabled
--
-- == 7. "HsBindgen.Frontend.Pass.MangleNames"
--
-- "HsBindgen.Frontend.Pass.MangleNames" assigns Haskell names for types,
-- constructors, fields, etc. It also deals with name clashes that can arise
-- from typedefs, squashing "unneeded" typedefs.
frontend ::
     Tracer FrontendMsg
  -> FrontendConfig
  -> BootArtefact
  -> IO FrontendArtefact
frontend tracer FrontendConfig{..} BootArtefact{..} = do
    parsePass <- cache "parse" $ fmap (fromMaybe emptyParseResult) $ do
      setup <- getSetup
      rootHeader <- getRootHeader
      liftIO $ withClang (contramap FrontendClang tracer) setup $ \unit -> Just <$> do
        (includeGraph, isMainHeader, isInMainHeaderDir, getMainHeadersAndInclude) <-
          processIncludes unit
        parseResults <- parseDecls
          (contramap FrontendParse tracer)
          rootHeader
          frontendParsePredicate
          isMainHeader
          isInMainHeaderDir
          getMainHeadersAndInclude
          unit
        pure
          ( parseResults
          , includeGraph
          , isMainHeader
          , isInMainHeaderDir
          , toGetMainHeaders getMainHeadersAndInclude
          )

    assignAnonIdsPass <- cache "assignAnonIds" $ do
      (afterParse, _, _, _, _) <- parsePass
      let (afterAssignAnonIds, msgsAssignAnonIds) = assignAnonIds afterParse
      forM_ msgsAssignAnonIds $ traceWith tracer . FrontendAssignAnonIds
      pure afterAssignAnonIds

    constructTranslationUnitPass <- cache "constructTranslationUnit" $ do
      (_, includeGraph, _, _, _) <- parsePass
      afterAssignAnonIds <- assignAnonIdsPass
      let (afterConstructTranslationUnit, msgsConstructTranslationUnit) =
            constructTranslationUnit afterAssignAnonIds includeGraph
      forM_ msgsConstructTranslationUnit $
        traceWith tracer . FrontendConstructTranslationUnit
      pure afterConstructTranslationUnit

    handleMacrosPass <- cache "handleMacros" $ do
      afterConstructTranslationUnit <- constructTranslationUnitPass
      let (afterHandleMacros, msgsHandleMacros) =
            handleMacros bootCStandard afterConstructTranslationUnit
      forM_ msgsHandleMacros $ traceWith tracer . FrontendHandleMacros
      pure afterHandleMacros

    resolveBindingSpecsPass <- cache "resolveBindingSpecs" $ do
      afterHandleMacros <- handleMacrosPass
      target   <- bootTarget
      extSpecs <- bootExternalBindingSpecs
      pSpec    <- bootPrescriptiveBindingSpec
      let (afterResolveBindingSpecs, msgsResolveBindingSpecs) =
            resolveBindingSpecs
              target
              (fromBaseModuleName bootBaseModule (Just CType))
              extSpecs
              pSpec
              afterHandleMacros
      forM_ msgsResolveBindingSpecs $ traceWith tracer . FrontendResolveBindingSpecs
      pure afterResolveBindingSpecs

    selectPass <- cache "select" $ do
      (_, _, isMainHeader, isInMainHeaderDir, _) <- parsePass
      afterResolveBindingSpecs <- resolveBindingSpecsPass
      let (afterSelect, msgsSelect) =
            selectDecls
              isMainHeader
              isInMainHeaderDir
              selectConfig
              afterResolveBindingSpecs
      forM_ msgsSelect $ traceWith tracer . FrontendSelect
      pure afterSelect

    mangleNamesPass <- cache "mangleNames" $ do
      afterSelect <- selectPass
      let (afterMangleNames, msgsMangleNames) = mangleNames afterSelect
      forM_ msgsMangleNames $ traceWith tracer . FrontendMangleNames
      pure afterMangleNames

    -- Unit.
    getCTranslationUnit <- cache "getCTranslationUnit" $ do
      afterMangleNames <- mangleNamesPass
      pure $ afterMangleNames

    -- Include graph predicate.
    getIncludeGraphP <- cache "getIncludeGraphP" $ do
      (_, _, isMainHeader, isInMainHeaderDir, _) <- parsePass
      pure $ \path ->
        matchParse isMainHeader isInMainHeaderDir path frontendParsePredicate
        && path /= RootHeader.name

    -- Graphs.
    frontendIncludeGraph <- cache "frontendIncludeGraph" $ do
      includeGraphP <- getIncludeGraphP
      (_, includeGraph, _, _, _) <- parsePass
      pure (includeGraphP, includeGraph)
    frontendGetMainHeaders <- cache "frontendGetMainHeaders" $ do
      (_, _, _, _, getMainHeaders) <- parsePass
      pure getMainHeaders
    frontendIndex <- cache "frontendIndex" $
      declIndex   . C.unitAnn <$> constructTranslationUnitPass
    frontendUseDeclGraph <- cache "frontendUseDeclGraph" $
      declUseDecl . C.unitAnn <$> constructTranslationUnitPass
    frontendDeclUseGraph <- cache "frontendDeclUseGraph" $
      declDeclUse . C.unitAnn <$> constructTranslationUnitPass

    -- Omitted types
    frontendOmitTypes <- cache "frontendOmitTypes" $
      Map.toList . DeclIndex.getOmitted . view ( #unitAnn % #declIndex) <$>
        resolveBindingSpecsPass

    -- Squashed types
    frontendSquashedTypes <- cache "frontendSquashedTypes" $
      Map.toList . DeclIndex.getSquashed . view ( #unitAnn % #declIndex) <$>
        mangleNamesPass

    -- Declarations.
    frontendCDecls <- cache "frontendDecls" $
      C.unitDecls <$> getCTranslationUnit

    -- Dependencies.
    frontendDependencies <- cache "frontendDependencies" $
      unitDeps <$> getCTranslationUnit

    pure FrontendArtefact{..}
  where
    getRootHeader :: Cached RootHeader
    getRootHeader = RootHeader.fromMainFiles <$> bootHashIncludeArgs

    getSetup :: Cached ClangSetup
    getSetup = do
      clangArgs <- bootClangArgs
      hContent <- RootHeader.content <$> getRootHeader
      let setup = defaultClangSetup clangArgs $ ClangInputMemory hFilePath hContent
      pure $ setup {
          clangFlags = bitfieldEnum [
            CXTranslationUnit_DetailedPreprocessingRecord
          , CXTranslationUnit_IncludeAttributedTypes
          , CXTranslationUnit_VisitImplicitAttributes
          ]
        }

    hFilePath :: FilePath
    hFilePath = getSourcePath RootHeader.name

    selectConfig :: SelectConfig
    selectConfig =
      SelectConfig
        frontendProgramSlicing
        frontendParsePredicate
        frontendSelectPredicate

    emptyParseResult :: (
        [ParseResult Parse]
      , IncludeGraph
      , IsMainHeader
      , IsInMainHeaderDir
      , GetMainHeaders
      )
    emptyParseResult =
      ( []
      , IncludeGraph.empty
      , const False
      , const False
      , const (Left "empty")
      )

    cache :: String -> Cached a -> IO (Cached a)
    cache = cacheWith (contramap (FrontendCache . SafeTrace) tracer) . Just

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data FrontendArtefact = FrontendArtefact {
    frontendIncludeGraph   :: Cached (IncludeGraph.Predicate, IncludeGraph.IncludeGraph)
  , frontendGetMainHeaders :: Cached GetMainHeaders
  , frontendIndex          :: Cached DeclIndex.DeclIndex
  , frontendUseDeclGraph   :: Cached UseDeclGraph.UseDeclGraph
  , frontendDeclUseGraph   :: Cached DeclUseGraph.DeclUseGraph
  , frontendOmitTypes      :: Cached [(DeclId, SourcePath)]
  , frontendSquashedTypes  :: Cached [(DeclId, (SourcePath, Hs.Identifier))]
  , frontendCDecls         :: Cached [C.Decl Final]
  , frontendDependencies   :: Cached [SourcePath]
  }

{-------------------------------------------------------------------------------
  Trace
-------------------------------------------------------------------------------}

-- | Frontend trace messages
--
-- Most passes in the frontend have their own set of trace messages.
data FrontendMsg =
    FrontendClang ClangMsg
  | FrontendParse (Msg Parse)
  | FrontendAssignAnonIds (Msg AssignAnonIds)
  | FrontendConstructTranslationUnit (Msg ConstructTranslationUnit)
  | FrontendHandleMacros (Msg HandleMacros)
  | FrontendResolveBindingSpecs (Msg ResolveBindingSpecs)
  | FrontendSelect (Msg Select)
  | FrontendMangleNames (Msg MangleNames)
  | FrontendCache (SafeTrace CacheMsg)
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
