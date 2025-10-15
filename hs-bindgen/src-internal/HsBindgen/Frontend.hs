module HsBindgen.Frontend
  ( frontend
  , FrontendArtefact (..)
  , FrontendMsg(..)
  ) where

import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Boot
import HsBindgen.Cache
import HsBindgen.Clang
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.AST.Finalize
import HsBindgen.Frontend.AST.Internal hiding (Type)
import HsBindgen.Frontend.Pass hiding (Config)
import HsBindgen.Frontend.Pass.ConstructTranslationUnit
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass qualified as ConstructTranslationUnit
import HsBindgen.Frontend.Pass.HandleMacros
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.HandleTypedefs
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass
import HsBindgen.Frontend.Pass.MangleNames
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.NameAnon
import HsBindgen.Frontend.Pass.NameAnon.IsPass
import HsBindgen.Frontend.Pass.Parse
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer

-- | Frontend.
--
-- Overview of passes (see documentation of 'HsBindgen.Frontend.Pass.IsPass'):
--
-- 1. 'Parse' (impure; all other passes are pure)
-- 2. 'ConstructTranslationUnit'
-- 3. 'HandleMacros'
-- 4. 'NameAnon'
-- 5. 'ResolveBindingSpecs'
-- 6. 'Select'
-- 7. 'HandleTypedefs'
-- 8. 'MangleNames'
--
-- Although the passes and their order are subject to change, we have to honor
-- various constraints:
--
-- - 'ConstructTranslationUnit' must come before the following passes because we need to process
--   declarations before their uses.
--
-- - 'HandleMacros': The macro parser needs to know which things are in scope
--   (other macros as well as typedefs), so we must process declarations in the
--   right order; that is, 'handleMacros' must be done after sorting the
--   declarations.
--
--   In principle it could run before or after renaming: macros can neither
--   refer to nor introduce new anonymous declarations, so the relative ordering
--   of these two passes does not really matter. However, as part of renaming we
--   replace typedefs around anonymous structs by named structs:
--
--   > typedef struct { .. fields .. } foo;
--
--   On the C side however @foo@ must be referred to as @foo@, not @struct foo@;
--   to avoid confusion, it is therefore cleaner to run macro parsing and
--   declaration reparsing /prior/ to this transformation.
--
-- - 'NameAnon' must come before 'ResolveBindingSpecs' because it enables users
--   to configure anonymous types using our generated names for them.
--
-- - 'ResolveBindingSpecs' must come before 'HandleTypedefs' to enable users to
--   configure if a specific typedef is squashed for not.
--
-- - 'Select' must come after 'ResolveBindingSpecs' so that selection is done
--   after declarations have been omitted via prescriptive binding
--   specification.  It must come before 'HandleTypedefs' because selection
--   should be done before renaming to be consistent with 'ResolveBindingSpecs'.
--
-- - 'MangleNames': Name mangling depends on information from the binding spec,
--   and must therefore happen after 'ResolveBindingSpecs'. It could be put into
--   the 'Hs' phase, but we have to draw the line somewhere.
frontend ::
     Tracer IO FrontendMsg
  -> FrontendConfig
  -> BootArtefact
  -> IO FrontendArtefact
frontend tracer FrontendConfig{..} BootArtefact{..} = do
    parsePass <- cache "parse" $ fmap (fromMaybe emptyParseResult) $ do
      setup <- getSetup
      withClang (contramap FrontendClang tracer) setup $ \unit -> Just <$> do
        (includeGraph, isMainHeader, isInMainHeaderDir, getMainHeadersAndInclude) <-
          processIncludes unit
        rootHeader <- getRootHeader
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

    sortPass <- cache "sort" $ do
      (afterParse, includeGraph, _, _, _) <- parsePass
      let (afterConstructTranslationUnit, msgsConstructTranslationUnit) =
            constructTranslationUnit afterParse includeGraph
      forM_ msgsConstructTranslationUnit $ traceWith tracer . FrontendConstructTranslationUnit
      pure afterConstructTranslationUnit

    handleMacrosPass <- cache "handleMacros" $ do
      afterConstructTranslationUnit <- sortPass
      let (afterHandleMacros, msgsHandleMacros) = handleMacros afterConstructTranslationUnit
      forM_ msgsHandleMacros $ traceWith tracer . FrontendHandleMacros
      pure afterHandleMacros

    nameAnonPass <- cache "nameAnon" $ do
      afterHandleMacros <- handleMacrosPass
      let (afterNameAnon, msgsNameAnon) = nameAnon afterHandleMacros
      forM_ msgsNameAnon $ traceWith tracer . FrontendNameAnon
      pure afterNameAnon

    resolveBindingSpecsPass <- cache "resolveBindingSpecs" $ do
      afterNameAnon <- nameAnonPass
      extlSpec <- bootExternalBindingSpec
      presSpec <- bootPrescriptiveBindingSpec
      let (afterResolveBindingSpecs, omitTypes, msgsResolveBindingSpecs) =
            resolveBindingSpecs
              extlSpec
              presSpec
              afterNameAnon
      forM_ msgsResolveBindingSpecs $ traceWith tracer . FrontendResolveBindingSpecs
      pure (afterResolveBindingSpecs, omitTypes)

    selectPass <- cache "select" $ do
      (_, _, isMainHeader, isInMainHeaderDir, _) <- parsePass
      (afterResolveBindingSpecs, _)           <- resolveBindingSpecsPass
      let (afterSelect, msgsSelect) =
            selectDecls
              isMainHeader
              isInMainHeaderDir
              selectConfig
              afterResolveBindingSpecs
      forM_ msgsSelect $ traceWith tracer . FrontendSelect
      pure afterSelect

    handleTypedefsPass <- cache "handleTypedefs" $ do
      afterSelect <- selectPass
      let (afterHandleTypedefs, msgsHandleTypedefs) = handleTypedefs afterSelect
      forM_ msgsHandleTypedefs $ traceWith tracer . FrontendHandleTypedefs
      pure afterHandleTypedefs

    mangleNamesPass <- cache "mangleNames" $ do
      afterHandleTypedefs <- handleTypedefsPass
      let (afterMangleNames, msgsMangleNames) = mangleNames afterHandleTypedefs
      forM_ msgsMangleNames $ traceWith tracer . FrontendMangleNames
      pure afterMangleNames

    -- Unit.
    getCTranslationUnit <- cache "getCTranslationUnit" $ do
      afterMangleNames <- mangleNamesPass
      pure $ finalize afterMangleNames

    -- Include graph predicate.
    getIncludeGraphP <- cache "getIncludeGraphP" $ do
      (_, _, isMainHeader, isInMainHeaderDir, _) <- parsePass
      pure $ \path ->
        matchParse isMainHeader isInMainHeaderDir path frontendParsePredicate
        && path /= name

    -- Graphs.
    frontendIncludeGraph <- cache "frontendIncludeGraph" $ do
      includeGraphP <- getIncludeGraphP
      (_, includeGraph, _, _, _) <- parsePass
      pure (includeGraphP, includeGraph)
    frontendGetMainHeaders <- cache "frontendGetMainHeaders" $ do
      (_, _, _, _, getMainHeaders) <- parsePass
      pure getMainHeaders
    frontendIndex <- cache "frontendIndex" $
      ConstructTranslationUnit.declIndex   . unitAnn <$> sortPass
    frontendUseDeclGraph <- cache "frontendUseDeclGraph" $
      ConstructTranslationUnit.declUseDecl . unitAnn <$> sortPass
    frontendDeclUseGraph <- cache "frontendDeclUseGraph" $
      ConstructTranslationUnit.declDeclUse . unitAnn <$> sortPass

    -- Omitted types
    frontendOmitTypes <- cache "frontendOmitTypes" $
      snd <$> resolveBindingSpecsPass

    -- Declarations.
    frontendCDecls <- cache "frontendDecls" $
      C.unitDecls <$> getCTranslationUnit

    -- Dependencies.
    frontendDependencies <- cache "frontendDependencies" $
      C.unitDeps <$> getCTranslationUnit

    pure FrontendArtefact{..}
  where
    getRootHeader :: IO RootHeader
    getRootHeader = fromMainFiles <$> bootHashIncludeArgs

    getSetup :: IO ClangSetup
    getSetup = do
      clangArgs <- bootClangArgs
      hContent <- content <$> getRootHeader
      let setup = defaultClangSetup clangArgs $ ClangInputMemory hFilePath hContent
      pure $ setup {
          clangFlags = bitfieldEnum [
            CXTranslationUnit_DetailedPreprocessingRecord
          , CXTranslationUnit_IncludeAttributedTypes
          , CXTranslationUnit_VisitImplicitAttributes
          ]
        }

    hFilePath :: FilePath
    hFilePath = getSourcePath name

    selectConfig :: SelectConfig
    selectConfig =
      SelectConfig frontendProgramSlicing frontendSelectPredicate

    emptyParseResult :: (
        [ParseResult]
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

    cache :: String -> IO a -> IO (IO a)
    cache = cacheWith (contramap (FrontendCache . SafeTrace) tracer) . Just

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

data FrontendArtefact = FrontendArtefact {
    frontendIncludeGraph   :: IO (IncludeGraph.Predicate, IncludeGraph.IncludeGraph)
  , frontendGetMainHeaders :: IO GetMainHeaders
  , frontendIndex          :: IO DeclIndex.DeclIndex
  , frontendUseDeclGraph   :: IO UseDeclGraph.UseDeclGraph
  , frontendDeclUseGraph   :: IO DeclUseGraph.DeclUseGraph
  , frontendOmitTypes      :: IO (Map C.QualName SourcePath)
  , frontendCDecls         :: IO [C.Decl]
  , frontendDependencies   :: IO [SourcePath]
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
  | FrontendConstructTranslationUnit (Msg ConstructTranslationUnit)
  | FrontendHandleMacros (Msg HandleMacros)
  | FrontendNameAnon (Msg NameAnon)
  | FrontendResolveBindingSpecs (Msg ResolveBindingSpecs)
  | FrontendSelect (Msg Select)
  | FrontendHandleTypedefs (Msg HandleTypedefs)
  | FrontendMangleNames (Msg MangleNames)
  | FrontendCache (SafeTrace CacheMsg)
  deriving stock    (Show, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
