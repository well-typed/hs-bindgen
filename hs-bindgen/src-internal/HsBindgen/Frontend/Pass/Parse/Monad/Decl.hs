-- | Monad for parsing declarations
--
-- Intended for unqualified import (unless context is unambiguous).
--
-- > import HsBindgen.Frontend.Pass.Parse.Monad.Decl (ParseDecl)
-- > import HsBindgen.Frontend.Pass.Parse.Monad.Decl qualified as ParseDecl
module HsBindgen.Frontend.Pass.Parse.Monad.Decl (
    -- * Definition
    ParseDecl
  , Env (..)
  , run
    -- * Functionality
    -- ** "Reader"
  , getTranslationUnit
  , getCStandard
  , evalGetMainHeadersAndInclude
    -- ** "State"
  , recordMacroDefinitionAt
  , getMacroDefinitions
  , recordMacroExpansionAt
  , getMacroExpansions
    -- ** Logging
  , traceImmediate
  , traceImmediateGlobal
    -- ** Errors
  , parseFail
  , parseFailNoInfo
  ) where

import Data.IORef
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.CStandard
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Eff
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Context
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition

  We are careful to distinguish between the state that the computation can
  depend on ('MacroExpansions') and the additional output that generate during
  parsing but that cannot otherwise affect the computation ('IncludeGraph').
-------------------------------------------------------------------------------}

-- | Monad used during folding
type ParseDecl = Eff ParseDeclMonad

data ParseDeclMonad a

-- | Support for 'ParseDecl' (internal type, not exported)
data ParseSupport = ParseSupport {
      env   :: Env              -- ^ Reader
    , state :: IORef ParseState -- ^ State
    }

type instance Support ParseDeclMonad = ParseSupport

run :: Env -> ParseDecl a -> IO a
run env f = do
    support <- ParseSupport env <$> newIORef initParseState
    unwrapEff f support

{-------------------------------------------------------------------------------
  "Reader"
-------------------------------------------------------------------------------}

data Env = Env {
      unit                     :: CXTranslationUnit
    , cStandard                :: ClangCStandard
    , getMainHeadersAndInclude :: GetMainHeadersAndInclude
    , tracer                   :: Tracer (Msg Parse)
    }

getTranslationUnit :: ParseDecl CXTranslationUnit
getTranslationUnit = wrapEff $ \support -> return support.env.unit

getCStandard :: ParseDecl ClangCStandard
getCStandard = wrapEff $ \support-> return support.env.cStandard

evalGetMainHeadersAndInclude ::
     SourcePath
  -> ParseDecl
      (Either DelayedParseMsg
        (NonEmpty HashIncludeArg, IncludeGraph.Include))
evalGetMainHeadersAndInclude path = wrapEff $ \support ->
    pure $
      first (\err -> ParseNoMainHeadersException err path) $
      support.env.getMainHeadersAndInclude path

{-------------------------------------------------------------------------------
  "State"
-------------------------------------------------------------------------------}

data ParseState = ParseState {
      -- | Macros definitions we have parsed
      macroDefinitions :: [(CXSourceLocation, ParseResult Parse)]

      -- | Where did Clang expand macros, and what are their names?
      --
      -- Declarations with expanded macros need to be reparsed.
      --
      -- We use a stacked map so we can lookup macro expansions in source
      -- location ranges reasonably fast.
    , macroExpansions :: Map SourcePath (Map Int (NonEmpty Text))
    }
  deriving (Generic)

initParseState :: ParseState
initParseState = ParseState{
      macroDefinitions = []
    , macroExpansions  = Map.empty
    }

recordMacroDefinitionAt :: CXSourceLocation -> ParseResult Parse -> ParseDecl ()
recordMacroDefinitionAt loc result = wrapEff $ \support ->
    modifyIORef support.state $ #macroDefinitions %~ ((loc, result) :)

getMacroDefinitions :: ParseDecl [(CXSourceLocation, ParseResult Parse)]
getMacroDefinitions = wrapEff $ \support -> do
    s <- readIORef support.state
    pure s.macroDefinitions

recordMacroExpansionAt :: SingleLoc -> Text -> ParseDecl ()
recordMacroExpansionAt loc macroName = wrapEff $ \support -> do
    modifyIORef support.state $ #macroExpansions %~ addMacro
  where
    addMacro ::
         Map SourcePath (Map Int (NonEmpty Text))
      -> Map SourcePath (Map Int (NonEmpty Text))
    addMacro = Map.alter addMacroAtFile loc.singleLocPath

    addMacroAtFile ::
         Maybe (Map Int (NonEmpty Text))
      -> Maybe (Map Int (NonEmpty Text))
    addMacroAtFile = Just . \case
      Nothing ->
        Map.singleton loc.singleLocLine $ NonEmpty.singleton macroName
      Just lineMap ->
        Map.alter addMacroAtLine loc.singleLocLine lineMap

    addMacroAtLine :: Maybe (NonEmpty Text) -> Maybe (NonEmpty Text)
    addMacroAtLine = Just . \case
      Nothing     -> NonEmpty.singleton macroName
      Just macros -> NonEmpty.cons      macroName macros

getMacroExpansions :: Range SingleLoc -> ParseDecl (Set Text)
getMacroExpansions range
  -- We do not support getting macro expansions for declarations spanning
  -- multiple files.
  | range.rangeStart.singleLocPath /= range.rangeEnd.singleLocPath = do
      traceImmediateGlobal (ParseGetMacroExpansionsMultipleFiles range)
      pure $ Set.empty
  | otherwise = do
    wrapEff $ \support ->
      aux . (.macroExpansions) <$> readIORef support.state
  where
    sourcePath :: SourcePath
    sourcePath = range.rangeStart.singleLocPath

    aux :: Map SourcePath (Map Int (NonEmpty Text)) -> Set Text
    aux fileMap = case Map.lookup sourcePath fileMap of
      Nothing      -> Set.empty
      Just lineMap ->
        Set.fromList $ concatMap NonEmpty.toList $ Map.elems $
          Map.takeWhileAntitone (range.rangeEnd.singleLocLine >=) $
            Map.dropWhileAntitone (range.rangeStart.singleLocLine >) lineMap

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

-- | Immediately emit a parse trace message with location information
traceImmediate ::
     PrelimDeclId
  -> SingleLoc
  -> ImmediateParseMsg
  -> ParseDecl ()
traceImmediate declId declLoc msg = wrapEff $ \support ->
    traceWith (contramap withCallStack support.env.tracer) $
      withCallStack WithLocationInfo{
        loc = prelimDeclIdLocationInfo declId [declLoc]
      , msg = msg
      }

-- | Immediately emit a global parse trace message without location information
traceImmediateGlobal :: ImmediateParseMsg -> ParseDecl ()
traceImmediateGlobal msg = wrapEff $ \support ->
    traceWith (contramap withCallStack support.env.tracer) $
      withCallStack WithLocationInfo{
        loc = LocationUnavailable
      , msg = msg
      }

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Record a parse failure
--
-- In contrast to 'parseSucceed' and 'parseDoNotAttempt', this is a monadic
-- action: It checks for immediate parse messages that should be
-- emitted directly.
parseFail ::
     ParseCtx
  -> PrelimDeclId
  -> SingleLoc
  -> DelayedParseMsg
  -> ParseDecl [ParseResult Parse]
parseFail ctx declId declLoc msg = do
    maybeEmitScopingMsg ctx.outer.scoping declId declLoc
    pure $ (:[]) $
      ParseResult{
          id             = declId
        , loc            = declLoc
        , classification = ParseResultFailure msg
        }

-- | Record a parse failure without having the declaration information readily
--   available
--
-- Retrieve the information using libclang and the provided cursor.
parseFailNoInfo :: ParseCtx -> DelayedParseMsg -> CXCursor -> ParseDecl [ParseResult Parse]
parseFailNoInfo ctx msg curr = do
    (declId, declLoc) <- getDeclInfoForTrace
    parseFail ctx declId declLoc msg
  where
    -- The declaration ID and the location are not always available while
    -- parsing, and so are not part of the declaration context. We have to
    -- obtain them again here.
    getDeclInfoForTrace :: ParseDecl (PrelimDeclId, SingleLoc)
    getDeclInfoForTrace = do
      declId  <- PrelimDeclId.atCursor curr ctx.outer.kind
      declLoc <- HighLevel.clang_getCursorLocation' curr
      pure (declId, declLoc)

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1820>
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1249>
-- Ideally we'd only emit the trace when we /use/ the declaration that
-- we fail to parse.
maybeEmitScopingMsg ::
  RequiredForScoping -> PrelimDeclId -> SingleLoc -> ParseDecl ()
maybeEmitScopingMsg scoping declId declLoc = case scoping of
    RequiredForScoping ->
      traceImmediate declId declLoc $
        ParseOfDeclarationRequiredForScopingFailed
    NotRequiredForScoping ->
      pure ()
