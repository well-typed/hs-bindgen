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

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Clang.Macros (MacroDefinition (..), MacroInvocation (..))
import HsBindgen.Eff
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Pass.Parse.Context
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad.SourceRangeMap (LookupResult (..),
                                                           SourceRangeMap,
                                                           initSourceRangeMap,
                                                           lookupRange,
                                                           recordAt)
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
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
    , getMainHeadersAndInclude :: GetMainHeadersAndInclude
    , tracer                   :: Tracer (Msg Parse)
    }

getTranslationUnit :: ParseDecl CXTranslationUnit
getTranslationUnit = wrapEff $ \support -> return support.env.unit

evalGetMainHeadersAndInclude ::
     SourcePath
  -> ParseDecl
      (Either DelayedParseMsg
        (NonEmpty C.HashIncludeArg, IncludeGraph.Include))
evalGetMainHeadersAndInclude path = wrapEff $ \support ->
    pure $
      first (\err -> ParseNoMainHeadersException err path) $
      support.env.getMainHeadersAndInclude path

{-------------------------------------------------------------------------------
  "State"
-------------------------------------------------------------------------------}

data ParseState = ParseState {
      -- | Macro definitions
      --
      -- Macro definitions need to be analysed for ambiguity in the
      -- @PrepareReparse@ pass.
      macroDefinitions :: [MacroDefinition]
      -- | Where did Clang expand macros, and what are their names?
      --
      -- Declarations with expanded macros need to be reparsed.
    , macroExpansions :: SourceRangeMap MacroInvocation
    }
  deriving (Generic)

initParseState :: ParseState
initParseState = ParseState{
      macroDefinitions = []
    , macroExpansions = initSourceRangeMap
    }

getParseState :: ParseDecl ParseState
getParseState = wrapEff $ \support -> readIORef support.state

modifyParseState :: (ParseState -> ParseState) -> ParseDecl ()
modifyParseState f = wrapEff $ \support -> modifyIORef support.state f

recordMacroDefinitionAt ::
     Text
  -> Range MultiLoc
  -> [Token TokenSpelling]
  -> ParseDecl ()
recordMacroDefinitionAt macroName locRange tokens =
    modifyParseState $ #macroDefinitions %~ (macroDefinition:)
  where
    macroDefinition :: MacroDefinition
    macroDefinition = MacroDefinition {
          name = macroName
        , locRange = locRange
        , tokens = tokens
        }

getMacroDefinitions :: ParseDecl [MacroDefinition]
getMacroDefinitions = reverse . (.macroDefinitions) <$> getParseState

recordMacroExpansionAt ::
     Text
  -> Range MultiLoc
  -> [Token TokenSpelling]
  -> ParseDecl ()
recordMacroExpansionAt macroName locRange tokens =
    modifyParseState $ #macroExpansions %~ recordAt loc macroInvocation
  where
    macroInvocation :: MacroInvocation
    macroInvocation = MacroInvocation {
          name = macroName
        , locRange = locRange
        , tokens = tokens
        }

    loc :: SingleLoc
    loc = locRange.rangeStart.multiLocExpansion

getMacroExpansions :: Range SingleLoc -> ParseDecl (Maybe (NonEmpty MacroInvocation))
getMacroExpansions range = do
    macroExpansions <- (.macroExpansions) <$> getParseState
    case lookupRange range macroExpansions of
      -- We do not support getting macro expansions for declarations spanning
      -- multiple files.
      LookupErrorMultipleFiles -> do
        traceImmediateGlobal (ParseGetMacroExpansionsMultipleFiles range)
        pure Nothing
      LookupNotFound ->
        pure Nothing
      LookupFound macroInvocations ->
        pure $ Just macroInvocations

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

-- | Immediately emit a parse trace message with location information
traceImmediate ::
     HasCallStack
  => C.PrelimDeclId
  -> SingleLoc
  -> ImmediateParseMsg
  -> ParseDecl ()
traceImmediate declId declLoc msg = wrapEff $ \support ->
    traceWith support.env.tracer $
      withCallStack C.WithLocationInfo{
        loc = C.prelimDeclIdLocationInfo declId [declLoc]
      , msg = msg
      }

-- | Immediately emit a global parse trace message without location information
traceImmediateGlobal :: HasCallStack => ImmediateParseMsg -> ParseDecl ()
traceImmediateGlobal msg = wrapEff $ \support ->
    traceWith support.env.tracer $
      withCallStack C.WithLocationInfo{
        loc = C.LocationUnavailable
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
  -> C.PrelimDeclId
  -> SingleLoc
  -> DelayedParseMsg
  -> ParseDecl [ParseResult l Parse]
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
parseFailNoInfo ::
     ParseCtx
  -> DelayedParseMsg
  -> CXCursor
  -> ParseDecl [ParseResult l Parse]
parseFailNoInfo ctx msg curr = do
    (declId, declLoc) <- getDeclInfoForTrace
    parseFail ctx declId declLoc msg
  where
    -- The declaration ID and the location are not always available while
    -- parsing, and so are not part of the declaration context. We have to
    -- obtain them again here.
    getDeclInfoForTrace :: ParseDecl (C.PrelimDeclId, SingleLoc)
    getDeclInfoForTrace = do
      declId  <- C.prelimDeclIdAtCursor curr ctx.outer.kind
      declLoc <- HighLevel.clang_getCursorLocation' curr
      pure (declId, declLoc)

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1820>
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1249>
-- Ideally we'd only emit the trace when we /use/ the declaration that
-- we fail to parse.
maybeEmitScopingMsg ::
  RequiredForScoping -> C.PrelimDeclId -> SingleLoc -> ParseDecl ()
maybeEmitScopingMsg scoping declId declLoc = case scoping of
    RequiredForScoping ->
      traceImmediate declId declLoc $
        ParseOfDeclarationRequiredForScopingFailed
    NotRequiredForScoping ->
      pure ()
