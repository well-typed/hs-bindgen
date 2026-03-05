-- | Monad for parsing declarations
--
-- Intended for unqualified import (unless context is unambiguous).
--
-- > import HsBindgen.Frontend.Pass.Parse.Decl.Monad (ParseDecl)
-- > import HsBindgen.Frontend.Pass.Parse.Decl.Monad qualified as ParseDecl
module HsBindgen.Frontend.Pass.Parse.Decl.Monad (
    -- * Definition
    ParseDecl
  , Env (..)
  , run
    -- * Functionality
    -- ** "Reader"
  , getTranslationUnit
  , evalGetMainHeadersAndInclude
  , evalPredicate
    -- ** "State"
  , recordMacroExpansionAt
  , checkHasMacroExpansion
    -- ** Logging
  , recordImmediateTrace
    -- ** Errors
  , parseFail
  , parseFailNoInfo
  ) where

import Data.IORef
import Data.Set qualified as Set

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Eff
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Context
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader (HashIncludeArg, RootHeader)
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
    , rootHeader               :: RootHeader
    , isMainHeader             :: IsMainHeader
    , isInMainHeaderDir        :: IsInMainHeaderDir
    , getMainHeadersAndInclude :: GetMainHeadersAndInclude
    , predicate                :: Boolean ParsePredicate
    , tracer                   :: Tracer (Msg Parse)
    }

getTranslationUnit :: ParseDecl CXTranslationUnit
getTranslationUnit = wrapEff $ \support -> return support.env.unit

evalGetMainHeadersAndInclude ::
     SourcePath
  -> ParseDecl
      (Either DelayedParseMsg
        (NonEmpty HashIncludeArg, IncludeGraph.Include))
evalGetMainHeadersAndInclude path = wrapEff $ \support ->
    pure $
      first (\err -> ParseNoMainHeadersException err path) $
      support.env.getMainHeadersAndInclude path

evalPredicate :: C.DeclInfo Parse -> ParseDecl Bool
evalPredicate info = wrapEff $ \support -> pure $
    matchParse
      support.env.isMainHeader
      support.env.isInMainHeaderDir
      (singleLocPath info.loc)
      support.env.predicate

{-------------------------------------------------------------------------------
  "State"
-------------------------------------------------------------------------------}

data ParseState = ParseState {
      -- | Where did clang expand macros?
      --
      -- Declarations with expanded macros need to be reparsed.
      macroExpansions :: Set SingleLoc
    }
  deriving (Generic)

initParseState :: ParseState
initParseState = ParseState{
      macroExpansions = Set.empty
    }

recordMacroExpansionAt :: SingleLoc -> ParseDecl ()
recordMacroExpansionAt loc = wrapEff $ \support ->
    modifyIORef support.state $ #macroExpansions %~ Set.insert loc

checkHasMacroExpansion :: Range SingleLoc -> ParseDecl Bool
checkHasMacroExpansion extent = do
    wrapEff $ \support ->
      aux extent . (.macroExpansions) <$> readIORef support.state
  where
    aux :: Range SingleLoc -> Set SingleLoc -> Bool
    aux range expansions = or [
        -- Do a quick O(log n) check first, for the common case that the macro
        -- is right at the start of the range. For example, this would capture
        -- cases such as
        --
        -- > #define T int
        -- >
        -- > struct ExampleStruct {
        -- >   T field;
        -- > };
        Set.member (rangeStart range) expansions

        -- If that fails, do a O(n) scan through all macro expansions
      , any (\e -> fromMaybe False (rangeContainsLoc range e)) expansions
      ]

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

-- | Directly emit a parse message that can not be attached to a parsed
-- declaration, usually because not enough information about the declaration is
-- available.
recordImmediateTrace ::
     PrelimDeclId
  -> SingleLoc
  -> ImmediateParseMsg
  -> ParseDecl ()
recordImmediateTrace declId declLoc msg = wrapEff $ \support ->
    traceWith support.env.tracer WithLocationInfo{
        loc = prelimDeclIdLocationInfo declId [declLoc]
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

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1249>
-- Ideally we'd only emit the trace when we /use/ the declaration that
-- we fail to parse.
maybeEmitScopingMsg ::
  RequiredForScoping -> PrelimDeclId -> SingleLoc -> ParseDecl ()
maybeEmitScopingMsg scoping declId declLoc = case scoping of
    RequiredForScoping ->
      recordImmediateTrace declId declLoc $
        ParseOfDeclarationRequiredForScopingFailed
    NotRequiredForScoping ->
      pure ()
