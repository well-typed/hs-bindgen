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
  , getEmptyMacros
  , evalGetMainHeadersAndInclude
    -- ** "State"
  , recordMacroDefinition
  , getMacroDefinitions
  , recordMacroExpansionAt
  , getMacroExpansions
  , getMacroExpansionsAt
    -- ** Logging
  , traceImmediate
  , traceImmediateGlobal
    -- ** Declaration information
  , CursorDeclInfo(..)
  , getCursorDeclInfo
    -- ** Errors
  , parseFail
  , parseFailNoInfo
  ) where

import Data.IORef

import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Runtime.Macro qualified as Runtime.Macro

import HsBindgen.Eff
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Pass.Parse.Builtin
import HsBindgen.Frontend.Pass.Parse.Context
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad.SourceRangeMap (LookupResult (..),
                                                           SourceRangeMap,
                                                           initSourceRangeMap,
                                                           lookupAt,
                                                           lookupRange,
                                                           recordAt)
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass
import HsBindgen.Macro.Error (MacroParseError)
import HsBindgen.Macro.Syntax (MacroDefinition (..), MacroInvocation (..))
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
    , emptyMacros              :: EmptyMacros
    , tracer                   :: Tracer (Msg Parse)
    }

getTranslationUnit :: ParseDecl CXTranslationUnit
getTranslationUnit = wrapEff $ \support -> return support.env.unit

getEmptyMacros :: ParseDecl EmptyMacros
getEmptyMacros = wrapEff $ \support -> return support.env.emptyMacros

evalGetMainHeadersAndInclude ::
     RealPath
  -> ParseDecl (Either String (NonEmpty C.HashIncludeArg, IncludeGraph.Include))
evalGetMainHeadersAndInclude realPath = wrapEff $ \support ->
    pure $ support.env.getMainHeadersAndInclude realPath

{-------------------------------------------------------------------------------
  "State"
-------------------------------------------------------------------------------}

data ParseState = ParseState {
      -- | Macro definitions
      --
      -- Once parsing is done, macro definitions are analysed for ambiguity; see
      -- "HsBindgen.Macro.UniqueExpansion".
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

recordMacroDefinition ::
     Text
  -> Either MacroParseError (Runtime.Macro.Raw (Token SourcePath TokenSpelling))
  -> ParseDecl ()
recordMacroDefinition macroName macro =
    modifyParseState $ #macroDefinitions %~ (macroDefinition:)
  where
    macroDefinition :: MacroDefinition
    macroDefinition = MacroDefinition {
          name  = macroName
        , macro = macro
        }

getMacroDefinitions :: ParseDecl [MacroDefinition]
getMacroDefinitions = reverse . (.macroDefinitions) <$> getParseState

recordMacroExpansionAt ::
     Text
  -> Range (MultiLoc RealPath)
  -> [Token SourcePath TokenSpelling]
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

    loc :: SingleLoc RealPath
    loc = locRange.rangeStart.multiLocExpansion

-- | The macro invocations starting in the given half-open range
getMacroExpansions :: Range (SingleLoc RealPath) -> ParseDecl (Maybe (NonEmpty MacroInvocation))
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

-- | The macro invocations starting at the given location
getMacroExpansionsAt :: SingleLoc RealPath -> ParseDecl (Maybe (NonEmpty MacroInvocation))
getMacroExpansionsAt loc = lookupAt loc . (.macroExpansions) <$> getParseState

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

-- | Immediately emit a parse trace message with location information
traceImmediate ::
     HasCallStack
  => C.PrelimDeclId
  -> SingleLoc C.DeclPath
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
  Declaration information
-------------------------------------------------------------------------------}

-- | Declaration information obtained from a cursor
data CursorDeclInfo =
    CursorDeclInfo C.DeclInfo
    -- | A declaration we do not attempt to parse
  | CursorDeclInfoNotAttempted NotAttemptedReason C.DeclInfo

-- | Obtain the information of the declaration at the given location
getCursorDeclInfo :: SingleLoc C.DeclPath -> CXCursor -> ParseDecl CursorDeclInfo
getCursorDeclInfo declLoc curr = do
    origin        <- getOrigin
    sAvailability <- clang_getCursorAvailability curr
    let mkInfo :: C.Deprecation -> C.DeclInfo
        mkInfo deprecation = C.DeclInfo{
              loc              = declLoc
              -- We initialize the source-order index to 'Nothing', and
              -- populate it when consolidating macros with non-macros in
              -- "HsBindgen.Frontend.Pass.Parse".
            , sourceOrderIndex = Nothing
            , origin           = origin
            , deprecation      = deprecation
            }

        notAttempted :: NotAttemptedReason -> CursorDeclInfo
        notAttempted reason =
          CursorDeclInfoNotAttempted reason $ mkInfo C.NotDeprecated
    pure $ case fromSimpleEnum sAvailability of
      Right CXAvailability_Available     -> CursorDeclInfo $ mkInfo C.NotDeprecated
      Right CXAvailability_Deprecated    -> CursorDeclInfo $ mkInfo C.Deprecated
      Right CXAvailability_NotAvailable  -> notAttempted PlatformUnavailable
      Right CXAvailability_NotAccessible -> notAttempted PlatformUnavailable
      Left _ -> notAttempted $ UnknownAvailability sAvailability
  where
    getOrigin :: ParseDecl C.DeclOrigin
    getOrigin = case declLoc.singleLocPath of
      C.OnCommandLine     -> pure C.FromCommandLine
      C.InRootHeader      -> pure C.FromRootDirective
      C.InHeader realPath -> C.FromHeader <$> getHeaderInfo realPath

    getHeaderInfo :: RealPath -> ParseDecl C.HeaderInfo
    getHeaderInfo realPath = do
      eRes <- evalGetMainHeadersAndInclude realPath
      case eRes of
        Left err ->
          throwIO_TODO 2323 $ concat [
              "header "
            , show (getRealPath realPath)
            , " is not reached by an #include: "
            , err
            ]
        Right (mainHeaders, include) -> pure C.HeaderInfo{
            mainHeaders     = mainHeaders
          , includeArg      = IncludeGraph.getIncludeArg      include
          , includeMacroArg = IncludeGraph.getIncludeMacroArg include
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
  -> C.DeclInfo
  -> DelayedParseMsg
  -> ParseDecl [ParseResult l Parse]
parseFail ctx declId info msg = do
    maybeEmitScopingMsg ctx.outer.scoping declId info.loc
    pure $ (:[]) $
      ParseResultFailure ParseFailure{
          id   = declId
        , info = info
        , msg  = msg
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
    -- The declaration ID and the declaration information are not always
    -- available while parsing, and so are not part of the declaration context.
    -- We have to obtain them again here.
    declId <- C.prelimDeclIdAtCursor curr ctx.outer.kind
    source <- getCursorSource curr
    declLoc <- case source of
      SourceDecl declLoc -> pure declLoc
      -- We skip built-ins before parsing them
      SourceBuiltin      -> panicIO "parse failure in a built-in"
    cursorInfo <- getCursorDeclInfo declLoc curr
    let info :: C.DeclInfo
        info = case cursorInfo of
          CursorDeclInfo i                 -> i
          -- The declaration fails anyway.
          CursorDeclInfoNotAttempted _ i -> i
    parseFail ctx declId info msg

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1820>
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1249>
-- Ideally we'd only emit the trace when we /use/ the declaration that
-- we fail to parse.
maybeEmitScopingMsg ::
  RequiredForScoping -> C.PrelimDeclId -> SingleLoc C.DeclPath -> ParseDecl ()
maybeEmitScopingMsg scoping declId declLoc = case scoping of
    RequiredForScoping ->
      traceImmediate declId declLoc $
        ParseOfDeclarationRequiredForScopingFailed
    NotRequiredForScoping ->
      pure ()
