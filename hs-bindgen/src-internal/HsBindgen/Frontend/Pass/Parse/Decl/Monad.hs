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
  , recordDecl
  , recordFailedDecl
    -- ** Logging
  , recordUnattachedTrace
    -- ** Errors
  , unknownCursorKind
    -- * Utility: dispatching
  , dispatch
  ) where

import Data.IORef
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Text qualified as Text

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Eff
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.ProcessIncludes (GetMainHeadersAndInclude)
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
      parseEnv   :: Env              -- ^ Reader
    , parseState :: IORef ParseState -- ^ State
    }

type instance Support ParseDeclMonad = ParseSupport

run :: Env -> ParseDecl a -> IO (Ann "TranslationUnit" Parse, a)
run env f = do
    support <- ParseSupport env <$> newIORef initParseState
    x <- unwrapEff f support
    state <- readIORef (parseState support)
    let meta = ParseDeclMeta {
            parseStatus = stateParseStatus state
          }
    pure (meta, x)

{-------------------------------------------------------------------------------
  "Reader"
-------------------------------------------------------------------------------}

data Env = Env {
      envUnit                     :: CXTranslationUnit
    , envRootHeader               :: RootHeader
    , envIsMainHeader             :: IsMainHeader
    , envIsInMainHeaderDir        :: IsInMainHeaderDir
    , envGetMainHeadersAndInclude :: GetMainHeadersAndInclude
    , envPredicate                :: Boolean ParsePredicate
    , envTracer                   :: Tracer IO UnattachedParseMsg
    }

getTranslationUnit :: ParseDecl CXTranslationUnit
getTranslationUnit = wrapEff $ \ParseSupport{parseEnv} ->
    return (envUnit parseEnv)

evalGetMainHeadersAndInclude ::
     SourcePath
  -> ParseDecl (NonEmpty HashIncludeArg, HashIncludeArg)
evalGetMainHeadersAndInclude path = wrapEff $ \ParseSupport{parseEnv} ->
    either panicIO return $ (envGetMainHeadersAndInclude parseEnv) path

evalPredicate :: C.DeclInfo Parse -> ParseDecl Bool
evalPredicate info = wrapEff $ \ParseSupport{parseEnv} -> pure $
    matchParse
      (envIsMainHeader parseEnv)
      (envIsInMainHeaderDir parseEnv)
      (singleLocPath (C.declLoc info))
      (envPredicate parseEnv)

{-------------------------------------------------------------------------------
  "State"
-------------------------------------------------------------------------------}

data ParseState = ParseState {
      -- | Where did clang expand macros?
      --
      -- Declarations with expanded macros need to be reparsed.
      stateMacroExpansions :: Set SingleLoc

      -- | Parse status of declarations
      --
      -- We track the parse status of all declarations.
      --
      -- For example, if we do not parse a declaration because the parse
      -- predicate does not match, we need to know which header the declaration
      -- is declared in so that we can resolve external bindings.
      --
      -- Moreover, we link parse traces to specific declarations. We only emit
      -- parse traces linked to selected and processed declarations.
    , stateParseStatus :: ParseParseStatus
    }

initParseState :: ParseState
initParseState = ParseState{
      stateMacroExpansions = Set.empty
    , stateParseStatus     = mempty
    }

recordMacroExpansionAt :: SingleLoc -> ParseDecl ()
recordMacroExpansionAt loc = do
    wrapEff $ \ParseSupport{parseState} ->
      modifyIORef parseState $ \st -> st{
          stateMacroExpansions = Set.insert loc (stateMacroExpansions st)
        }

checkHasMacroExpansion :: Range SingleLoc -> ParseDecl Bool
checkHasMacroExpansion extent = do
    wrapEff $ \ParseSupport{parseState} ->
      aux extent . stateMacroExpansions <$> readIORef parseState
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

-- TODO_PR: We should ensure that we call `recordDecl` for all declarations.
recordDecl :: C.DeclInfo Parse -> C.NameKind -> DeclStatus -> ParseDecl ()
recordDecl info@C.DeclInfo{..} kind declStatus =
    let -- TODO_PR: We add a deprecation trace for all successful parses. We
        -- should do this in a better place.
        declStatus' :: DeclStatus
        declStatus'      = case (declAvailability, declStatus) of
          (C.Deprecated, ParseSucceeded msgs) ->
            ParseSucceeded $ ParseDeprecated : msgs
          _otherwise ->
            declStatus

        parseStatusValue :: ParseStatusValue
        parseStatusValue = ParseStatusValue declLoc declAvailability declStatus'
    in wrapEff $ \ParseSupport{parseState} ->
      modifyIORef parseState $ \st -> st{
          stateParseStatus = addParseParseStatus
                              (qualPrelimDeclId, parseStatusValue)
                              (stateParseStatus st)
        }
  where
    qualPrelimDeclId :: C.QualPrelimDeclId
    qualPrelimDeclId = case (declId, kind) of
      (C.PrelimDeclIdNamed   n, k                 ) -> C.QualPrelimDeclIdNamed n k
      -- TODO_PR: Should this be a panic? I guess it should be a warning trace instead?
      -- Edsko agrees with me that a panic is OK, but we get a CI error:
      --
      -- https://github.com/well-typed/hs-bindgen/actions/runs/18343147007/job/52243182474?pr=1192
      --
      -- anonymous declaration without tag kind: (unnamed at /usr/include/x86_64-linux-gnu/bits/cmathcalls.h:130:1)
      --
      -- I can not reproduce the error locally, I think because I am using a
      -- newer version of LLVM.
      (C.PrelimDeclIdAnon    _, C.NameKindOrdinary) -> errAnonymousNoTagKind
      (C.PrelimDeclIdAnon    n, C.NameKindTagged t) -> C.QualPrelimDeclIdAnon  n t
      (C.PrelimDeclIdBuiltin n, _                 ) -> C.QualPrelimDeclIdBuiltin n

    errAnonymousNoTagKind :: a
    errAnonymousNoTagKind = panicPure $
      "anonymous declaration without tag kind: " <> show (prettyForTrace info)

recordFailedDecl :: C.DeclInfo Parse -> C.NameKind -> DelayedParseMsg -> ParseDecl ()
recordFailedDecl i k m = recordDecl i k $ ParseFailed $ NonEmpty.singleton m

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

-- | Directly emit a parse message that can not be attached to a declaration,
-- usually because not enough information about the declaration is available.
recordUnattachedTrace :: UnattachedParseMsg -> ParseDecl ()
recordUnattachedTrace trace = wrapEff $ \ParseSupport{parseEnv} ->
  traceWith (envTracer parseEnv) trace

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

unknownCursorKind :: MonadIO m => CXCursor -> CXCursorKind -> m x
unknownCursorKind curr kind = do
    loc      <- HighLevel.clang_getCursorLocation' curr
    spelling <- clang_getCursorKindSpelling (simpleEnum kind)
    panicIO $ concat [
        "Unknown cursor of kind "
      , show kind
      , " ("
      , Text.unpack spelling
      , ") at "
      , show loc
      ]

{-------------------------------------------------------------------------------
  Utility: dispatching based on the cursor kind
-------------------------------------------------------------------------------}

dispatch :: MonadIO m => CXCursor -> (CXCursorKind -> m a) -> m a
dispatch curr k = do
    mKind <- fromSimpleEnum <$> clang_getCursorKind curr
    case mKind of
      Right kind -> k kind
      Left  i    -> panicIO $ "Unrecognized CXCursorKind " ++ show i
