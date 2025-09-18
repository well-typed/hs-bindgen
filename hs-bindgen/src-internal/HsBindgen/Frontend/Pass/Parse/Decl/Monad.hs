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
  , recordNonParsedDecl
    -- ** Logging
  , recordTrace
    -- ** Errors
  , unknownCursorKind
    -- * Utility: dispatching
  , dispatch
  ) where

import Data.IORef
import Data.Set qualified as Set
import Data.Text qualified as Text

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Eff
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External (NameKind)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.NonParsedDecls (NonParsedDecls)
import HsBindgen.Frontend.NonParsedDecls qualified as NonParsedDecls
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Predicate qualified as Predicate
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
            parseDeclNonParsed = stateNonParsedDecls state
          , parseDeclParseMsg  = stateParseMsgs state
          }
    pure (meta, x)

{-------------------------------------------------------------------------------
  "Reader"
-------------------------------------------------------------------------------}

data Env = Env {
      envUnit                     :: CXTranslationUnit
    , envRootHeader               :: RootHeader
    , envIsMainHeader             :: Predicate.IsMainHeader
    , envIsInMainHeaderDir        :: Predicate.IsInMainHeaderDir
    , envGetMainHeadersAndInclude :: GetMainHeadersAndInclude
    , envPredicate                :: Predicate.ParsePredicate
    , envTracer                   :: Tracer IO ParseMsg
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
evalPredicate info = wrapEff $ \ParseSupport{parseEnv} -> do
    let selected = Predicate.matchParse
                     (envIsMainHeader parseEnv)
                     (envIsInMainHeaderDir parseEnv)
                     (C.declLoc info)
                     (envPredicate parseEnv)
    unless selected $ traceWith (envTracer parseEnv) (ParseExcluded info)
    return selected

{-------------------------------------------------------------------------------
  "State"
-------------------------------------------------------------------------------}

data ParseState = ParseState {
      -- | Where did clang expand macros?
      --
      -- Declarations with expanded macros need to be reparsed.
      stateMacroExpansions :: Set SingleLoc

      -- | Non-parsed declarations
      --
      -- We need to track which header each excluded declaration is declared in
      -- so that we can resolve external bindings.
    , stateNonParsedDecls :: NonParsedDecls

      -- | Some traces are linked to specific declarations. However, we only
      -- select and process a subset of all parsed declarations. To reduce
      -- noise, we only emit traces linked to selected and processed
      -- declarations. Since we change the info object between passes, we link
      -- messages to source locations. For a given declaration, the source
      -- location should be constant across all passes.
    , stateParseMsgs :: ParseMsgs Parse
    }

initParseState :: ParseState
initParseState = ParseState{
      stateMacroExpansions = Set.empty
    , stateNonParsedDecls  = NonParsedDecls.empty
    , stateParseMsgs       = emptyParseMsgs
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

recordNonParsedDecl :: C.DeclInfo Parse -> C.NameKind -> ParseDecl ()
recordNonParsedDecl declInfo nameKind =
    case declName of
      Just cname -> do
        let cQualName  = C.QualName cname nameKind
            sourcePath = singleLocPath (C.declLoc declInfo)
        wrapEff $ \ParseSupport{parseState} ->
          modifyIORef parseState $ \st -> st{
              stateNonParsedDecls =
                NonParsedDecls.insert cQualName sourcePath $
                  stateNonParsedDecls st
            }
      Nothing ->
        -- We __do not track unselected anonymous declarations__. If we want to
        -- use descriptive binding specification with anonymous declarations, we
        -- __must__ select these declarations.
        return ()
  where
    declName :: Maybe C.Name
    declName = case C.declId declInfo of
      C.PrelimDeclIdNamed   cname   -> Just cname
      C.PrelimDeclIdAnon{}          -> Nothing
      C.PrelimDeclIdBuiltin builtin -> Just builtin

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

recordTrace :: C.DeclInfo Parse -> NameKind -> ParseMsg -> ParseDecl ()
recordTrace info kind trace = wrapEff $ \ParseSupport{parseState} ->
    modifyIORef parseState $ \st -> st{
        stateParseMsgs = recordParseMsg info kind trace (stateParseMsgs st)
      }

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
