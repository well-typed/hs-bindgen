module HsBindgen.Frontend.Pass.Parse.Monad (
    -- * Definition
    M
  , ParseEnv (..)
  , runParseMonad
    -- * Functionality
    -- ** "Reader"
  , getTranslationUnit
  , skipBuiltin
  , evalPredicate
    -- ** "State"
  , updateMainHeader
  , getMainHeader
  , recordMacroExpansionAt
  , checkHasMacroExpansion
  , recordOmittedDecl
    -- ** Logging
  , ParseTrace(..)
  , recordTrace
  ) where

import Control.Tracer (Tracer)
import Data.IORef
import Data.Set qualified as Set
import GHC.Stack

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.C.Predicate (Predicate, IsMainFile)
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Eff
import HsBindgen.Errors
import HsBindgen.Frontend.OmittedDecls (OmittedDecls)
import HsBindgen.Frontend.OmittedDecls qualified as OmittedDecls
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Frontend.Util.Fold (dispatch)
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Definition

  We are careful to distinguish between the state that the computation can
  depend on ('MacroExpansions') and the additional output that generate during
  parsing but that cannot otherwise affect the computation ('IncludeGraph').
-------------------------------------------------------------------------------}

-- | Monad used during folding
type M = Eff ParseMonad

data ParseMonad a

-- | Support for 'M' (internal type, not exported)
data ParseSupport = ParseSupport {
      parseEnv   :: ParseEnv           -- ^ Reader
    , parseState :: IORef ParseState   -- ^ State
    }

type instance Support ParseMonad = ParseSupport

runParseMonad :: ParseEnv -> M a -> IO (OmittedDecls, a)
runParseMonad env f = do
    support <- ParseSupport env <$> newIORef initParseState
    x <- unwrapEff f support
    (, x) . stateOmittedDecls <$> readIORef (parseState support)

{-------------------------------------------------------------------------------
  "Reader"
-------------------------------------------------------------------------------}

data ParseEnv = ParseEnv {
      envUnit       :: CXTranslationUnit
    , envRootHeader :: RootHeader
    , envIsMainFile :: IsMainFile
    , envPredicate  :: Predicate
    , envTracer     :: Tracer IO (TraceWithCallStack ParseTrace)
    }

getTranslationUnit :: M CXTranslationUnit
getTranslationUnit = wrapEff $ \ParseSupport{parseEnv} ->
    return (envUnit parseEnv)

skipBuiltin :: CXCursor -> M Bool
skipBuiltin curr = wrapEff $ \ParseSupport{parseEnv} -> do
    isMatch <- Predicate.skipBuiltin curr
    case isMatch of
      Right ()     -> return False
      Left  reason -> do
        traceWithCallStack (envTracer parseEnv) $ Skipped reason
        return True

evalPredicate :: CXCursor -> M Bool
evalPredicate curr = wrapEff $ \ParseSupport{parseEnv} -> do
    isMatch <- Predicate.match
                 (envIsMainFile parseEnv)
                 (envPredicate  parseEnv)
                 curr
    case isMatch of
      Right ()     -> return True
      Left  reason -> do
        traceWithCallStack (envTracer parseEnv) $ Skipped reason
        return False

{-------------------------------------------------------------------------------
  "State"
-------------------------------------------------------------------------------}

data ParseState = ParseState {
      -- | Where did clang expand macros?
      --
      -- Declarations with expanded macros need to be reparsed.
      stateMacroExpansions :: Set SingleLoc

      -- | Current main header
      --
      -- This is exclusively used to set 'functionHeader'.
    , stateMainHeader :: Maybe CHeaderIncludePath

      -- | Omitted declarations
      --
      -- We need to track which header each omitted declaration is declared in
      -- so that we can resolve external bindings.
    , stateOmittedDecls :: OmittedDecls
    }

initParseState :: ParseState
initParseState = ParseState{
      stateMacroExpansions = Set.empty
    , stateMainHeader      = Nothing
    , stateOmittedDecls    = OmittedDecls.empty
    }

-- | Update the main header, when necessary
--
-- Should only be called on 'CXCursor_InclusionDirective'.
updateMainHeader :: CXCursor -> M ()
updateMainHeader curr = do
    wrapEff $ \ParseSupport{parseEnv, parseState} ->
      ifFromRootHeader_ (envRootHeader parseEnv) curr $ \path ->
        modifyIORef parseState $ \st -> st{stateMainHeader = Just path}

getMainHeader :: M CHeaderIncludePath
getMainHeader = wrapEff $ \ParseSupport{parseState} -> do
     state <- readIORef parseState
     case stateMainHeader state of
       Just header -> return header
       Nothing     ->
         -- This should not happen: we only ask for the main header when we
         -- are generating a function, which must ultimately come from one of
         -- the main headers.
         panicIO "No main header"

recordMacroExpansionAt :: SingleLoc -> M ()
recordMacroExpansionAt loc = do
    wrapEff $ \ParseSupport{parseState} ->
      modifyIORef parseState $ \st -> st{
          stateMacroExpansions = Set.insert loc (stateMacroExpansions st)
        }

checkHasMacroExpansion :: Range SingleLoc -> M Bool
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

recordOmittedDecl :: CXCursor -> M ()
recordOmittedDecl curr = do
    mNamespace <- dispatch curr $ return . \case
      CXCursor_MacroDefinition -> Just NamespaceMacro
      CXCursor_StructDecl      -> Just NamespaceStruct
      CXCursor_UnionDecl       -> Just NamespaceUnion
      CXCursor_TypedefDecl     -> Just NamespaceTypedef
      CXCursor_EnumDecl        -> Just NamespaceEnum
      -- We intentionally do selection as part of parsing, rather than a
      -- separate step: if the user does not select certain declarations
      -- (perhaps because they live deep in the bowels of some system
      -- libraries), we also do not need to parse them. Since 'recordSource' is
      -- called on all declarations, selected or not, we must ensure that we
      -- don't error out on such unsupported cases.
      _kind                    -> Nothing
    case mNamespace of
      Just namespace -> getDeclId curr >>= \case
        DeclNamed cname -> do
          sourcePath <-
            singleLocPath <$> HighLevel.clang_getCursorLocation' curr
          wrapEff $ \ParseSupport{parseState} -> do
            modifyIORef parseState $ \st -> st{
                stateOmittedDecls =
                  OmittedDecls.insert (cname, namespace) sourcePath $
                    stateOmittedDecls st
              }
        DeclAnon{} -> return ()
      Nothing -> return ()

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

data ParseTrace =
    -- | We skipped over a declaration
    Skipped Predicate.SkipReason

    -- | Struct with implicit fields
    --
    -- We record the name of the struct that has the implicit fields.
  | UnsupportedImplicitFields {
        unsupportedImplicitFieldsId  :: DeclId
      , unsupportedImplicitFieldsLoc :: SingleLoc
      }
  deriving stock (Show)

instance PrettyTrace ParseTrace where
  prettyTrace = \case
      Skipped reason ->
          prettyTrace reason
      UnsupportedImplicitFields {..} -> concat [
          "Unsupported implicit field with ID "
        , show unsupportedImplicitFieldsId
        , " at "
        , show unsupportedImplicitFieldsLoc
        ]

instance HasDefaultLogLevel ParseTrace where
  getDefaultLogLevel = \case
      Skipped reason              -> getDefaultLogLevel reason
      UnsupportedImplicitFields{} -> Error

instance HasSource ParseTrace where
    getSource = const HsBindgen

recordTrace :: HasCallStack => ParseTrace -> M ()
recordTrace trace = wrapEff $ \ParseSupport{parseEnv} ->
    traceWithCallStack (envTracer parseEnv) trace
