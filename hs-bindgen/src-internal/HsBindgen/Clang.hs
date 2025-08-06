-- | Top-level call into @clang@, with @hs-bindgen@ specific features
module HsBindgen.Clang (
    -- * Top-level call into @clang@
    ClangSetup(..)
  , ClangInput(..)
  , defaultClangSetup
  , withClang
  , withClang'
    -- * Trace messages
  , ClangMsg(..)
  , ExtraClangArgsMsg(..)
    -- * Low-level API (exported for tests)
  , splitArguments
  , getExtraClangArgs
  ) where

import Data.Text qualified as Text
import GHC.ResponseFile (unescapeArgs)
import System.Environment (lookupEnv)

import Clang.Args
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint (showToCtxDoc, string, textToCtxDoc, (><))

{-------------------------------------------------------------------------------
  Top-level call into clang
-------------------------------------------------------------------------------}

data ClangSetup = ClangSetup{
      clangArgs        :: ClangArgs
    , clangDiagnostics :: DisplayDiagnostics
    , clangInput       :: ClangInput
    , clangFlags       :: BitfieldEnum CXTranslationUnit_Flags
    }

data ClangInput =
    ClangInputFile SourcePath
  | ClangInputMemory FilePath String

defaultClangSetup :: ClangArgs -> ClangInput -> ClangSetup
defaultClangSetup clangArgs clangInput = ClangSetup{
      clangArgs
    , clangDiagnostics = DontDisplayDiagnostics
    , clangInput
    , clangFlags = bitfieldEnum [CXTranslationUnit_DetailedPreprocessingRecord]
    }

-- | Call clang to parse with the specified 'ClangSetup'
--
-- All diagnostics are traced.  'Nothing' is returned if any of them are errors.
-- The specified continuation is called only when there are no error
-- diagnostics.
withClang :: forall a.
     Tracer IO ClangMsg
  -> ClangSetup
  -> (CXTranslationUnit -> IO (Maybe a))
  -> IO (Maybe a)
withClang tracer setup k = withClang' tracer setup $ \unit -> do
    anyIsError <- traceDiagnostics unit
    if anyIsError
      then return Nothing
      else k unit
  where
    traceDiagnostics :: CXTranslationUnit -> IO Bool
    traceDiagnostics unit =
        go False =<< HighLevel.clang_getDiagnostics unit Nothing
      where
        go :: Bool -> [Diagnostic] -> IO Bool
        go !anyIsError []     = return anyIsError
        go !anyIsError (d:ds) = do
            traceWith (contramap ClangDiagnostic tracer) d
            go (anyIsError || diagnosticIsError d) ds

-- | Call clang to parse with the specified 'ClangSetup'
--
-- Diagnostics are not traced, and the specified continuation is called even if
-- there are error diagnostics.
--
-- This function is needed for @resolveHeaders@, where we need the paths for the
-- resolved headers even if some headers are not found.
withClang' :: forall a.
     Tracer IO ClangMsg
  -> ClangSetup
  -> (CXTranslationUnit -> IO (Maybe a))
  -> IO (Maybe a)
withClang' tracer setup k =
    withExtraClangArgs (contramap ClangExtraArgs tracer) clangArgs $ \args  ->
    HighLevel.withIndex clangDiagnostics $ \index -> do
      let withUnit :: SourcePath -> [CXUnsavedFile] -> IO (Maybe a)
          withUnit path unsaved =
             HighLevel.withTranslationUnit2
               index
               (Just path)
               args
               unsaved
               clangFlags
               onErrorCode
               k
      case clangInput of
        ClangInputFile path ->
          withUnit path []
        ClangInputMemory path contents -> do
          HighLevel.withUnsavedFile path contents $ \file  ->
            withUnit (SourcePath $ Text.pack path) [file]
  where
    ClangSetup{
        clangArgs
      , clangDiagnostics
      , clangInput
      , clangFlags
      } = setup

    onErrorCode :: SimpleEnum CXErrorCode -> IO (Maybe a)
    onErrorCode err = do
        traceWith tracer $ ClangErrorCode err
        return Nothing

{-------------------------------------------------------------------------------
  Log messages
-------------------------------------------------------------------------------}

-- | Errors and warnings resulting from interaction with clang
data ClangMsg =
    ClangExtraArgs ExtraClangArgsMsg
  | ClangErrorCode (SimpleEnum CXErrorCode)
  | ClangDiagnostic Diagnostic
  deriving stock (Show, Eq)

instance PrettyForTrace ClangMsg where
  prettyForTrace = \case
      ClangExtraArgs  x -> prettyForTrace x
      ClangErrorCode  x -> "clang error " >< showToCtxDoc x
      ClangDiagnostic Diagnostic{..}
        | RootHeader.isInRootHeader diagnosticLocation -> textToCtxDoc $
            case getFileNotFound diagnosticSpelling of
              Just header -> "unable to resolve #include <" <> header <> ">"
              Nothing     -> case getFileNotFoundQ diagnosticSpelling of
                Just header ->
                  "unable to resolve #include <" <> header
                    <> "> (must specify header relative to directory in C include search path)"
                Nothing     ->
                  Text.stripStart $ Text.dropWhile (/= ' ') diagnosticFormatted
        | otherwise -> textToCtxDoc diagnosticFormatted
    where
      getFileNotFound :: Text -> Maybe Text
      getFileNotFound =
        fmap (Text.dropWhile (== '\'')) . Text.stripSuffix "' file not found"

      getFileNotFoundQ :: Text -> Maybe Text
      getFileNotFoundQ =
          fmap (Text.dropWhile (== '\'') . Text.dropWhile (/= '\''))
        . Text.stripSuffix "' file not found with <angled> include; use \"quotes\" instead"

instance HasDefaultLogLevel ClangMsg where
  getDefaultLogLevel = \case
      ClangExtraArgs  x -> getDefaultLogLevel x
      ClangErrorCode  _ -> Error
      ClangDiagnostic x -> if diagnosticIsError x then Error else Warning

instance HasSource ClangMsg where
  getSource = \case
      ClangExtraArgs  x -> getSource x
      ClangErrorCode  _ -> Libclang
      ClangDiagnostic _ -> Libclang

{-------------------------------------------------------------------------------
  @BINDGEN_EXTRA_CLANG_ARGS@ environment variable
-------------------------------------------------------------------------------}

extraClangArgsEnvNameBase :: String
extraClangArgsEnvNameBase = "BINDGEN_EXTRA_CLANG_ARGS"

data ExtraClangArgsMsg =
    ExtraClangArgsNone
  | ExtraClangArgsParsed { envName    :: String
                         , envArgs    :: [String] }
  deriving stock (Show, Eq)

instance PrettyForTrace ExtraClangArgsMsg where
  prettyForTrace = \case
    ExtraClangArgsNone ->
      "No " >< string extraClangArgsEnvNameBase >< " environment variables"
    ExtraClangArgsParsed {..} ->
      "Picked up evironment variable " >< string envName ><
      "; parsed 'libclang' arguments: " >< showToCtxDoc envArgs

instance HasDefaultLogLevel ExtraClangArgsMsg where
  getDefaultLogLevel = \case
    ExtraClangArgsNone -> Debug
    ExtraClangArgsParsed {} -> Info

instance HasSource ExtraClangArgsMsg where
  getSource = const HsBindgen

-- | Run a continuation honoring @libclang@-specific environment variables.
--
-- Extra arguments to `libclang`:
--
-- - If compiling natively, and without a target, use @BINDGEN_EXTRA_CLANG_ARGS@.
--
-- - If cross-compiling to a given target, use
--   @BINDGEN_EXTRA_CLANG_ARGS_<TARGET>@, where @<TARGET>@ is a
--   'Args.targetTriple'. Fall back to @BINDGEN_EXTRA_CLANG_ARGS@ if the
--   target-specific environment variable is unset or empty. In particular, if
--   cross-compiling to a given target, a provided, non-empty, target-specific
--   environment variable takes precedence over `BINDGEN_EXTRA_CLANG_ARGS`,
--   which is unused.
--
-- The values are split into separate command line arguments using
-- 'splitArguments'.
withExtraClangArgs :: (HasCallStack, MonadIO m)
  => Tracer m ExtraClangArgsMsg
  -> ClangArgs -> (ClangArgs -> m a) -> m a
withExtraClangArgs tracer args k = do
  extraClangArgs <- getExtraClangArgs tracer (fst <$> clangTarget args)
  k $ args { clangOtherArgs = clangOtherArgs args <> extraClangArgs }

{-------------------------------------------------------------------------------
  Auxiliary functions.
-------------------------------------------------------------------------------}

getExtraClangArgsEnvName :: Maybe Target -> String
getExtraClangArgsEnvName Nothing       = extraClangArgsEnvNameBase
getExtraClangArgsEnvName (Just target) = extraClangArgsEnvNameBase <> "_"
                                           <> targetTriple target TargetEnvDefault

-- | Split string into command line arguments honoring shell escapes.
--
-- For expectations, see 'Test.HsBindgen.C.Environment.splitArgumentTests'.
splitArguments :: String -> [String]
splitArguments = unescapeArgs

-- | Get extra `clang` arguments from system environment.
--
-- For expectations, see 'Test.HsNindgen.C.Environment.envTests'.
getExtraClangArgs :: (HasCallStack, MonadIO m)
  => Tracer m ExtraClangArgsMsg -> Maybe Target -> m [String]
getExtraClangArgs tracer mtarget = do
  extraClangArgsStr <- liftIO $ lookupEnv extraClangArgsEnvName
  case extraClangArgsStr of
    Nothing ->
      if isJust mtarget
      then getExtraClangArgs tracer Nothing -- Always fall back to no target.
      else traceWith tracer ExtraClangArgsNone >> pure []
    Just content -> do
      let args = splitArguments content
      traceWith tracer $ ExtraClangArgsParsed extraClangArgsEnvName args
      pure args
  where
    extraClangArgsEnvName = getExtraClangArgsEnvName mtarget
