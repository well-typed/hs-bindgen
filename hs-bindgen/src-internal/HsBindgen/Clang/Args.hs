-- | Handle `libclang`-specific environment variables.

module HsBindgen.Clang.Args (
    withExtraClangArgs
  , ExtraClangArgsLog
  -- Exported for tests.
  , splitArguments
  , getExtraClangArgs
  ) where


import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Environment (lookupEnv)

import Clang.Args
import Control.Tracer (Tracer)
import Data.Maybe (isJust)
import GHC.ResponseFile (unescapeArgs)
import GHC.Stack (HasCallStack, callStack)
import HsBindgen.Util.Tracer (HasDefaultLogLevel (getDefaultLogLevel),
                              HasSource (getSource), Level (Debug, Info),
                              PrettyTrace (prettyTrace), Source (HsBindgen),
                              TraceWithCallStack, traceWithCallStack)

extraClangArgsEnvNameBase :: String
extraClangArgsEnvNameBase = "BINDGEN_EXTRA_CLANG_ARGS"

data ExtraClangArgsLog =
    ExtraClangArgsNone
  | ExtraClangArgsParsed { envName    :: String
                         , envArgs    :: [String] }
  deriving stock (Show)

instance PrettyTrace ExtraClangArgsLog where
  prettyTrace = \case
    ExtraClangArgsNone ->
      "No " <> extraClangArgsEnvNameBase <> " environment variables"
    ExtraClangArgsParsed {..} ->
      "Picked up evironment variable " <> envName <>
      "; parsed 'libclang' arguments: " <> show envArgs

instance HasDefaultLogLevel ExtraClangArgsLog where
  getDefaultLogLevel = \case
    ExtraClangArgsNone -> Debug
    ExtraClangArgsParsed {} -> Info

instance HasSource ExtraClangArgsLog where
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
  => Tracer m (TraceWithCallStack ExtraClangArgsLog)
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
  => Tracer m (TraceWithCallStack ExtraClangArgsLog) -> Maybe Target -> m [String]
getExtraClangArgs tracer mtarget = do
  extraClangArgsStr <- liftIO $ lookupEnv extraClangArgsEnvName
  case extraClangArgsStr of
    Nothing ->
      if isJust mtarget
      then getExtraClangArgs tracer Nothing -- Always fall back to no target.
      else traceWithCallStack tracer callStack ExtraClangArgsNone >> pure []
    Just content -> do
      let args = splitArguments content
      traceWithCallStack tracer callStack
        (ExtraClangArgsParsed extraClangArgsEnvName args)
      pure args
  where
    extraClangArgsEnvName = getExtraClangArgsEnvName mtarget
