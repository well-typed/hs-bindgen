-- | Handle `libclang`-specific environment variables.

module HsBindgen.Clang.Args (
    withExtraClangArgs
  -- Exported for tests.
  , splitArguments
  , getExtraClangArgs
  ) where


import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Environment (lookupEnv)

import Clang.Args
import Data.Maybe (isJust)
import GHC.ResponseFile (unescapeArgs)

extraClangArgsEnvNameBase :: String
extraClangArgsEnvNameBase = "BINDGEN_EXTRA_CLANG_ARGS"

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
withExtraClangArgs :: MonadIO m => ClangArgs -> (ClangArgs -> m a) -> m a
withExtraClangArgs args k = do
  extraClangArgs <- getExtraClangArgs (fst <$> clangTarget args)
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
getExtraClangArgs :: MonadIO m => Maybe Target -> m [String]
getExtraClangArgs mtarget = do
  extraClangArgsStr <- liftIO $ lookupEnv extraClangArgsEnvName
  case extraClangArgsStr of
    Nothing ->
      if isJust mtarget
      then getExtraClangArgs Nothing -- Always fall back to no target.
      else pure []
    -- TODO: Refactor, use tracer (#647).
    Just str -> let args = splitArguments str in
       do liftIO $ putStrLn okMsg
          liftIO $ print args
          pure args
  where
    extraClangArgsEnvName = getExtraClangArgsEnvName mtarget
    okMsg  = "Picked up extra arguments from " <> extraClangArgsEnvName <> ":"
