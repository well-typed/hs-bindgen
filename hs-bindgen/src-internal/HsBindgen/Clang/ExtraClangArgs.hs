module HsBindgen.Clang.ExtraClangArgs (
    -- * Trace messages
    ExtraClangArgsMsg(..)
    -- * API
  , getExtraClangArgs
  , applyExtraClangArgs
    -- * Low-level API (exported for tests)
  , splitArguments
  ) where

import GHC.ResponseFile (unescapeArgs)
import System.Environment (lookupEnv)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Config.ClangArgs
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ExtraClangArgsMsg =
    ExtraClangArgsNone
  | ExtraClangArgsParsed { envName :: String, envArgs :: [String] }
  deriving stock (Show)

instance PrettyForTrace ExtraClangArgsMsg where
  prettyForTrace = \case
    ExtraClangArgsNone -> PP.hsep [
        "No", PP.string envNameBase, "environment variables"
      ]
    ExtraClangArgsParsed{..} -> PP.hcat [
        "Picked up evironment variable ",  PP.string       envName
      , "; parsed 'libclang' arguments: ", PP.showToCtxDoc envArgs
      ]


instance IsTrace Level ExtraClangArgsMsg where
  getDefaultLogLevel = \case
    ExtraClangArgsNone     -> Debug
    ExtraClangArgsParsed{} -> Info
  getSource  = const HsBindgen
  getTraceId = const "extra-clang-args"

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Get extra Clang arguments from system environment
--
-- If compiling natively, without a target, @BINDGEN_EXTRA_CLANG_ARGS@ is used.
--
-- If cross-compiling to a given target, @BINDGEN_EXTRA_CLANG_ARGS_<TARGET>@ is
-- used, where @<TARGET>@ is a 'targetTriple'.  This function falls back to
-- @BINDGEN_EXTRA_CLANG_ARGS@ if the target-specific environment variable is
-- unset or empty.  In particular, if cross-compiling to a given target, a
-- provided, non-empty, target-specific environment variable takes precedence
-- over `BINDGEN_EXTRA_CLANG_ARGS`, which is unused.
getExtraClangArgs :: Tracer ExtraClangArgsMsg -> Maybe Target -> IO [String]
getExtraClangArgs tracer = aux
  where
    aux :: Maybe Target -> IO [String]
    aux mTarget =
      let envName = getEnvName mTarget
      in  fmap splitArguments <$> lookupEnv envName >>= \case
            Nothing
              | isJust mTarget -> aux Nothing  -- Fall back to no target
              | otherwise      -> [] <$ traceWith tracer ExtraClangArgsNone
            Just args ->
              args <$ traceWith tracer (ExtraClangArgsParsed envName args)

-- | Apply extra Clang arguments to 'ClangArgsConfig'
--
-- When configured, extra clang arguments are appended to 'clangArgsInner'.
applyExtraClangArgs :: [String] -> ClangArgsConfig path -> ClangArgsConfig path
applyExtraClangArgs args config = config {
      argsInner = argsInner config ++ args
    }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

envNameBase :: String
envNameBase = "BINDGEN_EXTRA_CLANG_ARGS"

getEnvName :: Maybe Target -> String
getEnvName = \case
    Nothing     -> envNameBase
    Just target -> envNameBase ++ '_' : targetTriple target

-- | Split string into command line arguments, honoring shell escapes
--
-- For expectations, see 'Test.HsBindgen.C.Environment.splitArgumentTests'.
splitArguments :: String -> [String]
splitArguments = unescapeArgs
