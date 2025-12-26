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
import Text.SimplePrettyPrint ((><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Config.ClangArgs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data ExtraClangArgsMsg =
    ExtraClangArgsNotSet
  | ExtraClangArgsEmpty
  | ExtraClangArgsParsed [String]
  deriving stock (Show)

instance PrettyForTrace ExtraClangArgsMsg where
  prettyForTrace = \case
    ExtraClangArgsNotSet -> PP.string envName >< " environment variable not set"
    ExtraClangArgsEmpty  -> PP.string envName >< " environment variable empty"
    ExtraClangArgsParsed args ->
      PP.string envName >< " environment variable parsed 'libclang' arguments: "
        >< PP.show args

instance IsTrace Level ExtraClangArgsMsg where
  getDefaultLogLevel = \case
    ExtraClangArgsNotSet   -> Debug
    ExtraClangArgsEmpty    -> Debug
    ExtraClangArgsParsed{} -> Info
  getSource  = const HsBindgen
  getTraceId = const "extra-clang-args"

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Get extra Clang arguments from system environment
--
-- This function reads the @BINDGEN_EXTRA_CLANG_ARGS@ environment variable.
-- Values are split into command-line arguments, respecting shell escapes.
getExtraClangArgs :: Tracer ExtraClangArgsMsg -> IO [String]
getExtraClangArgs tracer = fmap splitArguments <$> lookupEnv envName >>= \case
    Nothing   -> []   <$ traceWith tracer ExtraClangArgsNotSet
    Just []   -> []   <$ traceWith tracer ExtraClangArgsEmpty
    Just args -> args <$ traceWith tracer (ExtraClangArgsParsed args)

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

envName :: String
envName = "BINDGEN_EXTRA_CLANG_ARGS"

-- | Split string into command-line arguments, respecting shell escapes
--
-- For expectations, see @Test.HsBindgen.C.Environment.splitArgumentTests@.
splitArguments :: String -> [String]
splitArguments = unescapeArgs
