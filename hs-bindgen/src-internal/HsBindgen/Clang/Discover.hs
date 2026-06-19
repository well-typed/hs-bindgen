module HsBindgen.Clang.Discover (
    -- * Types
    Paths(..)
  , ClangExe
  , BuiltinIncDir
    -- * Trace messages
  , DiscoverMsg(..)
    -- * API
  , getPaths
  , applyPaths
  ) where

import Control.Exception (Exception (displayException))
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import GHC.Stack (CallStack)
import System.Environment qualified as Env
import System.IO.Unsafe (unsafePerformIO)
import Text.SimplePrettyPrint (string, (<+>))
import Text.SimplePrettyPrint qualified as PP

import Clang.Discover (BuiltinIncDir, ClangExe, Paths (..))
import Clang.Discover qualified as Discover

import HsBindgen.Config.ClangArgs
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | Trace messages emitted during path discovery
--
-- The actual discovery is performed by @libclang-bindings@
-- ('Discover.getPaths'); its trace messages are wrapped in 'DiscoverClang'. The
-- remaining constructors cover the @hs-bindgen@-specific environment variable
-- check (see 'getEnvConfig').
data DiscoverMsg =
    DiscoverEnvNone
  | DiscoverEnvSet BuiltinIncDirConfig
  | DiscoverEnvInvalid String
  | DiscoverClang Discover.DiscoverMsg
  deriving stock (Show)

instance PrettyForTrace DiscoverMsg where
  prettyForTrace = \case
    DiscoverEnvNone ->
      PP.string envName <+> "not set"
    DiscoverEnvSet config ->
      PP.string envName <+> "set:" <+> PP.string (show config)
    DiscoverEnvInvalid s ->
      PP.string envName <+> "invalid:" <+> PP.string (show s) <+> "(ignored)"
    DiscoverClang msg -> prettyDiscoverMsg msg

instance IsTrace Level DiscoverMsg where
  getDefaultLogLevel = \case
    DiscoverEnvNone      -> Debug
    DiscoverEnvSet{}     -> Info
    DiscoverEnvInvalid{} -> Warning
    DiscoverClang msg    -> discoverMsgLevel msg

  getSource  = const HsBindgen

  getTraceId = const "builtin-include-dir"

-- | Pretty-print an upstream 'Discover.DiscoverMsg'
prettyDiscoverMsg :: Discover.DiscoverMsg -> PP.CtxDoc
prettyDiscoverMsg = \case
    Discover.DiscoverLlvmPathNotFound path ->
      "$LLVM_PATH path not found or not directory (skipping):" <+> string path
    Discover.DiscoverLlvmConfigPathFound path ->
      "llvm-config found using $PATH:" <+> string path
    Discover.DiscoverLlvmConfigPrefixUnexpected s ->
      "llvm-config --prefix output is unexpected:" <+> string (show s)
    Discover.DiscoverLlvmConfigPrefixIOError e ->
      "IO error calling llvm-config --prefix:" <+> string (displayException e)
    Discover.DiscoverClangNotFound ->
      "clang not found"
    Discover.DiscoverClangVersionMismatch libclangVersion clangVersion ->
      PP.hangs' "clang version mismatch:" 2 [
          "libclang version:" <+> PP.text libclangVersion
        , "clang version:   " <+> PP.text clangVersion
        ]
    Discover.DiscoverClangIncDirNotFound path ->
      "builtin include directory not found using clang:" <+> string path
    Discover.DiscoverClangIncDirFound path ->
      "builtin include directory found using clang:" <+> string path
    Discover.DiscoverLlvmPathClangExeNotFound path ->
      "clang not found using $LLVM_PATH:" <+> string path
    Discover.DiscoverLlvmPathClangExeFound path ->
      "clang found using $LLVM_PATH:" <+> string path
    Discover.DiscoverLlvmConfigClangExeNotFound path ->
      "clang not found using llvm-config:" <+> string path
    Discover.DiscoverLlvmConfigClangExeFound path ->
      "clang found using llvm-config:" <+> string path
    Discover.DiscoverClangPathFound path ->
      "clang found using $PATH:" <+> string path
    Discover.DiscoverClangVersionUnexpected s ->
      "clang --version output is unexpected:" <+> string (show s)
    Discover.DiscoverClangVersionIOError e ->
      "IO error calling clang --version:" <+> string (displayException e)
    Discover.DiscoverClangPrintResourceDirUnexpected s ->
      "clang -print-resource-dir output is unexpected:" <+> string (show s)
    Discover.DiscoverClangPrintResourceDirIOError e ->
      "IO error calling clang -print-resource-dir:"
        <+> string (displayException e)

-- | Default log level for an upstream 'Discover.DiscoverMsg'
discoverMsgLevel :: Discover.DiscoverMsg -> Level
discoverMsgLevel = \case
    Discover.DiscoverLlvmPathNotFound{}                -> Warning
    Discover.DiscoverLlvmConfigPathFound{}             -> Debug
    Discover.DiscoverLlvmConfigPrefixUnexpected{}      -> Warning
    Discover.DiscoverLlvmConfigPrefixIOError{}         -> Warning
    Discover.DiscoverClangNotFound                     -> Debug
    Discover.DiscoverClangVersionMismatch _ _          -> Warning
    Discover.DiscoverClangIncDirNotFound _             -> Warning
    Discover.DiscoverClangIncDirFound{}                -> Info
    Discover.DiscoverLlvmPathClangExeNotFound{}        -> Debug
    Discover.DiscoverLlvmPathClangExeFound{}           -> Debug
    Discover.DiscoverLlvmConfigClangExeNotFound{}      -> Debug
    Discover.DiscoverLlvmConfigClangExeFound{}         -> Debug
    Discover.DiscoverClangPathFound{}                  -> Debug
    Discover.DiscoverClangVersionUnexpected _          -> Warning
    Discover.DiscoverClangVersionIOError _             -> Warning
    Discover.DiscoverClangPrintResourceDirUnexpected _ -> Warning
    Discover.DiscoverClangPrintResourceDirIOError _    -> Warning

{-------------------------------------------------------------------------------
  Global state
-------------------------------------------------------------------------------}

data DiscoverState =
    DiscoverStateInitial
  | DiscoverStateCached Discover.Paths

-- | Global state for caching discovered paths
--
-- Paths should only be discovered a single time. Calling 'getPaths' stores the
-- result in this global state, and any subsequent calls simply returns the
-- cached value.
discoverState :: IORef DiscoverState
discoverState = unsafePerformIO $ IORef.newIORef DiscoverStateInitial
{-# NOINLINE discoverState #-}

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Try to discover paths for the @clang@ executable, and the builtin include
-- directory
--
-- This wraps 'Discover.getPaths' from @libclang-bindings@, adding two
-- @hs-bindgen@-specific behaviours:
--
-- * The @BINDGEN_BUILTIN_INCLUDE_DIR@ environment variable, when set, overrides
--   the passed 'BuiltinIncDirConfig' (see 'getEnvConfig').
-- * Discovered paths are cached: discovery is only performed a single time, and
--   any subsequent call simply returns the cached value.
--
-- See 'Discover.getPaths' for a description of the discovery strategy.
getPaths ::
     Tracer DiscoverMsg
  -> BuiltinIncDirConfig
  -> IO Discover.Paths
getPaths tracer config =
    IORef.readIORef discoverState >>= \case
      DiscoverStateCached paths -> return paths
      DiscoverStateInitial -> do
        mEnvConfig <- getEnvConfig tracer
        paths <- toPaths <$>
          Discover.getPaths trace (fromMaybe config mEnvConfig)
        IORef.writeIORef discoverState (DiscoverStateCached paths)
        return paths
  where
    -- Feed upstream trace messages (paired with their callstack) into our
    -- tracer, wrapped in 'DiscoverClang'.
    trace :: CallStack -> Discover.DiscoverMsg -> IO ()
    trace cs msg = traceWith tracer (WithCallStack cs (DiscoverClang msg))

    toPaths :: Discover.Paths -> Discover.Paths
    toPaths p = Discover.Paths {
        pClangExe      = Discover.pClangExe p
      , pBuiltinIncDir = Discover.pBuiltinIncDir p
      }

-- | Apply the discovered paths to 'Clang.Args.ClangArgs'
--
-- When configured, the builtin include directory is passed with @-isystem@ as
-- the last argument.  This ensures that it is prioritized as close to the
-- default include directories as possible.
applyPaths ::
     Discover.Paths
  -> ClangArgsConfig path
  -> ClangArgsConfig path
applyPaths paths = case paths.pBuiltinIncDir of
    Nothing            -> id
    Just builtinIncDir -> #argsAfter %~ (++ ["-isystem", builtinIncDir])

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Environment variable name
envName :: String
envName = "BINDGEN_BUILTIN_INCLUDE_DIR"

-- | Load configuration from system environment, when available
getEnvConfig :: Tracer DiscoverMsg -> IO (Maybe BuiltinIncDirConfig)
getEnvConfig tracer = Env.lookupEnv envName >>= \case
    Nothing        -> Nothing <$ traceWith tracer (withCallStack DiscoverEnvNone)
    Just "disable" -> aux BuiltinIncDirDisable
    Just "clang"   -> aux BuiltinIncDirClang
    Just s         -> Nothing <$ traceWith tracer (withCallStack $ DiscoverEnvInvalid s)
  where
    aux :: BuiltinIncDirConfig -> IO (Maybe BuiltinIncDirConfig)
    aux config = Just config <$ traceWith tracer (withCallStack $ DiscoverEnvSet config)
