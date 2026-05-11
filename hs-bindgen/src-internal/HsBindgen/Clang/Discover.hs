{-# LANGUAGE CPP #-}

module HsBindgen.Clang.Discover (
    -- * Types
    BuiltinIncDir
    -- * Trace messages
  , DiscoverMsg(..)
    -- * API
  , getBuiltinIncDir
  , applyBuiltinIncDir
  ) where

import Control.Applicative ((<|>), asum)
import Control.Exception (Exception(displayException))
import Control.Monad.Trans.Maybe
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text qualified as Text
import System.Directory qualified as Dir
import System.Environment qualified as Env
import System.FilePath qualified as FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

#ifdef mingw32_HOST_OS
import Data.Char qualified as Char
import System.FilePath.Posix qualified as Posix
import System.FilePath.Windows qualified as Windows
#endif

import Clang.Version
import HsBindgen.Config.ClangArgs
import HsBindgen.Imports
import HsBindgen.Util.Process
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((<+>), string)
import Text.SimplePrettyPrint qualified as PP

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Builtin include directory
type BuiltinIncDir = FilePath

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data DiscoverMsg =
    DiscoverEnvNone
  | DiscoverEnvSet BuiltinIncDirConfig
  | DiscoverEnvInvalid String
  | DiscoverLlvmPathNotFound FilePath
  | DiscoverLlvmConfigEnvNotFound FilePath
  | DiscoverLlvmConfigEnvFound FilePath
  | DiscoverLlvmConfigPathFound FilePath
  | DiscoverLlvmConfigPrefixUnexpected String
  | DiscoverLlvmConfigPrefixIOError IOError
  | DiscoverClangNotFound
  | DiscoverClangVersionMismatch Text Text
  | DiscoverClangIncDirNotFound BuiltinIncDir
  | DiscoverClangIncDirFound BuiltinIncDir
  | DiscoverLlvmPathClangExeNotFound FilePath
  | DiscoverLlvmPathClangExeFound FilePath
  | DiscoverLlvmConfigClangExeNotFound FilePath
  | DiscoverLlvmConfigClangExeFound FilePath
  | DiscoverClangPathFound FilePath
  | DiscoverClangVersionUnexpected String
  | DiscoverClangVersionIOError IOError
  | DiscoverClangPrintResourceDirUnexpected String
  | DiscoverClangPrintResourceDirIOError IOError
  deriving stock (Show)

instance PrettyForTrace DiscoverMsg where
  prettyForTrace = \case
    DiscoverEnvNone ->
      PP.string envName <+> "not set"
    DiscoverEnvSet config ->
      PP.string envName <+> "set:" <+> PP.string (show config)
    DiscoverEnvInvalid s ->
      PP.string envName <+> "invalid:" <+> PP.string (show s) <+> "(ignored)"
    DiscoverLlvmPathNotFound path ->
      "$LLVM_PATH path not found or not directory (skipping):" <+> string path
    DiscoverLlvmConfigEnvNotFound path ->
      "$LLVM_CONFIG path not found or not file (skipping):" <+> string path
    DiscoverLlvmConfigEnvFound path ->
      "llvm-config found using $LLVM_CONFIG:" <+> string path
    DiscoverLlvmConfigPathFound path ->
      "llvm-config found using $PATH:" <+> string path
    DiscoverLlvmConfigPrefixUnexpected s ->
      "llvm-config --prefix output is unexpected:" <+> string (show s)
    DiscoverLlvmConfigPrefixIOError e ->
      "IO error calling llvm-config --prefix:" <+> string (displayException e)
    DiscoverClangNotFound ->
      "clang not found"
    DiscoverClangVersionMismatch libclangVersion clangVersion ->
      PP.hangs' "clang version mismatch:" 2 [
          "libclang version:" <+> PP.text libclangVersion
        , "clang version:   " <+> PP.text clangVersion
        ]
    DiscoverClangIncDirNotFound path ->
      "builtin include directory not found using clang:" <+> string path
    DiscoverClangIncDirFound path ->
      "builtin include directory found using clang:" <+> string path
    DiscoverLlvmPathClangExeNotFound path ->
      "clang not found using $LLVM_PATH:" <+> string path
    DiscoverLlvmPathClangExeFound path ->
      "clang found using $LLVM_PATH:" <+> string path
    DiscoverLlvmConfigClangExeNotFound path ->
      "clang not found using llvm-config:" <+> string path
    DiscoverLlvmConfigClangExeFound path ->
      "clang found using llvm-config:" <+> string path
    DiscoverClangPathFound path ->
      "clang found using $PATH:" <+> string path
    DiscoverClangVersionUnexpected s ->
      "clang --version output is unexpected:" <+> string (show s)
    DiscoverClangVersionIOError e ->
      "IO error calling clang --version:" <+> string (displayException e)
    DiscoverClangPrintResourceDirUnexpected s ->
      "clang -print-resource-dir output is unexpected:" <+> string (show s)
    DiscoverClangPrintResourceDirIOError e ->
      "IO error calling clang -print-resource-dir:"
        <+> string (displayException e)

instance IsTrace Level DiscoverMsg where
  getDefaultLogLevel = \case
    DiscoverEnvNone                           -> Debug
    DiscoverEnvSet{}                          -> Info
    DiscoverEnvInvalid{}                      -> Warning
    DiscoverLlvmPathNotFound{}                -> Warning
    DiscoverLlvmConfigEnvNotFound{}           -> Warning
    DiscoverLlvmConfigEnvFound{}              -> Debug
    DiscoverLlvmConfigPathFound{}             -> Debug
    DiscoverLlvmConfigPrefixUnexpected{}      -> Warning
    DiscoverLlvmConfigPrefixIOError{}         -> Warning
    DiscoverClangNotFound                     -> Debug
    DiscoverClangVersionMismatch _ _          -> Warning
    DiscoverClangIncDirNotFound _             -> Warning
    DiscoverClangIncDirFound{}                -> Info
    DiscoverLlvmPathClangExeNotFound{}        -> Debug
    DiscoverLlvmPathClangExeFound{}           -> Debug
    DiscoverLlvmConfigClangExeNotFound{}      -> Debug
    DiscoverLlvmConfigClangExeFound{}         -> Debug
    DiscoverClangPathFound{}                  -> Debug
    DiscoverClangVersionUnexpected _          -> Warning
    DiscoverClangVersionIOError _             -> Warning
    DiscoverClangPrintResourceDirUnexpected _ -> Warning
    DiscoverClangPrintResourceDirIOError _    -> Warning

  getSource  = const HsBindgen

  getTraceId = const "builtin-include-dir"

{-------------------------------------------------------------------------------
  Global state
-------------------------------------------------------------------------------}

-- | Builtin include directory state
data BuiltinIncDirState =
    BuiltinIncDirInitial
  | BuiltinIncDirCached (Maybe BuiltinIncDir)

-- | Builtin include directory global state
--
-- The builtin include directory should only be determined a single time.
-- Calling 'getBuiltinIncDir' stores the result in this global state, and any
-- subsequent calls simply returns the cached value.
builtinIncDirState :: IORef BuiltinIncDirState
builtinIncDirState = unsafePerformIO $ IORef.newIORef BuiltinIncDirInitial
{-# NOINLINE builtinIncDirState #-}

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Try to get the builtin include directory
--
-- LLVM/Clang determines the builtin include directory based on the path of the
-- executable being run.  When using @libclang@, there is not enough information
-- to determine the absolute builtin include directory.  @hs-bindgen@ can
-- attempt to determine and configure the builtin include directory
-- automatically so that users do not have to do so manually.
--
-- Upstream issues:
--
-- * https://github.com/llvm/llvm-project/issues/18150
-- * https://github.com/llvm/llvm-project/issues/51256
--
-- The builtin include directory is in the Clang resource directory, which
-- contains the executables, headers, and libraries used by the Clang compiler.
--
-- When 'BuiltinIncDirClang' is used, this function tries to determine the
-- builtin include directory using the first successful result of the following
-- strategies:
--
-- 1. @$(${LLVM_PATH}/bin/clang -print-resource-dir)/include@
-- 2. @$($(${LLVM_CONFIG} --prefix)/bin/clang -print-resource-dir)/include@
-- 3. @$($(llvm-config --prefix)/bin/clang -print-resource-dir)/include@
-- 4. @$(clang -print-resource-dir)/include@
--
-- The builtin include directory should only be determined a single time.
-- Calling 'getBuiltinIncDir' caches the result, and any subsequent calls simply
-- returns the cached value.
getBuiltinIncDir ::
     Tracer DiscoverMsg
  -> BuiltinIncDirConfig
  -> IO (Maybe BuiltinIncDir)
getBuiltinIncDir tracer config =
    IORef.readIORef builtinIncDirState >>= \case
      BuiltinIncDirCached mBuiltinIncDir -> return mBuiltinIncDir
      BuiltinIncDirInitial -> do
        mEnvConfig <- getEnvConfig tracer
        mBuiltinIncDir <- case fromMaybe config mEnvConfig of
          BuiltinIncDirDisable -> return Nothing
          BuiltinIncDirClang -> runMaybeT $ getBuiltinIncDirWithClang tracer
        IORef.writeIORef builtinIncDirState (BuiltinIncDirCached mBuiltinIncDir)
        return mBuiltinIncDir

-- | Apply the builtin include directory to 'Clang.Args.ClangArgs'
--
-- When configured, the builtin include directory is passed with @-isystem@ as
-- the last argument.  This ensures that it is prioritized as close to the
-- default include directories as possible.
applyBuiltinIncDir ::
     Maybe BuiltinIncDir
  -> ClangArgsConfig path -> ClangArgsConfig path
applyBuiltinIncDir = \case
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

-- | Get the builtin include directory using @clang@
--
-- @clang -print-file-name=include@ is called to get the builtin include
-- directory.
getBuiltinIncDirWithClang :: Tracer DiscoverMsg -> MaybeT IO BuiltinIncDir
getBuiltinIncDirWithClang tracer = do
    exe <- findClangExe tracer <|> do
      traceWith tracer (withCallStack DiscoverClangNotFound)
      MaybeT $ return Nothing
    clangVersionString <- getClangVersion tracer exe
    let clangVersion = parseClangVersion clangVersionString
    unless (isCompatibleClangVersion runtimeClangVersion clangVersion) $ do
      traceWith tracer . withCallStack $
        DiscoverClangVersionMismatch
          runtimeClangVersionString
          clangVersionString
      MaybeT $ return Nothing
    resourceDir <- getClangResourceDir tracer exe
    ifM
     tracer
     DiscoverClangIncDirNotFound
     DiscoverClangIncDirFound
     Dir.doesDirectoryExist
     (FilePath.joinPath [resourceDir, "include"])

-- | Find the @clang@ executable
--
-- 1. @${LLVM_PATH}/bin/clang@
-- 2. @$(${LLVM_CONFIG} --prefix)/bin/clang@
-- 3. @$(llvm-config --prefix)/bin/clang@
-- 4. Search @${PATH}@
findClangExe :: Tracer DiscoverMsg -> MaybeT IO FilePath
findClangExe tracer = asum [auxLlvmPath, auxLlvmConfig, auxPath]
  where
    auxLlvmPath :: MaybeT IO FilePath
    auxLlvmPath = do
      prefix <- lookupLlvmPath tracer
      ifM
        tracer
        DiscoverLlvmPathClangExeNotFound
        DiscoverLlvmPathClangExeFound
        Dir.doesFileExist
        (FilePath.joinPath [prefix, "bin", clangExe])

    auxLlvmConfig :: MaybeT IO FilePath
    auxLlvmConfig = do
      exe <- findLlvmConfigExe tracer
      prefix <- getLlvmConfigPrefix tracer exe
      ifM
        tracer
        DiscoverLlvmConfigClangExeNotFound
        DiscoverLlvmConfigClangExeFound
        Dir.doesFileExist
        (FilePath.joinPath [prefix, "bin", clangExe])

    auxPath :: MaybeT IO FilePath
    auxPath = do
      exe <- MaybeT $ Dir.findExecutable clangExe
      traceWith tracer (withCallStack $ DiscoverClangPathFound exe)
      return exe

-- | @clang@ executable name for the current platform
clangExe :: FilePath
clangExe =
#ifdef mingw32_HOST_OS
    "clang.exe"
#else
    "clang"
#endif

-- | Lookup @LLVM_PATH@ environment variable
lookupLlvmPath :: Tracer DiscoverMsg -> MaybeT IO FilePath
lookupLlvmPath tracer = do
    prefix <- MaybeT $ fmap normWinPath <$> Env.lookupEnv "LLVM_PATH"
    MaybeT $ Dir.doesDirectoryExist prefix >>= \case
      True  -> return (Just prefix)
      False -> do
        traceWith tracer (withCallStack $ DiscoverLlvmPathNotFound prefix)
        return Nothing

-- | Find the @llvm-config@ executable
--
-- 1. @${LLVM_CONFIG}@
-- 2. Search @${PATH}@
findLlvmConfigExe :: Tracer DiscoverMsg -> MaybeT IO FilePath
findLlvmConfigExe tracer = asum [auxLlvmConfigEnv, auxPath]
  where
    auxLlvmConfigEnv :: MaybeT IO FilePath
    auxLlvmConfigEnv = do
      exe <- MaybeT $ fmap normWinPath <$> Env.lookupEnv "LLVM_CONFIG"
      ifM
        tracer
        DiscoverLlvmConfigEnvNotFound
        DiscoverLlvmConfigEnvFound
        Dir.doesFileExist
        exe

    auxPath :: MaybeT IO FilePath
    auxPath = do
      exe <- MaybeT $ Dir.findExecutable llvmConfigExe
      traceWith tracer (withCallStack $ DiscoverLlvmConfigPathFound exe)
      return exe

-- | @llvm-config@ executable name for the current platform
llvmConfigExe :: FilePath
llvmConfigExe =
#ifdef mingw32_HOST_OS
    "llvm-config.exe"
#else
    "llvm-config"
#endif

-- | Get the prefix from @llvm-config@
--
-- This function calls @llvm-config --prefix@ and captures the output.
getLlvmConfigPrefix ::
     Tracer DiscoverMsg
  -> FilePath  -- ^ @llvm-config@ path
  -> MaybeT IO FilePath
getLlvmConfigPrefix tracer exe = MaybeT $
    checkOutput
      tracer
      DiscoverLlvmConfigPrefixUnexpected
      DiscoverLlvmConfigPrefixIOError
      (fmap normWinPath . parseSingleLine)
      (readProcess exe ["--prefix"] "")

-- | Get the Clang version from @clang@
--
-- This function calls @clang --version@ and captures the output.  The full
-- version string in the first line is returned.
getClangVersion ::
     Tracer DiscoverMsg
  -> FilePath  -- ^ @clang@ path
  -> MaybeT IO Text
getClangVersion tracer exe = MaybeT $
    checkOutput
      tracer
      DiscoverClangVersionUnexpected
      DiscoverClangVersionIOError
      (fmap Text.pack . parseFirstLine)
      (readProcess exe ["--version"] "")

-- | Get the resource directory from @clang@
--
-- This function calls @clang -print-resource-dir@ and captures the output.
getClangResourceDir ::
     Tracer DiscoverMsg
  -> FilePath  -- ^ @clang@ path
  -> MaybeT IO FilePath
getClangResourceDir tracer exe = MaybeT $
    checkOutput
      tracer
      DiscoverClangPrintResourceDirUnexpected
      DiscoverClangPrintResourceDirIOError
      (fmap normWinPath . parseSingleLine)
      (readProcess exe ["-print-resource-dir"] "")

--------------------------------------------------------------------------------

-- | Normalise Windows paths
--
-- This is just the identity function on non-Windows platforms.
normWinPath :: FilePath -> FilePath
#ifdef mingw32_HOST_OS
normWinPath path
    -- | Do not change paths with no @/@ in them
    | '/' `notElem` path = path
    | otherwise = case path of
        -- Convert POSIX absolute paths specifying the Windows drive
        '/' : drv : '/' : relPath -> Char.toUpper drv : ":\\" ++ aux relPath
        -- Do not change other POSIX absolute paths
        '/' : _                   -> path
        -- Normalise hybrid paths
        drv : ':' : '/' : relPath -> Char.toUpper drv : ":\\" ++ aux relPath
        -- Normalise relative paths
        relPath                   -> aux relPath
  where
    aux :: FilePath -> FilePath
    aux = Windows.joinPath . Posix.splitDirectories
#else
normWinPath = id
#endif

-- | Return a path only if it passes a predicate, tracing result
ifM ::
     Tracer DiscoverMsg
  -> (FilePath -> DiscoverMsg)  -- ^ not found constructor
  -> (FilePath -> DiscoverMsg)  -- ^ found constructor
  -> (FilePath -> IO Bool)           -- ^ predicate
  -> FilePath                        -- ^ path
  -> MaybeT IO FilePath
ifM tracer mkNotFound mkFound p path = MaybeT $ p path >>= \case
    True  -> Just path <$ traceWith tracer (withCallStack $ mkFound    path)
    False -> Nothing   <$ traceWith tracer (withCallStack $ mkNotFound path)
