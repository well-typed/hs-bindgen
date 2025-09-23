{-# LANGUAGE CPP #-}

module HsBindgen.Clang.BuiltinIncDir (
    -- * Types
    BuiltinIncDir
    -- * Trace messages
  , BuiltinIncDirMsg(..)
    -- * API
  , getBuiltinIncDir
  , applyBuiltinIncDir
  ) where

import Control.Applicative ((<|>), asum)
import Control.Exception (Exception(displayException))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Maybe
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import System.Directory qualified as Dir
import System.Environment qualified as Env
import System.FilePath qualified as FilePath
import System.IO.Error (tryIOError)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

#ifdef mingw32_HOST_OS
import Data.Char qualified as Char
import System.FilePath.Posix qualified as Posix
import System.FilePath.Windows qualified as Windows
#endif

import Clang.Version (clang_getClangVersion)
import HsBindgen.Config.ClangArgs
import HsBindgen.Imports
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

data BuiltinIncDirMsg =
    BuiltinIncDirEnvNone
  | BuiltinIncDirEnvSet BuiltinIncDirConfig
  | BuiltinIncDirEnvInvalid String
  | BuiltinIncDirLlvmPathNotFound FilePath
  | BuiltinIncDirLlvmConfigEnvNotFound FilePath
  | BuiltinIncDirLlvmConfigEnvFound FilePath
  | BuiltinIncDirLlvmConfigPathFound FilePath
  | BuiltinIncDirLlvmConfigPrefixUnexpected String
  | BuiltinIncDirLlvmConfigPrefixIOError IOError
  | BuiltinIncDirClangNotFound
  | BuiltinIncDirClangVersionMismatch Text Text
  | BuiltinIncDirClangIncDirNotFound BuiltinIncDir
  | BuiltinIncDirClangIncDirFound BuiltinIncDir
  | BuiltinIncDirLlvmPathClangExeNotFound FilePath
  | BuiltinIncDirLlvmPathClangExeFound FilePath
  | BuiltinIncDirLlvmConfigClangExeNotFound FilePath
  | BuiltinIncDirLlvmConfigClangExeFound FilePath
  | BuiltinIncDirClangPathFound FilePath
  | BuiltinIncDirClangVersionUnexpected String
  | BuiltinIncDirClangVersionIOError IOError
  | BuiltinIncDirClangPrintResourceDirUnexpected String
  | BuiltinIncDirClangPrintResourceDirIOError IOError
  deriving stock (Show)

instance PrettyForTrace BuiltinIncDirMsg where
  prettyForTrace = \case
    BuiltinIncDirEnvNone ->
      PP.string envName <+> "not set"
    BuiltinIncDirEnvSet config ->
      PP.string envName <+> "set:" <+> PP.string (show config)
    BuiltinIncDirEnvInvalid s ->
      PP.string envName <+> "invalid:" <+> PP.string (show s) <+> "(ignored)"
    BuiltinIncDirLlvmPathNotFound path ->
      "$LLVM_PATH path not found or not directory (skipping):" <+> string path
    BuiltinIncDirLlvmConfigEnvNotFound path ->
      "$LLVM_CONFIG path not found or not file (skipping):" <+> string path
    BuiltinIncDirLlvmConfigEnvFound path ->
      "llvm-config found using $LLVM_CONFIG:" <+> string path
    BuiltinIncDirLlvmConfigPathFound path ->
      "llvm-config found using $PATH:" <+> string path
    BuiltinIncDirLlvmConfigPrefixUnexpected s ->
      "llvm-config --prefix output is unexpected:" <+> string (show s)
    BuiltinIncDirLlvmConfigPrefixIOError e ->
      "IO error calling llvm-config --prefix:" <+> string (displayException e)
    BuiltinIncDirClangNotFound ->
      "clang not found"
    BuiltinIncDirClangVersionMismatch libclangVersion clangVersion ->
      PP.hangs' "clang version mismatch:" 2 [
          "libclang version:" <+> PP.textToCtxDoc libclangVersion
        , "clang version:   " <+> PP.textToCtxDoc clangVersion
        ]
    BuiltinIncDirClangIncDirNotFound path ->
      "builtin include directory not found using clang:" <+> string path
    BuiltinIncDirClangIncDirFound path ->
      "builtin include directory found using clang:" <+> string path
    BuiltinIncDirLlvmPathClangExeNotFound path ->
      "clang not found using $LLVM_PATH:" <+> string path
    BuiltinIncDirLlvmPathClangExeFound path ->
      "clang found using $LLVM_PATH:" <+> string path
    BuiltinIncDirLlvmConfigClangExeNotFound path ->
      "clang not found using llvm-config:" <+> string path
    BuiltinIncDirLlvmConfigClangExeFound path ->
      "clang found using llvm-config:" <+> string path
    BuiltinIncDirClangPathFound path ->
      "clang found using $PATH:" <+> string path
    BuiltinIncDirClangVersionUnexpected s ->
      "clang --version output is unexpected:" <+> string (show s)
    BuiltinIncDirClangVersionIOError e ->
      "IO error calling clang --version:" <+> string (displayException e)
    BuiltinIncDirClangPrintResourceDirUnexpected s ->
      "clang -print-resource-dir output is unexpected:" <+> string (show s)
    BuiltinIncDirClangPrintResourceDirIOError e ->
      "IO error calling clang -print-resource-dir:"
        <+> string (displayException e)

instance IsTrace Level BuiltinIncDirMsg where
  getDefaultLogLevel = \case
    BuiltinIncDirEnvNone                           -> Debug
    BuiltinIncDirEnvSet{}                          -> Info
    BuiltinIncDirEnvInvalid{}                      -> Warning
    BuiltinIncDirLlvmPathNotFound{}                -> Warning
    BuiltinIncDirLlvmConfigEnvNotFound{}           -> Warning
    BuiltinIncDirLlvmConfigEnvFound{}              -> Debug
    BuiltinIncDirLlvmConfigPathFound{}             -> Debug
    BuiltinIncDirLlvmConfigPrefixUnexpected{}      -> Warning
    BuiltinIncDirLlvmConfigPrefixIOError{}         -> Warning
    BuiltinIncDirClangNotFound                     -> Debug
    BuiltinIncDirClangVersionMismatch _ _          -> Warning
    BuiltinIncDirClangIncDirNotFound _             -> Warning
    BuiltinIncDirClangIncDirFound{}                -> Info
    BuiltinIncDirLlvmPathClangExeNotFound{}        -> Debug
    BuiltinIncDirLlvmPathClangExeFound{}           -> Debug
    BuiltinIncDirLlvmConfigClangExeNotFound{}      -> Debug
    BuiltinIncDirLlvmConfigClangExeFound{}         -> Debug
    BuiltinIncDirClangPathFound{}                  -> Debug
    BuiltinIncDirClangVersionUnexpected _          -> Warning
    BuiltinIncDirClangVersionIOError _             -> Warning
    BuiltinIncDirClangPrintResourceDirUnexpected _ -> Warning
    BuiltinIncDirClangPrintResourceDirIOError _    -> Warning

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
     Tracer IO BuiltinIncDirMsg
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

-- | Apply the builtin include directory to 'ClangArgs'
--
-- When configured, the builtin include directory is passed with @-isystem@ as
-- the last argument.  This ensures that it is prioritized as close to the
-- default include directories as possible.
applyBuiltinIncDir ::
  Maybe BuiltinIncDir -> ClangArgsConfig path -> ClangArgsConfig path
applyBuiltinIncDir mBuiltinIncDir config = case mBuiltinIncDir of
    Nothing            -> config
    Just builtinIncDir -> config{
        clangArgsAfter = clangArgsAfter config ++ ["-isystem", builtinIncDir]
      }

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Environment variable name
envName :: String
envName = "BINDGEN_BUILTIN_INCLUDE_DIR"

-- | Load configuration from system environment, when available
getEnvConfig :: Tracer IO BuiltinIncDirMsg -> IO (Maybe BuiltinIncDirConfig)
getEnvConfig tracer = Env.lookupEnv envName >>= \case
    Nothing        -> Nothing <$ traceWith tracer BuiltinIncDirEnvNone
    Just "disable" -> aux BuiltinIncDirDisable
    Just "clang"   -> aux BuiltinIncDirClang
    Just s         -> Nothing <$ traceWith tracer (BuiltinIncDirEnvInvalid s)
  where
    aux :: BuiltinIncDirConfig -> IO (Maybe BuiltinIncDirConfig)
    aux config = Just config <$ traceWith tracer (BuiltinIncDirEnvSet config)

-- | Get the builtin include directory using @clang@
--
-- @clang -print-file-name=include@ is called to get the builtin include
-- directory.
getBuiltinIncDirWithClang :: Tracer IO BuiltinIncDirMsg -> MaybeT IO BuiltinIncDir
getBuiltinIncDirWithClang tracer = do
    exe <- findClangExe tracer <|> do
      lift $ traceWith tracer BuiltinIncDirClangNotFound
      MaybeT $ return Nothing
    clangVer <- getClangVersion tracer exe
    libclangVer <- lift clang_getClangVersion
    unless (clangVer == libclangVer) $ do
      lift . traceWith tracer $
        BuiltinIncDirClangVersionMismatch libclangVer clangVer
      MaybeT $ return Nothing
    resourceDir <- getClangResourceDir tracer exe
    ifM
     tracer
     BuiltinIncDirClangIncDirNotFound
     BuiltinIncDirClangIncDirFound
     Dir.doesDirectoryExist
     (FilePath.joinPath [resourceDir, "include"])

-- | Find the @clang@ executable
--
-- 1. @${LLVM_PATH}/bin/clang@
-- 2. @$(${LLVM_CONFIG} --prefix)/bin/clang@
-- 3. @$(llvm-config --prefix)/bin/clang@
-- 4. Search @${PATH}@
findClangExe :: Tracer IO BuiltinIncDirMsg -> MaybeT IO FilePath
findClangExe tracer = asum [auxLlvmPath, auxLlvmConfig, auxPath]
  where
    auxLlvmPath :: MaybeT IO FilePath
    auxLlvmPath = do
      prefix <- lookupLlvmPath tracer
      ifM
        tracer
        BuiltinIncDirLlvmPathClangExeNotFound
        BuiltinIncDirLlvmPathClangExeFound
        Dir.doesFileExist
        (FilePath.joinPath [prefix, "bin", clangExe])

    auxLlvmConfig :: MaybeT IO FilePath
    auxLlvmConfig = do
      exe <- findLlvmConfigExe tracer
      prefix <- getLlvmConfigPrefix tracer exe
      ifM
        tracer
        BuiltinIncDirLlvmConfigClangExeNotFound
        BuiltinIncDirLlvmConfigClangExeFound
        Dir.doesFileExist
        (FilePath.joinPath [prefix, "bin", clangExe])

    auxPath :: MaybeT IO FilePath
    auxPath = do
      exe <- MaybeT $ Dir.findExecutable clangExe
      lift $ traceWith tracer (BuiltinIncDirClangPathFound exe)
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
lookupLlvmPath :: Tracer IO BuiltinIncDirMsg -> MaybeT IO FilePath
lookupLlvmPath tracer = do
    prefix <- MaybeT $ fmap normWinPath <$> Env.lookupEnv "LLVM_PATH"
    MaybeT $ Dir.doesDirectoryExist prefix >>= \case
      True  -> return (Just prefix)
      False -> do
        traceWith tracer (BuiltinIncDirLlvmPathNotFound prefix)
        return Nothing

-- | Find the @llvm-config@ executable
--
-- 1. @${LLVM_CONFIG}@
-- 2. Search @${PATH}@
findLlvmConfigExe :: Tracer IO BuiltinIncDirMsg -> MaybeT IO FilePath
findLlvmConfigExe tracer = asum [auxLlvmConfigEnv, auxPath]
  where
    auxLlvmConfigEnv :: MaybeT IO FilePath
    auxLlvmConfigEnv = do
      exe <- MaybeT $ fmap normWinPath <$> Env.lookupEnv "LLVM_CONFIG"
      ifM
        tracer
        BuiltinIncDirLlvmConfigEnvNotFound
        BuiltinIncDirLlvmConfigEnvFound
        Dir.doesFileExist
        exe

    auxPath :: MaybeT IO FilePath
    auxPath = do
      exe <- MaybeT $ Dir.findExecutable llvmConfigExe
      lift $ traceWith tracer (BuiltinIncDirLlvmConfigPathFound exe)
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
     Tracer IO BuiltinIncDirMsg
  -> FilePath  -- ^ @llvm-config@ path
  -> MaybeT IO FilePath
getLlvmConfigPrefix tracer exe =
    checkOutput
      tracer
      BuiltinIncDirLlvmConfigPrefixUnexpected
      BuiltinIncDirLlvmConfigPrefixIOError
      (fmap normWinPath . parseSingleLine)
      (readProcess exe ["--prefix"] "")

-- | Get the Clang version from @clang@
--
-- This function calls @clang --version@ and captures the output.  The full
-- version string in the first line is returned.
getClangVersion ::
     Tracer IO BuiltinIncDirMsg
  -> FilePath  -- ^ @clang@ path
  -> MaybeT IO Text
getClangVersion tracer exe =
    checkOutput
      tracer
      BuiltinIncDirClangVersionUnexpected
      BuiltinIncDirClangVersionIOError
      (fmap Text.pack . parseFirstLine)
      (readProcess exe ["--version"] "")

-- | Get the resource directory from @clang@
--
-- This function calls @clang -print-resource-dir@ and captures the output.
getClangResourceDir ::
     Tracer IO BuiltinIncDirMsg
  -> FilePath  -- ^ @clang@ path
  -> MaybeT IO FilePath
getClangResourceDir tracer exe =
    checkOutput
      tracer
      BuiltinIncDirClangPrintResourceDirUnexpected
      BuiltinIncDirClangPrintResourceDirIOError
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
     Tracer IO BuiltinIncDirMsg
  -> (FilePath -> BuiltinIncDirMsg)  -- ^ not found constructor
  -> (FilePath -> BuiltinIncDirMsg)  -- ^ found constructor
  -> (FilePath -> IO Bool)           -- ^ predicate
  -> FilePath                        -- ^ path
  -> MaybeT IO FilePath
ifM tracer mkNotFound mkFound p path = MaybeT $ p path >>= \case
    True  -> Just path <$ traceWith tracer (mkFound    path)
    False -> Nothing   <$ traceWith tracer (mkNotFound path)

-- | Run a read action and check the output
checkOutput ::
     Tracer IO BuiltinIncDirMsg
  -> (String -> BuiltinIncDirMsg)   -- ^ Unexpected output constructor
  -> (IOError -> BuiltinIncDirMsg)  -- ^ Error constructor
  -> (String -> Maybe a)            -- ^ Output parser
  -> IO String                      -- ^ Read action
  -> MaybeT IO a
checkOutput tracer mkUnexpected mkError parse action = MaybeT $
    tryIOError action >>= \case
      Right s -> do
        let mx = parse s
        when (isNothing mx) $ traceWith tracer (mkUnexpected (abbr s))
        return mx
      Left  e -> Nothing <$ traceWith tracer (mkError e)
  where
    -- Abbreviate arbitrarily long strings in trace messages
    abbr :: String -> String
    abbr s = case splitAt 60 s of
      (_, []) -> s
      (s', _) -> s' ++ " ..."

-- | Parse a single line of output
parseSingleLine :: String -> Maybe String
parseSingleLine s = case lines s of
    [s'] -> Just s'
    _    -> Nothing

-- | Parse the first line of output
parseFirstLine :: String -> Maybe String
parseFirstLine = listToMaybe . lines
