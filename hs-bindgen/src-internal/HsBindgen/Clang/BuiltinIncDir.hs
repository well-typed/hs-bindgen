{-# LANGUAGE CPP #-}

module HsBindgen.Clang.BuiltinIncDir (
    -- * Types
    BuiltinIncDir
    -- * Trace messages
  , BuiltinIncDirMsg(..)
    -- * Configuration
  , BuiltinIncDirConfig(..)
    -- * API
  , getBuiltinIncDir
  , applyBuiltinIncDir
  ) where

import Control.Exception (Exception(displayException))
import Control.Monad ((<=<))
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text qualified as Text
import System.Directory qualified as Dir
import System.Environment qualified as Env
import System.FilePath qualified as FilePath
import System.IO.Error (tryIOError)
import System.IO.Silently qualified as Silently
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

#ifdef mingw32_HOST_OS
import Data.Char qualified as Char
import System.FilePath.Posix qualified as Posix
import System.FilePath.Windows qualified as Windows
#endif

import Clang.Args
import Clang.LowLevel.Core
import HsBindgen.Clang
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((<+>), string)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Builtin include directory
type BuiltinIncDir = FilePath

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data BuiltinIncDirMsg =
    BuiltinIncDirResourceDirEmpty
  | BuiltinIncDirResourceDirAbort
  | BuiltinIncDirResourceDirResolved FilePath
  | BuiltinIncDirAbsResourceDirIncDirNotFound BuiltinIncDir
  | BuiltinIncDirAbsResourceDirIncDirFound BuiltinIncDir
  | BuiltinIncDirLlvmPathNotFound FilePath
  | BuiltinIncDirLlvmPathIncDirNotFound BuiltinIncDir
  | BuiltinIncDirLlvmPathIncDirFound BuiltinIncDir
  | BuiltinIncDirLlvmConfigIncDirNotFound BuiltinIncDir
  | BuiltinIncDirLlvmConfigIncDirFound BuiltinIncDir
  | BuiltinIncDirLlvmConfigEnvNotFound FilePath
  | BuiltinIncDirLlvmConfigEnvFound FilePath
  | BuiltinIncDirLlvmConfigPathFound FilePath
  | BuiltinIncDirLlvmConfigPrefixUnexpected String
  | BuiltinIncDirLlvmConfigPrefixIOError IOError
  | BuiltinIncDirClangIncDirNotFound BuiltinIncDir
  | BuiltinIncDirClangIncDirFound BuiltinIncDir
  | BuiltinIncDirLlvmPathClangExeNotFound FilePath
  | BuiltinIncDirLlvmPathClangExeFound FilePath
  | BuiltinIncDirLlvmConfigClangExeNotFound FilePath
  | BuiltinIncDirLlvmConfigClangExeFound FilePath
  | BuiltinIncDirClangPathFound FilePath
  | BuiltinIncDirClangUnexpected String
  | BuiltinIncDirClangIOError IOError
  deriving stock (Eq, Show)

instance PrettyForTrace BuiltinIncDirMsg where
  prettyForTrace = \case
    BuiltinIncDirResourceDirEmpty ->
      "empty resource directory"
    BuiltinIncDirResourceDirAbort ->
      "aborted resource directory resolution"
    BuiltinIncDirResourceDirResolved path ->
      "resource directory found:" <+> string path
    BuiltinIncDirAbsResourceDirIncDirNotFound path ->
      "builtin include directory not found using absolute resource directory:"
        <+> string path
    BuiltinIncDirAbsResourceDirIncDirFound path ->
      "builtin include directory found using absolute resource directory:"
        <+> string path
    BuiltinIncDirLlvmPathNotFound path ->
      "$LLVM_PATH path not found or not directory (skipping):" <+> string path
    BuiltinIncDirLlvmPathIncDirNotFound path ->
      "builtin include directory not found using $LLVM_PATH:" <+> string path
    BuiltinIncDirLlvmPathIncDirFound path ->
      "builtin include directory found using $LLVM_PATH:" <+> string path
    BuiltinIncDirLlvmConfigIncDirNotFound path ->
      "builtin include directory not found using llvm-config:" <+> string path
    BuiltinIncDirLlvmConfigIncDirFound path ->
      "builtin include directory found using llvm-config:" <+> string path
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
    BuiltinIncDirClangUnexpected s ->
      "clang -print-file-name=include output is unexpected:" <+> string (show s)
    BuiltinIncDirClangIOError e ->
      "IO error calling clang -print-file-name=include:"
        <+> string (displayException e)

instance IsTrace Level BuiltinIncDirMsg where
  getDefaultLogLevel = \case
    BuiltinIncDirResourceDirEmpty{}             -> Warning
    BuiltinIncDirResourceDirAbort{}             -> Warning
    BuiltinIncDirResourceDirResolved{}          -> Debug
    BuiltinIncDirAbsResourceDirIncDirNotFound{} -> Warning
    BuiltinIncDirAbsResourceDirIncDirFound{}    -> Debug
    BuiltinIncDirLlvmPathNotFound{}             -> Warning
    BuiltinIncDirLlvmPathIncDirNotFound{}       -> Warning
    BuiltinIncDirLlvmPathIncDirFound{}          -> Debug
    BuiltinIncDirLlvmConfigIncDirNotFound{}     -> Warning
    BuiltinIncDirLlvmConfigIncDirFound{}        -> Debug
    BuiltinIncDirLlvmConfigEnvNotFound{}        -> Warning
    BuiltinIncDirLlvmConfigEnvFound{}           -> Debug
    BuiltinIncDirLlvmConfigPathFound{}          -> Debug
    BuiltinIncDirLlvmConfigPrefixUnexpected{}   -> Warning
    BuiltinIncDirLlvmConfigPrefixIOError{}      -> Warning
    BuiltinIncDirClangIncDirNotFound{}          -> Warning
    BuiltinIncDirClangIncDirFound{}             -> Debug
    BuiltinIncDirLlvmPathClangExeNotFound{}     -> Debug
    BuiltinIncDirLlvmPathClangExeFound{}        -> Debug
    BuiltinIncDirLlvmConfigClangExeNotFound{}   -> Debug
    BuiltinIncDirLlvmConfigClangExeFound{}      -> Debug
    BuiltinIncDirClangPathFound{}               -> Debug
    BuiltinIncDirClangUnexpected{}              -> Warning
    BuiltinIncDirClangIOError{}                 -> Warning

  getSource = const HsBindgen

  getTraceId = const "builtin-include-dir"

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Configure how to get the builtin include directory
data BuiltinIncDirConfig =
    -- | Do not configure the builtin include directory
    BuiltinIncDirDisable

    -- | Configure the builtin include directory using the resource directory
    -- from @libclang@
  | BuiltinIncDirAuto

    -- | Configure the builtin include directory using just @clang@
  | BuiltinIncDirClang
  deriving (Eq, Show)

instance Default BuiltinIncDirConfig where
  def = BuiltinIncDirAuto

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
-- The builtin include directory is in the LLVM resource directory:
-- @${RESOURCE_DIR}/include@.  @libclang@ can print the resource directory, but
-- it currently prints a directory relative to the LLVM prefix due to the same
-- issue.  Note that the builtin include directory /cannot/ be determined from
-- just the LLVM prefix, because the resource directory path includes an
-- LLVM/Clang version number that may be either a full version number or just
-- the major version number, depending on how LLVM/Clang was installed.
--
-- With 'BuiltinIncDirAuto', we redirect @STDOUT@ and attempt to get the
-- resource directory from the @libclang@ library that is dynamically loaded at
-- runtime.  Since @STDOUT@ is redirected for the whole process, this function
-- is /not/ thread-safe.
--
-- If @libclang@ reports an absolute resource directory (when running a
-- fixed/patched version of LLVM), the builtin include directory is
-- @${RESOURCE_DIR}/include@.
--
-- If @libclang@ reports a relative resource directory, we can determine the
-- builtin include directory if we can determine the LLVM prefix.  This
-- function tries to determine the builtin include directory using the first
-- successful result of the following strategies:
--
-- 1. @${LLVM_PATH}/${RESOURCE_DIR}/include@
-- 2. @$(${LLVM_CONFIG} --prefix)/${RESOURCE_DIR}/include@
-- 3. @$(llvm-config --prefix)/${RESOURCE_DIR}/include@
-- 4. @$(${LLVM_PATH}/bin/clang -print-file-name=include)@
-- 5. @$($(${LLVM_CONFIG} --prefix)/bin/clang -print-file-name=include)@
-- 6. @$($(llvm-config --prefix)/bin/clang -print-file-name=include)@
-- 7. @$(clang -print-file-name=include)@
--
-- If @libclang@ does not report a resource directory or 'BuiltinIncDirClang'
-- is used, this function tries to determine the builtin include directory using
-- the first successful result of the following strategies:
--
-- 1. @$(${LLVM_PATH}/bin/clang -print-file-name=include)@
-- 2. @$($(${LLVM_CONFIG} --prefix)/bin/clang -print-file-name=include)@
-- 3. @$($(llvm-config --prefix)/bin/clang -print-file-name=include)@
-- 4. @$(clang -print-file-name=include)@
--
-- The builtin include directory should only be determined a single time.
-- Calling 'getBuiltinIncDir' caches the result, and any subsequent calls simply
-- returns the cached value.
getBuiltinIncDir ::
     Tracer IO BuiltinIncDirMsg
  -> BuiltinIncDirConfig
  -> IO (Maybe BuiltinIncDir)
getBuiltinIncDir tracer config = IORef.readIORef builtinIncDirState >>= \case
    BuiltinIncDirCached mPath -> return mPath
    BuiltinIncDirInitial      -> saveState =<< case config of
      BuiltinIncDirDisable -> return Nothing
      BuiltinIncDirAuto    -> aux . fmap normWinPath =<< getResourceDir tracer
      BuiltinIncDirClang   -> aux Nothing
  where
    saveState :: Maybe BuiltinIncDir -> IO (Maybe BuiltinIncDir)
    saveState mPath = do
      IORef.writeIORef builtinIncDirState (BuiltinIncDirCached mPath)
      return mPath

    aux :: Maybe FilePath -> IO (Maybe BuiltinIncDir)
    aux = \case
      Just resourceDir
        | FilePath.isAbsolute resourceDir ->
            ifM
              tracer
              BuiltinIncDirAbsResourceDirIncDirNotFound
              BuiltinIncDirAbsResourceDirIncDirFound
              Dir.doesDirectoryExist
              (FilePath.joinPath [resourceDir, "include"])
        | otherwise -> sequenceUntilJust [
              getBuiltinIncDirWithLlvmPath   tracer resourceDir
            , getBuiltinIncDirWithLlvmConfig tracer resourceDir
            , getBuiltinIncDirWithClang      tracer
            ]
      Nothing -> getBuiltinIncDirWithClang tracer

-- | Apply the builtin include directory to 'ClangArgs'
--
-- When configured, the builtin include directory is passed with @-isystem@ as
-- the last argument.  This ensures that it is prioritized as close to the
-- default include directories as possible.
applyBuiltinIncDir :: ClangArgs -> Maybe BuiltinIncDir -> ClangArgs
applyBuiltinIncDir args = \case
    Nothing            -> args
    Just builtinIncDir -> args{
        clangArgsAfter = clangArgsAfter args ++ ["-isystem", builtinIncDir]
      }

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Try to get the resource directory from @libclang@
--
-- This function uses Clang option @-print-resource-dir@ to get the resource
-- directory as reported by the @libclang@ library that is dynamically loaded at
-- runtime.
--
-- The reported directory is generally relative to the LLVM prefix.  We need to
-- get it because it includes a version number that may be either a full version
-- number or just the major version number, depending on how LLVM/Clang was
-- installed.
--
-- This function redirects @STDOUT@ in order to capture the @libclang@ output.
-- It writes additional characters in order to force @libclang@ to flush its
-- @outs@ buffer, which has a variable size with different defaults on different
-- platforms.
--
-- WARNING: This function is /not/ thread-safe because it redirects @STDOUT@ for
-- the whole process.
--
-- NOTE: A newline is printed to @STDOUT@ as a side effect.  This happens
-- because the @outs@ buffer is only flushed when overfull.
getResourceDir :: Tracer IO BuiltinIncDirMsg -> IO (Maybe FilePath)
getResourceDir tracer = auxOut 0 >>= \case
    -- Assumption: printed resource directory is significantly shorter than the
    -- size of the buffer
    Just (out, numFillBytes) -> do
      -- Invariant: @numPrintBytes <= buffSize@
      let (mResourceDir, numPrintBytes) =
            case takeWhile isEOL <$> break isEOL out of
              (s, n)
                | null s    -> (Nothing, length n)
                | otherwise -> (Just s,  length s + length n)
          -- Could be a multiple of the buffer size if @initNumBytes@ is larger
          -- than the buffer size
          buffSize = length out
          -- Bytes remaining in the buffer, @> 0@ given the above assumption
          numRemainBytes = buffSize + buffSize - numPrintBytes - numFillBytes
      void . Silently.capture_ $
        auxFillBuffer (numRemainBytes + 1) (Just "-- Clang buffer flushed")
      traceWith tracer $
        maybe
          BuiltinIncDirResourceDirEmpty
          BuiltinIncDirResourceDirResolved
          mResourceDir
      return mResourceDir
    Nothing -> do
      traceWith tracer BuiltinIncDirResourceDirAbort
      return Nothing
  where
    -- Repeatedly outputs filler content until the buffer is filled and flushed
    -- or the limit is reached
    auxOut ::
         Int                       -- Number of filler bytes already output
      -> IO (Maybe (String, Int))  -- Captured output, number of filler bytes
    auxOut !numBytes
      | numBytes == 0 = do
          out <- Silently.capture_ $ do
            auxPrintResourceDir
            auxFillBuffer initNumBytes Nothing
          if null out
            then auxOut initNumBytes
            else return $ Just (out, initNumBytes)
      | numBytes < maxNumBytes = do
          out <- Silently.capture_ $ auxFillBuffer numBytes Nothing
          if null out
            then auxOut (numBytes * 2)
            else return $ Just (out, numBytes * 2)
      | otherwise = return Nothing

    -- Number of bytes to initially output
    --
    -- The default buffer size on Windows is 16KB.  The default buffer size on
    -- Linux and macOS is 4KB.
    initNumBytes :: Int
#ifdef mingw32_HOST_OS
    initNumBytes = 16_384
#else
    initNumBytes = 4_096
#endif

    -- Maximum number of bytes to output
    maxNumBytes :: Int
    maxNumBytes = 32_768

    -- Get @libclang@ to print the resource directory
    --
    -- @withClang'@ returns an error in this case, which is normal behaviour.
    -- No translation unit is available.
    --
    -- This function must not trace any output (to @STDOUT@).
    auxPrintResourceDir :: IO ()
    auxPrintResourceDir = void $
      let args'       = def { clangArgsBefore = ["-print-resource-dir"] }
          clangSetup' = defaultClangSetup args' $ ClangInputMemory filename ""
      in  withClang' nullTracer clangSetup' $ const (return Nothing)

    -- Bet @libclang@ to print to @STDOUT@ in order to fill the -- @outs@ buffer
    --
    -- This function outputs visible characters and limits line length to
    -- minimize confusion if something goes wrong.
    --
    -- Clang only flushes the buffer when there is overflow.  There is always at
    -- least one character remaining in the buffer.  A newline overflow characer
    -- is used by default, and a final overflow string can be specified.
    --
    -- This function must not trace any output (to @STDOUT@).
    auxFillBuffer ::
         Int
      -- ^ Number of bytes including overflow newline (@(> 0)@)
      -> Maybe String
      -- ^ Overflow string not including newline (@maybe True ((> 0) . length)@)
      -> IO ()
    auxFillBuffer numBytes mOverflow =
      void . withClang' nullTracer clangSetup $ \unit -> do
        let (numLines, numBytes') = numBytes `divMod` maxLineLength
            t = Text.pack . concat $ case mOverflow of
              Nothing
                | numBytes' > 0 ->
                    -- full lines and partial line
                    replicate numLines filler
                      ++ [take (numBytes' - 1) filler ++ "\n"]
                | otherwise ->
                    -- full lines only
                    replicate numLines filler
              Just overflow
                | numBytes' > 1 ->
                    -- ensure newline before overflow string
                    let l = take (numBytes' - 2) filler
                          ++ '\n' : overflow ++ "\n"
                    in  replicate numLines filler ++ [l]
                | numBytes' == 1 ->
                    -- previous line already ends with newline
                    replicate numLines filler ++ [overflow ++ "\n"]
                | numBytes' == 0 ->
                    -- overflow happens from last character of a line
                    let l = take (maxLineLength - 2) filler
                          ++ '\n' : overflow ++ "\n"
                    in  replicate (numLines - 1) filler ++ [l]
                | otherwise ->
                    -- impossible when invariants met
                    panicPure "auxFillBuffer numBytes == 0"
        file <- clang_getFile unit filenameT
        sloc <- clang_getLocation unit file 1 1
        bracket
          (clang_CXRewriter_create unit)
          clang_CXRewriter_dispose
          $ \rewriter -> do
              clang_CXRewriter_insertTextBefore rewriter sloc t
              clang_CXRewriter_writeMainFileToStdOut rewriter
        return Nothing

    clangSetup :: ClangSetup
    clangSetup = defaultClangSetup def $ ClangInputMemory filename ""

    filename :: FilePath
    filename = "hs-bindgen-print-resource-dir.h"

    filenameT :: Text
    filenameT = Text.pack filename

    isEOL :: Char -> Bool
    isEOL = (`elem` ("\r\n" :: [Char]))

    -- Maximum line length, including newline
    maxLineLength :: Int
    maxLineLength = 1024

    filler :: String
    filler =
      take (maxLineLength - 1) (cycle "--hs-bindgen-builtin-include-dir")
        ++ "\n"

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
  -> IO (Maybe FilePath)
ifM tracer mkNotFound mkFound p path = p path >>= \case
    True  -> Just path <$ traceWith tracer (mkFound    path)
    False -> Nothing   <$ traceWith tracer (mkNotFound path)

-- | Sequentially run actions until the first 'Just' return value
--
-- This function only returns 'Nothing' when all actions return 'Nothing'.
sequenceUntilJust :: [IO (Maybe a)] -> IO (Maybe a)
sequenceUntilJust = \case
    (action:actions) -> action >>= \case
      Nothing -> sequenceUntilJust actions
      Just x  -> return (Just x)
    []               -> return Nothing

-- | Run an action that should return a single path
checkOutput ::
     Tracer IO BuiltinIncDirMsg
  -> (String -> BuiltinIncDirMsg)   -- ^ unexpected output constructor
  -> (IOError -> BuiltinIncDirMsg)  -- ^ error constructor
  -> IO String                          -- ^ action
  -> IO (Maybe FilePath)
checkOutput tracer mkUnexpected mkError = aux <=< tryIOError
  where
    aux :: Either IOError String -> IO (Maybe FilePath)
    aux = \case
      Right s -> case lines s of
        [path] -> return (Just path)
        _      -> Nothing <$ traceWith tracer (mkUnexpected s)
      Left e  -> Nothing <$ traceWith tracer (mkError e)

-- | Get the builtin include directory using @LLVM_PATH@
--
-- If @LLVM_PATH@ is set, then the builtin include directory is
-- @${LLVM_PATH}/${RESOURCE_DIR}/include@.
getBuiltinIncDirWithLlvmPath ::
     Tracer IO BuiltinIncDirMsg
  -> FilePath  -- ^ resource directory (relative to LLVM prefix)
  -> IO (Maybe BuiltinIncDir)
getBuiltinIncDirWithLlvmPath tracer resourceDir =
    (fmap normWinPath <$> Env.lookupEnv "LLVM_PATH") >>= \case
      Just prefix -> Dir.doesDirectoryExist prefix >>= \case
        False -> do
          traceWith tracer (BuiltinIncDirLlvmPathNotFound prefix)
          return Nothing
        True ->
          ifM
            tracer
            BuiltinIncDirLlvmPathIncDirNotFound
            BuiltinIncDirLlvmPathIncDirFound
            Dir.doesDirectoryExist
            (FilePath.joinPath [prefix, resourceDir, "include"])
      Nothing -> return Nothing

-- | Get the builtin include directory using @llvm-config@
--
-- @llvm-config --prefix@ is used to get the LLVM prefix.  The builtin include
-- directory is @${LLVM_PREFIX}/${RESOURCE_DIR}/include@.
getBuiltinIncDirWithLlvmConfig ::
     Tracer IO BuiltinIncDirMsg
  -> FilePath  -- ^ resource directory (relative to LLVM prefix)
  -> IO (Maybe BuiltinIncDir)
getBuiltinIncDirWithLlvmConfig tracer resourceDir =
    findLlvmConfigExe tracer >>= \case
      Just exe -> getLlvmConfigPrefix tracer exe >>= \case
        Just prefix ->
          ifM
            tracer
            BuiltinIncDirLlvmConfigIncDirNotFound
            BuiltinIncDirLlvmConfigIncDirFound
            Dir.doesDirectoryExist
            (FilePath.joinPath [prefix, resourceDir, "include"])
        Nothing -> return Nothing
      Nothing -> return Nothing

-- | Find the @llvm-config@ executable
--
-- 1. @${LLVM_CONFIG}@
-- 2. Search @${PATH}@
findLlvmConfigExe :: Tracer IO BuiltinIncDirMsg -> IO (Maybe FilePath)
findLlvmConfigExe tracer = sequenceUntilJust [auxLlvmConfigEnv, auxPath]
  where
    auxLlvmConfigEnv :: IO (Maybe FilePath)
    auxLlvmConfigEnv =
      (fmap normWinPath <$> Env.lookupEnv "LLVM_CONFIG") >>= \case
        Just exe ->
          ifM
            tracer
            BuiltinIncDirLlvmConfigEnvNotFound
            BuiltinIncDirLlvmConfigEnvFound
            Dir.doesFileExist
            exe
        Nothing -> return Nothing

    auxPath :: IO (Maybe FilePath)
    auxPath = Dir.findExecutable llvmConfigExe >>= \case
      Just exe -> do
        traceWith tracer (BuiltinIncDirLlvmConfigPathFound exe)
        return (Just exe)
      Nothing -> return Nothing

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
  -> IO (Maybe FilePath)
getLlvmConfigPrefix tracer exe = fmap normWinPath <$>
    checkOutput
      tracer
      BuiltinIncDirLlvmConfigPrefixUnexpected
      BuiltinIncDirLlvmConfigPrefixIOError
      (readProcess exe ["--prefix"] "")

-- | Get the builtin include directory using @clang@
--
-- @clang -print-file-name=include@ is called to get the builtin include
-- directory.
getBuiltinIncDirWithClang ::
     Tracer IO BuiltinIncDirMsg
  -> IO (Maybe BuiltinIncDir)
getBuiltinIncDirWithClang tracer =
    findClangExe tracer >>= \case
      Just exe -> getClangBuiltinIncDir tracer exe >>= \case
        Just includeDir ->
          ifM
            tracer
            BuiltinIncDirClangIncDirNotFound
            BuiltinIncDirClangIncDirFound
            Dir.doesDirectoryExist
            includeDir
        Nothing -> return Nothing
      Nothing -> return Nothing

-- | Find the @clang@ executable
--
-- 1. @${LLVM_PATH}/bin/clang@
-- 2. @$(${LLVM_CONFIG} --prefix)/bin/clang@
-- 3. @$(llvm-config --prefix)/bin/clang@
-- 4. Search @${PATH}@
findClangExe :: Tracer IO BuiltinIncDirMsg -> IO (Maybe FilePath)
findClangExe tracer = sequenceUntilJust [auxLlvmPath, auxLlvmConfig, auxPath]
  where
    auxLlvmPath :: IO (Maybe FilePath)
    auxLlvmPath = (fmap normWinPath <$> Env.lookupEnv "LLVM_PATH") >>= \case
      Just prefix -> Dir.doesDirectoryExist prefix >>= \case
        False -> do
          traceWith tracer (BuiltinIncDirLlvmPathNotFound prefix)
          return Nothing
        True ->
          ifM
            tracer
            BuiltinIncDirLlvmPathClangExeNotFound
            BuiltinIncDirLlvmPathClangExeFound
            Dir.doesFileExist
            (FilePath.joinPath [prefix, "bin", clangExe])
      Nothing -> return Nothing

    auxLlvmConfig :: IO (Maybe FilePath)
    auxLlvmConfig = findLlvmConfigExe tracer >>= \case
      Just exe -> getLlvmConfigPrefix tracer exe >>= \case
        Just prefix ->
          ifM
            tracer
            BuiltinIncDirLlvmConfigClangExeNotFound
            BuiltinIncDirLlvmConfigClangExeFound
            Dir.doesFileExist
            (FilePath.joinPath [prefix, "bin", clangExe])
        Nothing -> return Nothing
      Nothing -> return Nothing

    auxPath :: IO (Maybe FilePath)
    auxPath = Dir.findExecutable clangExe >>= \case
      Just exe -> do
        traceWith tracer (BuiltinIncDirClangPathFound exe)
        return (Just exe)
      Nothing -> return Nothing

-- | @clang@ executable name for the current platform
clangExe :: FilePath
clangExe =
#ifdef mingw32_HOST_OS
    "clang.exe"
#else
    "clang"
#endif

-- | Get the builtin include directory from @clang@
--
-- This function calls @clang -print-file-name=include@ and captures the output.
getClangBuiltinIncDir ::
     Tracer IO BuiltinIncDirMsg
  -> FilePath  -- ^ @clang@ path
  -> IO (Maybe BuiltinIncDir)
getClangBuiltinIncDir tracer exe = fmap normWinPath <$>
    checkOutput
      tracer
      BuiltinIncDirClangUnexpected
      BuiltinIncDirClangIOError
      (readProcess exe ["-print-file-name=include"] "")
