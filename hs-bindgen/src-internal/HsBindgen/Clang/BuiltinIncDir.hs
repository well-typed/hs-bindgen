{-# LANGUAGE CPP #-}

module HsBindgen.Clang.BuiltinIncDir (
    -- * Trace messages
    BuiltinIncDirMsg(..)
    -- * Configuration
  , BuiltinIncDirConfig(..)
    -- * API
  , getBuiltinIncDir
  ) where

import Control.Exception (Exception(displayException))
import Control.Monad ((<=<))
import Data.Text qualified as Text
import System.Directory qualified as Dir
import System.Environment qualified as Env
import System.FilePath qualified as FilePath
import System.IO.Error (tryIOError)
import System.IO.Silently qualified as Silently
import System.Process (readProcess)

#ifdef mingw32_HOST_OS
import Data.Char qualified as Char
import System.FilePath.Posix qualified as Posix
import System.FilePath.Windows qualified as Windows
#endif

import Clang.Args
import Clang.LowLevel.Core
import HsBindgen.Clang
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((<+>), string)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data BuiltinIncDirMsg =
    BuiltinIncDirClangBufferSize Int
  | BuiltinIncDirResourceDirEmpty
  | BuiltinIncDirResourceDirAbort
  | BuiltinIncDirResourceDirResolved FilePath
  | BuiltinIncDirAbsResourceDirNotFound FilePath
  | BuiltinIncDirAbsResourceDirFound FilePath
  | BuiltinIncDirLlvmPathNotFound FilePath
  | BuiltinIncDirLlvmPathIncludeDirNotFound FilePath
  | BuiltinIncDirLlvmPathIncludeDirFound FilePath
  | BuiltinIncDirLlvmConfigIncludeDirNotFound FilePath
  | BuiltinIncDirLlvmConfigIncludeDirFound FilePath
  | BuiltinIncDirLlvmConfigEnvNotFound FilePath
  | BuiltinIncDirLlvmConfigEnvFound FilePath
  | BuiltinIncDirLlvmConfigPathFound FilePath
  | BuiltinIncDirLlvmConfigPrefixUnexpected String
  | BuiltinIncDirLlvmConfigPrefixIOError IOError
  | BuiltinIncDirClangIncludeDirNotFound FilePath
  | BuiltinIncDirClangIncludeDirFound FilePath
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
    BuiltinIncDirClangBufferSize size ->
      "Clang outs buffer size:" <+> string (show size) <+> "bytes"
    BuiltinIncDirResourceDirEmpty ->
      "empty resource directory"
    BuiltinIncDirResourceDirAbort ->
      "aborted resource directory resolution"
    BuiltinIncDirResourceDirResolved path ->
      "resource directory found:" <+> string path
    BuiltinIncDirAbsResourceDirNotFound path ->
      "builtin include directory not found using absolute resource directory:"
        <+> string path
    BuiltinIncDirAbsResourceDirFound path ->
      "builtin include directory found using absolute resource directory:"
        <+> string path
    BuiltinIncDirLlvmPathNotFound path ->
      "$LLVM_PATH path not found or not directory (skipping):" <+> string path
    BuiltinIncDirLlvmPathIncludeDirNotFound path ->
      "builtin include directory not found using $LLVM_PATH:" <+> string path
    BuiltinIncDirLlvmPathIncludeDirFound path ->
      "builtin include directory found using $LLVM_PATH:" <+> string path
    BuiltinIncDirLlvmConfigIncludeDirNotFound path ->
      "builtin include directory not found using llvm-config:" <+> string path
    BuiltinIncDirLlvmConfigIncludeDirFound path ->
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
    BuiltinIncDirClangIncludeDirNotFound path ->
      "builtin include directory not found using clang:" <+> string path
    BuiltinIncDirClangIncludeDirFound path ->
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
    BuiltinIncDirClangBufferSize{}              -> Debug
    BuiltinIncDirResourceDirEmpty{}             -> Warning
    BuiltinIncDirResourceDirAbort{}             -> Warning
    BuiltinIncDirResourceDirResolved{}          -> Debug
    BuiltinIncDirAbsResourceDirNotFound{}       -> Warning
    BuiltinIncDirAbsResourceDirFound{}          -> Debug
    BuiltinIncDirLlvmPathNotFound{}             -> Warning
    BuiltinIncDirLlvmPathIncludeDirNotFound{}   -> Warning
    BuiltinIncDirLlvmPathIncludeDirFound{}      -> Debug
    BuiltinIncDirLlvmConfigIncludeDirNotFound{} -> Warning
    BuiltinIncDirLlvmConfigIncludeDirFound{}    -> Debug
    BuiltinIncDirLlvmConfigEnvNotFound{}        -> Warning
    BuiltinIncDirLlvmConfigEnvFound{}           -> Debug
    BuiltinIncDirLlvmConfigPathFound{}          -> Debug
    BuiltinIncDirLlvmConfigPrefixUnexpected{}   -> Warning
    BuiltinIncDirLlvmConfigPrefixIOError{}      -> Warning
    BuiltinIncDirClangIncludeDirNotFound{}      -> Warning
    BuiltinIncDirClangIncludeDirFound{}         -> Debug
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
-- runtime.
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
getBuiltinIncDir ::
     Tracer IO BuiltinIncDirMsg
  -> BuiltinIncDirConfig
  -> IO (Maybe FilePath)
getBuiltinIncDir tracer = \case
    BuiltinIncDirDisable -> return Nothing
    BuiltinIncDirAuto    -> aux . fmap normWinPath =<< getResourceDir tracer
    BuiltinIncDirClang   -> aux Nothing
  where
    aux :: Maybe FilePath -> IO (Maybe FilePath)
    aux = \case
      Just resourceDir
        | FilePath.isAbsolute resourceDir ->
            ifM
              tracer
              BuiltinIncDirAbsResourceDirNotFound
              BuiltinIncDirAbsResourceDirFound
              Dir.doesDirectoryExist
              (FilePath.joinPath [resourceDir, "include"])
        | otherwise -> sequenceUntilJust [
              getBuiltinIncDirWithLlvmPath   tracer resourceDir
            , getBuiltinIncDirWithLlvmConfig tracer resourceDir
            , getBuiltinIncDirWithClang      tracer
            ]
      Nothing -> getBuiltinIncDirWithClang tracer

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
    Just (out, numFillBytes) -> do
      let (mResourceDir, numPrintBytes) =
            case takeWhile isEOL <$> break isEOL out of
              (s, n)
                | null s    -> (Nothing, length n)
                | otherwise -> (Just s,  length s + length n)
          buffSize = length out
          numBytes = buffSize + buffSize - numPrintBytes - numFillBytes + 1
      when (numBytes > 0) . void $ Silently.capture_ (auxFillBuffer numBytes)
      traceWith tracer $ BuiltinIncDirClangBufferSize buffSize
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
    -- This function repeatedly outputs filler content until the buffer is
    -- filled and flushed or the limit is reached.
    auxOut ::
         Int                       -- number of filler bytes already output
      -> IO (Maybe (String, Int))  -- captured output, number of filler bytes
    auxOut !numBytes
      | numBytes == 0 = do
          out <- Silently.capture_ $ do
            auxPrintResourceDir
            auxFillBuffer initNumBytes
          if null out
            then auxOut initNumBytes
            else return $ Just (out, initNumBytes)
      | numBytes < maxNumBytes = do
          out <- Silently.capture_ $ auxFillBuffer numBytes
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

    -- This function gets @libclang@ to print the resource directory.
    -- @withClang'@ returns an error in this case, which is normal behaviour.
    -- No translation unit is available.
    --
    -- This function must not trace any output (to @STDOUT@).
    auxPrintResourceDir :: IO ()
    auxPrintResourceDir = void $
      let args'       = def { clangArgsBefore = ["-print-resource-dir"] }
          clangSetup' = defaultClangSetup args' $ ClangInputMemory filename ""
      in  withClang' nullTracer clangSetup' $ const (return Nothing)

    -- This function gets @libclang@ to print to @STDOUT@ in order to fill the
    -- @outs@ buffer.  It outputs visible characters and limits line length to
    -- minimize confusion if something goes wrong.
    --
    -- This function must not trace any output (to @STDOUT@).
    auxFillBuffer :: Int -> IO ()
    auxFillBuffer numBytes =
      void . withClang' nullTracer clangSetup $ \unit -> do
        let (numLines, numBytes') = numBytes `divMod` maxLineLength
            lastLine
              | numBytes' > 0 = [take (numBytes' - 1) filler ++ "\n"]
              | otherwise     = []
            t = Text.pack . concat $ replicate numLines filler ++ lastLine
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
  -> (FilePath -> IO Bool)               -- ^ predicate
  -> FilePath                            -- ^ path
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
  -> IO (Maybe FilePath)
getBuiltinIncDirWithLlvmPath tracer resourceDir =
    (fmap normWinPath <$> Env.lookupEnv "LLVM_PATH") >>= \case
      Just prefix -> Dir.doesDirectoryExist prefix >>= \case
        False -> do
          traceWith tracer (BuiltinIncDirLlvmPathNotFound prefix)
          return Nothing
        True ->
          ifM
            tracer
            BuiltinIncDirLlvmPathIncludeDirNotFound
            BuiltinIncDirLlvmPathIncludeDirFound
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
  -> IO (Maybe FilePath)
getBuiltinIncDirWithLlvmConfig tracer resourceDir =
    findLlvmConfigExe tracer >>= \case
      Just exe -> getLlvmConfigPrefix tracer exe >>= \case
        Just prefix ->
          ifM
            tracer
            BuiltinIncDirLlvmConfigIncludeDirNotFound
            BuiltinIncDirLlvmConfigIncludeDirFound
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
  -> IO (Maybe FilePath)
getBuiltinIncDirWithClang tracer =
    findClangExe tracer >>= \case
      Just exe -> getClangBuiltinIncDir tracer exe >>= \case
        Just includeDir ->
          ifM
            tracer
            BuiltinIncDirClangIncludeDirNotFound
            BuiltinIncDirClangIncludeDirFound
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
  -> IO (Maybe FilePath)
getClangBuiltinIncDir tracer exe = fmap normWinPath <$>
    checkOutput
      tracer
      BuiltinIncDirClangUnexpected
      BuiltinIncDirClangIOError
      (readProcess exe ["-print-file-name=include"] "")
