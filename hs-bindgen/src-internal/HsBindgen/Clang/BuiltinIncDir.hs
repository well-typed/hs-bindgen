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
import Clang.Version (clang_getClangVersion)
import HsBindgen.Clang
import HsBindgen.Config.ClangArgs
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint ((<+>), hangs', string, textToCtxDoc)

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
  | BuiltinIncDirClangNotFound IsUserRequested
  | BuiltinIncDirClangVersionMismatch IsUserRequested Text Text
  | BuiltinIncDirClangIncDirNotFound IsUserRequested BuiltinIncDir
  | BuiltinIncDirClangIncDirFound BuiltinIncDir
  | BuiltinIncDirLlvmPathClangExeNotFound FilePath
  | BuiltinIncDirLlvmPathClangExeFound FilePath
  | BuiltinIncDirLlvmConfigClangExeNotFound FilePath
  | BuiltinIncDirLlvmConfigClangExeFound FilePath
  | BuiltinIncDirClangPathFound FilePath
  | BuiltinIncDirClangVersionUnexpected IsUserRequested String
  | BuiltinIncDirClangVersionIOError IsUserRequested IOError
  | BuiltinIncDirClangPrintResourceDirUnexpected IsUserRequested String
  | BuiltinIncDirClangPrintResourceDirIOError IsUserRequested IOError
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
    BuiltinIncDirClangNotFound _iur ->
      "clang not found"
    BuiltinIncDirClangVersionMismatch _iur libclangVersion clangVersion ->
      hangs' "clang version mismatch:" 2 [
          "libclang version:" <+> textToCtxDoc libclangVersion
        , "clang version:   " <+> textToCtxDoc clangVersion
        ]
    BuiltinIncDirClangIncDirNotFound _iur path ->
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
    BuiltinIncDirClangVersionUnexpected _iur s ->
      "clang --version output is unexpected:" <+> string (show s)
    BuiltinIncDirClangVersionIOError _iur e ->
      "IO error calling clang --version:" <+> string (displayException e)
    BuiltinIncDirClangPrintResourceDirUnexpected _iur s ->
      "clang -print-resource-dir output is unexpected:" <+> string (show s)
    BuiltinIncDirClangPrintResourceDirIOError _iur e ->
      "IO error calling clang -print-resource-dir:"
        <+> string (displayException e)

instance IsTrace Level BuiltinIncDirMsg where
  getDefaultLogLevel = \case
    BuiltinIncDirResourceDirEmpty{}                    -> Warning
    BuiltinIncDirResourceDirAbort{}                    -> Warning
    BuiltinIncDirResourceDirResolved{}                 -> Debug
    BuiltinIncDirAbsResourceDirIncDirNotFound{}        -> Warning
    BuiltinIncDirAbsResourceDirIncDirFound{}           -> Debug
    BuiltinIncDirLlvmPathNotFound{}                    -> Warning
    BuiltinIncDirLlvmPathIncDirNotFound{}              -> Warning
    BuiltinIncDirLlvmPathIncDirFound{}                 -> Debug
    BuiltinIncDirLlvmConfigIncDirNotFound{}            -> Warning
    BuiltinIncDirLlvmConfigIncDirFound{}               -> Debug
    BuiltinIncDirLlvmConfigEnvNotFound{}               -> Warning
    BuiltinIncDirLlvmConfigEnvFound{}                  -> Debug
    BuiltinIncDirLlvmConfigPathFound{}                 -> Debug
    BuiltinIncDirLlvmConfigPrefixUnexpected{}          -> Warning
    BuiltinIncDirLlvmConfigPrefixIOError{}             -> Warning
    BuiltinIncDirClangNotFound iur                     ->
      if iur == UserRequested then Error else Debug
    BuiltinIncDirClangVersionMismatch iur _ _          ->
      if iur == UserRequested then Error else Warning
    BuiltinIncDirClangIncDirNotFound iur _             ->
      if iur == UserRequested then Error else Warning
    BuiltinIncDirClangIncDirFound{}                    -> Debug
    BuiltinIncDirLlvmPathClangExeNotFound{}            -> Debug
    BuiltinIncDirLlvmPathClangExeFound{}               -> Debug
    BuiltinIncDirLlvmConfigClangExeNotFound{}          -> Debug
    BuiltinIncDirLlvmConfigClangExeFound{}             -> Debug
    BuiltinIncDirClangPathFound{}                      -> Debug
    BuiltinIncDirClangVersionUnexpected iur _          ->
      if iur == UserRequested then Error else Warning
    BuiltinIncDirClangVersionIOError iur _             ->
      if iur == UserRequested then Error else Warning
    BuiltinIncDirClangPrintResourceDirUnexpected iur _ ->
      if iur == UserRequested then Error else Warning
    BuiltinIncDirClangPrintResourceDirIOError iur _    ->
      if iur == UserRequested then Error else Warning

  getSource = const HsBindgen

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
-- The builtin include directory is in the Clang resource directory,
-- @{{RESOURCE_DIR}}@ below, which contains the executables, headers, and
-- libraries used by the Clang compiler.  @libclang@ can print the resource
-- directory, but it currently prints a directory relative to the LLVM prefix
-- due to the same issue.  Note that the builtin include directory /cannot/ be
-- determined from just the LLVM prefix, because the resource directory path
-- includes an LLVM/Clang version number that may be either a full version
-- number or just the major version number, depending on how LLVM/Clang was
-- installed.
--
-- With 'BuiltinIncDirAuto', we redirect @STDOUT@ and attempt to get the
-- resource directory from the @libclang@ library that is dynamically loaded at
-- runtime.  Since @STDOUT@ is redirected for the whole process, this function
-- is /not/ thread-safe.  This requires filling the @libclang@ output buffer,
-- and there is unavoidable overflow that is printed to the real @STDOUT@.  We
-- print @-- Clang buffer flushed@ by default.
--
-- If @libclang@ reports an absolute resource directory (when running a
-- fixed/patched version of LLVM), the builtin include directory is
-- @{{RESOURCE_DIR}}/include@.
--
-- If @libclang@ reports a relative resource directory, we can determine the
-- builtin include directory if we can determine the LLVM prefix.  This
-- function tries to determine the builtin include directory using the first
-- successful result of the following strategies:
--
-- 1. @${LLVM_PATH}/{{RESOURCE_DIR}}/include@
-- 2. @$(${LLVM_CONFIG} --prefix)/{{RESOURCE_DIR}}/include@
-- 3. @$(llvm-config --prefix)/{{RESOURCE_DIR}}/include@
-- 4. @$(${LLVM_PATH}/bin/clang -print-resource-dir)/include@
-- 5. @$($(${LLVM_CONFIG} --prefix)/bin/clang -print-resource-dir)/include@
-- 6. @$($(llvm-config --prefix)/bin/clang -print-resource-dir)/include@
-- 7. @$(clang -print-resource-dir)/include@
--
-- If @libclang@ does not report a resource directory or 'BuiltinIncDirClang'
-- is used, this function tries to determine the builtin include directory using
-- the first successful result of the following strategies:
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
getBuiltinIncDir tracer config = IORef.readIORef builtinIncDirState >>= \case
    BuiltinIncDirCached mPath -> return mPath
    BuiltinIncDirInitial -> saveState =<< case config of
      BuiltinIncDirDisable -> return Nothing
      BuiltinIncDirAuto ->
        aux . fmap normWinPath =<< getResourceDir tracer Nothing
      BuiltinIncDirAutoWithOverflow overflow ->
        aux . fmap normWinPath =<< getResourceDir tracer (Just overflow)
      BuiltinIncDirClang -> aux Nothing
  where
    saveState :: Maybe BuiltinIncDir -> IO (Maybe BuiltinIncDir)
    saveState mPath = do
      IORef.writeIORef builtinIncDirState (BuiltinIncDirCached mPath)
      return mPath

    aux :: Maybe FilePath -> IO (Maybe BuiltinIncDir)
    aux = runMaybeT . \case
      Just resourceDir
        | FilePath.isAbsolute resourceDir ->
            ifM
              tracer
              BuiltinIncDirAbsResourceDirIncDirNotFound
              BuiltinIncDirAbsResourceDirIncDirFound
              Dir.doesDirectoryExist
              (FilePath.joinPath [resourceDir, "include"])
        | otherwise -> asum [
              getBuiltinIncDirWithLlvmPath   tracer resourceDir
            , getBuiltinIncDirWithLlvmConfig tracer resourceDir
            , getBuiltinIncDirWithClang      tracer NotUserRequested
            ]
      Nothing -> getBuiltinIncDirWithClang tracer $
        userRequestedIf (config == BuiltinIncDirClang)

-- | Apply the builtin include directory to 'ClangArgs'
--
-- When configured, the builtin include directory is passed with @-isystem@ as
-- the last argument.  This ensures that it is prioritized as close to the
-- default include directories as possible.
applyBuiltinIncDir :: Maybe BuiltinIncDir -> ClangArgsConfig -> ClangArgsConfig
applyBuiltinIncDir mBuiltinIncDir config = case mBuiltinIncDir of
    Nothing            -> config
    Just builtinIncDir -> config{
        clangArgsAfter = clangArgsAfter config ++ ["-isystem", builtinIncDir]
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
getResourceDir ::
     Tracer IO BuiltinIncDirMsg
  -> Maybe Text  -- ^ 'Just' overflow string or 'Nothing' for default
  -> IO (Maybe FilePath)
getResourceDir tracer mOverflow = auxOut 0 >>= \case
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
      void . Silently.capture_ . auxFillBuffer (numRemainBytes + 1) $
        case mOverflow of
          Just overflow -> Just (Text.unpack overflow)
          Nothing       -> Just defOverflowString
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
      let args'       = ClangArgs ["-print-resource-dir"]
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
    auxFillBuffer numBytes mOverflow' =
      void . withClang' nullTracer clangSetup $ \unit -> do
        let (numLines, numBytes') = numBytes `divMod` maxLineLength
            t = Text.pack . concat $ case mOverflow' of
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

    defOverflowString :: String
    defOverflowString = "-- Clang buffer flushed"

-- | Get the builtin include directory using @LLVM_PATH@
--
-- If @LLVM_PATH@ is set, then the builtin include directory is
-- @${LLVM_PATH}/${RESOURCE_DIR}/include@.
getBuiltinIncDirWithLlvmPath ::
     Tracer IO BuiltinIncDirMsg
  -> FilePath  -- ^ resource directory (relative to LLVM prefix)
  -> MaybeT IO BuiltinIncDir
getBuiltinIncDirWithLlvmPath tracer resourceDir = do
    prefix <- lookupLlvmPath tracer
    ifM
      tracer
      BuiltinIncDirLlvmPathIncDirNotFound
      BuiltinIncDirLlvmPathIncDirFound
      Dir.doesDirectoryExist
      (FilePath.joinPath [prefix, resourceDir, "include"])

-- | Lookup @LLVM_PATH@ environment variable
lookupLlvmPath :: Tracer IO BuiltinIncDirMsg -> MaybeT IO FilePath
lookupLlvmPath tracer = do
    prefix <- MaybeT $ fmap normWinPath <$> Env.lookupEnv "LLVM_PATH"
    MaybeT $ Dir.doesDirectoryExist prefix >>= \case
      True  -> return (Just prefix)
      False -> do
        traceWith tracer (BuiltinIncDirLlvmPathNotFound prefix)
        return Nothing

-- | Get the builtin include directory using @llvm-config@
--
-- @llvm-config --prefix@ is used to get the LLVM prefix.  The builtin include
-- directory is @${LLVM_PREFIX}/${RESOURCE_DIR}/include@.
getBuiltinIncDirWithLlvmConfig ::
     Tracer IO BuiltinIncDirMsg
  -> FilePath  -- ^ resource directory (relative to LLVM prefix)
  -> MaybeT IO BuiltinIncDir
getBuiltinIncDirWithLlvmConfig tracer resourceDir = do
    exe <- findLlvmConfigExe tracer
    prefix <- getLlvmConfigPrefix tracer exe
    ifM
      tracer
      BuiltinIncDirLlvmConfigIncDirNotFound
      BuiltinIncDirLlvmConfigIncDirFound
      Dir.doesDirectoryExist
      (FilePath.joinPath [prefix, resourceDir, "include"])

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

-- | Get the builtin include directory using @clang@
--
-- @clang -print-file-name=include@ is called to get the builtin include
-- directory.
getBuiltinIncDirWithClang ::
     Tracer IO BuiltinIncDirMsg
  -> IsUserRequested
  -> MaybeT IO BuiltinIncDir
getBuiltinIncDirWithClang tracer isUserRequested = do
    exe <- findClangExe tracer <|> do
      lift $ traceWith tracer (BuiltinIncDirClangNotFound isUserRequested)
      MaybeT $ return Nothing
    clangVer <- getClangVersion tracer isUserRequested exe
    libclangVer <- lift clang_getClangVersion
    unless (clangVer == libclangVer) $ do
      lift . traceWith tracer $
        BuiltinIncDirClangVersionMismatch isUserRequested libclangVer clangVer
      MaybeT $ return Nothing
    resourceDir <- getClangResourceDir tracer isUserRequested exe
    ifM
     tracer
     (BuiltinIncDirClangIncDirNotFound isUserRequested)
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

-- | Get the Clang version from @clang@
--
-- This function calls @clang --version@ and captures the output.  The full
-- version string in the first line is returned.
getClangVersion ::
     Tracer IO BuiltinIncDirMsg
  -> IsUserRequested
  -> FilePath  -- ^ @clang@ path
  -> MaybeT IO Text
getClangVersion tracer isUserRequested exe =
    checkOutput
      tracer
      (BuiltinIncDirClangVersionUnexpected isUserRequested)
      (BuiltinIncDirClangVersionIOError isUserRequested)
      (fmap Text.pack . parseFirstLine)
      (readProcess exe ["--version"] "")

-- | Get the resource directory from @clang@
--
-- This function calls @clang -print-resource-dir@ and captures the output.
getClangResourceDir ::
     Tracer IO BuiltinIncDirMsg
  -> IsUserRequested
  -> FilePath  -- ^ @clang@ path
  -> MaybeT IO FilePath
getClangResourceDir tracer isUserRequested exe =
    checkOutput
      tracer
      (BuiltinIncDirClangPrintResourceDirUnexpected isUserRequested)
      (BuiltinIncDirClangPrintResourceDirIOError isUserRequested)
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
