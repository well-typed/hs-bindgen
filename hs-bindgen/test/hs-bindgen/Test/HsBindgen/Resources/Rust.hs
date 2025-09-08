{-# LANGUAGE CPP #-}

-- | Interaction with @rust-bindgen@
--
-- Intended for unqualified import.
module Test.HsBindgen.Resources.Rust (
    RustBindgen(..)
  , initRustBindgen
  , freeRustBindgen
    -- * Execution
  , runRustBindgen
  ) where

import Control.Exception
import System.Directory (findExecutable, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (getCanonicalTemporaryDirectory, createTempDirectory)
import System.Process (readProcessWithExitCode)
import System.Process qualified as P

import Clang.Args
import HsBindgen.Config.ClangArgs

{-------------------------------------------------------------------------------
  Get rust-bindgen
-------------------------------------------------------------------------------}

data RustBindgen =
    RustBindgenInPath FilePath
  | RustBindgenDownloaded FilePath
  | RustBindgenUnavailable

-- | Get path to @rust-bindgen@, downloading it if necessary
initRustBindgen :: IO RustBindgen
initRustBindgen =
    if hostIsLinux then do
      mInPath <- findExecutable "bindgen"
      case mInPath of
        Just inPath -> return $ RustBindgenInPath inPath
        Nothing     -> RustBindgenDownloaded <$> downloadRustBindgen
    else
      return RustBindgenUnavailable

downloadRustBindgen :: IO FilePath
downloadRustBindgen = do
    tmpDir <- getCanonicalTemporaryDirectory
    tmpDir' <- createTempDirectory tmpDir "rust-bindgen"

    callProcessCwd tmpDir' "curl"
        [ "-sLf", "-o", tmpDir' </> "bindgen.tar.xz"
        , rustBindgenSources
        ]

    callProcessCwd tmpDir' "tar"
        [ "--strip-components=1"
        , "-xf", tmpDir' </> "bindgen.tar.xz"
        ]

    return (tmpDir' </> "bindgen")

-- | Delete previously downloaded @rust-bindgen@, if any
freeRustBindgen :: RustBindgen -> IO ()
freeRustBindgen = \case
    RustBindgenDownloaded path ->
      ignoringIOErrors $ removeDirectoryRecursive $ takeDirectory path
    _otherwise ->
      return ()
  where
    ignoringIOErrors :: IO () -> IO ()
    ignoringIOErrors ioe = ioe `catch` h
      where
        h :: IOException -> IO ()
        h _ = return ()

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

-- | Call @rust-bindgen@
--
-- Returns the process' exit code, stdout and stderr.
runRustBindgen ::
     ClangArgsConfig
  -> FilePath -- ^ Path to the @rust-bindgen@ executable
  -> FilePath -- ^ Input header to run it on
  -> IO (ExitCode, String, String)
runRustBindgen clangArgsConfig pathToExe input = do
    clangArgs <- either throwIO (return . unClangArgs) $
      getClangArgs clangArgsConfig
    -- We use `--formatter=prettyplease`, as we don't necessarily have the Rust
    -- toolchain installed.
    let args :: [String]
        args = concat [
              [ "--formatter=prettyplease"
              , "--allowlist-file=" ++ input
              , input
              , "--"
              ]
            , clangArgs
            ]
    readProcessWithExitCode pathToExe args ""

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

callProcessCwd :: FilePath -> FilePath -> [String] -> IO ()
callProcessCwd pwd cmd args = do
    exit_code <-
      P.withCreateProcess
         ( (P.proc cmd args){
               P.cwd           = Just pwd
             , P.delegate_ctlc = True
             }
         )
         ( \_ _ _ p -> P.waitForProcess p )
    case exit_code of
      ExitSuccess    -> return ()
      ExitFailure _r -> fail "callProcessCwd"

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Location of the rust-bindgen sources
--
-- The golden tests are tied to a specific version of @rust-bindgen@.
rustBindgenSources :: String
rustBindgenSources = "https://github.com/rust-lang/rust-bindgen/releases/download/v0.71.1/bindgen-cli-x86_64-unknown-linux-gnu.tar.xz"

-- | Are we on Linux?
--
-- We only look for @rust-bindgen@ on Linux. By setting this boolean variable
-- based on CPP, and the rest regular Haskell code, we avoid warnings about
-- unnecessary imports (and unnecessary package imports) on other OSs.
hostIsLinux :: Bool
#ifdef linux_HOST_OS
hostIsLinux = True
#else
hostIsLinux = False
#endif
