{-# LANGUAGE CPP #-}
-- | Separate module for running Rust's @bindgen@ (as comparison) tests.
module Test.Internal.Rust (
    goldenRust,
    rustExpectPanic,
    withRustBindgen,
) where

#ifndef linux_HOST_OS

import Test.Tasty (TestTree, TestName)
import Test.Tasty.HUnit (testCase)

-- to avoid unused package warning
import System.Process ()
import System.IO.Temp ()

withRustBindgen :: (IO FilePath -> TestTree) -> TestTree
withRustBindgen k = k (return "")

goldenRust :: IO FilePath -> TestName -> TestTree
goldenRust _bindgen _name = testCase "rust" $ do
    return () -- do nothing on non-linux systems.

rustExpectPanic :: IO FilePath -> TestName -> TestTree
rustExpectPanic _bindgen _name = testCase "rust-panic" $ do
    return () -- do nothing on non-linux systems.

#else

-- https://github.com/rust-lang/rust-bindgen/releases/tag/v0.70.1
-- https://github.com/rust-lang/rustfmt/releases/tag/v1.6.0

import Control.Monad (when)
import System.FilePath ((</>), takeDirectory)
import Test.Tasty (TestTree, TestName, withResource)
import Control.Exception (catch, IOException)
import System.Process (readProcessWithExitCode)
import System.Directory (findExecutable, removeDirectoryRecursive)
import System.IO.Temp (getCanonicalTemporaryDirectory, createTempDirectory)
import System.Process qualified as P
import System.Exit (ExitCode (..))

import Test.Internal.Misc
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

-- | The golden tests are tied to a specific version of @rust-bindgen@.
rustBindgenVersion :: String
rustBindgenVersion = "0.70.1"

withRustBindgen :: (IO FilePath -> TestTree) -> TestTree
withRustBindgen k = withResource getRustBindgen cleanupRustBindgen (k . fmap snd) -- TODO: unlink

getRustBindgen :: IO (Bool, FilePath)
getRustBindgen = do
    inPath <- findExecutable "bindgen"
    case inPath of
        Just inPath' -> return (True, inPath')
        Nothing -> do
            bindgen <- getRustBindgen'
            return (False, bindgen)

callProcessCwd :: FilePath -> FilePath -> [String] -> IO ()
callProcessCwd pwd cmd args = do
    exit_code <- P.withCreateProcess
       (P.proc cmd args) { P.cwd = Just pwd, P.delegate_ctlc = True } $ \_ _ _ p -> P.waitForProcess p
    case exit_code of
      ExitSuccess    -> return ()
      ExitFailure _r -> fail "callProcessCwd"

getRustBindgen' :: IO FilePath
getRustBindgen' = do
    tmpDir <- getCanonicalTemporaryDirectory
    tmpDir' <- createTempDirectory tmpDir "rust-bindgen"

    callProcessCwd tmpDir' "curl"
        [ "-sLf", "-o", tmpDir' </> "bindgen.tar.xz"
        , "https://github.com/rust-lang/rust-bindgen/releases/download/v"
          <> rustBindgenVersion
          <> "/bindgen-cli-x86_64-unknown-linux-gnu.tar.xz"
        ]

    callProcessCwd tmpDir' "tar"
        [ "--strip-components=1"
        , "-xf", tmpDir' </> "bindgen.tar.xz"
        ]

    return (tmpDir' </> "bindgen")

cleanupRustBindgen :: (Bool, FilePath) -> IO ()
cleanupRustBindgen (True, _) = return ()
cleanupRustBindgen (False, bindgen) = do
    ignoringIOErrors $ removeDirectoryRecursive $ takeDirectory bindgen

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `catch` h
  where
    h :: IOException -> IO ()
    h _ = return ()

callRust :: IO FilePath -> FilePath -> IO (ExitCode, String, String)
callRust getRustBindgenCmd filePath = do
    bindgen <- getRustBindgenCmd
    readProcessWithExitCode bindgen args ""
    -- We use `--formatter=prettyplease`, as we don't necessarily have the Rust
    -- toolchain installed.
  where args = [ "--formatter=prettyplease"
               , "--allowlist-file=" ++ filePath
               , filePath]


-- | bindgen --allowlist-file=hs-bindgen/examples/golden/fixedwidth.h hs-bindgen/examples/golden/fixedwidth.h
goldenRust :: IO FilePath -> TestName -> TestTree
goldenRust gb name =  goldenVsStringDiff_ "rust" ("fixtures" </> (name ++ ".rs")) $ \report -> do
    -- The package root is not used as we don't specify the location of
    -- `stdlib`.
    let fp = "examples" </> "golden" </> (name ++ ".h")
    (exitCode, contents, err) <- callRust gb fp
    -- Report errors when bindgen failed.
    when (exitCode /= ExitSuccess) $ do
      report $ "Exit code: " <> show exitCode
      report $ "stderr: " <> err
    exitCode @?= ExitSuccess
    pure contents

rustExpectPanic :: IO FilePath -> TestName -> TestTree
rustExpectPanic gb name = testCase "rust-panic" $ do
    let fp = "examples" </> "golden-norust" </> (name ++ ".h")
    (errorCode, _, _) <- callRust gb fp
    assertBool "expected rust-bindgen to fail" $ errorCode /= ExitSuccess
  where

#endif
