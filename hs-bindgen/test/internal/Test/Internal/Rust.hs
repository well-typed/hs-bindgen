{-# LANGUAGE CPP #-}
-- | Separate module for running Rust's @bindgen@ (as comparison) tests.
module Test.Internal.Rust (
    goldenRust,
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
goldenRust _bindgen name = testCase name $ do
    return () -- do nothing on non-linux systems.

#else

-- https://github.com/rust-lang/rust-bindgen/releases/tag/v0.70.1
-- https://github.com/rust-lang/rustfmt/releases/tag/v1.6.0

import Control.Monad (unless)
import System.FilePath ((</>), takeDirectory)
import Test.Tasty (TestTree, TestName, withResource)
import Control.Exception (catch, IOException)
import System.Process (readProcessWithExitCode)
import System.Directory (findExecutable, removeDirectoryRecursive)
import System.IO.Temp (getCanonicalTemporaryDirectory, createTempDirectory)
import System.Process qualified as P
import System.Exit (ExitCode (..))

import Test.Internal.Misc

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
        , "https://github.com/rust-lang/rust-bindgen/releases/download/v0.70.1/bindgen-cli-x86_64-unknown-linux-gnu.tar.xz"
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

-- | bindgen --allowlist-file=hs-bindgen/examples/fixedwidth.h hs-bindgen/examples/fixedwidth.h
goldenRust :: IO FilePath -> TestName -> TestTree
goldenRust gb name =  goldenVsStringDiff_ "rust" ("fixtures" </> (name ++ ".rs")) $ \report -> do
    -- package root is not used as we don't specify the location of stdlib
    let fp = "examples" </> (name ++ ".h")

    bindgen <- gb

    -- we use --formatter=prettyplease, as we don't necessarily have rust toolchain installed
    (_ec, contents, err) <- readProcessWithExitCode bindgen ["--formatter=prettyplease", "--allowlist-file=" ++ fp, fp] ""
    unless (null err) $ report err

    return contents

#endif
