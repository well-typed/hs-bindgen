module Test.Internal.C (
    cCheck,
) where

import Test.Tasty (TestTree, TestName)
import Test.Tasty.HUnit (testCaseSteps)
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)
import System.FilePath ((</>))
import System.Process qualified as P
import System.Exit (ExitCode (..))

cCheck :: FilePath -> TestName -> TestTree
cCheck packageRoot name = testCaseSteps "header-check" $ \info -> do
    tmpDir' <- getCanonicalTemporaryDirectory
    withTempDirectory tmpDir' "header-check" $ \tmpDir -> do
        writeFile (tmpDir </> "tmp.c") source

        let proc :: P.CreateProcess
            proc = (P.proc "cc" arguments) { P.cwd = Just tmpDir }

        (ec, out, err) <- P.readCreateProcessWithExitCode proc ""
        case ec of
            ExitSuccess    -> info $ unlines [out, err]
            ExitFailure {} -> info $ unlines [out, err]
  where
    header = name ++ ".h"

    source = unlines
        [ "#include \"" ++ header ++ "\""
        ]

    arguments :: [String]
    arguments =
        [ "-c"
        , "-Wall"
        , "-I" ++ (packageRoot </> "examples")
        , "-o", "tmp.o"
        , "tmp.c"
        ]
