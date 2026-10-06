module Test.HsBindgen.Integration.LibraryMode (tests) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Integration.LibraryMode" [
      testRun getTestResources
    , testCrossModuleReference getTestResources
    , testPlan getTestResources
    , testPlanGrouping getTestResources
    , testUsageErrors getTestResources
    , testCollisions
    ]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

headerDir :: TestResources -> FilePath
headerDir root = root.packageRoot </> "test-artefacts" </> "headers"

runLibraryMode ::
     TestResources
  -> FilePath
  -> [String]
  -> IO (ExitCode, String, String)
runLibraryMode root tmpDir extraArgs = do
    let hDir = headerDir root
    readProcessWithExitCode "hs-bindgen-cli"
      ([ "preprocess"
       , "-I", hDir
       , "--library", hDir
       , "--module", "MyLib"
       , "--hs-output-dir", tmpDir
       , "--unique-id", "test-pl"
       , "--create-output-dirs"
       , "--overwrite-files"
       , hDir </> "mylib.h"
       ] ++ extraArgs)
      ""

-- | Library mode over one directory of headers, with base module @M@
runLibraryModeIn ::
     FilePath  -- ^ Header directory, also the @--library@ directory
  -> FilePath  -- ^ Root header, relative to that directory
  -> FilePath  -- ^ Output directory
  -> [String]
  -> IO (ExitCode, String, String)
runLibraryModeIn dir header tmpDir extraArgs =
    readProcessWithExitCode "hs-bindgen-cli"
      ([ "preprocess"
       , "-I", dir
       , "--library", dir
       , "--module", "M"
       , "--hs-output-dir", tmpDir
       , "--unique-id", "test-pl"
       , "--create-output-dirs"
       , "--overwrite-files"
       , dir </> header
       ] ++ extraArgs)
      ""

assertFilesExist :: String -> [FilePath] -> IO ()
assertFilesExist label paths =
    mapM_ (\f -> do
      exists <- doesFileExist f
      assertBool (label ++ ": expected " ++ f) exists) paths

-- | Read a whole file, so no handle is left open when the directory is removed
readFileStrict :: FilePath -> IO String
readFileStrict path = do
    contents <- readFile path
    _ <- evaluate (length contents)
    pure contents

assertFilesAbsent :: String -> [FilePath] -> IO ()
assertFilesAbsent label paths =
    mapM_ (\f -> do
      exists <- doesFileExist f
      assertBool (label ++ ": unexpected " ++ f) (not exists)) paths

{-------------------------------------------------------------------------------
  Generating
-------------------------------------------------------------------------------}

testRun :: IO TestResources -> TestTree
testRun getTestResources =
    testCase "generates one module and one binding spec per sub-header" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        let specDir = tmpDir </> "specs"
        (exitCode, stdout, stderr) <- runLibraryMode root tmpDir
          ["--gen-binding-spec-dir", specDir]
        assertEqual stderr ExitSuccess exitCode
        assertBool ("expected a summary, got: " ++ stdout) $
          ("Generated 4 modules from 4 headers in " ++ tmpDir)
            `isInfixOf` stdout
        assertFilesExist stderr
          [ tmpDir </> "MyLib" </> "Mylib" </> "Types.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Internal.hs"
          , tmpDir </> "MyLib" </> "Mylib" </> "Ops" </> "Safe.hs"
          , specDir </> "MyLib" </> "Mylib.yaml"
          , specDir </> "MyLib" </> "Mylib" </> "Types.yaml"
          , specDir </> "MyLib" </> "Mylib" </> "Ops.yaml"
          , specDir </> "MyLib" </> "Mylib" </> "Internal.yaml"
          ]

-- | b.h includes a.h, but a.h uses the struct that b.h defines
testCrossModuleReference :: IO TestResources -> TestTree
testCrossModuleReference getTestResources =
    testCase "a type lands in the module of the header that defines it" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        (exitCode, _stdout, stderr) <- runLibraryModeIn
          (headerDir root </> "forward_decl") "b.h" tmpDir []
        exitCode @?= ExitSuccess
        let safeModule = tmpDir </> "M" </> "A" </> "Safe.hs"
        assertFilesExist stderr [tmpDir </> "M" </> "B.hs", safeModule]
        -- a.h only declares the struct forward, so it has no types module
        assertFilesAbsent stderr [tmpDir </> "M" </> "A.hs"]
        contents <- readFileStrict safeModule
        assertBool "expected shape_draw to use the struct from M.B" $
          "M.B.Shape" `isInfixOf` contents

{-------------------------------------------------------------------------------
  The plan
-------------------------------------------------------------------------------}

-- | The same two headers: the module of b.h has to come first
testPlan :: IO TestResources -> TestTree
testPlan getTestResources =
    testCase "the plan is in processing order, and showing it writes nothing" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        let run = runLibraryModeIn (headerDir root </> "forward_decl") "b.h" tmpDir
        (listed, names, _stderr) <- run ["--list-base-module-names"]
        listed @?= ExitSuccess
        lines names @?= ["M.B", "M.A"]
        (dryRun, plan, _stderr) <- run ["--dry-run"]
        dryRun @?= ExitSuccess
        assertBool ("expected the plan, got: " ++ plan) $
          "2 modules to generate" `isInfixOf` plan
        assertFilesAbsent "showing the plan should not generate files"
          [ tmpDir </> "M" </> "B.hs"
          ]

-- | Which headers get a module, seen through the names of the modules
testPlanGrouping :: IO TestResources -> TestTree
testPlanGrouping getTestResources =
    testCase "headers are grouped and left out as their declarations require" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        forM_ (cases (headerDir root)) $ \(label, dir, header, args, expected) -> do
          (exitCode, stdout, stderr) <- runLibraryModeIn dir header tmpDir
            ("--list-base-module-names" : args)
          assertEqual (label ++ ": " ++ stderr) ExitSuccess exitCode
          assertEqual label expected (lines stdout)
  where
    cases :: FilePath -> [(String, FilePath, FilePath, [String], [String])]
    cases hDir = [
        ( "headers whose declarations use each other share one module"
        , hDir </> "golden" </> "macros" </> "parse", "elaborate.h", []
        , ["M.Elaborate"]
        )
        -- Including each other is not enough: these only declare int typedefs
      , ( "headers that only include each other get a module each"
        , hDir </> "golden" </> "program-analysis", "circular_includes.h", []
        , ["M.Circular_includes", "M.Circular_includes_inner"]
        )
      , ( "headers that generate nothing get no module"
        , hDir </> "golden" </> "macros" </> "parse"
        , "macro_typedef_scope_multiple.h", []
        , [ "M.Macro_typedef_scope_multiple_inner1"
          , "M.Macro_typedef_scope_multiple_inner2"
          ]
        )
      , ( "--except-library leaves a header out"
        , hDir, "mylib.h", ["--except-library", "internal"]
        , ["M.Mylib.Types", "M.Mylib.Ops", "M.Mylib"]
        )
      ]

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

testUsageErrors :: IO TestResources -> TestTree
testUsageErrors getTestResources =
    testCase "flags that do not go together are usage errors" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        root <- getTestResources
        let hDir = headerDir root
        forM_ (cases hDir tmpDir) $ \(args, message) -> do
          (exitCode, stdout, _stderr) <- readProcessWithExitCode "hs-bindgen-cli"
            ([ "preprocess"
             , "-I", hDir
             , "--module", "MyLib"
             , "--hs-output-dir", tmpDir
             ] ++ args ++ [hDir </> "mylib.h"])
            ""
          assertEqual (unwords args) (ExitFailure 2) exitCode
          assertBool ("expected " ++ show message ++ ", got: " ++ stdout) $
            message `isInfixOf` stdout
  where
    cases :: FilePath -> FilePath -> [([String], String)]
    cases hDir tmpDir = [
        ( ["--dry-run"]
        , "--dry-run requires --library"
        )
      , ( ["--library", hDir, "--gen-binding-spec", tmpDir </> "spec.yaml"]
        , "--gen-binding-spec cannot be used with --library"
        )
        -- No header is under a directory that does not exist, so a mistyped
        -- directory would give a run that generates nothing and succeeds
      , ( ["--library", tmpDir </> "no-such-directory"]
        , "--library is not a directory"
        )
      ]

-- | The collision detection itself is covered by the unit tests
testCollisions :: TestTree
testCollisions =
    testCase "module name collisions stop the run" $
      withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
        -- A dash is not allowed in a module name and becomes an underscore,
        -- so both headers give M.Foo_bar
        let names = tmpDir </> "names"
        createDirectoryIfMissing True names
        writeFile (names </> "foo-bar.h") "typedef int foo_dash;\n"
        writeFile (names </> "foo_bar.h") "typedef int foo_underscore;\n"
        writeFile (names </> "all.h") $ unlines [
            "#include \"foo-bar.h\""
          , "#include \"foo_bar.h\""
          ]
        (sameName, report, _stderr) <-
          runLibraryModeIn names "all.h" (tmpDir </> "gen") []
        sameName @?= ExitFailure 4
        assertBool ("expected collision message, got: " ++ report) $
          "Module name collision: M.Foo_bar" `isInfixOf` report

        -- The safe foreign import of foo.h goes to M/Foo/Safe.hs, which is
        -- also where the types of foo/safe.h go. Without a function, foo.h
        -- only writes M/Foo.hs.
        let overlap fooFunctions = do
              let dir = tmpDir </> "lib"
              createDirectoryIfMissing True (dir </> "foo")
              writeFile (dir </> "foo.h") $ unlines $
                "struct foo { int v; };" : fooFunctions
              writeFile (dir </> "foo" </> "safe.h") "struct fsafe { int v; };\n"
              writeFile (dir </> "all.h") $ unlines [
                  "#include \"foo.h\""
                , "#include \"foo/safe.h\""
                ]
              runLibraryModeIn dir "all.h" (tmpDir </> "gen") []
        (typesOnly, _stdout, stderr) <- overlap []
        assertEqual stderr ExitSuccess typesOnly
        (withFunction, stdout, _stderr) <- overlap ["int foo_get(struct foo *f);"]
        withFunction @?= ExitFailure 4
        assertBool ("expected an overlap, got: " ++ stdout) $
          "Category overlap on: M.Foo.Safe" `isInfixOf` stdout
