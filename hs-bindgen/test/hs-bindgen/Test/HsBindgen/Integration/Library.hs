-- | @hs-bindgen-cli preprocess --library@
--
-- The modules themselves have golden tests in "Test.HsBindgen.Golden.Library".
-- The tests here are about the command: what it writes, what it refuses, and
-- how it exits.
module Test.HsBindgen.Integration.Library (tests) where

import Control.Monad (forM)
import Data.List (isInfixOf, sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

import Test.HsBindgen.Resources

tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Integration.Library" [
      testCase "one module for each header" $
        inOutputDir $ \outDir -> do
          (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl []
          exitCode @?= ExitSuccess
          written <- listFiles outDir
          written @?= [
              "M" </> "A" </> "FunPtr.hs"
            , "M" </> "A" </> "Safe.hs"
            , "M" </> "A" </> "Unsafe.hs"
            , "M" </> "B.hs"
            ]
          safe <- readFile (outDir </> "M" </> "A" </> "Safe.hs")
          assertBool "M.A.Safe imports M.B" $
            "import qualified M.B" `isInfixOf` safe
    , testCase "one file for each header" $
        inOutputDir $ \outDir -> do
          (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
            ["--single-file", "--unsafe", ""]
          exitCode @?= ExitSuccess
          written <- listFiles outDir
          written @?= ["M" </> "A.hs", "M" </> "B.hs"]
    , testCase "a dry run writes nothing" $
        inOutputDir $ \outDir -> do
          (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
            ["--dry-run"]
          exitCode @?= ExitSuccess
          written <- listFiles outDir
          written @?= []
    , testGroup "usage errors" [
          testCase flag $
            inOutputDir $ \outDir -> do
              (exitCode, _, err) <-
                preprocess getTestResources outDir forwardDecl options
              exitCode @?= ExitFailure 2
              -- The command line parses, and is refused for what it asks for
              assertBool ("unexpected message: " ++ err) $
                "--library cannot be combined with" `isInfixOf` err
              written <- listFiles outDir
              written @?= []
        | (flag, options) <- [
              ("--gen-binding-spec", ["--gen-binding-spec", "spec.yaml"])
            ]
        ]
    , testGroup "selection by header" [
          testGroup "every declaration, said or not" [
              testCase flag $
                inOutputDir $ \outDir -> do
                  (exitCode, _, _) <-
                    preprocess getTestResources outDir forwardDecl [flag]
                  exitCode @?= ExitSuccess
                  written <- listFiles outDir
                  written @?= [
                      "M" </> "A" </> "FunPtr.hs"
                    , "M" </> "A" </> "Safe.hs"
                    , "M" </> "A" </> "Unsafe.hs"
                    , "M" </> "B.hs"
                    ]
              -- a.h is in the directory of b.h, which is the header given
            | flag <- [
                  "--select-all"
                , "--select-from-all-headers"
                , "--select-from-main-header-dirs"
                ]
            ]
        , testCase "the declarations of the given header only" $
            inOutputDir $ \outDir -> do
              -- b.h is given and a.h is not, so the function of a.h is not
              -- selected and a.h has nothing
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                ["--select-from-main-headers"]
              exitCode @?= ExitSuccess
              written <- listFiles outDir
              written @?= ["M" </> "B.hs"]
        , testCase "a given header without declarations" $
            inOutputDir $ \outDir ->
            withSystemTempDirectory "hs-bindgen-test" $ \headers -> do
              -- root.h is the only header given and it only includes
              writeFile (headers </> "a.h") "struct a { int x; };\n"
              writeFile (headers </> "root.h") "#include \"a.h\"\n"
              (exitCode, _, _) <- run headers [headers] outDir
                ["--select-from-main-headers"] "root.h"
              exitCode @?= ExitSuccess
              written <- listFiles outDir
              written @?= []
        , testCase "a header that is selected by its path" $
            inOutputDir $ \outDir -> do
              -- The struct of b.h is not selected, and the function of a.h
              -- has it in its type.  It is generated where it is needed.
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                ["--select-by-header-path", "a\\.h$"]
              exitCode @?= ExitSuccess
              written <- listFiles outDir
              written @?= [
                  "M" </> "A.hs"
                , "M" </> "A" </> "FunPtr.hs"
                , "M" </> "A" </> "Safe.hs"
                , "M" </> "A" </> "Unsafe.hs"
                ]
              types <- readFile (outDir </> "M" </> "A.hs")
              assertBool "M.A has the struct" $ "data Shape" `isInfixOf` types
        , testCase "a header that is excepted by its path" $
            inOutputDir $ \outDir -> do
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                ["--select-except-by-header-path", "a\\.h$"]
              exitCode @?= ExitSuccess
              written <- listFiles outDir
              written @?= ["M" </> "B.hs"]
        , testCase "the path that is matched is the whole path" $
            inOutputDir $ \outDir -> do
              -- Not the path in the library directory, which is what a module
              -- is named after
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                ["--select-by-header-path", "^b\\.h$"]
              exitCode @?= ExitSuccess
              written <- listFiles outDir
              written @?= []
        , testCase "a header and a declaration, each selected" $
            inOutputDir $ \outDir -> do
              -- What one flag selects is added to what another one selects
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                [ "--select-by-header-path", "a\\.h$"
                , "--select-by-decl-name", "^struct shape$"
                ]
              exitCode @?= ExitSuccess
              written <- listFiles outDir
              written @?= [
                  "M" </> "A" </> "FunPtr.hs"
                , "M" </> "A" </> "Safe.hs"
                , "M" </> "A" </> "Unsafe.hs"
                , "M" </> "B.hs"
                ]
        , testCase "a header selected and its declaration excepted" $
            inOutputDir $ \outDir -> do
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                [ "--select-by-header-path", "a\\.h$"
                , "--select-except-by-decl-name", "^shape_draw$"
                ]
              exitCode @?= ExitSuccess
              written <- listFiles outDir
              written @?= []
        , testGroup "a typedef in one header and its struct in another" [
              testCase "the header of the struct, excepted by its path" $
                inOutputDir $ \outDir -> splitTypedef $ \headers -> do
                  -- point.h gets a module all the same.  The typedef is
                  -- selected, and the type that the two are goes to the
                  -- header of the struct.
                  (exitCode, _, _) <- run headers [headers] outDir
                    ["--select-except-by-header-path", "point\\.h$"] "user.h"
                  exitCode @?= ExitSuccess
                  written <- listFiles outDir
                  written @?= ["M" </> "Point.hs", "M" </> "User.hs"]
            , testCase "the typedef excepted as well, by its name" $
                inOutputDir $ \outDir -> splitTypedef $ \headers -> do
                  -- Nothing selects the type now, so point.h gets no module.
                  -- user.h needs the type and generates it.
                  (exitCode, _, _) <- run headers [headers] outDir
                    [ "--select-except-by-header-path", "point\\.h$"
                    , "--select-except-by-decl-name", "^point$"
                    ]
                    "user.h"
                  exitCode @?= ExitSuccess
                  written <- listFiles outDir
                  written @?= ["M" </> "User.hs"]
                  types <- readFile (outDir </> "M" </> "User.hs")
                  assertBool "M.User has both structs" $
                    all (`isInfixOf` types)
                      ["data Point = Point", "data User = User"]
            ]
        ]
    , testGroup "selection by declaration" [
          testCase "a declaration that is selected by name" $
            inOutputDir $ \outDir -> do
              -- The function of a.h is not selected, so a.h has nothing.  The
              -- name that is matched has the tag of the struct.
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                ["--select-by-decl-name", "^struct shape$"]
              exitCode @?= ExitSuccess
              written <- listFiles outDir
              written @?= ["M" </> "B.hs"]
        , testCase "a declaration that is excepted by name" $
            inOutputDir $ \outDir -> do
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                ["--select-except-by-decl-name", "^shape_draw$"]
              exitCode @?= ExitSuccess
              written <- listFiles outDir
              written @?= ["M" </> "B.hs"]
        , testCase "a declaration that a selected one needs" $
            inOutputDir $ \outDir -> do
              -- The struct of b.h is not selected, and the function of a.h
              -- has it in its type.  It is generated where it is needed.
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                ["--select-by-decl-name", "^shape_draw$"]
              exitCode @?= ExitSuccess
              written <- listFiles outDir
              written @?= [
                  "M" </> "A.hs"
                , "M" </> "A" </> "FunPtr.hs"
                , "M" </> "A" </> "Safe.hs"
                , "M" </> "A" </> "Unsafe.hs"
                ]
              types <- readFile (outDir </> "M" </> "A.hs")
              assertBool "M.A has the struct" $ "data Shape" `isInfixOf` types
        , testCase "a deprecated declaration that is excepted" $
            inOutputDir $ \outDir ->
            withSystemTempDirectory "hs-bindgen-test" $ \headers -> do
              writeFile (headers </> "a.h") $ unlines [
                  "void old_name(void) __attribute__((deprecated));"
                , "void new_name(void);"
                ]
              (exitCode, _, _) <- run headers [headers] outDir
                ["--select-except-deprecated"] "a.h"
              exitCode @?= ExitSuccess
              safe <- readFile (outDir </> "M" </> "A" </> "Safe.hs")
              assertBool "M.A.Safe has the function that is not deprecated" $
                "new_name" `isInfixOf` safe
              assertBool "M.A.Safe does not have the deprecated function" $
                not ("old_name" `isInfixOf` safe)
        ]
    , testGroup "binding specifications" [
          testCase "one file for each module" $
            inOutputDir $ \outDir -> inOutputDir $ \specDir -> do
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                ["--gen-binding-spec-dir", specDir]
              exitCode @?= ExitSuccess
              specs <- listFiles specDir
              specs @?= ["M.A.yaml", "M.B.yaml"]
              -- The module of b.h has the struct, the one of a.h a function
              specB <- readFile (specDir </> "M.B.yaml")
              assertBool "M.B.yaml is for M.B and has the struct" $
                all (`isInfixOf` specB) ["hsmodule: M.B\n", "cname: struct shape"]
              specA <- readFile (specDir </> "M.A.yaml")
              assertBool "M.A.yaml is for M.A and has no type" $
                "hsmodule: M.A\n" `isInfixOf` specA
                  && not ("ctypes" `isInfixOf` specA)
        , testCase "a module that is not written has no file" $
            inOutputDir $ \outDir -> inOutputDir $ \specDir ->
            withSystemTempDirectory "hs-bindgen-test" $ \headers -> do
              -- guard.h has a macro and nothing that gets a binding
              writeFile (headers </> "guard.h") $ unlines [
                  "#ifndef GUARD_H"
                , "#define GUARD_H"
                , "#endif"
                ]
              writeFile (headers </> "a.h") $ unlines [
                  "#include \"guard.h\""
                , "struct a { int x; };"
                ]
              (exitCode, _, _) <- run headers [headers] outDir
                ["--gen-binding-spec-dir", specDir] "a.h"
              exitCode @?= ExitSuccess
              written <- listFiles outDir
              written @?= ["M" </> "A.hs"]
              specs <- listFiles specDir
              specs @?= ["M.A.yaml"]
        , testCase "a dry run writes no file" $
            inOutputDir $ \outDir -> inOutputDir $ \specDir -> do
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                ["--gen-binding-spec-dir", specDir, "--dry-run"]
              exitCode @?= ExitSuccess
              specs <- listFiles specDir
              specs @?= []
        , testCase "a directory that does not exist is an error" $
            inOutputDir $ \outDir -> do
              -- No module is written either
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                ["--gen-binding-spec-dir", outDir </> "specs"]
              exitCode @?= ExitFailure 4
              written <- listFiles outDir
              written @?= []
        , testCase "a directory that is created" $
            inOutputDir $ \outDir -> do
              (exitCode, _, _) <- preprocess getTestResources outDir forwardDecl
                [ "--gen-binding-spec-dir", outDir </> "specs"
                , "--create-output-dirs"
                ]
              exitCode @?= ExitSuccess
              specs <- listFiles (outDir </> "specs")
              specs @?= ["M.A.yaml", "M.B.yaml"]
        , testCase "--gen-binding-spec-dir without --library is a usage error" $
            inOutputDir $ \outDir -> inOutputDir $ \specDir -> do
              (exitCode, _, err) <- preprocess getTestResources outDir
                (const []) ["--gen-binding-spec-dir", specDir]
              exitCode @?= ExitFailure 2
              assertBool ("unexpected message: " ++ err) $
                "--gen-binding-spec-dir can only be used with --library"
                  `isInfixOf` err
              written <- listFiles outDir
              written @?= []
        ]
    , testCase "a library directory that does not exist is a usage error" $
        inOutputDir $ \outDir -> do
          (exitCode, _, _) <- preprocess getTestResources outDir
            (\headers -> [headers </> "no_such_directory"]) []
          exitCode @?= ExitFailure 2
          written <- listFiles outDir
          written @?= []
    , testCase "no header in the library is an error" $
        inOutputDir $ \outDir -> do
          -- The directory exists, and the header that is read is not in it
          (exitCode, _, _) <- preprocess getTestResources outDir
            (\headers -> [headers </> "mylib"]) []
          exitCode @?= ExitFailure 4
          written <- listFiles outDir
          written @?= []
    , testCase "headers that use each other share a module" $
        inOutputDir $ \outDir ->
        withSystemTempDirectory "hs-bindgen-test" $ \headers -> do
          -- Each of the two structs has a pointer to the other
          writeFile (headers </> "a.h") $ unlines [
              "struct b;"
            , "struct a { struct b *b; };"
            ]
          writeFile (headers </> "b.h") $ unlines [
              "#include \"a.h\""
            , "struct b { struct a *a; };"
            ]
          (exitCode, _, _) <- run headers [headers] outDir [] "b.h"
          exitCode @?= ExitSuccess
          -- The module has the name of the header that includes the other
          written <- listFiles outDir
          written @?= ["M" </> "B.hs"]
          types <- readFile (outDir </> "M" </> "B.hs")
          assertBool "M.B has both structs" $
            all (`isInfixOf` types) ["data A = A", "data B = B"]
    ]
  where
    inOutputDir :: (FilePath -> IO a) -> IO a
    inOutputDir = withSystemTempDirectory "hs-bindgen-test"

    -- The library directories, given the directory with the headers
    forwardDecl :: FilePath -> [FilePath]
    forwardDecl headers = [headers </> "forward_decl"]

-- | Run @preprocess --library@ on @forward_decl\/b.h@, with base module @M@
preprocess ::
     IO TestResources
  -> FilePath                  -- ^ Output directory
  -> (FilePath -> [FilePath])  -- ^ Library directories
  -> [String]                  -- ^ More options
  -> IO (ExitCode, String, String)
preprocess getTestResources outDir libraryDirs options = do
    root <- (.packageRoot) <$> getTestResources
    let headers = root </> "test-artefacts" </> "headers"
    run headers (libraryDirs headers) outDir options "forward_decl/b.h"

-- | Run @preprocess --library@ with base module @M@
run ::
     FilePath    -- ^ Directory on the include path
  -> [FilePath]  -- ^ Library directories
  -> FilePath    -- ^ Output directory
  -> [String]    -- ^ More options
  -> String      -- ^ The header to read
  -> IO (ExitCode, String, String)
run headers libraryDirs outDir options header =
    readProcessWithExitCode
      "hs-bindgen-cli"
      (concat [
          ["preprocess", "-I", headers, "--module", "M"]
        , concat [ ["--library", dir] | dir <- libraryDirs ]
        , ["--hs-output-dir", outDir]
        , options
        , [header]
        ])
      ""

-- | Run an action in a directory with a typedef in @types.h@ for the struct
-- that @point.h@ defines
--
-- @point.h@ includes @types.h@.  @user.h@ includes @point.h@ and has a struct
-- with a field of the type.
splitTypedef :: (FilePath -> IO a) -> IO a
splitTypedef k = withSystemTempDirectory "hs-bindgen-test" $ \headers -> do
    writeFile (headers </> "types.h") "typedef struct point point;\n"
    writeFile (headers </> "point.h") $ unlines [
        "#include \"types.h\""
      , "struct point { int x; };"
      ]
    writeFile (headers </> "user.h") $ unlines [
        "#include \"point.h\""
      , "struct user { point p; };"
      ]
    k headers

-- | The files below a directory, with their paths below it, sorted
listFiles :: FilePath -> IO [FilePath]
listFiles dir = sort <$> go ""
  where
    go :: FilePath -> IO [FilePath]
    go below = do
        entries <- listDirectory (dir </> below)
        fmap concat $ forM entries $ \entry -> do
          let path = below </> entry
          isDir <- doesDirectoryExist (dir </> path)
          if isDir then go path else pure [path]
