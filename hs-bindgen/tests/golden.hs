{-# LANGUAGE CPP               #-}

module Main (main) where

import Data.TreeDiff.Golden (ediffGolden1)
import System.FilePath ((</>))
import Test.Tasty (TestTree, TestName, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import TastyGolden (goldenTestSteps)
import Orphans ()
import Rust
import Misc

#if __GLASGOW_HASKELL__ >=904
import TH
#endif

import HsBindgen.Clang.Paths
import HsBindgen.Lib
import HsBindgen.Backend.PP.Render qualified as Backend.PP

main :: IO ()
main = do
    packageRoot <- findPackageDirectory "hs-bindgen"
    defaultMain $ withRustBindgen $ \bg -> main' packageRoot bg

main' :: FilePath -> IO FilePath -> TestTree
main' packageRoot bg = testGroup "golden"
    [ testCase "target-triple" $ do
        relPath <- either fail return $ mkCHeaderRelPath "simple_structs.h"
        let args = clangArgs packageRoot
        includeDirs <- either fail return =<<
          resolveCIncludeAbsPathDirs (clangIncludePathDirs args)
        absPath <- either fail return =<< resolveHeader includeDirs relPath

        triple <- withC nullTracer args absPath getTargetTriple

        -- macos-latest (macos-14) returns "arm64-apple-macosx14.0.0"
        -- windows-latest (???) returns "x86_64-pc-windows-msvc19.41.34120"
        triple @?= "x86_64-pc-linux-gnu"

    , golden "simple_structs"
    , golden "recursive_struct"
    , golden "nested_types"
    , golden "enums"
    , golden "primitive_types"
    , golden "typedefs"
    , golden "macros"
    , golden "macro_functions"
    , golden "uses_utf8"
    , golden "typedef_vs_macro"
    , golden "headers"
    , golden "fixedwidth"
    , golden "fixedarray"
    , golden "unnamed-struct"
    , golden "forward_declaration"
    , golden "opaque_declaration"
    , golden "distilled_lib_1"
    , golden "flam"
    , golden "typenames"
    , golden "bool"
    , golden "anonymous"
    , golden "simple_func"
    , golden "weird01"
    , golden "bitfields"
    ]
  where
    golden name = testGroup name
        [ goldenTreeDiff name
        , goldenHs name
-- Pretty-printing of TH differs between GHC versions; for example, @()@ becomes
-- @Unit@ in 9.8 <https://github.com/ghc-proposals/ghc-proposals/pull/475>.
-- We therefore test TH only with one specific GHC version.
#if __GLASGOW_HASKELL__ >=904
        , goldenTh packageRoot name
#endif
        , goldenPP name
        , goldenRust bg name
        ]

    goldenTreeDiff name = ediffGolden1 goldenTestSteps "treediff" ("fixtures" </> (name ++ ".tree-diff.txt")) $ \report -> do
        relPath <- either fail return $ mkCHeaderRelPath (name ++ ".h")
        let args = clangArgs packageRoot
        includeDirs <- either fail return =<<
          resolveCIncludeAbsPathDirs (clangIncludePathDirs args)
        absPath <- either fail return =<< resolveHeader includeDirs relPath

        let tracer = mkTracer report report report False

        parseC tracer args absPath

    goldenHs name = ediffGolden1 goldenTestSteps "hs" ("fixtures" </> (name ++ ".hs")) $ \report -> do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        relPath <- either fail return $ mkCHeaderRelPath (name ++ ".h")
        let args = clangArgs packageRoot
        includeDirs <- either fail return =<<
          resolveCIncludeAbsPathDirs (clangIncludePathDirs args)
        absPath <- either fail return =<< resolveHeader includeDirs relPath

        let tracer = mkTracer report report report False

        header <- parseC tracer args absPath
        return $ genHsDecls relPath defaultTranslationOpts header

    goldenPP :: TestName -> TestTree
    goldenPP name = goldenVsStringDiff_ "pp" ("fixtures" </> (name ++ ".pp.hs")) $ \report -> do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        relPath <- either fail return $ mkCHeaderRelPath (name ++ ".h")
        let args = clangArgs packageRoot
        includeDirs <- either fail return =<<
          resolveCIncludeAbsPathDirs (clangIncludePathDirs args)
        absPath <- either fail return =<< resolveHeader includeDirs relPath

        let tracer = mkTracer report report report False

        header <- parseC tracer args absPath

        -- TODO: PP.render should add trailing '\n' itself.
        return $ (Backend.PP.render renderOpts $ unwrapHsModule $ genModule relPath defaultTranslationOpts moduleOpts header) ++ "\n"
      where
        moduleOpts :: HsModuleOpts
        moduleOpts = HsModuleOpts
            { hsModuleOptsName = "Example"
            }

        renderOpts :: HsRenderOpts
        renderOpts = HsRenderOpts
            { hsLineLength = 120
            }

withC ::
     Tracer IO String
  -> ClangArgs
  -> CHeaderAbsPath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withC tracer args headerPath =
    withTranslationUnit tracerD args headerPath
  where
    tracerD = contramap show tracer

parseC ::
     Tracer IO String
  -> ClangArgs
  -> CHeaderAbsPath
  -> IO CHeader
parseC tracer args headerPath =
    withC tracer args headerPath $
      parseCHeader tracerP SelectFromMainFile
  where
    tracerP = contramap prettyLogMsg tracer
