{-# LANGUAGE CPP               #-}

module Main (main) where

import Data.TreeDiff.Golden (ediffGolden1)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Test.Tasty (TestTree, TestName, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import TastyGolden (goldenTestSteps)
import Orphans ()
import Rust
import Misc

#if __GLASGOW_HASKELL__ ==908
import TH
#endif

import HsBindgen.Lib
import HsBindgen.Backend.PP.Render qualified as Backend.PP

main :: IO ()
main = do
    packageRoot <- findPackageDirectory "hs-bindgen"
    defaultMain $ withRustBindgen $ \bg -> main' packageRoot bg

main' :: FilePath -> IO FilePath -> TestTree
main' packageRoot bg = testGroup "golden"
    [ testCase "target-triple" $ do
        let fp = "examples/simple_structs.h"
            args = clangArgs packageRoot
        relPath <- Just <$> Dir.getCurrentDirectory
        triple <- withC relPath nullTracer args fp $ getTargetTriple

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
#if __GLASGOW_HASKELL__ ==908
        , goldenTh packageRoot name
#endif
        , goldenPP name
        , goldenRust bg name
        ]

    goldenTreeDiff name = ediffGolden1 goldenTestSteps "treediff" ("fixtures" </> (name ++ ".tree-diff.txt")) $ \report -> do
        let fp = "examples" </> (name ++ ".h")
            args = clangArgs packageRoot

        let tracer = mkTracer report report report False

        relPath <- Just <$> Dir.getCurrentDirectory
        header <- parseC relPath tracer args fp
        return header

    goldenHs name = ediffGolden1 goldenTestSteps "hs" ("fixtures" </> (name ++ ".hs")) $ \report -> do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let fp = "examples" </> (name ++ ".h")
            args = clangArgs packageRoot

        let tracer = mkTracer report report report False

        relPath <- Just <$> Dir.getCurrentDirectory
        header <- parseC relPath tracer args fp
        return $ genHsDecls header

    goldenPP :: TestName -> TestTree
    goldenPP name = goldenVsStringDiff_ "pp" ("fixtures" </> (name ++ ".pp.hs")) $ \report -> do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let fp = "examples" </> (name ++ ".h")
            args = clangArgs packageRoot

        let tracer = mkTracer report report report False

        relPath <- Just <$> Dir.getCurrentDirectory
        header <- parseC relPath tracer args fp

        -- TODO: PP.render should add trailing '\n' itself.
        return $ (Backend.PP.render renderOpts $ unwrapHsModule $ genModule moduleOpts header) ++ "\n"
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
     Maybe FilePath -- ^ Directory to make paths relative to
  -> Tracer IO String
  -> ClangArgs
  -> FilePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withC relPath tracer args fp =
    withTranslationUnit relPath tracerD args fp
  where
    tracerD = contramap show tracer

parseC ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> Tracer IO String
  -> ClangArgs
  -> FilePath
  -> IO CHeader
parseC relPath tracer args fp =
    withC relPath tracer args fp $
      parseCHeader relPath tracerP SelectFromMainFile
  where
    tracerP = contramap prettyLogMsg tracer
