{-# LANGUAGE OverloadedStrings #-}
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

import HsBindgen.Hs.AST qualified as Hs
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
        triple <- withC nullTracer args fp $ getTargetTriple

        -- macos-latest (macos-14) returns "arm64-apple-macosx14.0.0"
        -- windows-latest (???) returns "x86_64-pc-windows-msvc19.41.34120"
        triple @?= "x86_64-pc-linux-gnu"

    , golden "simple_structs"
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
    ]
  where
    golden name = testGroup name
        [ goldenTreeDiff name
        , goldenHs name
-- Since GHC-9.4 the Template Haskell ppr function has changed slightly
#if __GLASGOW_HASKELL__ >=904
        , goldenTh packageRoot name
#endif
        , goldenPP name
        , goldenRust bg name
        ]

    goldenTreeDiff name = ediffGolden1 goldenTestSteps "treediff" ("fixtures" </> (name ++ ".tree-diff.txt")) $ \report -> do
        let fp = "examples" </> (name ++ ".h")
            args = clangArgs packageRoot

        let tracer = mkTracer report report report False

        header <- parseC tracer args fp
        return header

    goldenHs name = goldenVsStringDiff_ "hs" ("fixtures" </> (name ++ ".hs")) $ \report -> do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let fp = "examples" </> (name ++ ".h")
            args = clangArgs packageRoot

        let tracer = mkTracer report report report False

        header <- parseC tracer args fp
        let decls :: [Hs.Decl]
            decls = genHsDecls header

        return $ unlines $ map show decls

    goldenPP :: TestName -> TestTree
    goldenPP name = goldenVsStringDiff_ "pp" ("fixtures" </> (name ++ ".pp.hs")) $ \report -> do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let fp = "examples" </> (name ++ ".h")
            args = clangArgs packageRoot

        let tracer = mkTracer report report report False

        header <- parseC tracer args fp

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
     Tracer IO String
  -> ClangArgs
  -> FilePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withC tracer args fp =
    withTranslationUnit tracerD args fp
  where
    tracerD = contramap show tracer

parseC ::
     Tracer IO String
  -> ClangArgs
  -> FilePath
  -> IO CHeader
parseC tracer args fp =
    withC tracer args fp $ parseCHeader tracerP SelectFromMainFile
  where
    tracerP = contramap prettyLogMsg tracer
