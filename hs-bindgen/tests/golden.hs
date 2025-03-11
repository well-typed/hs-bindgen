{-# LANGUAGE CPP               #-}

module Main (main) where

import Data.Foldable (toList)
import Data.List (sort)
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
        let headerIncludePath = CHeaderQuoteIncludePath "simple_structs.h"
            args = clangArgs packageRoot
        src <- resolveHeader args headerIncludePath
        triple <- withC nullTracer args src getTargetTriple

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
    , testGroup "macro_strings" $ goldenNoRust "macro_strings" -- rs-bindgen panics on this
    , golden "macro_functions"
    , golden "macro_in_fundecl"
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
    , golden "unions"
    ]
  where
    golden name =
      testGroup name $ goldenNoRust name ++ [ goldenRust bg name ]

    goldenNoRust name =
        [ goldenTreeDiff name
        , goldenHs name
        , goldenExtensions name
-- Pretty-printing of TH differs between GHC versions; for example, @()@ becomes
-- @Unit@ in 9.8 <https://github.com/ghc-proposals/ghc-proposals/pull/475>.
-- We therefore test TH only with one specific GHC version.
#if __GLASGOW_HASKELL__ >=904
        , goldenTh packageRoot name
#endif
        , goldenPP name
        ]

    goldenTreeDiff name = ediffGolden1 goldenTestSteps "treediff" ("fixtures" </> (name ++ ".tree-diff.txt")) $ \report -> do
        let headerIncludePath = CHeaderQuoteIncludePath $ name ++ ".h"
            args = clangArgs packageRoot
            tracer = mkTracer report report report False
        src <- resolveHeader args headerIncludePath
        parseC tracer args src

    goldenHs name = ediffGolden1 goldenTestSteps "hs" ("fixtures" </> (name ++ ".hs")) $ \report -> do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let headerIncludePath = CHeaderQuoteIncludePath $ name ++ ".h"
            args = clangArgs packageRoot
            tracer = mkTracer report report report False
        src <- resolveHeader args headerIncludePath
        header <- parseC tracer args src
        return $ genHsDecls headerIncludePath defaultTranslationOpts header

    goldenExtensions name = goldenVsStringDiff_ "exts" ("fixtures" </> (name ++ ".exts.txt")) $ \report -> do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let headerIncludePath = CHeaderQuoteIncludePath $ name ++ ".h"
            args = clangArgs packageRoot
            tracer = mkTracer report report report False
        src <- resolveHeader args headerIncludePath
        header <- parseC tracer args src
        return $ unlines $ map show $ sort $ toList $
            genExtensions headerIncludePath defaultTranslationOpts header

    goldenPP :: TestName -> TestTree
    goldenPP name = goldenVsStringDiff_ "pp" ("fixtures" </> (name ++ ".pp.hs")) $ \report -> do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let headerIncludePath = CHeaderQuoteIncludePath $ name ++ ".h"
            args = clangArgs packageRoot
            tracer = mkTracer report report report False
        src <- resolveHeader args headerIncludePath
        header <- parseC tracer args src

        -- TODO: PP.render should add trailing '\n' itself.
        return $ (Backend.PP.render renderOpts $ unwrapHsModule $
          genModule headerIncludePath defaultTranslationOpts moduleOpts header) ++ "\n"
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
  -> SourcePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withC tracer args src =
    withTranslationUnit tracerD args src
  where
    tracerD = contramap show tracer

parseC ::
     Tracer IO String
  -> ClangArgs
  -> SourcePath
  -> IO CHeader
parseC tracer args src =
    withC tracer args src $
      parseCHeader tracerP SelectFromMainFile emptyExtBindings
  where
    tracerP = contramap prettyLogMsg tracer
