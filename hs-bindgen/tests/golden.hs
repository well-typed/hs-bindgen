{-# LANGUAGE CPP #-}

module Main (main) where

import Data.Foldable (toList)
import Data.List (sort)
import Data.TreeDiff.Golden (ediffGolden1)
import Test.Tasty (TestTree, TestName, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import TastyGolden (goldenTestSteps)
import Orphans ()
import Rust
import Misc

#if __GLASGOW_HASKELL__ >=904
import TH
#endif

import HsBindgen.C.Parser (getTargetTriple)
import HsBindgen.Clang.Paths
import HsBindgen.Lib
import HsBindgen.Pipeline qualified as Pipeline

main :: IO ()
main = do
    packageRoot <- findPackageDirectory "hs-bindgen"
    defaultMain $ withRustBindgen $ \bg -> main' packageRoot bg

main' :: FilePath -> IO FilePath -> TestTree
main' packageRoot bg = testGroup "golden"
    [ testCase "target-triple" $ do
        triple <- getTargetTriple $ clangArgs packageRoot

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
        let headerIncludePath = mkHeaderIncludePath name
        snd <$> Pipeline.parseCHeader (mkOpts report) headerIncludePath

    goldenHs name = ediffGolden1 goldenTestSteps "hs" ("fixtures" </> (name ++ ".hs")) $ \report -> do
        let headerIncludePath = mkHeaderIncludePath name
            opts' = mkOpts report
        header <- snd <$> Pipeline.parseCHeader opts' headerIncludePath
        return $ Pipeline.genHsDecls opts' headerIncludePath header

    goldenExtensions name = goldenVsStringDiff_ "exts" ("fixtures" </> (name ++ ".exts.txt")) $ \report -> do
        let headerIncludePath = mkHeaderIncludePath name
            opts' = mkOpts report
        header <- snd <$> Pipeline.parseCHeader opts' headerIncludePath
        return $ unlines $ map show $ sort $ toList $
              Pipeline.genExtensions
            . Pipeline.genSHsDecls
            $ Pipeline.genHsDecls opts' headerIncludePath header

    goldenPP :: TestName -> TestTree
    goldenPP name = goldenVsStringDiff_ "pp" ("fixtures" </> (name ++ ".pp.hs")) $ \report -> do
        let headerIncludePath = mkHeaderIncludePath name
            opts' = mkOpts report
        header <- snd <$> Pipeline.parseCHeader opts' headerIncludePath

        -- TODO: PP.render should add trailing '\n' itself.
        return $
          Pipeline.preprocessPure opts' ppOpts headerIncludePath header ++ "\n"

    -- -<.> does weird stuff for filenames with multiple dots;
    -- I usually simply avoid using it.
    mkHeaderIncludePath :: String -> CHeaderIncludePath
    mkHeaderIncludePath = CHeaderQuoteIncludePath . (++ ".h")

    opts :: Pipeline.Opts
    opts = Pipeline.defaultOpts {
        Pipeline.optsClangArgs  = clangArgs packageRoot
      }

    mkOpts :: (String -> IO ()) -> Pipeline.Opts
    mkOpts report =
      let tracer = mkTracer report report report False
      in  opts {
              Pipeline.optsDiagTracer = tracer
            , Pipeline.optsSkipTracer = tracer
            }

    ppOpts :: Pipeline.PPOpts
    ppOpts = Pipeline.defaultPPOpts {
        Pipeline.ppOptsModule = HsModuleOpts { hsModuleOptsName = "Example" }
      }
