{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Exception (try, displayException, evaluate)
import Control.DeepSeq (force)
import Data.ByteString.UTF8 qualified as UTF8
import Data.Foldable (toList)
import Data.List (sort)
import Data.TreeDiff.Golden (ediffGolden1)
import Test.Tasty (TestTree, TestName, defaultMain, testGroup)

import Test.HsBindgen.C.Parser qualified
import Test.Internal.Misc
import Test.Internal.Rust
import Test.Internal.TastyGolden (goldenTestSteps)
import Test.Internal.TreeDiff.Orphans ()

#if __GLASGOW_HASKELL__ >=904
import Test.Internal.TH
#endif

import Clang.Paths
import HsBindgen.Errors
import HsBindgen.ExtBindings qualified as ExtBindings
import HsBindgen.ExtBindings.Gen qualified as ExtBindings
import HsBindgen.Lib
import HsBindgen.Pipeline qualified as Pipeline

main :: IO ()
main = do
    packageRoot <- findPackageDirectory "hs-bindgen"
    defaultMain $ withRustBindgen $ \bg -> main' packageRoot bg

main' :: FilePath -> IO FilePath -> TestTree
main' packageRoot bg = testGroup "golden"
    [ Test.HsBindgen.C.Parser.tests $ clangArgs packageRoot
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

    , testGroup "failures"
        [ failing "long_double"
        ]
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
        , goldenExtBindings name
        ]

    goldenTreeDiff name = ediffGolden1 goldenTestSteps "treediff" ("fixtures" </> (name ++ ".tree-diff.txt")) $ \report -> do
        let headerIncludePath = mkHeaderIncludePath name
        snd <$> Pipeline.parseCHeader (mkOpts report) headerIncludePath

    goldenHs name = ediffGolden1 goldenTestSteps "hs" ("fixtures" </> (name ++ ".hs")) $ \report -> do
        let headerIncludePath = mkHeaderIncludePath name
        Pipeline.translateCHeader (mkOpts report) headerIncludePath

    goldenExtensions name = goldenVsStringDiff_ "exts" ("fixtures" </> (name ++ ".exts.txt")) $ \report -> do
        let headerIncludePath = mkHeaderIncludePath name
        decls <- Pipeline.translateCHeader (mkOpts report) headerIncludePath
        return $ unlines $ map show $ sort $ toList $
              Pipeline.genExtensions
            $ Pipeline.genSHsDecls decls

    goldenPP :: TestName -> TestTree
    goldenPP name = goldenVsStringDiff_ "pp" ("fixtures" </> (name ++ ".pp.hs")) $ \report -> do
        let headerIncludePath = mkHeaderIncludePath name
            opts' = mkOpts report
        decls <- Pipeline.translateCHeader opts' headerIncludePath

        -- TODO: PP.render should add trailing '\n' itself.
        return $ Pipeline.preprocessPure ppOpts decls ++ "\n"

    goldenExtBindings :: TestName -> TestTree
    goldenExtBindings name = goldenVsStringDiff_ "extbindings" ("fixtures" </> (name ++ ".extbindings.yaml")) $ \report -> do
        let headerIncludePath = mkHeaderIncludePath name
        decls <- Pipeline.translateCHeader (mkOpts report) headerIncludePath
        return . UTF8.toString . ExtBindings.encodeUnresolvedExtBindingsYaml $
          ExtBindings.genExtBindings
            headerIncludePath
            (ExtBindings.HsPackageName "example")
            (ExtBindings.HsModuleName "Example")
            decls

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

    failing :: TestName -> TestTree
    failing name = goldenVsStringDiff_ name ("fixtures" </> (name ++ ".failure.txt")) $ \report -> do
        result <- try $ do
            let headerIncludePath = mkHeaderIncludePath name
                opts' = mkOpts report
            decls <- Pipeline.translateCHeader opts' headerIncludePath

            -- TODO: PP.render should add trailing '\n' itself.
            evaluate $ force $ Pipeline.preprocessPure ppOpts decls ++ "\n"

        case result of
            Right result'  -> fail $ "Expected failure; unexpected success\n" ++ result'
            Left exc -> return $ map windows $ displayException (exc :: HsBindgenException)

    windows :: Char -> Char
    windows '\\' = '/'
    windows c    = c
