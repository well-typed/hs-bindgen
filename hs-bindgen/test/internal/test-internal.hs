{-# LANGUAGE CPP #-}
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (try, displayException, evaluate)
import Data.ByteString.UTF8 qualified as UTF8
import Data.List qualified as List
import Data.TreeDiff.Golden (ediffGolden1)
import Test.Tasty (TestTree, TestName, defaultMain, testGroup)
import Text.Regex.Applicative qualified as R
import Text.Regex.Applicative.Common qualified as R

import Clang.Paths
import HsBindgen.Imports
import HsBindgen.Errors
import HsBindgen.ExtBindings qualified as ExtBindings
import HsBindgen.ExtBindings.Gen qualified as ExtBindings
import HsBindgen.Lib
import HsBindgen.Pipeline qualified as Pipeline

import Test.HsBindgen.C.Parser qualified
import Test.Internal.Misc
import Test.Internal.Rust
import Test.Internal.TastyGolden (goldenTestSteps)
import Test.Internal.TreeDiff.Orphans ()

#if __GLASGOW_HASKELL__ >=904
import Test.Internal.TH
#endif

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    packageRoot <- findPackageDirectory "hs-bindgen"
    defaultMain . withRustBindgen $ tests packageRoot

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: FilePath -> IO FilePath -> TestTree
tests packageRoot rustBindgen = testGroup "test-internal" [
      Test.HsBindgen.C.Parser.tests args
    , testGroup "examples" [
          golden "simple_structs"
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
        , golden "nested_enums"
        , golden "nested_unions"
        , golden "adios"
        ]
    , testGroup "failing-examples" [
          failing "long_double"
        , failing "struct_arg_a"
        ]
    ]
  where
    args :: ClangArgs
    args = clangArgs packageRoot

    golden :: TestName -> TestTree
    golden name =
      testGroup name $ goldenNoRust name ++ [goldenRust rustBindgen name]

    goldenNoRust :: TestName -> [TestTree]
    goldenNoRust name = [
          goldenTreeDiff name
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

    goldenTreeDiff :: TestName -> TestTree
    goldenTreeDiff name = do
      let target = "fixtures" </> (name ++ ".tree-diff.txt")
          headerIncludePath = mkHeaderIncludePath name
      ediffGolden1 goldenTestSteps "treediff" target $ \report ->
        snd <$> Pipeline.parseCHeader (mkOpts report) headerIncludePath

    goldenHs :: TestName -> TestTree
    goldenHs name = do
      let target = "fixtures" </> (name ++ ".hs")
          headerIncludePath = mkHeaderIncludePath name
      ediffGolden1 goldenTestSteps "hs" target $ \report ->
        Pipeline.translateCHeader (mkOpts report) headerIncludePath

    goldenExtensions :: TestName -> TestTree
    goldenExtensions name = do
      let target = "fixtures" </> (name ++ ".exts.txt")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "exts" target $ \report -> do
        decls <- Pipeline.translateCHeader (mkOpts report) headerIncludePath
        return $ unlines $ map show $ List.sort $ toList $
              Pipeline.genExtensions
            $ Pipeline.genSHsDecls decls

    goldenPP :: TestName -> TestTree
    goldenPP name = do
      let target = "fixtures" </> (name ++ ".pp.hs")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "pp" target $ \report -> do
        decls <- Pipeline.translateCHeader (mkOpts report) headerIncludePath

        -- TODO: PP.render should add trailing '\n' itself.
        return $ Pipeline.preprocessPure ppOpts decls ++ "\n"

    goldenExtBindings :: TestName -> TestTree
    goldenExtBindings name = do
      let target = "fixtures" </> (name ++ ".extbindings.yaml")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "extbindings" target $ \report -> do
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
    failing name = do
      let target = "fixtures" </> (name ++ ".failure.txt")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ name target $ \report -> do
        result <- try $ do
          decls <- Pipeline.translateCHeader (mkOpts report) headerIncludePath

          -- TODO: PP.render should add trailing '\n' itself.
          evaluate $ force $ Pipeline.preprocessPure ppOpts decls ++ "\n"

        case result of
          Right result' -> fail $
            "Expected failure; unexpected success\n" ++ result'
          Left exc -> return $ normalise $
            displayException (exc :: HsBindgenException)

-- | Normalise the test fixture output so it doesn't change that often, nor across various systems.
normalise :: String -> String
normalise s =
    R.replace pkgname $
    R.replace rowcol $
    map windows s
  where
    windows :: Char -> Char
    windows '\\' = '/'
    windows c    = c

    rowcol :: R.RE Char String
    rowcol = sequenceA [ R.sym ':', digits, R.sym ':', digits ]
      where
        digits = '0' <$ R.few (R.digit @Int)

    pkgname :: R.RE Char String
    pkgname = concat <$> sequenceA [ "hs-bindgen-", "0" <$ R.few R.anySym, "-inplace" ]
