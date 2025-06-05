{-# LANGUAGE CPP #-}
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (displayException, evaluate, try)
import Control.Tracer (Tracer, nullTracer)
import Data.Bool (bool)
import Data.ByteString.UTF8 qualified as UTF8
import Data.List qualified as List
import Data.TreeDiff.Golden (ediffGolden1)
import System.Console.ANSI (hSupportsANSIColor)
import System.IO (stdout)
import Test.Tasty (TestName, TestTree, defaultIngredients, defaultMain,
                   testGroup)
import Test.Tasty.Ingredients.ConsoleReporter (UseColor (..))
import Test.Tasty.Options (lookupOption)
import Test.Tasty.Runners (parseOptions)
import Text.Regex.Applicative qualified as R
import Text.Regex.Applicative.Common qualified as R

import Clang.Paths
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Lib
import HsBindgen.Pipeline qualified as Pipeline

import Test.HsBindgen.C.Parser qualified
import Test.HsBindgen.Clang.Args qualified
import Test.HsBindgen.Util.Tracer qualified
import Test.Internal.Misc
import Test.Internal.Rust
import Test.Internal.TastyGolden (goldenTestSteps)
import Test.Internal.Trace (degradeKnownTraces)
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
    ansiColor <- getAnsiColor
    _ <- withTracerCustom ansiColor defaultTracerConf degradeKnownTraces putStrLn $
           \tracer -> defaultMain $ withRustBindgen $ tests ansiColor tracer packageRoot
    pure ()

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: AnsiColor -> (Tracer IO (TraceWithCallStack Trace)) -> FilePath -> IO FilePath -> TestTree
tests ansiColor tracer packageRoot rustBindgen =
  testGroup "test-internal" [
      Test.HsBindgen.C.Parser.tests tracer args
    , Test.HsBindgen.Clang.Args.tests tracer
    , Test.HsBindgen.Util.Tracer.tests
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
        , golden "macro_in_fundecl_vs_typedef"
        , golden "macro_types"
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
        , golden "type_naturals"
        , golden "bool"
        , golden "anonymous"
        , golden "simple_func"
        , golden "weird01"
        , golden "bitfields"
        , golden "unions"
        , golden "nested_enums"
        , golden "nested_unions"
        , golden "adios"
        , golden "manual_examples"
        , golden "names"
        , golden "attributes"
        , golden "vector"
        , golden "struct_arg"
        ]
    , testGroup "failing-examples" [
          failing "long_double"
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
        withOpts report $ \opts ->
          Pipeline.parseCHeader opts headerIncludePath

    goldenHs :: TestName -> TestTree
    goldenHs name = do
      let target = "fixtures" </> (name ++ ".hs")
          headerIncludePath = mkHeaderIncludePath name
      ediffGolden1 goldenTestSteps "hs" target $ \report ->
        withOpts report $ \opts ->
          Pipeline.translateCHeader "testmodule" opts headerIncludePath

    goldenExtensions :: TestName -> TestTree
    goldenExtensions name = do
      let target = "fixtures" </> (name ++ ".exts.txt")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "exts" target $ \report ->
        withOpts report $ \opts -> do
          decls <- Pipeline.translateCHeader "testmodule" opts headerIncludePath
          return $ unlines $ map show $ List.sort $ toList $
                Pipeline.genExtensions
              $ Pipeline.genSHsDecls decls

    goldenPP :: TestName -> TestTree
    goldenPP name = do
      let target = "fixtures" </> (name ++ ".pp.hs")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "pp" target $ \report ->
        withOpts report $ \opts -> do
          decls <- Pipeline.translateCHeader "testmodule" opts headerIncludePath

          -- TODO: PP.render should add trailing '\n' itself.
          return $ Pipeline.preprocessPure ppOpts decls ++ "\n"

    goldenExtBindings :: TestName -> TestTree
    goldenExtBindings name = do
      let target = "fixtures" </> (name ++ ".extbindings.yaml")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "extbindings" target $ \report ->
        withOpts report $ \opts -> do
          decls <- Pipeline.translateCHeader "testmodule" opts headerIncludePath
          return . UTF8.toString . BindingSpec.encodeYaml $
            BindingSpec.genBindingSpec
              headerIncludePath
              (Hs.HsModuleName "Example")
              decls

    -- -<.> does weird stuff for filenames with multiple dots;
    -- I usually simply avoid using it.
    mkHeaderIncludePath :: String -> CHeaderIncludePath
    mkHeaderIncludePath = CHeaderQuoteIncludePath . (++ ".h")

    withOpts :: (String -> IO ()) -> (Pipeline.Opts -> IO a) -> IO a
    withOpts report action = fst <$>
      let tracerConf = defaultTracerConf { tVerbosity = Verbosity Warning } in
      withTracerCustom ansiColor tracerConf degradeKnownTraces report $
        \tracer' -> action $ Pipeline.defaultOpts {
            Pipeline.optsClangArgs = clangArgs packageRoot
          , Pipeline.optsTracer = tracer'
          }

    ppOpts :: Pipeline.PPOpts
    ppOpts = Pipeline.defaultPPOpts {
        Pipeline.ppOptsModule = HsModuleOpts { hsModuleOptsName = "Example" }
      }

    failing :: TestName -> TestTree
    failing name = do
      let target = "fixtures" </> (name ++ ".failure.txt")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ name target $ \report ->
        withOpts report $ \opts -> do
          result <- try $ do
            decls <- Pipeline.translateCHeader "testmodule" opts headerIncludePath

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

-- | Determine ANSI color.
--
-- Tricky, because we need to have access to the options of Tasty and query the
-- `stdout` handle.
getAnsiColor :: IO AnsiColor
getAnsiColor = do
    supportsAnsiColor <- hSupportsANSIColor stdout
    opts <- parseOptions defaultIngredients fakeTestTree
    let useColor :: UseColor = lookupOption opts
    pure $ case useColor of
          Never -> DisableAnsiColor
          Always -> EnableAnsiColor
          Auto -> bool DisableAnsiColor EnableAnsiColor supportsAnsiColor
    where
      -- Build a fake 'TestTree' to parse options.
      fakeTestTree :: TestTree
      fakeTestTree = tests EnableAnsiColor nullTracer "" (pure "")
