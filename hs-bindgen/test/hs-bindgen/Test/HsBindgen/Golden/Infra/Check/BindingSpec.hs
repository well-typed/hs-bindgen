-- | Golden test: generated binding specifications
module Test.HsBindgen.Golden.Infra.Check.BindingSpec (
    check
  , checkValue
  ) where

import Data.ByteString.UTF8 qualified as UTF8
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, askOption)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen
import HsBindgen.ArtefactM (DirPolicy (..), FilePolicy (..))
import HsBindgen.BindingSpec (Prescriptive (..))
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.Boot (ClangArtefacts (..), getClangArtefacts)
import HsBindgen.Config.Internal (BootConfig (..))
import HsBindgen.Util.Tracer (nullTracer)

import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check getTestResources test =
    goldenAnsiDiff "bindingspec" fixture $ \report -> do
      -- We encode the binding specification inside the artefact so that the
      -- result does not mention the macro-language tag @l@ (which appears in
      -- 'getDeclIndex'); this keeps 'runTestHsBindgenSuccess' polymorphic over
      -- the macro language.
      output <-
        runTestHsBindgenSuccess report getTestResources test $ do
          includeGraph <- getIncludeGraph
          declIndex    <- getDeclIndex
          spec         <- generatedBindingSpec
          pure $ UTF8.toString $
            BindingSpec.encodeBindingSpec
              BindingSpec.FormatYAML
              includeGraph
              declIndex
              spec
      return $ ActualValue output
  where
    fixture :: FilePath
    fixture = test.outputDir </> "bindingspec" <.> "yaml"

-- | The generated binding specification is the one that its file loads as
--
-- A module can then be given the specification of another module of the same
-- translation unit as a value, and it gets what it would get from the file.
checkValue :: IO TestResources -> TestCase -> TestTree
checkValue getTestResources test =
    askOption $ \(Debug debug) -> do
      let report :: String -> IO ()
          report = case debug of
            False -> const $ pure ()
            True  -> putStrLn
      testCase "bindingspec-value" $
        withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
          let path = tmpDir </> "bindingspec" <.> "yaml"
          spec <-
            runTestHsBindgenSuccess report getTestResources test $
                 generatedBindingSpec
              <* writeBindingSpec DoNotOverwriteFiles DoNotCreateOutputDirs path
          bootConfig <- getTestBootConfig test <$> getTestResources
          clang      <- getClangArtefacts nullTracer bootConfig.clangArgs
          loaded     <-
            BindingSpec.loadPrescriptiveBindingSpec
              nullTracer
              clang.clangArgs
              (BindingSpec.moduleName spec)
              BindingSpec.BindingSpecStrict
              (Just path)
          loaded @?= Prescriptive{
              declared = Just (BindingSpec.moduleName spec)
            , spec     = spec
            }
