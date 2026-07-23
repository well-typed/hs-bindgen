-- | Golden test: generated binding specifications
module Test.HsBindgen.Golden.Infra.Check.BindingSpec (check) where

import Data.ByteString.UTF8 qualified as UTF8
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree)

import HsBindgen
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen qualified as BindingSpec

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
      -- We compute the binding specification inside the artefact so that the
      -- result does not mention the macro-language tag @l@ (which appears in
      -- 'getDeclIndex' and 'HsDecls'); this keeps 'runTestHsBindgenSuccess'
      -- polymorphic over the macro language.
      output <-
        runTestHsBindgenSuccess report getTestResources test $ do
          includeGraph   <- getIncludeGraph
          declIndex      <- getDeclIndex
          getMainHeaders <- getGetMainHeaders
          omitTypes      <- getOmittedTypes
          squashedTypes  <- getSquashedTypes
          hsDecls        <- HsDecls
          pure $ UTF8.toString $
            BindingSpec.genBindingSpec
              BindingSpec.FormatYAML
              "Example"
              includeGraph
              declIndex
              getMainHeaders
              omitTypes
              squashedTypes
              (concat hsDecls)
      return $ ActualValue output
  where
    fixture :: FilePath
    fixture = test.outputDir </> "bindingspec" <.> "yaml"
