-- | Golden test: generated binding specifications
module Test.HsBindgen.Golden.Check.BindingSpec (check) where

import Data.ByteString.UTF8 qualified as UTF8
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree)

import HsBindgen
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen qualified as BindingSpec

import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenAnsiDiff "bindingspec" fixture $ \report -> do
      let artefacts =
            (,,,,,,)
              <$> Target
              <*> IncludeGraph
              <*> DeclIndex
              <*> GetMainHeaders
              <*> OmitTypes
              <*> SquashedTypes
              <*> HsDecls
      (target, (_, includeGraph), declIndex, getMainHeaders, omitTypes, squashedTypes, hsDecls) <-
        runTestHsBindgenSuccess report testResources test artefacts

      let output :: String
          output = UTF8.toString $
              BindingSpec.genBindingSpec
                BindingSpec.FormatYAML
                target
                "Example"
                includeGraph
                declIndex
                getMainHeaders
                omitTypes
                squashedTypes
                -- TODO https://github.com/well-typed/hs-bindgen/issues/1089:
                -- Test all binding categories.
                (concat hsDecls)

      return $ ActualValue output
  where
    fixture :: FilePath
    fixture = testOutputDir test </> "bindingspec" <.> "yaml"
