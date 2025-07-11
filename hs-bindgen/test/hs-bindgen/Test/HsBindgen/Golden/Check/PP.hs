-- | Golden test: pretty-printed Haskell code
module Test.HsBindgen.Golden.Check.PP (check) where

import System.FilePath ((</>))
import Test.Tasty

import HsBindgen.Pipeline qualified as Pipeline

import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Resources
import Test.HsBindgen.Golden.TestCase

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenAnsiDiff "pp" fixture $ \_report -> do
      config <- getTestConfig testResources test
      decls  <- runTestTranslate testResources test

      let output :: String
          output = Pipeline.preprocessPure config decls

      return $ ActualValue output
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".pp.hs")
