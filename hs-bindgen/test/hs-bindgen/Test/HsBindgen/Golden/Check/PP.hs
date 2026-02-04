-- | Golden test: pretty-printed Haskell code
module Test.HsBindgen.Golden.Check.PP (check) where

import Data.Maybe (catMaybes)
import Optics.Core (view)
import System.FilePath
import Test.Tasty

import HsBindgen (getBindingsMultiple)
import HsBindgen hiding (getBindingsMultiple)
import HsBindgen.Backend.Category
import HsBindgen.Config (BackendConfig (..))
import HsBindgen.Config.Prelims
import HsBindgen.Errors (panicPure)
import HsBindgen.Language.Haskell qualified as Hs

import Test.Common.Util.FSTree
import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden (ActualValue (..))
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenDirAnsiDiff "pp" fixture $ \report -> do
      let backendCfg = getTestBackendConfig test
          mrc = ModuleRenderConfig {
              qualifiedStyle = view #qualifiedStyle backendCfg
            }
          artefacts = (,)
                    <$> FinalModuleBaseName
                    <*> getBindingsMultiple (view #fieldNamingStrategy backendCfg) mrc
      (baseName, output)
        <- runTestHsBindgenSuccess report testResources test artefacts
      pure $ renderFSTree baseName output
  where
    fixture :: FilePath
    fixture = test.outputDir </> "pp"

    getOutput ::
         Category
      -> BaseModuleName
      -> ByCategory_ (Maybe String)
      -> Maybe (FilePath, String)
    getOutput c base output = do
        ppOutput <- view (lensForCategory c) output
        pure (Hs.moduleNamePath moduleName , ppOutput)
      where
        moduleName :: Hs.ModuleName
        moduleName = fromBaseModuleName base (Just c)

    renderFSTree ::
         BaseModuleName
      -> ByCategory_ (Maybe String)
      -> ActualValue (Shortcut String)
    renderFSTree baseName output =
        case ts of
          [] -> ActualNoOutput
          _ -> ActualValue $ Shortcut {
                    shortcut = fixture
                  , tree = either panicPure id (merges1 ts)
                  }
      where
        ts = catMaybes [
                  uncurry fromFile <$> getOutput c baseName output
                | c <- allCategories
                ]
