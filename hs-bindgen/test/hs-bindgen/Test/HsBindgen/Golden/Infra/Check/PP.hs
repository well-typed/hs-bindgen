-- | Golden test: pretty-printed Haskell code
module Test.HsBindgen.Golden.Infra.Check.PP (check) where

import Control.Monad (when)
import Data.IORef
import Data.List.NonEmpty qualified as NonEmpty
import Optics.Core (view)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit (assertFailure)

import HsBindgen (getBindingsMultiple)
import HsBindgen hiding (getBindingsMultiple)
import HsBindgen.Backend.Category
import HsBindgen.Backend.HsModule.Translation (hasHsBoot)
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Errors (panicIO)
import HsBindgen.Language.Haskell qualified as Hs

import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- Run @hs-bindgen@ once and share the result across all golden sub-tests via
-- 'withResource'. Since 'runTestHsBindgen' returns an 'Either', the cached
-- result is always available: the primary sub-test ('NonEmpty.head
-- allCategories') reports the full error, while the others give a short
-- cross-reference. Trace messages produced during the run (formatted by
-- 'withTraceConfigPredicate') are captured in an 'IORef' so that they can be
-- replayed through each sub-test's @report@ callback.
check :: IO TestResources -> TestCase -> TestTree
check getTestResources test =
    withExampleDir $
    askOption $ \runMode ->
    let categories = case runMode of
          Fast -> filter (/= CTerm CUnsafe) (NonEmpty.toList allCategories)
          Full -> NonEmpty.toList allCategories
    in withResource setupBindgen (\_ -> pure ()) $ \getResult ->
       testGroup "pp" [
         goldenAnsiDiff (testName bc file) (fixture bc file) $ \report -> do
           result <- getResult
           let firstCategory = NonEmpty.head allCategories
           case result of
             Left e
               -- The primary sub-test reports the full error. The other
               -- sub-tests just reference it, avoiding redundant output.
               | bc == firstCategory, ModuleFile <- file ->
                 assertFailure $
                   "Expected 'hs-bindgen' to succeed, but received an error: "
                   ++ show e
               | otherwise ->
                 assertFailure $
                   "hs-bindgen failed (see "
                   ++ show firstCategory
                   ++ " for details)"
             Right (baseName, traceMessages, output) -> do
               -- Replay trace messages through the golden test's @report@
               -- callback, so that they appear in tasty's @--debug@ output.
               mapM_ report traceMessages

               -- A sanity check to make sure that that the modules we're rendering
               -- have the expected @Example@ base name.
               when (baseName /= "Example") $
                 panicIO "The module base name should be Example!"

               -- Render the Haskell module, or its boot file
               let ppOutput = view (lensForCategory bc) output >>= select file
               return $ case ppOutput of
                 Nothing -> ActualNoOutput
                 Just x  -> ActualValue x
       | (bc :: Category) <- categories
       , file <- ModuleFile : [BootFile | hasHsBoot bc]
       ]
  where
    select :: File -> (String, Maybe String) -> Maybe String
    select ModuleFile = Just . fst
    select BootFile   = snd

    testName :: Category -> File -> String
    testName bc ModuleFile = show bc
    testName bc BootFile   = show bc ++ " boot"

    setupBindgen ::
         IO (Either BindgenError (BaseModuleName, [String], ByCategory_ (Maybe (String, Maybe String))))
    setupBindgen = do
      traceMessages <- newIORef []
      let collectTrace msg = modifyIORef' traceMessages (msg :)
          mrc = ModuleRenderConfig {
              qualifiedStyle = test.qualifiedStyle
            }
      result <- runTestHsBindgen collectTrace getTestResources test
                  ((,) <$> ModuleBaseName <*> getBindingsMultiple mrc)
      msgs <- reverse <$> readIORef traceMessages
      pure $ fmap (\(baseName, output) -> (baseName, msgs, output)) result

    -- === Filepaths
    --
    -- A golden test needs to know at which filepath to store its output
    -- /before/ it actually runs the test, as it's typically static information.
    -- As such, we can't rely on the invocation of @hs-bindgen@ to generate the
    -- filepath names for us, so we redefine the logic for constructing
    -- filepaths here.

    -- | Everything except the top-level @Example.hs@ module is in the @Example@
    -- directory.
    withExampleDir :: TestTree -> TestTree
    withExampleDir k =
        withResource
          (createDirectoryIfMissing False (test.outputDir </> "Example"))
          (\_ -> pure ())
          (\_ -> k)

    -- | The names of sub-modules are based solely on the binding category
    fixture :: Category -> File -> FilePath
    fixture bc file = test.outputDir </> path moduleName
      where
        moduleName :: Hs.ModuleName
        moduleName = fromBaseModuleName "Example" (Just bc)

        path :: Hs.ModuleName -> FilePath
        path = case file of
          ModuleFile -> Hs.moduleNamePath
          BootFile   -> Hs.moduleNameBootPath

-- | The files written for the module of a category
data File =
    -- | The module itself
    ModuleFile
    -- | Its boot file
  | BootFile
