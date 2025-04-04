module Test.HsBindgen.ExtBindings (tests) where

import Control.Exception (throwIO)
import System.FilePath ((</>))
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty)

import HsBindgen.ExtBindings

import Test.Internal.QuickCheck.Orphans ()
import Test.Internal.Tasty

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.ExtBindings" [
      withTmpDir $ \getTmpDir -> testGroup "configuration"  [
          testProperty "writeLoadJsonEq" $ prop_writeLoadJsonEq getTmpDir
        , testProperty "writeLoadYamlEq" $ prop_writeLoadYamlEq getTmpDir
        ]
    ]

prop_writeLoadJsonEq :: IO FilePath -> UnresolvedExtBindings -> Property
prop_writeLoadJsonEq getTmpDir bindings = QCM.monadicIO $ do
    path <- (</> "writeLoadJsonEq.json") <$> QCM.run getTmpDir
    bindings' <- QCM.run $ do
      either throwIO return =<< writeUnresolvedExtBindings path bindings
      either throwIO return =<< loadUnresolvedExtBindings path
    QCM.assert $ bindings' == bindings

prop_writeLoadYamlEq :: IO FilePath -> UnresolvedExtBindings -> Property
prop_writeLoadYamlEq getTmpDir bindings = QCM.monadicIO $ do
    path <- (</> "writeLoadYamlEq.yaml") <$> QCM.run getTmpDir
    bindings' <- QCM.run $ do
      either throwIO return =<< writeUnresolvedExtBindings path bindings
      either throwIO return =<< loadUnresolvedExtBindings path
    QCM.assert $ bindings' == bindings
