module Test.HsBindgen.Unit.PreprocessLibrary (tests) where

import Data.Text qualified as Text
import System.FilePath ((</>))
import System.Info (os)
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Paths

import HsBindgen.Config.Prelims (BaseModuleName (..))
import HsBindgen.PreprocessLibrary.Plan

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.PreprocessLibrary" [
      testGroup "deriveModuleName" [
          testDeriveSimple
        , testDeriveNested
        , testDeriveNoMatchingRoot
        , testDeriveMultipleRoots
        ]
    , testGroup "isUnderDir" [
          testIsUnderDirBasic
        , testIsUnderDirNotUnder
        , testIsUnderDirExact
        , testIsUnderDirDifferentPrefix
        ]
    , testGroup "moduleToPath" [
          testModuleToPathSimple
        , testModuleToPathNested
        ]
    ]

{-------------------------------------------------------------------------------
  Absolute path helper

  On Windows, paths without a drive letter (e.g. /usr/include) are treated
  as relative by System.FilePath, which breaks makeRelative/isRelative.
  This helper prepends a drive letter on Windows so that tests exercise
  the same absolute-path logic on all platforms.
-------------------------------------------------------------------------------}

absRoot :: FilePath
absRoot
  | os == "mingw32" = "C:\\"
  | otherwise       = "/"

mkAbs :: FilePath -> FilePath
mkAbs p = absRoot </> p

{-------------------------------------------------------------------------------
  deriveModuleName
-------------------------------------------------------------------------------}

testDeriveSimple :: TestTree
testDeriveSimple = testCase "single component" $
    deriveModuleName [mkAbs "usr/include"] (BaseModuleName "Widget") (SourcePath $ Text.pack $ mkAbs "usr/include/core.h")
      @?= BaseModuleName "Widget.Core"

testDeriveNested :: TestTree
testDeriveNested = testCase "nested path" $
    deriveModuleName [mkAbs "usr/include"] (BaseModuleName "Widget") (SourcePath $ Text.pack $ mkAbs "usr/include/widget/core.h")
      @?= BaseModuleName "Widget.Widget.Core"

testDeriveNoMatchingRoot :: TestTree
testDeriveNoMatchingRoot = testCase "no matching root falls back to full path" $
    deriveModuleName [mkAbs "opt/other"] (BaseModuleName "Lib") (SourcePath $ Text.pack $ mkAbs "usr/include/foo.h")
      @?= BaseModuleName expected
  where
    expected
      | os == "mingw32" = "Lib.C:\\.Usr.Include.Foo"
      | otherwise       = "Lib./.Usr.Include.Foo"

testDeriveMultipleRoots :: TestTree
testDeriveMultipleRoots = testCase "picks the first matching root" $
    deriveModuleName
      [mkAbs "usr/include", mkAbs "usr/include/widget"]
      (BaseModuleName "W")
      (SourcePath $ Text.pack $ mkAbs "usr/include/widget/core.h")
      @?= BaseModuleName "W.Widget.Core"

{-------------------------------------------------------------------------------
  isUnderDir
-------------------------------------------------------------------------------}

testIsUnderDirBasic :: TestTree
testIsUnderDirBasic = testCase "nested path is under directory" $
    assertBool "expected True" $
      mkAbs "usr/include/widget/core.h" `isUnderDir` mkAbs "usr/include"

testIsUnderDirNotUnder :: TestTree
testIsUnderDirNotUnder = testCase "disjoint path is not under directory" $
    assertBool "expected False" $
      not $ mkAbs "opt/lib/core.h" `isUnderDir` mkAbs "usr/include"

testIsUnderDirExact :: TestTree
testIsUnderDirExact = testCase "exact match counts as under" $
    assertBool "expected True" $
      mkAbs "usr/include" `isUnderDir` mkAbs "usr/include"

testIsUnderDirDifferentPrefix :: TestTree
testIsUnderDirDifferentPrefix = testCase "partial prefix does not match" $
    assertBool "expected False" $
      not $ mkAbs "usr/include-extra/core.h" `isUnderDir` mkAbs "usr/include"

{-------------------------------------------------------------------------------
  moduleToPath
-------------------------------------------------------------------------------}

testModuleToPathSimple :: TestTree
testModuleToPathSimple = testCase "simple module" $
    moduleToPath (BaseModuleName "Widget") @?= "Widget"

testModuleToPathNested :: TestTree
testModuleToPathNested = testCase "dotted module" $
    moduleToPath (BaseModuleName "Widget.Core.Types") @?= "Widget/Core/Types"
