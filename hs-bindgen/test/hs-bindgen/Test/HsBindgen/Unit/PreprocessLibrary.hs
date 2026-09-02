module Test.HsBindgen.Unit.PreprocessLibrary (tests) where

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
  deriveModuleName
-------------------------------------------------------------------------------}

testDeriveSimple :: TestTree
testDeriveSimple = testCase "single component" $
    deriveModuleName ["/usr/include"] (BaseModuleName "Widget") (SourcePath "/usr/include/core.h")
      @?= BaseModuleName "Widget.Core"

testDeriveNested :: TestTree
testDeriveNested = testCase "nested path" $
    deriveModuleName ["/usr/include"] (BaseModuleName "Widget") (SourcePath "/usr/include/widget/core.h")
      @?= BaseModuleName "Widget.Widget.Core"

testDeriveNoMatchingRoot :: TestTree
testDeriveNoMatchingRoot = testCase "no matching root falls back to full path" $
    deriveModuleName ["/opt/other"] (BaseModuleName "Lib") (SourcePath "/usr/include/foo.h")
      @?= BaseModuleName "Lib./.Usr.Include.Foo"

testDeriveMultipleRoots :: TestTree
testDeriveMultipleRoots = testCase "picks the first matching root" $
    deriveModuleName
      ["/usr/include", "/usr/include/widget"]
      (BaseModuleName "W")
      (SourcePath "/usr/include/widget/core.h")
      @?= BaseModuleName "W.Widget.Core"

{-------------------------------------------------------------------------------
  isUnderDir
-------------------------------------------------------------------------------}

testIsUnderDirBasic :: TestTree
testIsUnderDirBasic = testCase "nested path is under directory" $
    assertBool "expected True" $
      "/usr/include/widget/core.h" `isUnderDir` "/usr/include"

testIsUnderDirNotUnder :: TestTree
testIsUnderDirNotUnder = testCase "disjoint path is not under directory" $
    assertBool "expected False" $
      not $ "/opt/lib/core.h" `isUnderDir` "/usr/include"

testIsUnderDirExact :: TestTree
testIsUnderDirExact = testCase "exact match counts as under" $
    assertBool "expected True" $
      "/usr/include" `isUnderDir` "/usr/include"

testIsUnderDirDifferentPrefix :: TestTree
testIsUnderDirDifferentPrefix = testCase "partial prefix does not match" $
    assertBool "expected False" $
      not $ "/usr/include-extra/core.h" `isUnderDir` "/usr/include"

{-------------------------------------------------------------------------------
  moduleToPath
-------------------------------------------------------------------------------}

testModuleToPathSimple :: TestTree
testModuleToPathSimple = testCase "simple module" $
    moduleToPath (BaseModuleName "Widget") @?= "Widget"

testModuleToPathNested :: TestTree
testModuleToPathNested = testCase "dotted module" $
    moduleToPath (BaseModuleName "Widget.Core.Types") @?= "Widget/Core/Types"
