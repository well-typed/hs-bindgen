module Test.HsBindgen.Unit.PreprocessLibrary (tests) where

import Data.Text qualified as Text
import System.FilePath ((</>))
import System.Info (os)
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Paths (RealPath (..))

import HsBindgen.Config.Prelims (BaseModuleName (..))
import HsBindgen.PreprocessLibrary.Naming (Collision (..),
                                           LibraryHeaderResult (..),
                                           deriveModuleName, detectCollisions,
                                           filterByLibraryRoot, isUnderDir,
                                           moduleToPath)

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
    , testGroup "filterByLibraryRoot" [
          testFilterBasic
        , testFilterExclude
        , testFilterNoMatchingRoot
        ]
    , testGroup "detectCollisions" [
          testNoCollisions
        , testDirectCollision
        , testDotSlashEquivalence
        , testCategoryOverlap
        , testCategoryOverlapDisabledInSingleFile
        , testNoCollisionDistinctModules
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

rp :: FilePath -> RealPath
rp = RealPath . Text.pack . mkAbs

{-------------------------------------------------------------------------------
  deriveModuleName
-------------------------------------------------------------------------------}

testDeriveSimple :: TestTree
testDeriveSimple = testCase "single component" $
    deriveModuleName [mkAbs "usr/include"] (BaseModuleName "Widget") (rp "usr/include/core.h")
      @?= BaseModuleName "Widget.Core"

testDeriveNested :: TestTree
testDeriveNested = testCase "nested path" $
    deriveModuleName [mkAbs "usr/include"] (BaseModuleName "Widget") (rp "usr/include/widget/core.h")
      @?= BaseModuleName "Widget.Widget.Core"

testDeriveNoMatchingRoot :: TestTree
testDeriveNoMatchingRoot = testCase "no matching root falls back to full path" $
    deriveModuleName [mkAbs "opt/other"] (BaseModuleName "Lib") (rp "usr/include/foo.h")
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
      (rp "usr/include/widget/core.h")
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

{-------------------------------------------------------------------------------
  filterByLibraryRoot
-------------------------------------------------------------------------------}

testFilterBasic :: TestTree
testFilterBasic = testCase "headers under root are included" $
    let result = filterByLibraryRoot
          [mkAbs "inc"] [] (map rp ["inc/foo.h", "inc/bar.h", "sys/stdio.h"])
    in do
      result.included    @?= map rp ["inc/foo.h", "inc/bar.h"]
      result.outsideRoots @?= [rp "sys/stdio.h"]
      result.excluded    @?= []

testFilterExclude :: TestTree
testFilterExclude = testCase "excluded patterns remove from included set" $
    let result = filterByLibraryRoot
          [mkAbs "inc"] ["internal"] (map rp ["inc/foo.h", "inc/internal.h", "inc/bar.h"])
    in do
      result.included @?= map rp ["inc/foo.h", "inc/bar.h"]
      result.excluded @?= [rp "inc/internal.h"]

testFilterNoMatchingRoot :: TestTree
testFilterNoMatchingRoot = testCase "no matching root puts all in outsideRoots" $
    let result = filterByLibraryRoot
          [mkAbs "other"] [] (map rp ["inc/foo.h", "inc/bar.h"])
    in do
      result.included @?= []
      length result.outsideRoots @?= 2

{-------------------------------------------------------------------------------
  detectCollisions
-------------------------------------------------------------------------------}

testNoCollisions :: TestTree
testNoCollisions = testCase "no collisions for distinct modules" $
    detectCollisions True
      [ (rp "inc/foo.h",     BaseModuleName "M.Foo")
      , (rp "inc/bar.h",     BaseModuleName "M.Bar")
      , (rp "inc/baz/qux.h", BaseModuleName "M.Baz.Qux")
      ]
    @?= []

testDirectCollision :: TestTree
testDirectCollision = testCase "case-folded headers collide" $
    let result = detectCollisions True
          [ (rp "inc/foo.h", BaseModuleName "M.Foo")
          , (rp "inc/Foo.h", BaseModuleName "M.Foo")
          ]
    in assertBool "expected DirectCollision" $ case result of
         [DirectCollision "M.Foo" _] -> True
         _                           -> False

testDotSlashEquivalence :: TestTree
testDotSlashEquivalence =
    testCase "dot in filename collides with directory separator" $
      let roots = [mkAbs "inc"]
          base  = BaseModuleName "M"
          hDot  = rp "inc/Widget.Core.h"
          hDir  = rp "inc/widget/core.h"
          mDot  = deriveModuleName roots base hDot
          mDir  = deriveModuleName roots base hDir
      in do
        mDot @?= mDir
        let result = detectCollisions False [(hDot, mDot), (hDir, mDir)]
        assertBool ("expected DirectCollision, got: " ++ show result) $
          case result of
            [DirectCollision _ _] -> True
            _                     -> False

testCategoryOverlap :: TestTree
testCategoryOverlap = testCase "foo.h Types vs foo/safe.h Safe category" $
    let result = detectCollisions True
          [ (rp "inc/foo.h",      BaseModuleName "M.Foo")
          , (rp "inc/foo/safe.h", BaseModuleName "M.Foo.Safe")
          ]
    in assertBool ("expected CategoryOverlap, got: " ++ show result) $
         case result of
           [CategoryOverlap "M.Foo.Safe" _ _ _ _] -> True
           _                                      -> False

testCategoryOverlapDisabledInSingleFile :: TestTree
testCategoryOverlapDisabledInSingleFile =
    testCase "category overlap not checked in single-file mode" $
      detectCollisions False
        [ (rp "inc/foo.h",      BaseModuleName "M.Foo")
        , (rp "inc/foo/safe.h", BaseModuleName "M.Foo.Safe")
        ]
      @?= []

testNoCollisionDistinctModules :: TestTree
testNoCollisionDistinctModules =
    testCase "non-overlapping category suffixes are fine" $
      detectCollisions True
        [ (rp "inc/foo.h",       BaseModuleName "M.Foo")
        , (rp "inc/foo/types.h", BaseModuleName "M.Foo.Types2")
        ]
      @?= []
