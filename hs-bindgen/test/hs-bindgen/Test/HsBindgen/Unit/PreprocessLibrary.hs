module Test.HsBindgen.Unit.PreprocessLibrary (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import System.FilePath ((</>))
import System.Info (os)
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Paths (RealPath (..))

import HsBindgen.Config.Prelims (BaseModuleName (..))
import HsBindgen.PreprocessLibrary.Naming (Collision (..), LibraryUnit (..),
                                           detectCollisions)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.PreprocessLibrary" [
      testGroup "detectCollisions" [
          testNoCollisions
        , testDirectCollision
        , testCategoryOverlap
        , testCategoryOverlapDisabledInSingleFile
        ]
    ]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

absRoot :: FilePath
absRoot
  | os == "mingw32" = "C:\\"
  | otherwise       = "/"

rp :: FilePath -> RealPath
rp = RealPath . Text.pack . (absRoot </>)

single :: FilePath -> BaseModuleName -> LibraryUnit
single hdr m = LibraryUnit { headers = rp hdr :| [], moduleName = m }

{-------------------------------------------------------------------------------
  detectCollisions
-------------------------------------------------------------------------------}

testNoCollisions :: TestTree
testNoCollisions = testCase "distinct modules" $
    detectCollisions True
      [ single "inc/foo.h"     (BaseModuleName "M.Foo")
      , single "inc/bar.h"     (BaseModuleName "M.Bar")
      , single "inc/baz/qux.h" (BaseModuleName "M.Baz.Qux")
      ]
    @?= []

testDirectCollision :: TestTree
testDirectCollision = testCase "same module name from different headers" $
    let result = detectCollisions True
          [ single "inc/foo.h" (BaseModuleName "M.Foo")
          , single "inc/Foo.h" (BaseModuleName "M.Foo")
          ]
    in assertBool "expected DirectCollision" $ case result of
         [DirectCollision "M.Foo" _] -> True
         _                           -> False

testCategoryOverlap :: TestTree
testCategoryOverlap = testCase "base module vs category submodule" $
    let result = detectCollisions True
          [ single "inc/foo.h"      (BaseModuleName "M.Foo")
          , single "inc/foo/safe.h" (BaseModuleName "M.Foo.Safe")
          ]
    in assertBool ("expected CategoryOverlap, got: " ++ show result) $
         case result of
           [CategoryOverlap "M.Foo.Safe" _ _ _ _] -> True
           _                                      -> False

testCategoryOverlapDisabledInSingleFile :: TestTree
testCategoryOverlapDisabledInSingleFile =
    testCase "category overlap not checked in single-file mode" $
      detectCollisions False
        [ single "inc/foo.h"      (BaseModuleName "M.Foo")
        , single "inc/foo/safe.h" (BaseModuleName "M.Foo.Safe")
        ]
      @?= []
