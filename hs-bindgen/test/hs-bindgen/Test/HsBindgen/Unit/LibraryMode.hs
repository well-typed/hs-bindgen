module Test.HsBindgen.Unit.LibraryMode (tests) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text qualified as Text
import System.FilePath ((</>))
import System.Info (os)
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Paths (RealPath (..))

import HsBindgen.Backend.Category (Category (..), TermCategory (..),
                                   allCategories)
import HsBindgen.Config.Prelims (BaseModuleName (..))
import HsBindgen.Frontend.Predicate (matchTest, quoteRegex)
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.LibraryMode.Plan (Collision (..), LibraryUnit (..),
                                   categoryOverlaps, deriveModuleName,
                                   directCollisions)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.LibraryMode" [
      testGroup "collisions" [
          testNoCollisions
        , testDirectCollision
        , testCategoryOverlap
        , testCategoryOverlapPerCategory
        , testCategoryOverlapNeedsTypes
        ]
    , testDeriveModuleName
    , testQuoteRegex
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

-- | A unit of one header that writes its base module and every category
-- submodule
single :: FilePath -> BaseModuleName -> LibraryUnit
single hdr m = LibraryUnit {
      headers         = rp hdr :| []
    , moduleName      = m
    , categories      = Set.fromList (toList allCategories)
    , forwardTypedefs = Set.empty
    }

-- | The same unit when its header has nothing but declarations of the given
-- categories
only :: [Category] -> LibraryUnit -> LibraryUnit
only cs unit = unit { categories = Set.fromList cs }

{-------------------------------------------------------------------------------
  Collisions
-------------------------------------------------------------------------------}

testNoCollisions :: TestTree
testNoCollisions = testCase "distinct modules" $
    directCollisions units ++ categoryOverlaps units @?= []
  where
    units :: [LibraryUnit]
    units = [
        single "inc/foo.h"     (BaseModuleName "M.Foo")
      , single "inc/bar.h"     (BaseModuleName "M.Bar")
      , single "inc/baz/qux.h" (BaseModuleName "M.Baz.Qux")
      ]

testDirectCollision :: TestTree
testDirectCollision = testCase "same module name from different headers" $
    directCollisions [
        single "inc/foo.h" (BaseModuleName "M.Foo")
      , single "inc/Foo.h" (BaseModuleName "M.Foo")
      ]
    @?= [
        DirectCollision (BaseModuleName "M.Foo") [
            rp "inc/foo.h" :| []
          , rp "inc/Foo.h" :| []
          ]
      ]

testCategoryOverlap :: TestTree
testCategoryOverlap = testCase "base module vs category submodule" $
    categoryOverlaps [
        single "inc/foo.h"      (BaseModuleName "M.Foo")
      , single "inc/foo/safe.h" (BaseModuleName "M.Foo.Safe")
      ]
    @?= [
        CategoryOverlap (Hs.ModuleName "M.Foo.Safe")
          (rp "inc/foo.h" :| []) CSafe (rp "inc/foo/safe.h" :| [])
      ]

testCategoryOverlapPerCategory :: TestTree
testCategoryOverlapPerCategory =
    testCase "only the category submodules a unit writes can overlap" $
      categoryOverlaps [
          only [CTerm CGlobal] $ single "inc/foo.h" (BaseModuleName "M.Foo")
        , single "inc/foo/safe.h"   (BaseModuleName "M.Foo.Safe")
        , single "inc/foo/global.h" (BaseModuleName "M.Foo.Global")
        ]
      @?= [
          CategoryOverlap (Hs.ModuleName "M.Foo.Global")
            (rp "inc/foo.h" :| []) CGlobal (rp "inc/foo/global.h" :| [])
        ]

-- | A header with nothing but functions writes no base module, so its name
-- can be the category submodule of another unit
testCategoryOverlapNeedsTypes :: TestTree
testCategoryOverlapNeedsTypes =
    testCase "no overlap with a unit that writes no base module" $
      categoryOverlaps [
          single "inc/foo.h" (BaseModuleName "M.Foo")
        , only [CTerm CSafe, CTerm CUnsafe, CTerm CFunPtr] $
            single "inc/foo/safe.h" (BaseModuleName "M.Foo.Safe")
        ]
      @?= []

{-------------------------------------------------------------------------------
  deriveModuleName
-------------------------------------------------------------------------------}

-- | A header can have a name that a module cannot
testDeriveModuleName :: TestTree
testDeriveModuleName = testCase "every header name gives a valid module name" $ do
    map (name [inc]) ["glib-object.h", "3d.h", "foo.bar.h", "sub-dir/x.h"]
      @?= ["M.Glib_object", "M.C3D", "M.Foo_bar", "M.Sub_dir.X"]
    -- A library directory inside another one does not change the name,
    -- whichever is given first
    map (\roots -> name roots "sub/x.h") [[inc, inc </> "sub"], [inc </> "sub", inc]]
      @?= ["M.Sub.X", "M.Sub.X"]
  where
    inc :: FilePath
    inc = absRoot </> "inc"

    name :: [FilePath] -> FilePath -> BaseModuleName
    name roots hdr =
      deriveModuleName roots (BaseModuleName "M") (rp ("inc" </> hdr))

{-------------------------------------------------------------------------------
  quoteRegex
-------------------------------------------------------------------------------}

-- | Library mode matches header paths through 'quoteRegex'. On Windows a path
-- has @\\E@ in it wherever a directory name starts with @E@.
testQuoteRegex :: TestTree
testQuoteRegex = testCase "quoteRegex matches a path and nothing else" $ do
    assertBool "expected the path to match itself" $
      all (\path -> path `matches` path) paths
    assertBool "expected the dot to match only a dot" $
      not ("lib/a.h" `matches` "lib/axh")
  where
    paths :: [String]
    paths = ["lib/a.h", "C:\\Users\\Eric\\lib\\a.h", "C:\\End\\"]

    matches :: String -> String -> Bool
    matches path = matchTest (fromString ("^" ++ quoteRegex path ++ "$")) . Text.pack
