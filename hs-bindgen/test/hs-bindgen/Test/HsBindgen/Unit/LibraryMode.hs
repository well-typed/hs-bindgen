module Test.HsBindgen.Unit.LibraryMode (tests) where

import Data.Text qualified as Text
import System.FilePath ((</>))
import System.Info (os)
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Paths (RealPath (..))

import HsBindgen.Config.Prelims (BaseModuleName (..))
import HsBindgen.LibraryMode.Plan (deriveModuleName)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.LibraryMode" [
      testDeriveModuleName
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
