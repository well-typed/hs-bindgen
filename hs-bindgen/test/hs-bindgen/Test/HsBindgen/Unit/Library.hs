-- | Unit tests of the names of a library's modules and of the spellings of its
-- whole translation
module Test.HsBindgen.Unit.Library (tests) where

import System.FilePath (splitDirectories)
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Paths (RealPath (..))

import HsBindgen.Frontend.Pass.MangleNames (splitQualifiedTypeName)
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Library (fileClashes, moduleNameClashes, moduleNameOf)

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.Library" [
      testGroup "moduleNameOf" [
          testCase path $
            moduleNameOf "Lib" (splitDirectories path) @?= expected
        | (path, expected) <- [
              ("types.h"      , Just "Lib.Types")
            , ("mylib/types.h", Just "Lib.Mylib.Types")
            , ("foo-bar.h"    , Just "Lib.Foo_bar")
            , ("foo.inc.h"    , Just "Lib.Foo_inc")
            , ("3d.h"         , Just "Lib.C3D")
            , ("_x.h"         , Just "Lib.C_X")
            , ("noext"        , Just "Lib.Noext")
            , (".h"           , Nothing)
            ]
        ]
    , testGroup "clashes ignore case" [
          testCase "module names" $
            moduleNameClashes [
                ("Lib.Foo", RealPath "/lib/foo.h")
              , ("Lib.Bar", RealPath "/lib/bar.h")
              , ("Lib.FOO", RealPath "/lib/FOO.h")
              ]
              @?= [ ("Lib.Foo", RealPath "/lib/foo.h")
                      :| [("Lib.FOO", RealPath "/lib/FOO.h")]
                  ]
        , testCase "files" $
            fileClashes [
                ("Lib.Foo"     , ["out/Lib/Foo/Safe.hs"])
              , ("Lib.Foo.SAFE", ["out/Lib/Foo/SAFE.hs"])
              ]
              @?= [ ("Lib.Foo", "out/Lib/Foo/Safe.hs")
                      :| [("Lib.Foo.SAFE", "out/Lib/Foo/SAFE.hs")]
                  ]
        ]
    , testGroup "splitQualifiedTypeName" [
          testCase "qualified" $
            splitQualifiedTypeName (Hs.UnsafeName "Lib.Geo.Point.Point")
              @?= Just ("Lib.Geo.Point", Hs.UnsafeName "Point")
        , testCase "plain" $
            splitQualifiedTypeName (Hs.UnsafeName "Point") @?= Nothing
        ]
    ]
