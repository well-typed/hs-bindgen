-- | Guard test for the runtime-module name table
-- ("HsBindgen.Backend.Runtime").
--
-- The explicit imports below pin every runtime module the table names. Renaming
-- (or removing) one of these modules without updating the table breaks the
-- build here, rather than silently emitting a bad import into generated code.
--
-- For most modules the table derives the path via Template Haskell from a name
-- defined in the module, so it cannot go stale. The re-export-only modules
-- (support, compat @HasField@, @LibC@) carry an explicit path in the table; the
-- assertions below check those paths against the modules imported here.
module Test.HsBindgen.Unit.Runtime (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.Runtime.LibC ()
import HsBindgen.Runtime.Support ()
import HsBindgen.Runtime.Support.CompatHasField ()

import HsBindgen.Backend.Runtime qualified as Runtime
import HsBindgen.Language.Haskell qualified as Hs

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.Runtime" [
      testCase "every module has a well-formed path" $
        mapM_ assertWellFormed [minBound .. maxBound]
    , testCase "re-export-only module paths" $ do
        Runtime.moduleName Runtime.Support
          @?= "HsBindgen.Runtime.Support"
        Runtime.moduleName Runtime.CompatHasField
          @?= "HsBindgen.Runtime.Support.CompatHasField"
        Runtime.moduleName Runtime.LibC
          @?= "HsBindgen.Runtime.LibC"
    ]
  where
    -- A path is well-formed if it is a non-empty, dot-separated list of
    -- non-empty components, each starting with an upper-case letter.
    assertWellFormed :: Runtime.RuntimeModule -> IO ()
    assertWellFormed rm =
        isWellFormed (Hs.moduleNameToString (Runtime.moduleName rm)) @?= True
      where
        isWellFormed = all validComponent . splitOnDots
        splitOnDots s = case break (== '.') s of
          (c, '.' : rest) -> c : splitOnDots rest
          (c, _)          -> [c]
        validComponent []      = False
        validComponent (c : _) = c `elem` ['A' .. 'Z']
