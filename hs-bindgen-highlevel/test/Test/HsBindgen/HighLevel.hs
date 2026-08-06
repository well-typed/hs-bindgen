-- | The @ToHighLevel@ combinator test suite, grouped by concern.
module Test.HsBindgen.HighLevel (tests) where

import Test.Tasty (TestTree, testGroup)

import Test.HsBindgen.HighLevel.Auto qualified as Auto
import Test.HsBindgen.HighLevel.Closers qualified as Closers
import Test.HsBindgen.HighLevel.Errors qualified as Errors
import Test.HsBindgen.HighLevel.Inputs qualified as Inputs
import Test.HsBindgen.HighLevel.Outputs qualified as Outputs
import Test.HsBindgen.HighLevel.Scratch qualified as Scratch
import Test.HsBindgen.HighLevel.Struct qualified as Struct
import Test.HsBindgen.HighLevel.Unlifted qualified as Unlifted

tests :: TestTree
tests = testGroup "HighLevel"
    [ Inputs.tests
    , Scratch.tests
    , Outputs.tests
    , Auto.tests
    , Errors.tests
    , Closers.tests
    , Unlifted.tests
    , Struct.tests
    ]
