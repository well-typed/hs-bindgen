-- | Golden tests: globals
module Test.HsBindgen.Golden.Globals (testCases) where

import HsBindgen.IR.C qualified as C
import HsBindgen.TraceMsg

import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.TestCase

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = [
      test_globals_globals
    , test_globals_untagged
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_globals_globals :: TestCase
test_globals_globals =
    testTraceMulti "globals/globals" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          -- non-extern non-static globals
          "nesBinary"
        , "nesBool"
        , "nesCast"
        , "nesCharacter"
        , "nesCompound"
        , "nesConditional"
        , "nesFloating"
        , "nesImaginary"
        , "nesInitList"
        , "nesInteger"
        , "nesParen"
        , "nesString1"
        , "nesString2"
        , "nesUnary"
        , "some_global_struct"
        , "streamBinary_len"
        , "streamBinary"
        ]

test_globals_untagged :: TestCase
test_globals_untagged = defaultTest "globals/untagged"
