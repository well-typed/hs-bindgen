-- | Golden tests: manual examples
module Test.HsBindgen.Golden.Manual (testCases) where

import HsBindgen.Config.Internal
import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.TraceMsg

import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.TestCase

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = [
      defaultTest "manual/arrays"
    , defaultTest "manual/function_pointers"
    , defaultTest "manual/omit_field_prefixes"
      & #onFrontend .~ ( #fieldNamingStrategy .~ OmitFieldPrefixes )
    , defaultTest "manual/zero_copy"
    , test_manual_globals
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_manual_globals :: TestCase
test_manual_globals =
    testTraceMulti "manual/globals" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      MatchDelayed name ParseUnsupportedAnonInExtern{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = [
        -- potential duplicate symbols
        "nonExternGlobalInt"
        -- unexpected anonymous declaration in extern
      , "unusableAnon"
      ]
