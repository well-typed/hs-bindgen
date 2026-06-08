{-# LANGUAGE RoleAnnotations #-}

module Test.HsBindgen.Clang.Macros.UniqueExpansion (
    tests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import HsBindgen.Clang.Macros.UniqueExpansion
import HsBindgen.Clang.Macros.UniqueExpansion.Types

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Clang.Macros.UniqueExpansion" [
      testGroup "propIsExpansionUnique" [
          testProperty "example1" example1
        , testProperty "example2" example2
        , testProperty "example3" example3
        , testProperty "example4" example4
        , testProperty "example5" example5
        , testProperty "example6" example6
          -- Parameter names
        , testProperty "example7" example7
        , testProperty "example8" example8
          -- Undefined macros
        , testProperty "example9"  example9
        , testProperty "example10" example10
        , testProperty "example11" example11
        , testProperty "example12" example12
        , testProperty "example13" example13
        ]
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

propIsExpansionUnique :: Bool -> [Definition] -> Invocation -> Property
propIsExpansionUnique expected defs inv =
    expected === isExpansionUnique (fmap liftDefinition defs) (liftInvocation inv)

{-------------------------------------------------------------------------------
  Unit tests
-------------------------------------------------------------------------------}

-- | Invoked object-like macro has no dependencies. Expansion is unique.
example1 :: Property
example1 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "A" []
    defs = [
        Definition "A" [] []
      ]

-- | Invoked object-like macro has dependencies. Expansion is unique.
example2 :: Property
example2 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "B" []
    defs = [
        Definition "A" [] []
      , Definition "B" [] [FreeVar "A"]
      ]

-- | Invoked object-like macro has no dependencies. Invoked macro has two definitions.
-- Expansion is not unique.
example3 :: Property
example3 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "A" []
    defs = [
        Definition "A" [] []
      , Definition "A" [] []
      ]

-- | Invoked macro has dependencies. Dependencies do not have unique expansions.
-- Expansion is not unique.
example4 :: Property
example4 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "B" []
    defs = [
        Definition "A" [] []
      , Definition "A" [] []
      , Definition "B" [] [FreeVar "A"]
      ]

-- | Invoked function-like macro has no dependencies. Invoked with a argument
-- that has a unique expansion. Expansion is unique.
example5 :: Property
example5 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["B"]
    defs = [
        Definition "A" [] []
      , Definition "A" [] []
      , Definition "B" [] []
      , Definition "F" ["C"] [LocalParam "C"]
      ]

-- | Invoked function-like macro has no dependencies. Invoked with an argument
-- that has no unique expansion. Expansion is not unique.
example6 :: Property
example6 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "F" ["A"]
    defs = [
        Definition "A" [] []
      , Definition "A" [] []
      , Definition "B" [] []
      , Definition "F" ["C"] [LocalParam "C"]
      ]

--
-- Parameter names
--

-- | Invoked function-like macro has no dependencies. The parameter name matches
-- a macro that has no unique expansion. Invoked with an argument that has a
-- unique expansion. Expansion is unique.
example7 :: Property
example7 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["B"]
    defs = [
        Definition "A" [] []
      , Definition "A" [] []
      , Definition "B" [] []
      , Definition "F" ["A"] [LocalParam "A"]
      ]

-- | Invoked function-like macro has no dependencies. The parameter name matches
-- a macro that has no unique expansion. Invoked with that macro as an argument.
-- Expansion is not unique
example8 :: Property
example8 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "F" ["A"]
    defs = [
        Definition "A" [] []
      , Definition "A" [] []
      , Definition "B" [] []
      , Definition "F" ["A"] [LocalParam "A"]
      ]

--
-- Undefined macros
--

-- | Invoked object-like macro is undefined. Expansion is unique.
example9 :: Property
example9 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "A" []
    defs = [
      ]

-- | Invoked object-like macro has undefined dependencies. Expansion is unique.
example10 :: Property
example10 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "B" []
    defs = [
        Definition "B" [] [FreeVar "A"]
      ]

-- | Invoked function-like macro is undefined. Expansion is unique.
example11 :: Property
example11 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["A"]
    defs = [
        Definition "A" [] []
      ]

-- | Invoked function-like macro has undefined dependencies. Expansion is
-- unique.
example12 :: Property
example12 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["C"]
    defs = [
        Definition "C" [] []
      , Definition "F" ["A"] [FreeVar "B"]
      ]

-- | Invoked function-like macro with an undefined argument. Expansion is unique.
example13 :: Property
example13 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["B"]
    defs = [
        Definition "F" ["A"] [LocalParam "A"]
      ]
