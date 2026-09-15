{-# LANGUAGE RoleAnnotations #-}

module Test.HsBindgen.Macro.UniqueExpansion (
    tests
  ) where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@=?))
import Test.Tasty.QuickCheck

import HsBindgen.Runtime.Macro (Params (NoParams, Params), Raw (Raw),
                                Variadic (Ellipsis, NamedEllipsis, NotVariadic))

import HsBindgen.Macro.Syntax (MacroDefinition (..), MacroInvocation (..),
                               splitMacro)
import HsBindgen.Macro.UniqueExpansion
import HsBindgen.Macro.UniqueExpansion.Types

import Test.HsBindgen.Macro.Infra

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Macro.UniqueExpansion" [
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
          -- Variadic macros
        , testProperty "example14" example14
        , testProperty "example15" example15
        , testProperty "example16" example16
        ]
    , testGroup "parseDefinition" [
          testCase "#define F(x) x + G" $
            definitionParsesTo (Definition (Raw "F" (Params ["x"] NotVariadic) ["x", "G"])) "F" [
                ident "F", punc "(", ident "x", punc ")"
              , spc, ident "x", spc, punc "+", spc, ident "G"
              ]
          -- A keyword is a name wherever a name may stand: as the macro's own
          -- name, as a parameter, and in the body.
        , testCase "#define F(bool) bool" $
            definitionParsesTo (Definition (Raw "F" (Params ["bool"] NotVariadic) ["bool"])) "F" [
                ident "F", punc "(", kw "bool", punc ")", spc, kw "bool"
              ]
        , testCase "#define bool int" $
            definitionParsesTo (Definition (Raw "bool" NoParams ["int"])) "bool" [
                kw "bool", spc, kw "int"
              ]
          -- @libclang@ gives the name twice: as the cursor spelling and as the
          -- first token. The split does not compare them, so this does.
        , testCase "cursor spelling disagrees with the tokens" $
            definitionFailsWith (NameMismatch "G" "F") "G" [
                ident "F", spc, lit "1"
              ]
        , testCase "unsplittable definition" $
            definitionFailsWithParseError "F" [
                ident "F", punc "(", lit "1", punc ")"
              ]
        ]
    , testGroup "parseInvocation" [
          testCase "FOO" $
            invocationParsesTo (Invocation "FOO" []) "FOO" [
                ident "FOO"
              ]
        , testCase "F(a)" $
            invocationParsesTo (Invocation "F" ["a"]) "F" [
                ident "F", punc "(", ident "a", punc ")"
              ]
          -- Every name in the argument list is collected, however deeply
          -- nested, and whatever it means there.
        , testCase "F(1 + a, g(b))" $
            invocationParsesTo (Invocation "F" ["a", "g", "b"]) "F" [
                ident "F", punc "(", lit "1", spc, punc "+", spc, ident "a"
              , punc ",", spc, ident "g", punc "(", ident "b", punc ")", punc ")"
              ]
          -- The invoked macro and its arguments may both be keywords, just as a
          -- definition's name and parameters may be.
        , testCase "bool" $
            invocationParsesTo (Invocation "bool" []) "bool" [
                kw "bool"
              ]
        , testCase "F(bool)" $
            invocationParsesTo (Invocation "F" ["bool"]) "F" [
                ident "F", punc "(", kw "bool", punc ")"
              ]
        , testCase "cursor spelling disagrees with the tokens" $
            invocationFailsWith (NameMismatch "G" "F") "G" [
                ident "F"
              ]
        , testCase "unbalanced parentheses" $
            invocationFailsWithParseError "F" [
                ident "F", punc "(", ident "a"
              ]
        ]
    ]

{-------------------------------------------------------------------------------
  Parsing definitions and invocations

  'parseDefinition' and 'parseInvocation' project a 'MacroDefinition' or
  'MacroInvocation' onto the names it mentions; the examples above start from
  the projections instead. The pieces are laid out as if they had been
  tokenized from source; see "Test.HsBindgen.Macro.Infra".
-------------------------------------------------------------------------------}

definitionParsesTo :: Definition -> Text -> [Piece] -> Assertion
definitionParsesTo expected name pieces =
    Right expected @=? (parseDefinition (mkDefinition name pieces)).result

definitionFailsWith :: Error -> Text -> [Piece] -> Assertion
definitionFailsWith expected name pieces =
    Left expected @=? (parseDefinition (mkDefinition name pieces)).result

definitionFailsWithParseError :: Text -> [Piece] -> Assertion
definitionFailsWithParseError name pieces =
    assertParseError (parseDefinition (mkDefinition name pieces)).result

invocationParsesTo :: Invocation -> Text -> [Piece] -> Assertion
invocationParsesTo expected name pieces =
    Right expected @=? (parseInvocation (mkInvocation name pieces)).result

invocationFailsWith :: Error -> Text -> [Piece] -> Assertion
invocationFailsWith expected name pieces =
    Left expected @=? (parseInvocation (mkInvocation name pieces)).result

invocationFailsWithParseError :: Text -> [Piece] -> Assertion
invocationFailsWithParseError name pieces =
    assertParseError (parseInvocation (mkInvocation name pieces)).result

-- | The message of a 'ParseError' is @parsec@'s, so only its shape is asserted
assertParseError :: Show a => Either Error a -> Assertion
assertParseError = \case
    Left ParseError{} -> return ()
    other             -> assertFailure $
      "expected a parse error, but got " ++ show other

-- | A definition as the @Parse@ pass records it
--
-- The name is the cursor spelling, which 'parseDefinition' compares against the
-- tokens; the tokens are split by 'splitMacro', exactly as in the pass.
mkDefinition :: Text -> [Piece] -> MacroDefinition
mkDefinition name pieces = MacroDefinition {
      name     = name
    , locRange = extentOf tokens
    , macro    = splitMacro tokens
    }
  where
    tokens = layout pieces

mkInvocation :: Text -> [Piece] -> MacroInvocation
mkInvocation name pieces = MacroInvocation {
      name     = name
    , locRange = extentOf tokens
    , tokens   = tokens
    }
  where
    tokens = layout pieces

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
    defs = map Definition [
        Raw "A" NoParams []
      ]

-- | Invoked object-like macro has dependencies. Expansion is unique.
example2 :: Property
example2 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "B" []
    defs = map Definition [
        Raw "A" NoParams []
      , Raw "B" NoParams ["A"]
      ]

-- | Invoked object-like macro has no dependencies. Invoked macro has two definitions.
-- Expansion is not unique.
example3 :: Property
example3 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "A" []
    defs = map Definition [
        Raw "A" NoParams []
      , Raw "A" NoParams []
      ]

-- | Invoked macro has dependencies. Dependencies do not have unique expansions.
-- Expansion is not unique.
example4 :: Property
example4 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "B" []
    defs = map Definition [
        Raw "A" NoParams []
      , Raw "A" NoParams []
      , Raw "B" NoParams ["A"]
      ]

-- | Invoked function-like macro has no dependencies. Invoked with a argument
-- that has a unique expansion. Expansion is unique.
example5 :: Property
example5 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["B"]
    defs = map Definition [
        Raw "A" NoParams []
      , Raw "A" NoParams []
      , Raw "B" NoParams []
      , Raw "F" (Params ["C"] NotVariadic) ["C"]
      ]

-- | Invoked function-like macro has no dependencies. Invoked with an argument
-- that has no unique expansion. Expansion is not unique.
example6 :: Property
example6 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "F" ["A"]
    defs = map Definition [
        Raw "A" NoParams []
      , Raw "A" NoParams []
      , Raw "B" NoParams []
      , Raw "F" (Params ["C"] NotVariadic) ["C"]
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
    defs = map Definition [
        Raw "A" NoParams []
      , Raw "A" NoParams []
      , Raw "B" NoParams []
      , Raw "F" (Params ["A"] NotVariadic) ["A"]
      ]

-- | Invoked function-like macro has no dependencies. The parameter name matches
-- a macro that has no unique expansion. Invoked with that macro as an argument.
-- Expansion is not unique
example8 :: Property
example8 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "F" ["A"]
    defs = map Definition [
        Raw "A" NoParams []
      , Raw "A" NoParams []
      , Raw "B" NoParams []
      , Raw "F" (Params ["A"] NotVariadic) ["A"]
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
    defs = map Definition [
        Raw "B" NoParams ["A"]
      ]

-- | Invoked function-like macro is undefined. Expansion is unique.
example11 :: Property
example11 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["A"]
    defs = map Definition [
        Raw "A" NoParams []
      ]

-- | Invoked function-like macro has undefined dependencies. Expansion is
-- unique.
example12 :: Property
example12 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["C"]
    defs = map Definition [
        Raw "C" NoParams []
      , Raw "F" (Params ["A"] NotVariadic) ["B"]
      ]

-- | Invoked function-like macro with an undefined argument. Expansion is unique.
example13 :: Property
example13 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["B"]
    defs = map Definition [
        Raw "F" (Params ["A"] NotVariadic) ["A"]
      ]

--
-- Variadic macros
--

-- | Invoked function-like macro is variadic. Expansion is unique.
example14 :: Property
example14 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["A", "B"]
    defs = map Definition [
        Raw "F" (Params [] Ellipsis) []
      ]

-- | Invoked function-like macro is variadic. The variadic function uses
-- reserved @__VA_ARGS__@ in its body. Expansion is unique.
example15 :: Property
example15 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["A", "B"]
    defs = map Definition [
        Raw "F" (Params [] Ellipsis) ["__VA_ARGS__"]
      ]

-- | Invoked function-like macro uses the GNU named-variadic form. The name of
-- the trailing arguments is a parameter, not a dependency on the ambiguous
-- macro @A@ that happens to share its name. Expansion is unique.
example16 :: Property
example16 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["B"]
    defs = map Definition [
        Raw "A" NoParams []
      , Raw "A" NoParams []
      , Raw "B" NoParams []
      , Raw "F" (Params [] (NamedEllipsis "A")) ["A"]
      ]
