{-# LANGUAGE RoleAnnotations #-}

module Test.HsBindgen.Macro.UniqueExpansion (
    tests
  ) where

import Data.Set qualified as Set
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@=?))
import Test.Tasty.QuickCheck

import Clang.HighLevel.Types (MultiLoc, Range, SourcePath)
import Clang.Paths (RealPath (..), getSourcePathText)

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
          -- Undefined macros
        , testProperty "example7"  example7
        , testProperty "example8"  example8
        , testProperty "example9"  example9
        , testProperty "example10" example10
        , testProperty "example11" example11
          -- Variadic macros
        , testProperty "example12" example12
        ]
    , testGroup "parseDefinition" [
          testCase "#define F(x) x + G" $
            definitionParsesTo (mkDefinition "F" ["G"]) "F" [
                ident "F", punc "(", ident "x", punc ")"
              , spc, ident "x", spc, punc "+", spc, ident "G"
              ]
          -- A keyword is a name wherever a name may stand: as the macro's own
          -- name, as a parameter, and in the body.
        , testCase "#define F(bool) bool" $
            definitionParsesTo (mkDefinition "F" []) "F" [
                ident "F", punc "(", kw "bool", punc ")", spc, kw "bool"
              ]
        , testCase "#define bool int" $
            definitionParsesTo (mkDefinition "bool" ["int"]) "bool" [
                kw "bool", spc, kw "int"
              ]
          -- A name the body mentions twice is one dependency.
        , testCase "#define A B + B" $
            definitionParsesTo (mkDefinition "A" ["B"]) "A" [
                ident "A", spc, ident "B", spc, punc "+", spc, ident "B"
              ]
          -- A parameter shadows the macro of the same name, so it is not a
          -- dependency: the expansion of @F@ does not change when @A@ is
          -- redefined.
        , testCase "#define F(A) A" $
            definitionParsesTo (mkDefinition "F" []) "F" [
                ident "F", punc "(", ident "A", punc ")", spc, ident "A"
              ]
          -- The reserved names of a variadic macro stand for its arguments, so
          -- they are parameters rather than dependencies.
        , testCase "#define F(...) __VA_ARGS__ __VA_OPT__(,)" $
            definitionParsesTo (mkDefinition "F" []) "F" [
                ident "F", punc "(", punc "...", punc ")"
              , spc, ident "__VA_ARGS__"
              , spc, ident "__VA_OPT__", punc "(", punc ",", punc ")"
              ]
          -- In the GNU named-variadic form the name standing for the trailing
          -- arguments is a parameter like any other.
        , testCase "#define F(args...) g(args)" $
            definitionParsesTo (mkDefinition "F" ["g"]) "F" [
                ident "F", punc "(", ident "args", punc "...", punc ")"
              , spc, ident "g", punc "(", ident "args", punc ")"
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

-- | A definition as 'parseDefinition' reduces it: a name and its dependencies
mkDefinition :: Name -> [Name] -> Definition
mkDefinition name deps = Definition name (Set.fromList deps)

definitionParsesTo :: Definition -> Text -> [Piece] -> Assertion
definitionParsesTo expected name pieces =
    Right expected @=? (parseDefinition (mkMacroDefinition name pieces)).result

definitionFailsWith :: Error -> Text -> [Piece] -> Assertion
definitionFailsWith expected name pieces =
    Left expected @=? (parseDefinition (mkMacroDefinition name pieces)).result

definitionFailsWithParseError :: Text -> [Piece] -> Assertion
definitionFailsWithParseError name pieces =
    assertParseError (parseDefinition (mkMacroDefinition name pieces)).result

invocationParsesTo :: Invocation -> Text -> [Piece] -> Assertion
invocationParsesTo expected name pieces =
    Right expected @=? (parseInvocation (mkMacroInvocation name pieces)).result

invocationFailsWith :: Error -> Text -> [Piece] -> Assertion
invocationFailsWith expected name pieces =
    Left expected @=? (parseInvocation (mkMacroInvocation name pieces)).result

invocationFailsWithParseError :: Text -> [Piece] -> Assertion
invocationFailsWithParseError name pieces =
    assertParseError (parseInvocation (mkMacroInvocation name pieces)).result

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
mkMacroDefinition :: Text -> [Piece] -> MacroDefinition
mkMacroDefinition name pieces = MacroDefinition {
      name     = name
    , locRange = toRealPathRange (extentOf tokens)
    , macro    = splitMacro tokens
    }
  where
    tokens = layout pieces

mkMacroInvocation :: Text -> [Piece] -> MacroInvocation
mkMacroInvocation name pieces = MacroInvocation {
      name     = name
    , locRange = toRealPathRange (extentOf tokens)
    , tokens   = tokens
    }
  where
    tokens = layout pieces

-- | Coerce the dummy @\<test\>@ SourcePath extent to RealPath.
--
-- Safe because the path is a test-only dummy that never hits disk or
-- identity comparisons.
toRealPathRange :: Range (MultiLoc SourcePath) -> Range (MultiLoc RealPath)
toRealPathRange = fmap (fmap (RealPath . getSourcePathText))

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

propIsExpansionUnique :: Bool -> [Definition] -> Invocation -> Property
propIsExpansionUnique expected defs inv =
    expected === isExpansionUnique (fmap liftDefinition defs) (liftInvocation inv)

{-------------------------------------------------------------------------------
  Unit tests

  The definitions are already reduced to their dependencies; which names of a
  definition are dependencies is 'parseDefinition''s business, tested above.
-------------------------------------------------------------------------------}

-- | Invoked object-like macro has no dependencies. Expansion is unique.
example1 :: Property
example1 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "A" []
    defs = [
        mkDefinition "A" []
      ]

-- | Invoked object-like macro has dependencies. Expansion is unique.
example2 :: Property
example2 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "B" []
    defs = [
        mkDefinition "A" []
      , mkDefinition "B" ["A"]
      ]

-- | Invoked object-like macro has no dependencies. Invoked macro has two definitions.
-- Expansion is not unique.
example3 :: Property
example3 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "A" []
    defs = [
        mkDefinition "A" []
      , mkDefinition "A" []
      ]

-- | Invoked macro has dependencies. Dependencies do not have unique expansions.
-- Expansion is not unique.
example4 :: Property
example4 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "B" []
    defs = [
        mkDefinition "A" []
      , mkDefinition "A" []
      , mkDefinition "B" ["A"]
      ]

-- | Invoked function-like macro has no dependencies. Invoked with a argument
-- that has a unique expansion. Expansion is unique.
example5 :: Property
example5 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["B"]
    defs = [
        mkDefinition "A" []
      , mkDefinition "A" []
      , mkDefinition "B" []
      , mkDefinition "F" []
      ]

-- | Invoked function-like macro has no dependencies. Invoked with an argument
-- that has no unique expansion. Expansion is not unique.
example6 :: Property
example6 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "F" ["A"]
    defs = [
        mkDefinition "A" []
      , mkDefinition "A" []
      , mkDefinition "B" []
      , mkDefinition "F" []
      ]

--
-- Undefined macros
--

-- | Invoked object-like macro is undefined. Expansion is unique.
example7 :: Property
example7 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "A" []
    defs = [
      ]

-- | Invoked object-like macro has undefined dependencies. Expansion is unique.
example8 :: Property
example8 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "B" []
    defs = [
        mkDefinition "B" ["A"]
      ]

-- | Invoked function-like macro is undefined. Expansion is unique.
example9 :: Property
example9 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["A"]
    defs = [
        mkDefinition "A" []
      ]

-- | Invoked function-like macro has undefined dependencies. Expansion is
-- unique.
example10 :: Property
example10 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["C"]
    defs = [
        mkDefinition "C" []
      , mkDefinition "F" ["B"]
      ]

-- | Invoked function-like macro with an undefined argument. Expansion is unique.
example11 :: Property
example11 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["B"]
    defs = [
        mkDefinition "F" []
      ]

--
-- Variadic macros
--

-- | Invoked function-like macro is variadic. All trailing arguments are
-- checked. Expansion is unique.
example12 :: Property
example12 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["A", "B"]
    defs = [
        mkDefinition "A" []
      , mkDefinition "B" []
      , mkDefinition "F" []
      ]
