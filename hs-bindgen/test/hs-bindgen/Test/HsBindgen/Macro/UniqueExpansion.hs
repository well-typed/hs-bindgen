{-# LANGUAGE RoleAnnotations #-}

module Test.HsBindgen.Macro.UniqueExpansion (
    tests
  ) where

import Data.Set qualified as Set
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@=?))
import Test.Tasty.QuickCheck

import HsBindgen.Runtime.Macro qualified as Runtime.Macro

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
          -- Redefinitions
        , testProperty "example13" example13
        , testProperty "example14" example14
        , testProperty "example15" example15
        ]
    , testGroup "redefinition" [
          testCase "identical" $
            Benign @=? redefinitionOf "A" [
                mkDefinition "A" [] `withBody` ["1"]
              , mkDefinition "A" [] `withBody` ["1"]
              ]
        , testCase "different" $
            NotBenign @=? redefinitionOf "A" [
                mkDefinition "A" [] `withBody` ["1"]
              , mkDefinition "A" [] `withBody` ["2"]
              ]
          -- Ambiguity of a dependency does not make a redefinition a
          -- conflict: the definitions of @B@ are interchangeable.
        , testCase "identical, depending on an ambiguous macro" $
            Benign @=? redefinitionOf "B" [
                mkDefinition "A" [] `withBody` ["1"]
              , mkDefinition "B" ["A"]
              , mkDefinition "A" [] `withBody` ["2"]
              , mkDefinition "B" ["A"]
              ]
        , testCase "unsplittable" $
            NotBenign @=? redefinitionOfResults "A" [
                liftDefinition $ mkDefinition "A" []
              , unsplittable "A"
              ]
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
        , testCase "spelling of #define F(x) x + G" $
            Right (Runtime.Macro.Raw "F" (Runtime.Macro.Params ["x"] Runtime.Macro.NotVariadic) ["x", "+", "G"])
              @=? (.spelled) <$> (parseDefinition (mkMacroDefinition "F" [
                      ident "F", punc "(", ident "x", punc ")"
                    , spc, ident "x", spc, punc "+", spc, ident "G"
                    ])).result
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

-- | A definition as 'parseDefinition' reduces it
--
-- The definition is object-like, and its body spells its dependencies.
mkDefinition :: Name -> [Name] -> Definition
mkDefinition name deps = Definition{
      name    = name
    , deps    = Set.fromList deps
    , spelled = Runtime.Macro.Raw {
          name   = name.unwrap
        , params = Runtime.Macro.NoParams
        , body   = map (.unwrap) deps
        }
    }

-- | Replace the spelled body, keeping the dependencies
withBody :: Definition -> [Text] -> Definition
withBody def body = def{spelled = def.spelled{Runtime.Macro.body = body}}

-- | Compares the name and the dependencies; see "spelling of ..." for the rest
definitionParsesTo :: Definition -> Text -> [Piece] -> Assertion
definitionParsesTo expected name pieces =
        Right (projection expected)
    @=? projection <$> (parseDefinition (mkMacroDefinition name pieces)).result
  where
    projection :: Definition -> (Name, Set.Set Name)
    projection def = (def.name, def.deps)

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
      name  = name
    , macro = splitMacro (layout pieces)
    }

mkMacroInvocation :: Text -> [Piece] -> MacroInvocation
mkMacroInvocation name pieces = MacroInvocation {
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
propIsExpansionUnique expected defs =
    propIsExpansionUniqueResults expected (fmap liftDefinition defs)

propIsExpansionUniqueResults ::
     Bool
  -> [ParseResult Definition]
  -> Invocation
  -> Property
propIsExpansionUniqueResults expected defs inv =
        expected
    === isExpansionUnique
          (ambiguity (analyseMacroDefinitions defs))
          (liftInvocation inv)

redefinitionOf :: Name -> [Definition] -> Redefinition
redefinitionOf name defs = redefinitionOfResults name (fmap liftDefinition defs)

redefinitionOfResults :: Name -> [ParseResult Definition] -> Redefinition
redefinitionOfResults name defs =
    redefinition (analyseMacroDefinitions defs) name

-- | A definition that could not be analysed
unsplittable :: Name -> ParseResult Definition
unsplittable name = ParseResult name (Left (NameMismatch name.unwrap "X"))

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

-- | Invoked object-like macro has no dependencies. Invoked macro has two
-- different definitions. Expansion is not unique.
example3 :: Property
example3 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "A" []
    defs = [
        mkDefinition "A" [] `withBody` ["1"]
      , mkDefinition "A" [] `withBody` ["2"]
      ]

-- | Invoked macro has dependencies. Dependencies do not have unique expansions.
-- Expansion is not unique.
example4 :: Property
example4 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "B" []
    defs = [
        mkDefinition "A" [] `withBody` ["1"]
      , mkDefinition "A" [] `withBody` ["2"]
      , mkDefinition "B" ["A"]
      ]

-- | Invoked function-like macro has no dependencies. Invoked with a argument
-- that has a unique expansion. Expansion is unique.
example5 :: Property
example5 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "F" ["B"]
    defs = [
        mkDefinition "A" [] `withBody` ["1"]
      , mkDefinition "A" [] `withBody` ["2"]
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
        mkDefinition "A" [] `withBody` ["1"]
      , mkDefinition "A" [] `withBody` ["2"]
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

--
-- Redefinitions
--

-- | Invoked macro has two identical definitions. Expansion is unique.
example13 :: Property
example13 = once $ propIsExpansionUnique True defs inv
  where
    inv = Invocation "A" []
    defs = [
        mkDefinition "A" [] `withBody` ["1"]
      , mkDefinition "A" [] `withBody` ["1"]
      ]

-- | Invoked macro has two identical definitions, but depends on a macro with
-- two different definitions. Expansion is not unique.
example14 :: Property
example14 = once $ propIsExpansionUnique False defs inv
  where
    inv = Invocation "B" []
    defs = [
        mkDefinition "A" [] `withBody` ["Foo"]
      , mkDefinition "B" ["A"]
      , mkDefinition "A" [] `withBody` ["Bar"]
      , mkDefinition "B" ["A"]
      ]

-- | Invoked macro depends on a macro that could not be analysed. Expansion is
-- not unique.
example15 :: Property
example15 = once $ propIsExpansionUniqueResults False defs inv
  where
    inv = Invocation "B" []
    defs = [
        unsplittable "A"
      , liftDefinition $ mkDefinition "B" ["A"]
      ]
