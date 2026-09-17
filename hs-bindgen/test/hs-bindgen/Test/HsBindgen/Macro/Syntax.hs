-- | Tests for 'splitMacro', the one language-independent macro parser
--
-- Every macro definition passes through it, so this is where the
-- object-like\/function-like distinction, the parameter list grammar and the
-- treatment of keywords are pinned down. The body is not interpreted here; it
-- is returned verbatim.
module Test.HsBindgen.Macro.Syntax (tests) where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@=?))

import HsBindgen.Runtime.Macro qualified as RawMacro

import HsBindgen.Macro.Error (MacroParseError (..))
import HsBindgen.Macro.Parse (spelling)
import HsBindgen.Macro.Syntax (splitMacro)

import Test.HsBindgen.Macro.Infra

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Macro.Syntax" [
      testGroup "splitMacro" [
          testGroup "object-like" [
              testCase "#define FOO 1" $
                splitsTo (RawMacro.objectLike "FOO" ["1"]) [
                    ident "FOO", spc, lit "1"
                  ]
            , testCase "#define FOO" $
                splitsTo (RawMacro.objectLike "FOO" []) [
                    ident "FOO"
                  ]
              -- White space before the @(@ makes this object-like, with the
              -- parentheses part of the body; see #1903.
            , testCase "#define FOO (1)" $
                splitsTo (RawMacro.objectLike "FOO" ["(", "1", ")"]) [
                    ident "FOO", spc, punc "(", lit "1", punc ")"
                  ]
            ]
        , testGroup "function-like" [
              testCase "#define ADD(x, y) x + y" $
                splitsTo (RawMacro.functionLike "ADD" ["x", "y"] ["x", "+", "y"]) [
                    ident "ADD", punc "(", ident "x", punc ",", spc, ident "y", punc ")"
                  , spc, ident "x", spc, punc "+", spc, ident "y"
                  ]
              -- Distinct from the object-like @#define NOW 0@: the empty
              -- parameter list is 'RawMacro.Params' @[] False@, not
              -- 'RawMacro.NoParams'.
            , testCase "#define NOW() 0" $
                splitsTo (RawMacro.functionLike "NOW" [] ["0"]) [
                    ident "NOW", punc "(", punc ")", spc, lit "0"
                  ]
            , testCase "#define IGNORE(x)" $
                splitsTo (RawMacro.functionLike "IGNORE" ["x"] []) [
                    ident "IGNORE", punc "(", ident "x", punc ")"
                  ]
            ]
        , testGroup "variadic" [
              testCase "#define LOG(fmt, ...) fmt" $
                splitsTo (RawMacro.variadic "LOG" ["fmt"] ["fmt"]) [
                    ident "LOG", punc "(", ident "fmt", punc ",", spc, punc "..."
                  , punc ")", spc, ident "fmt"
                  ]
            , testCase "#define WARN(...) __VA_ARGS__" $
                splitsTo (RawMacro.variadic "WARN" [] ["__VA_ARGS__"]) [
                    ident "WARN", punc "(", punc "...", punc ")"
                  , spc, ident "__VA_ARGS__"
                  ]
              -- The GNU named variadic form: the name before the @...@ stands
              -- for the trailing arguments, so it is not a named parameter.
            , testCase "#define GNU(args...) args" $
                splitsTo (RawMacro.variadicNamed "GNU" [] "args" ["args"]) [
                    ident "GNU", punc "(", ident "args", punc "...", punc ")"
                  , spc, ident "args"
                  ]
            , testCase "#define GNU(fmt, args...) fmt" $
                splitsTo (RawMacro.variadicNamed "GNU" ["fmt"] "args" ["fmt"]) [
                    ident "GNU", punc "(", ident "fmt", punc ",", spc, ident "args"
                  , punc "...", punc ")", spc, ident "fmt"
                  ]
            ]
        , testGroup "keywords" [
              -- A macro definition may give a new meaning to a keyword, so the
              -- name accepts one.
              testCase "#define bool int" $
                splitsTo (RawMacro.objectLike "bool" ["int"]) [
                    kw "bool", spc, kw "int"
                  ]
              -- A parameter may not be a keyword. Under LLVM 14, and for C17
              -- and earlier, @bool@ is an identifier and this does split; the
              -- classification is an input here, so the test pins the keyword
              -- case only.
            , testCase "#define F(bool) bool" $
                failsToSplit [
                    ident "F", punc "(", kw "bool", punc ")", spc, kw "bool"
                  ]
            ]
        , testGroup "malformed" [
              testCase "no tokens at all" $
                failsToSplit []
            , testCase "name is punctuation" $
                failsToSplit [punc "+", spc, lit "1"]
              -- Function-like, since there is no white space before the @(@,
              -- and 1 is not a parameter name. Before the splitter this fell
              -- back to an object-like macro with body @(1)@.
            , testCase "#define FOO(1)" $
                failsToSplit [
                    ident "FOO", punc "(", lit "1", punc ")"
                  ]
            , testCase "#define F(x,) x" $
                failsToSplit [
                    ident "F", punc "(", ident "x", punc ",", punc ")"
                  , spc, ident "x"
                  ]
            , testCase "#define F(..., x) x" $
                failsToSplit [
                    ident "F", punc "(", punc "...", punc ",", spc, ident "x", punc ")"
                  , spc, ident "x"
                  ]
            , testCase "#define F(x x" $
                failsToSplit [
                    ident "F", punc "(", ident "x", spc, ident "x"
                  ]
            ]
          -- A line continuation splices the lines before the macro is parsed,
          -- so the @(@ is still adjacent to the name. @libclang@ reports the
          -- continuation as part of the punctuation token.
        , testGroup "line continuations" [
              testCase "between the name and the parameter list" $
                splitsTo (RawMacro.functionLike "F" ["x"] ["x"]) [
                    ident "F", punc "\\\n(", ident "x", punc ")", spc, ident "x"
                  ]
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Assert that the pieces split into the given macro
--
-- Only the spellings are compared; the splitter does not change them.
splitsTo :: RawMacro.Raw Text -> [Piece] -> Assertion
splitsTo expected pieces = Right expected @=? split pieces

-- | Assert that the pieces do not split
failsToSplit :: [Piece] -> Assertion
failsToSplit pieces =
    case split pieces of
      Left _    -> return ()
      Right raw -> assertFailure $
        "expected a parse failure, but got " ++ show (RawMacro.render raw)

split :: [Piece] -> Either String (RawMacro.Raw Text)
split pieces =
    case splitMacro (layout pieces) of
      Left  err -> Left err.macroParseError
      Right raw -> Right $ spelling <$> raw
