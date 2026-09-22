-- | Tests for 'splitMacro', the one language-independent macro parser
--
-- Every macro definition passes through it, so this is where the
-- object-like\/function-like distinction, the parameter list grammar and the
-- treatment of keywords are pinned down. The body is not interpreted here; it
-- is returned verbatim.
module Test.HsBindgen.Macro.Syntax (tests) where

import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@=?))

import HsBindgen.Runtime.Macro qualified as Runtime.Macro

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
                splitsTo (Runtime.Macro.objectLike "FOO" ["1"]) [
                    ident "FOO", spc, lit "1"
                  ]
            , testCase "#define FOO" $
                splitsTo (Runtime.Macro.objectLike "FOO" []) [
                    ident "FOO"
                  ]
              -- White space before the @(@ makes this object-like, with the
              -- parentheses part of the body; see #1903.
            , testCase "#define FOO (1)" $
                splitsTo (Runtime.Macro.objectLike "FOO" ["(", "1", ")"]) [
                    ident "FOO", spc, punc "(", lit "1", punc ")"
                  ]
            ]
        , testGroup "function-like" [
              testCase "#define ADD(x, y) x + y" $
                splitsTo (Runtime.Macro.functionLike "ADD" ["x", "y"] ["x", "+", "y"]) [
                    ident "ADD", punc "(", ident "x", punc ",", spc, ident "y", punc ")"
                  , spc, ident "x", spc, punc "+", spc, ident "y"
                  ]
              -- Distinct from the object-like @#define NOW 0@: the empty
              -- parameter list is 'Runtime.Macro.Params' @[] False@, not
              -- 'Runtime.Macro.NoParams'.
            , testCase "#define NOW() 0" $
                splitsTo (Runtime.Macro.functionLike "NOW" [] ["0"]) [
                    ident "NOW", punc "(", punc ")", spc, lit "0"
                  ]
            , testCase "#define IGNORE(x)" $
                splitsTo (Runtime.Macro.functionLike "IGNORE" ["x"] []) [
                    ident "IGNORE", punc "(", ident "x", punc ")"
                  ]
              -- C23 6.10.1p6 forbids repeating a parameter name and @clang@
              -- rejects the definition, so this never reaches us through the
              -- pipeline. The splitter splits; it does not validate C.
            , testCase "#define F(x, x) x" $
                splitsTo (Runtime.Macro.functionLike "F" ["x", "x"] ["x"]) [
                    ident "F", punc "(", ident "x", punc ",", spc, ident "x"
                  , punc ")", spc, ident "x"
                  ]
            ]
        , testGroup "variadic" [
              testCase "#define LOG(fmt, ...) fmt" $
                splitsTo (Runtime.Macro.variadic "LOG" ["fmt"] ["fmt"]) [
                    ident "LOG", punc "(", ident "fmt", punc ",", spc, punc "..."
                  , punc ")", spc, ident "fmt"
                  ]
            , testCase "#define WARN(...) __VA_ARGS__" $
                splitsTo (Runtime.Macro.variadic "WARN" [] ["__VA_ARGS__"]) [
                    ident "WARN", punc "(", punc "...", punc ")"
                  , spc, ident "__VA_ARGS__"
                  ]
              -- The GNU named variadic form: the name before the @...@ stands
              -- for the trailing arguments, so it is not a named parameter.
            , testCase "#define GNU(args...) args" $
                splitsTo (Runtime.Macro.variadicNamed "GNU" [] "args" ["args"]) [
                    ident "GNU", punc "(", ident "args", punc "...", punc ")"
                  , spc, ident "args"
                  ]
            , testCase "#define GNU(fmt, args...) fmt" $
                splitsTo (Runtime.Macro.variadicNamed "GNU" ["fmt"] "args" ["fmt"]) [
                    ident "GNU", punc "(", ident "fmt", punc ",", spc, ident "args"
                  , punc "...", punc ")", spc, ident "fmt"
                  ]
            ]
        , testGroup "keywords" [
              -- A macro definition may give a new meaning to a keyword, so the
              -- name accepts one.
              testCase "#define bool int" $
                splitsTo (Runtime.Macro.objectLike "bool" ["int"]) [
                    kw "bool", spc, kw "int"
                  ]
              -- A parameter may be a keyword too. Whether @libclang@ calls
              -- @bool@ a keyword depends on the C standard, and the splitter
              -- must not care; see 'Test.HsBindgen.Macro.Syntax.Clang', which
              -- runs this definition under both.
            , testCase "#define F(bool) bool" $
                splitsTo (Runtime.Macro.functionLike "F" ["bool"] ["bool"]) [
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
            , testCase "#define F(" $
                failsToSplit [
                    ident "F", punc "("
                  ]
            , testCase "#define F(x" $
                failsToSplit [
                    ident "F", punc "(", ident "x"
                  ]
              -- The name must be an identifier or a keyword; a literal is
              -- neither.
            , testCase "#define 1 2" $
                failsToSplit [
                    lit "1", spc, lit "2"
                  ]
            ]
          -- A comment is a token, not white space: it separates the name from
          -- the @(@, and it stays in the body.
        , testGroup "comments" [
              testCase "#define FOO/*c*/(1)" $
                splitsTo (Runtime.Macro.objectLike "FOO" ["/*c*/", "(", "1", ")"]) [
                    ident "FOO", comment "/*c*/", punc "(", lit "1", punc ")"
                  ]
            ]
          -- A line continuation splices the lines before the macro is parsed,
          -- so the tokens around it are still adjacent. @libclang@ reports the
          -- continuation as part of the punctuation token that follows it.
        , testGroup "line continuations" [
              testCase "between the name and the parameter list" $
                splitsTo (Runtime.Macro.functionLike "F" ["x"] ["x"]) [
                    ident "F", punc "\\\n(", ident "x", punc ")", spc, ident "x"
                  ]
            , testCase "inside the parameter list" $
                splitsTo (Runtime.Macro.functionLike "F" ["x", "y"] ["x"]) [
                    ident "F", punc "(", ident "x", punc "\\\n,", spc, ident "y"
                  , punc ")", spc, ident "x"
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
splitsTo :: Runtime.Macro.Raw String -> [Piece] -> Assertion
splitsTo expected pieces = Right expected @=? split pieces

-- | Assert that the pieces do not split
failsToSplit :: [Piece] -> Assertion
failsToSplit pieces =
    case split pieces of
      Left _    -> return ()
      Right raw -> assertFailure $
        "expected a parse failure, but got " ++ show (Runtime.Macro.render raw)

split :: [Piece] -> Either String (Runtime.Macro.Raw String)
split pieces =
    case splitMacro (layout pieces) of
      Left  err -> Left err.macroParseError
      Right raw -> Right $ Text.unpack . spelling <$> raw
