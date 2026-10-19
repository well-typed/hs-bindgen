-- | Tests for 'readRawMacro', the one language-independent macro parser
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
import HsBindgen.Macro.Syntax (readRawMacro)

import Test.HsBindgen.Macro.Infra

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Macro.Syntax" [
      testGroup "readRawMacro" [
          testGroup "object-like" [
              testCase "#define FOO 1" $
                readsTo (Runtime.Macro.objectLike "FOO" ["1"]) [
                    ident "FOO", spc, lit "1"
                  ]
            , testCase "#define FOO" $
                readsTo (Runtime.Macro.objectLike "FOO" []) [
                    ident "FOO"
                  ]
              -- White space before the @(@ makes this object-like, with the
              -- parentheses part of the body; see #1903.
            , testCase "#define FOO (1)" $
                readsTo (Runtime.Macro.objectLike "FOO" ["(", "1", ")"]) [
                    ident "FOO", spc, punc "(", lit "1", punc ")"
                  ]
            ]
        , testGroup "function-like" [
              testCase "#define ADD(x, y) x + y" $
                readsTo (Runtime.Macro.functionLike "ADD" ["x", "y"] ["x", "+", "y"]) [
                    ident "ADD", punc "(", ident "x", punc ",", spc, ident "y", punc ")"
                  , spc, ident "x", spc, punc "+", spc, ident "y"
                  ]
              -- Distinct from the object-like @#define NOW 0@: the empty
              -- parameter list is 'Runtime.Macro.Params' @[] False@, not
              -- 'Runtime.Macro.NoParams'.
            , testCase "#define NOW() 0" $
                readsTo (Runtime.Macro.functionLike "NOW" [] ["0"]) [
                    ident "NOW", punc "(", punc ")", spc, lit "0"
                  ]
            , testCase "#define IGNORE(x)" $
                readsTo (Runtime.Macro.functionLike "IGNORE" ["x"] []) [
                    ident "IGNORE", punc "(", ident "x", punc ")"
                  ]
              -- C23 6.10.1p6 forbids repeating a parameter name and @clang@
              -- rejects the definition, so this never reaches us through the
              -- pipeline. The reader reads; it does not validate C.
            , testCase "#define F(x, x) x" $
                readsTo (Runtime.Macro.functionLike "F" ["x", "x"] ["x"]) [
                    ident "F", punc "(", ident "x", punc ",", spc, ident "x"
                  , punc ")", spc, ident "x"
                  ]
            ]
        , testGroup "variadic" [
              testCase "#define LOG(fmt, ...) fmt" $
                readsTo (Runtime.Macro.variadic "LOG" ["fmt"] ["fmt"]) [
                    ident "LOG", punc "(", ident "fmt", punc ",", spc, punc "..."
                  , punc ")", spc, ident "fmt"
                  ]
            , testCase "#define WARN(...) __VA_ARGS__" $
                readsTo (Runtime.Macro.variadic "WARN" [] ["__VA_ARGS__"]) [
                    ident "WARN", punc "(", punc "...", punc ")"
                  , spc, ident "__VA_ARGS__"
                  ]
              -- The GNU named variadic form: the name before the @...@ stands
              -- for the trailing arguments, so it is not a named parameter.
            , testCase "#define GNU(args...) args" $
                readsTo (Runtime.Macro.variadicNamed "GNU" [] "args" ["args"]) [
                    ident "GNU", punc "(", ident "args", punc "...", punc ")"
                  , spc, ident "args"
                  ]
            , testCase "#define GNU(fmt, args...) fmt" $
                readsTo (Runtime.Macro.variadicNamed "GNU" ["fmt"] "args" ["fmt"]) [
                    ident "GNU", punc "(", ident "fmt", punc ",", spc, ident "args"
                  , punc "...", punc ")", spc, ident "fmt"
                  ]
            ]
        , testGroup "keywords" [
              -- A macro definition may give a new meaning to a keyword, so the
              -- name accepts one.
              testCase "#define bool int" $
                readsTo (Runtime.Macro.objectLike "bool" ["int"]) [
                    kw "bool", spc, kw "int"
                  ]
              -- A parameter may be a keyword too. Whether @libclang@ calls
              -- @bool@ a keyword depends on the C standard, and the reader
              -- must not care; see 'Test.HsBindgen.Macro.Syntax.Clang', which
              -- runs this definition under both.
            , testCase "#define F(bool) bool" $
                readsTo (Runtime.Macro.functionLike "F" ["bool"] ["bool"]) [
                    ident "F", punc "(", kw "bool", punc ")", spc, kw "bool"
                  ]
            ]
        , testGroup "malformed" [
              testCase "no tokens at all" $
                failsToRead []
            , testCase "name is punctuation" $
                failsToRead [punc "+", spc, lit "1"]
              -- Function-like, since there is no white space before the @(@,
              -- and 1 is not a parameter name. Before the reader this fell
              -- back to an object-like macro with body @(1)@.
            , testCase "#define FOO(1)" $
                failsToRead [
                    ident "FOO", punc "(", lit "1", punc ")"
                  ]
            , testCase "#define F(x,) x" $
                failsToRead [
                    ident "F", punc "(", ident "x", punc ",", punc ")"
                  , spc, ident "x"
                  ]
            , testCase "#define F(..., x) x" $
                failsToRead [
                    ident "F", punc "(", punc "...", punc ",", spc, ident "x", punc ")"
                  , spc, ident "x"
                  ]
            , testCase "#define F(x x" $
                failsToRead [
                    ident "F", punc "(", ident "x", spc, ident "x"
                  ]
            , testCase "#define F(" $
                failsToRead [
                    ident "F", punc "("
                  ]
            , testCase "#define F(x" $
                failsToRead [
                    ident "F", punc "(", ident "x"
                  ]
              -- The name must be an identifier or a keyword; a literal is
              -- neither.
            , testCase "#define 1 2" $
                failsToRead [
                    lit "1", spc, lit "2"
                  ]
            ]
          -- A comment is a token, not white space: it separates the name from
          -- the @(@, and it stays in the body.
        , testGroup "comments" [
              testCase "#define FOO/*c*/(1)" $
                readsTo (Runtime.Macro.objectLike "FOO" ["/*c*/", "(", "1", ")"]) [
                    ident "FOO", comment "/*c*/", punc "(", lit "1", punc ")"
                  ]
            ]
          -- A line continuation splices the lines before the macro is parsed,
          -- so the tokens around it are still adjacent. @libclang@ reports the
          -- continuation as part of the punctuation token that follows it.
        , testGroup "line continuations" [
              testCase "between the name and the parameter list" $
                readsTo (Runtime.Macro.functionLike "F" ["x"] ["x"]) [
                    ident "F", punc "\\\n(", ident "x", punc ")", spc, ident "x"
                  ]
            , testCase "inside the parameter list" $
                readsTo (Runtime.Macro.functionLike "F" ["x", "y"] ["x"]) [
                    ident "F", punc "(", ident "x", punc "\\\n,", spc, ident "y"
                  , punc ")", spc, ident "x"
                  ]
            ]
        ]
    ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Assert that the pieces read into the given raw macro
--
-- Only the spellings are compared; the reader does not change them.
readsTo :: Runtime.Macro.Raw String -> [Piece] -> Assertion
readsTo expected pieces = Right expected @=? readRaw pieces

-- | Assert that the pieces do not read into a raw macro
failsToRead :: [Piece] -> Assertion
failsToRead pieces =
    case readRaw pieces of
      Left _    -> return ()
      Right raw -> assertFailure $
        "expected a parse failure, but got " ++ show (Runtime.Macro.render raw)

readRaw :: [Piece] -> Either String (Runtime.Macro.Raw String)
readRaw pieces =
    case readRawMacro (layout pieces) of
      Left  err -> Left err.macroParseError
      Right raw -> Right $ Text.unpack . spelling <$> raw
