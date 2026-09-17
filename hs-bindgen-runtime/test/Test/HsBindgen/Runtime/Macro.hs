{-# LANGUAGE OverloadedStrings #-}

module Test.HsBindgen.Runtime.Macro (tests) where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@=?))

import HsBindgen.Runtime.Macro qualified as Macro

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.Runtime.Macro" [
      testGroup "render" [
          testCase "object-like" $
            rendersAs "#define FOO 1 + 2" $
              Macro.objectLike "FOO" ["1", "+", "2"]
        , testCase "object-like, empty body" $
            rendersAs "#define FOO" $
              Macro.objectLike "FOO" []
        , testCase "function-like" $
            rendersAs "#define ADD(x, y) x + y" $
              Macro.functionLike "ADD" ["x", "y"] ["x", "+", "y"]
        , testCase "function-like, no parameters" $
            rendersAs "#define NOW() 0" $
              Macro.functionLike "NOW" [] ["0"]
        , testCase "function-like, empty body" $
            rendersAs "#define IGNORE(x)" $
              Macro.functionLike "IGNORE" ["x"] []
        , testCase "variadic" $
            rendersAs "#define LOG(fmt, ...) printf ( fmt , __VA_ARGS__ )" $
              Macro.variadic "LOG" ["fmt"]
                ["printf", "(", "fmt", ",", "__VA_ARGS__", ")"]
        , testCase "variadic, no named parameters" $
            rendersAs "#define WARN(...) __VA_ARGS__" $
              Macro.variadic "WARN" [] ["__VA_ARGS__"]
          -- The GNU form keeps its own spelling: rendering it as
          -- @#define LOG(fmt, ...)@ would pass only @fmt@ on to @printf@.
        , testCase "GNU named variadic" $
            rendersAs "#define LOG(fmt, args...) printf ( fmt , args )" $
              Macro.variadicNamed "LOG" ["fmt"] "args"
                ["printf", "(", "fmt", ",", "args", ")"]
        , testCase "GNU named variadic, no named parameters" $
            rendersAs "#define WARN(args...) args" $
              Macro.variadicNamed "WARN" [] "args" ["args"]
        ]
    ]

rendersAs :: Text -> Macro.Raw Text -> Assertion
rendersAs expected raw = expected @=? Macro.render raw
