-- | Tests for the glue between 'splitMacro' and @c-expr-dsl@
--
-- 'HsBindgen.Macro.cExpr' assembles a @c-expr-dsl@ macro from the split that
-- 'HsBindgen.Macro.Syntax.splitMacro' produced and the body that
-- @C.Expr.Parse.parseMacroBody@ parsed. These tests pin the two conventions the
-- glue owns: the order of the parameters, and what happens to a variadic macro.
module Test.HsBindgen.Macro.CExpr (tests) where

import Data.Foldable (toList)
import DeBruijn (idxToInt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@=?))

import C.Expr.Syntax qualified as CExpr

import Clang.CStandard (CStandard (C17), ClangCStandard (ClangCStandard),
                        Gnu (DisableGnu))

import HsBindgen.Macro (cExpr)
import HsBindgen.Macro.Error (MacroParseError (..))
import HsBindgen.Macro.Interface qualified as Macro
import HsBindgen.Macro.Syntax (splitMacro)

import Test.HsBindgen.Macro.Infra

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Macro.CExpr" [
      testCase "#define CONST(X, Y) X" sourceOrder
    , testCase "#define VA(...) 0" variadicDeclined
    ]

-- | Parameters reach @c-expr-dsl@ in source order
--
-- The glue passes them in source order and @parseMacroBody@ reverses them
-- internally, so the /last/ parameter is the innermost binder: in
-- @CONST(X, Y)@, the body @X@ is de Bruijn index 1, not 0. Nothing else states
-- this, and it silently swaps the arguments of every function-like macro if it
-- changes.
sourceOrder :: Assertion
sourceOrder =
    case parse [ ident "CONST", punc "(", ident "X", punc ",", spc, ident "Y"
               , punc ")", spc, ident "X"
               ] of
      Left err -> assertFailure err.macroParseError
      Right CExpr.Macro{CExpr.macroParams = params, CExpr.macroExpr = body} -> do
        ["X", "Y"] @=? toList params
        case body of
          CExpr.Term (CExpr.LocalParam idx) -> 1 @=? idxToInt idx
          other -> assertFailure $
            "expected a reference to a parameter, but got " ++ show other

-- | A variadic macro is declined, with an explanation
--
-- @CExpr.Macro@ has no representation for @...@; without the check,
-- @__VA_ARGS__@ would parse as a free variable and the macro would translate to
-- something that quietly ignores the variadic arguments.
variadicDeclined :: Assertion
variadicDeclined =
    case parse [ident "VA", punc "(", punc "...", punc ")", spc, lit "0"] of
      Left err   -> "variadic macros are not supported" @=? err.macroParseError
      Right mcro -> assertFailure $
        "expected the macro to be declined, but got " ++ show mcro

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Split the pieces and hand the result to the @CExpr@ macro language
parse :: [Piece] -> Either MacroParseError (CExpr.Macro ())
parse pieces = do
    raw    <- splitMacro (layout pieces)
    parsed <- (cExpr cStd).parse raw
    pure parsed.unwrap
  where
    cStd :: ClangCStandard
    cStd = ClangCStandard C17 DisableGnu
