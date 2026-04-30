-- | Unit tests for 'C.Expr.Parse.Expr.parseMacro'
--
-- Tests the full macro parser, focusing on:
--
-- * Type bodies vs expression bodies (disambiguation)
-- * Object-like and function-like expression macros
module Test.CExpr.Parse.Macro (tests) where

import Data.Either (isLeft, isRight)
import Data.Vec.Lazy (Vec (..))
import Test.Tasty
import Test.Tasty.HUnit

import Clang.CStandard

import C.Expr.Syntax

import Test.CExpr.Parse.Infra
import Clang.HighLevel.Types

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Parse.Macro" [
      testsWithCStd cStd | cStd <- [minBound .. maxBound :: CStandard]
    ]

testsWithCStd :: CStandard -> TestTree
testsWithCStd cStd = testGroup (show cStd) [
      testGroup "type bodies"               $ tests_typeBody         std
    , testGroup "function-like type bodies" $ tests_funcLikeTypeBody std
    , testGroup "expression bodies"         $ tests_exprBody         std
    , testGroup "disambiguation"            $ tests_disambiguation   std
    ]
  where
    std = ClangCStandard cStd DisableGnu

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | A fixed macro name token used in all tests
macroNameTok :: Token TokenSpelling
macroNameTok = ident "FOO"

-- | True when the macro expression looks like a type: it has an 'Type' or
-- 'TyApp' at its core (bare identifier cases are intentionally excluded
-- because a bare name is structurally identical in both type and expression
-- position after the refactor).
isTypeBody :: Either e Macro -> Bool
isTypeBody (Right Macro{macroExpr}) = case macroExpr of
    Term (Literal (TypeLit _)) -> True
    TyApp {}       -> True
    _               -> False
isTypeBody _ = False

-- | True when the macro expression is unambiguously an expression (a literal
-- or an operator application), not a type.
isExprBody :: Either e Macro -> Bool
isExprBody (Right Macro{macroExpr}) = case macroExpr of
    Term (Literal (ValueLit (ValueInt _)))    -> True
    Term (Literal (ValueLit (ValueFloat _)))  -> True
    Term (Literal (ValueLit (ValueChar _)))   -> True
    Term (Literal (ValueLit (ValueString _))) -> True
    VaApp {}         -> True
    _                 -> False
isExprBody _ = False

getMacroExpr :: Either e Macro -> Maybe (Expr Ps)
getMacroExpr (Right Macro{macroExpr}) = Just macroExpr
getMacroExpr _                        = Nothing

{-------------------------------------------------------------------------------
  Type bodies
-------------------------------------------------------------------------------}

tests_typeBody :: ClangCStandard -> [TestTree]
tests_typeBody cStd = [
      testCase "int" $
        -- #define FOO int
        getMacroExpr (checkMacro cStd [macroNameTok, kw "int"])
          @?= Just (tyLit (TypeInt Nothing (Just SizeInt)))
    , testCase "unsigned long" $
        -- #define FOO unsigned long
        getMacroExpr (checkMacro cStd [macroNameTok, kw "unsigned", kw "long"])
          @?= Just (tyLit (TypeInt (Just Unsigned) (Just SizeLong)))
    , testCase "const int*" $
        -- #define FOO const int *
        getMacroExpr (checkMacro cStd [macroNameTok, kw "const", kw "int", punc "*"])
          @?= Just (TyApp Pointer (TyApp Const (tyLit (TypeInt Nothing (Just SizeInt)) ::: VNil) ::: VNil))
    , testCase "void*" $
        -- #define FOO void *
        getMacroExpr (checkMacro cStd [macroNameTok, kw "void", punc "*"])
          @?= Just (TyApp Pointer (tyLit TypeVoid ::: VNil))
    , testCase "struct Foo" $
        -- #define FOO struct Foo
        getMacroExpr (checkMacro cStd [macroNameTok, kw "struct", ident "Foo"])
          @?= Just (Term $ Literal (TypeTagged TagStruct "Foo"))
    , testCase "size_t" $
        -- #define FOO size_t (bare identifier; typechecker decides it's a type)
        getMacroExpr (checkMacro cStd [macroNameTok, ident "size_t"])
          @?= Just (Term (Var NoXVar "size_t" []))
    , testCase "_Bool" $
        -- #define FOO _Bool
        getMacroExpr (checkMacro cStd [macroNameTok, kw "_Bool"])
          @?= Just (tyLit TypeBool)
    , testCase "_Bool" $
        -- #define FOO size_t const * const
        getMacroExpr (checkMacro cStd [macroNameTok, ident "size_t", kw "const", punc "*", kw "const" ])
          @?= Just (TyApp Const (TyApp Pointer (TyApp Const (Term (Var NoXVar "size_t" []) ::: VNil) ::: VNil) ::: VNil))
    ]

{-------------------------------------------------------------------------------
  Function-like type bodies (local args)
-------------------------------------------------------------------------------}

tests_funcLikeTypeBody :: ClangCStandard -> [TestTree]
tests_funcLikeTypeBody cStd = [
      testCase "PTR(T) = T*" $
        -- #define PTR(T) T*
        -- T is a local arg; the body is a pointer type parameterised by T.
        getMacroExpr (checkMacro cStd
            [ macroNameTok, punc "(", ident "T", punc ")"
            , ident "T", punc "*"
            ])
          @?= Just (TyApp Pointer (Term (LocalArg "T") ::: VNil))
    , testCase "CONST_PTR(T) = const T*" $
        -- #define CONST_PTR(T) const T*
        getMacroExpr (checkMacro cStd
            [ macroNameTok, punc "(", ident "T", punc ")"
            , kw "const", ident "T", punc "*"
            ])
          @?= Just (TyApp Pointer (TyApp Const (Term (LocalArg "T") ::: VNil) ::: VNil))
    , testCase "free var is not a local arg" $
        -- #define PTR(T) size_t*
        -- size_t is not a formal arg, so it stays as Var, not LocalArg.
        getMacroExpr (checkMacro cStd
            [ macroNameTok, punc "(", ident "T", punc ")"
            , ident "size_t", punc "*"
            ])
          @?= Just (TyApp Pointer (Term (Var NoXVar "size_t" []) ::: VNil))
    ]

{-------------------------------------------------------------------------------
  Expression bodies
-------------------------------------------------------------------------------}

tests_exprBody :: ClangCStandard -> [TestTree]
tests_exprBody cStd = [
      -- Object-like macros
      testCase "integer literal" $
        -- #define FOO 42
        assertBool "expected expression body" $
          isExprBody (checkMacro cStd [macroNameTok, lit "42"])
    , testCase "negative literal" $
        -- #define FOO -1
        assertBool "expected expression body" $
          isExprBody (checkMacro cStd [macroNameTok, punc "-", lit "1"])
    , testCase "arithmetic expression" $
        -- #define FOO 1 + 2
        assertBool "expected expression body" $
          isExprBody (checkMacro cStd [macroNameTok, lit "1", punc "+", lit "2"])
      -- Function-like macros
      -- A bare identifier body (e.g. x) is structurally identical for type and
      -- expression positions after the Expr unification; we just check it parses.
    , testCase "identity function" $
        -- #define FOO(x) x
        assertBool "expected parse success" $
          isRight $
            checkMacro cStd [
                macroNameTok, punc "(", ident "x", punc ")"
              , ident "x"
              ]
    , testCase "two-argument function" $
        -- #define FOO(a, b) a + b
        assertBool "expected expression body" $
          isExprBody $
            checkMacro cStd [
                macroNameTok
              , punc "(", ident "a", punc ",", ident "b", punc ")"
              , ident "a", punc "+", ident "b"
              ]
      -- Zero-argument function-like macro (#define FOO() 0) is
      -- parsed as objectLike since empty parens are not valid formalArgs;
      -- the result is still an expression body
    ]

{-------------------------------------------------------------------------------
  Disambiguation: types vs. expressions
-------------------------------------------------------------------------------}

tests_disambiguation :: ClangCStandard -> [TestTree]
tests_disambiguation cStd = [
      -- A bare identifier like 'size_t' is now structurally identical whether
      -- it came from the type parser or the expression parser (both produce
      -- Term (Var ...)).  We just verify that parsing succeeds.
      testCase "bare name parses successfully" $
        -- #define FOO size_t
        assertBool "expected parse success" $
          isRight (checkMacro cStd [macroNameTok, ident "size_t"])
    , testCase "void is a type body, not an identifier expression" $
        -- #define FOO void
        assertBool "expected type body" $
          isTypeBody (checkMacro cStd [macroNameTok, kw "void"])
      -- An integer literal cannot be a type, so it falls through to expression.
    , testCase "literal falls through to expression" $
        -- #define FOO 0
        assertBool "expected expression body" $
          isExprBody (checkMacro cStd [macroNameTok, lit "0"])
      -- An expression that starts with parenthesised identifiers could look
      -- like formal arguments.
    , testCase "parenthesised expression is not a type" $
        -- #define FOO (1)
        assertBool "expected expression body" $
          isExprBody (checkMacro cStd [macroNameTok, punc "(", lit "1", punc ")"])
      -- Completely unparseable input
    , testCase "bare comma fails" $
        -- #define FOO ,
        assertBool "expected failure" $
          isLeft (checkMacro cStd [macroNameTok, punc ","])
    , testCase "empty body fails" $
        -- #define FOO
        assertBool "expected failure" $
          isLeft (checkMacro cStd [macroNameTok])
    ]
