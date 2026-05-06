module Test.CExpr.Typecheck.Infra (
    -- * Macro definitions
    MacDef
  , runTcSeq
  , classifyOne
    -- * Classification predicates
  , isTypeMacro
  , isValueMacro
    -- * Assertion helpers
  , assertTypeMacro
  , assertValueMacro
    -- * Expression helpers
  , tyLit
  , constOf
  , ptrOf
  , intLit
  , add
  , shiftLeft
  , mlocal
  , mvar
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Map.Strict qualified as Map
import Data.Nat (Nat (..))
import Data.Text qualified as Text
import Data.Vec.Lazy (Vec (..))
import DeBruijn (Idx (..))
import Test.Tasty.HUnit

import C.Type qualified as Runtime

import C.Expr.Syntax
import C.Expr.Typecheck

type MacDef = (Name, Expr Z Ps)

tcMacroSimple ::
     forall ctx.
     TypeEnv
  -> Name
  -> Vec ctx Name
  -> Expr ctx Ps
  -> Either MacroTcError (MacroTcResult Name Name)
tcMacroSimple tyEnv name params expr =
    runIdentity $
      tcMacro tyEnv pure injectTaggedName pure name params expr
  where
    tagToName :: TagKind -> Name
    tagToName = \case
      TagStruct -> "struct"
      TagUnion  -> "union"
      TagEnum   -> "enum"

    injectTaggedName :: TagKind -> Name -> Identity Name
    injectTaggedName tag nm = pure $ tagToName tag <> " " <> nm

runTcSeq :: [MacDef] -> [Either MacroTcError (MacroTcResult Name Name)]
runTcSeq = go Map.empty
  where
    go :: TypeEnv -> [MacDef] -> [Either MacroTcError (MacroTcResult Name Name)]
    go _   []                    = []
    go env ((name, body) : rest) =
      let result = tcMacroSimple env name VNil body
          env'   = case result of
                     Right (MacroTcValueExpr vexpr) ->
                       Map.insert name (macroValueType vexpr) env
                     _ -> env
      in  result : go env' rest

classifyOne ::
     forall ctx.
     Name
  -> Vec ctx Name
  -> Expr ctx Ps
  -> Either MacroTcError (MacroTcResult Name Name)
classifyOne n params body = tcMacroSimple Map.empty n params body

isTypeMacro :: MacroTcResult a b -> Bool
isTypeMacro (MacroTcTypeExpr _) = True
isTypeMacro _                   = False

isValueMacro :: MacroTcResult a b -> Bool
isValueMacro (MacroTcValueExpr _) = True
isValueMacro _                    = False

assertTypeMacro :: Show e => Either e (MacroTcResult a b) -> Assertion
assertTypeMacro = \case
    Left e  -> assertFailure $ "expected MacroTcTypeExpr, got Left: " ++ show e
    Right r -> assertBool "expected MacroTcTypeExpr" (isTypeMacro r)

assertValueMacro :: Show e => Either e (MacroTcResult a b) -> Assertion
assertValueMacro = \case
    Left e  -> assertFailure $ "expected MacroTcValueExpr, got Left: " ++ show e
    Right r -> assertBool "expected MacroTcValueExpr" (isValueMacro r)

tyLit :: TypeLit -> Expr ctx Ps
tyLit = Term . Literal . TypeLit

constOf :: Expr ctx Ps -> Expr ctx Ps
constOf e = TyApp Const (e ::: VNil)

ptrOf :: Expr ctx Ps -> Expr ctx Ps
ptrOf e = TyApp Pointer (e ::: VNil)

-- | Construct an integer literal expression with a 'signed int' type hint.
-- Suitable for tests where the exact inferred integer type is not the subject
-- under test.
intLit :: Integer -> Expr ctx Ps
intLit n = Term $ Literal $ ValueLit $ ValueInt $
    IntegerLiteral
      (Text.pack (show n))
      (Runtime.Int Runtime.Signed)
      n

add :: Expr ctx Ps -> Expr ctx Ps -> Expr ctx Ps
add a b = VaApp NoXApp MAdd (a ::: b ::: VNil)

shiftLeft :: Expr ctx Ps -> Expr ctx Ps -> Expr ctx Ps
shiftLeft a b = VaApp NoXApp MShiftLeft (a ::: b ::: VNil)

mlocal :: Idx ctx -> Expr ctx Ps
mlocal i = Term $ LocalParam i

mvar :: Name -> Expr ctx Ps
mvar n = Term $ Var NoXVar n []
