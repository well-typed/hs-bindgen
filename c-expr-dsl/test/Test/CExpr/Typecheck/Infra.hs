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
  , mvar
  ) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Vec.Lazy (Vec (..))
import Test.Tasty.HUnit

import C.Expr.Syntax
import C.Expr.Typecheck (MacroTcError, MacroTcResult (..), TypeEnv, tcMacro)
import C.Type qualified as Runtime
import Data.Functor.Identity (Identity (..))

type MacDef = (Name, [Name], Expr Ps)

tcMacroSimple ::
     TypeEnv
  -> Name
  -> [Name]
  -> Expr Ps
  -> Either MacroTcError (MacroTcResult Name Name)
tcMacroSimple tyEnv name args expr =
    runIdentity $
      tcMacro tyEnv pure injectTaggedName pure name args expr
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
    go _   []                              = []
    go env ((name, args, body) : rest) =
      let result = tcMacroSimple env name args body
          env'   = case result of
                     Right (MacroTcValueExpr quant _) ->
                       Map.insert name quant env
                     _ -> env
      in  result : go env' rest

classifyOne ::
     Name
  -> [Name]
  -> Expr Ps
  -> Either MacroTcError (MacroTcResult Name Name)
classifyOne n args body = tcMacroSimple Map.empty n args body

isTypeMacro :: MacroTcResult a b -> Bool
isTypeMacro (MacroTcTypeExpr _ _) = True
isTypeMacro _                     = False

isValueMacro :: MacroTcResult a b -> Bool
isValueMacro (MacroTcValueExpr _ _) = True
isValueMacro _                      = False

assertTypeMacro :: Show e => Either e (MacroTcResult a b) -> Assertion
assertTypeMacro = \case
    Left e  -> assertFailure $ "expected MacroTcTypeExpr, got Left: " ++ show e
    Right r -> assertBool "expected MacroTcTypeExpr" (isTypeMacro r)

assertValueMacro :: Show e => Either e (MacroTcResult a b) -> Assertion
assertValueMacro = \case
    Left e  -> assertFailure $ "expected MacroTcValueExpr, got Left: " ++ show e
    Right r -> assertBool "expected MacroTcValueExpr" (isValueMacro r)

tyLit :: TypeLit -> Expr Ps
tyLit = Term . Literal . TypeLit

constOf :: Expr Ps -> Expr Ps
constOf e = TyApp Const (e ::: VNil)

ptrOf :: Expr Ps -> Expr Ps
ptrOf e = TyApp Pointer (e ::: VNil)

-- | Construct an integer literal expression with a 'signed int' type hint.
-- Suitable for tests where the exact inferred integer type is not the subject
-- under test.
intLit :: Integer -> Expr Ps
intLit n = Term $ Literal $ ValueLit $ ValueInt $
    IntegerLiteral
      (Text.pack (show n))
      (Runtime.Int Runtime.Signed)
      n

add :: Expr Ps -> Expr Ps -> Expr Ps
add a b = VaApp NoXApp MAdd (a ::: b ::: VNil)

shiftLeft :: Expr Ps -> Expr Ps -> Expr Ps
shiftLeft a b = VaApp NoXApp MShiftLeft (a ::: b ::: VNil)

mvar :: Name -> Expr Ps
mvar n = Term $ Var NoXVar n []
