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

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Nat (Nat (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vec.Lazy (Vec (..))
import Data.Void (Void)
import DeBruijn (Idx (..))
import Test.Tasty.HUnit

import C.Type qualified as Runtime

import C.Expr.Syntax
import C.Expr.Typecheck

import Test.CExpr.Util

type MacDef = (Name, Expr Z Ps)

-- | Run 'tcMacros' on a single macro
--
-- Convenience for tests that exercise one macro in isolation; threads no
-- typedef context.
classifyOne ::
     forall ctx.
     Name
  -> Vec ctx Name
  -> Expr ctx Ps
  -> MacroTcResult Void Name
classifyOne name params body =
    case Map.toList (runTcMacros [Macro fakeLoc name params body]) of
      ((_, x):_) -> x
      []         -> error "classifyOne: unexpected empty typecheck result"

-- | Typecheck a sequence of nullary macros in order, threading each successful
-- result into the typing environment for later macros to reference.
runTcSeq :: [MacDef] -> Map Name (MacroTcResult Void Name)
runTcSeq defs =
    runTcMacros [Macro fakeLoc nm VNil body | (nm, body) <- defs]

-- | Shared 'tcMacros' driver for the test helpers: no typedefs in scope,
-- variable injection is the identity, tagged-type injection renders as
-- @"<tag> <name>"@ to match the textual form expected by tests.
--
-- Tagged-type injection never fails here (uses 'Void' as the inject error
-- type), so 'MacroTcInjectError' results are not produced.
runTcMacros :: [Macro] -> Map Name (MacroTcResult Void Name)
runTcMacros macros =
    tcMacros Set.empty (const id) id injectTaggedName macros
  where
    injectTaggedName :: Applicative m => TagKind -> Name -> m Name
    injectTaggedName tag nm = pure $ tagToName tag <> " " <> nm

    tagToName :: TagKind -> Name
    tagToName = \case
      TagStruct -> "struct"
      TagUnion  -> "union"
      TagEnum   -> "enum"

isTypeMacro :: MacroTcResult e a -> Bool
isTypeMacro (MacroTcTypeExpr _) = True
isTypeMacro _                   = False

isValueMacro :: MacroTcResult e a -> Bool
isValueMacro (MacroTcValueExpr _) = True
isValueMacro _                    = False

assertTypeMacro :: (Show e, Show a) => MacroTcResult e a -> Assertion
assertTypeMacro r =
    assertBool ("expected MacroTcTypeExpr, got: " ++ show r) (isTypeMacro r)

assertValueMacro :: (Show e, Show a) => MacroTcResult e a -> Assertion
assertValueMacro r =
    assertBool ("expected MacroTcValueExpr, got: " ++ show r) (isValueMacro r)

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
