module C.Expr.Typecheck (
    tcMacro
  , MacroTcResult(..)
  , TypeEnv

    -- * Errors
  , MacroTcError(..)
  , pprMacroTcError
  ) where

import Data.Vec.Lazy qualified as Vec

import C.Expr.Syntax
import C.Expr.Typecheck.Expr
import C.Expr.Typecheck.Type
import C.Expr.Typecheck.Interface.Type qualified as T
import C.Expr.Typecheck.Interface.Value qualified as V
import Control.Exception (Exception (displayException))

{-------------------------------------------------------------------------------
  Typechecking macros: public entry point
-------------------------------------------------------------------------------}

-- | The result of typechecking a macro is either a type or value expression
data MacroTcResult =
    MacroTcTypeExpr  (T.Expr Name) (Quant (FunValue, Type Ty))
    -- ^ The macro is C type expression (e.g., @#define FOO int@).
  | MacroTcValueExpr (V.Expr Name) (Quant (FunValue, Type Ty))
    -- ^ The macro is a value expression (e.g., @#define BAR 1@).
  deriving stock Show

-- | Typecheck a macro
tcMacro
  :: TypeEnv
  -> Name    -- ^ macro name
  -> [Name]  -- ^ macro args
  -> Expr Ps -- ^ macro body
  -> Either MacroTcError MacroTcResult
tcMacro tyEnv name args expr =
    Vec.reifyList args $ \args' -> do
      checkedExpr <- tcExpr tyEnv name args' expr
      pure $ classify checkedExpr
  where
    classify :: (Type Ty, Quant (FunValue, Type Ty)) -> MacroTcResult
    classify = \case
      (MacroTypeTy, quant) ->
        let typeExpr  = panicOnFailure $ T.fromExpr expr
        in  MacroTcTypeExpr typeExpr  quant
      (_, quant)           ->
        let valueExpr = panicOnFailure $ V.fromExpr expr
        in  MacroTcValueExpr valueExpr quant

    panicOnFailure :: Exception e => Either e a -> a
    panicOnFailure = \case
      Left  e ->
        let msg = unlines [
                "Classification and conversion of typechecked macro failed:"
              , displayException e
              , ""
              -- TODO <https://github.com/well-typed/hs-bindgen/issues/943>
              --
              -- Amend when `c-expr-dsl` is separated from `hs-bindgen`.
              , "Please report this as a bug at https://github.com/well-typed/hs-bindgen/issues/"
              ]
        in  error msg
      Right x -> x
