module C.Expr.Typecheck (
    tcMacro
  , MacroTcResult(..)
  , TypeEnv
  , buildTypedefEnv

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
import C.Expr.Syntax qualified as M

{-------------------------------------------------------------------------------
  Typechecking macros: public entry point
-------------------------------------------------------------------------------}

-- | The result of typechecking a macro is either a type or value expression
data MacroTcResult tvar vvar =
    -- | The macro is C type expression (e.g., @#define FOO int@).
    MacroTcTypeExpr  (Quant (FunValue, Type Ty)) (T.Expr tvar)

    -- | The macro is a value expression (e.g., @#define BAR 1@).
  | MacroTcValueExpr (Quant (FunValue, Type Ty)) (V.Expr vvar)

deriving stock instance (Show tvar, Show vvar) => Show (MacroTcResult tvar vvar)

-- | Typecheck a macro
tcMacro ::
     forall m tvar vvar.
     Applicative m
  => TypeEnv
  -> (Name -> m tvar)              -- ^ Inject type
  -> (M.TagKind -> Name -> m tvar) -- ^ Inject tagged type
  -> (Name -> m vvar)              -- ^ Inject value
  -> Name                          -- ^ macro name
  -> [Name]                        -- ^ macro args
  -> Expr Ps                       -- ^ macro body
  -> m (Either MacroTcError (MacroTcResult tvar vvar))
tcMacro tyEnv injectType injectTaggedType injectValue name args expr =
    Vec.reifyList args $ \args' -> do
      case tcExpr tyEnv name args' expr of
        Left  err -> pure $ Left err
        Right res -> Right <$> classify res
  where
    classify ::
         (Type Ty, Quant (FunValue, Type Ty))
      -> m (MacroTcResult tvar vvar)
    classify = \case
      (MacroTypeTy, quant) ->
        MacroTcTypeExpr quant <$>
          T.fromExpr injectType injectTaggedType expr
      (_, quant)           ->
        MacroTcValueExpr quant <$>
          V.fromExpr injectValue expr
