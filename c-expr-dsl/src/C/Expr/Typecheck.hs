module C.Expr.Typecheck (
    tcMacro
  , CheckedMacroTypeExpr(..)
  , CheckedMacroValueExpr(..)
  , MacroTcResult(..)
  , TypeEnv
  , buildTypedefEnv

    -- * Errors
  , MacroTcError(..)
  , pprMacroTcError
  ) where

import Data.Type.Equality ((:~:) (..))
import Data.Type.Nat qualified as Nat
import Data.Vec.Lazy (Vec)
import Data.Vec.Lazy qualified as Vec

import C.Expr.Syntax
import C.Expr.Syntax qualified as M
import C.Expr.Typecheck.Expr
import C.Expr.Typecheck.Interface.Type qualified as T
import C.Expr.Typecheck.Interface.Value qualified as V
import C.Expr.Typecheck.Type

{-------------------------------------------------------------------------------
  Typechecking macros: public entry point
-------------------------------------------------------------------------------}

-- | The macro is C type expression (e.g., @#define FOO int@).
data CheckedMacroTypeExpr var = CheckedMacroTypeExpr{
      macroTypeBody :: T.Expr var
    , macroTypeType :: Quant (FunValue, Type Ty)
    }
  deriving stock (Eq, Show)

-- | The macro is a value expression (e.g., @#define BAR 1@).
data CheckedMacroValueExpr var = forall ctx. CheckedMacroValueExpr{
      macroValueParams :: Vec ctx Name
    , macroValueBody   :: V.Expr ctx var
    , macroValueType   :: Quant (FunValue, Type Ty)
    }
instance Eq var => Eq (CheckedMacroValueExpr var) where
  (CheckedMacroValueExpr @_ @c1 p1 b1 t1) == (CheckedMacroValueExpr @_ @c2 p2 b2 t2) =
    t1 == t2 && (
      Vec.withDict p1 $ Vec.withDict p2 $
        case Nat.eqNat @c1 @c2 of
          Just Refl -> p1 == p2 && b1 == b2
          Nothing   -> False
    )
deriving stock instance Show var => Show (CheckedMacroValueExpr var)

-- | The result of typechecking a macro is either a type or value expression
data MacroTcResult tvar vvar =
    MacroTcTypeExpr  (CheckedMacroTypeExpr  tvar)
  | MacroTcValueExpr (CheckedMacroValueExpr vvar)

deriving stock instance (Show tvar, Show vvar) => Show (MacroTcResult tvar vvar)

-- | Typecheck a macro
tcMacro ::
     forall m ctx tvar vvar.
     Applicative m
  => TypeEnv
  -> (Name -> m tvar)              -- ^ Inject type
  -> (M.TagKind -> Name -> m tvar) -- ^ Inject tagged type
  -> (Name -> m vvar)              -- ^ Inject value
  -> Name                          -- ^ macro name
  -> Vec ctx Name                  -- ^ macro local params
  -> Expr ctx Ps                   -- ^ macro body
  -> m (Either MacroTcError (MacroTcResult tvar vvar))
tcMacro tyEnv injectType injectTaggedType injectValue name params expr =
    case tcExpr tyEnv name params expr of
      Left  err -> pure $ Left err
      Right res -> Right <$> classify res
  where
    classify ::
         (Type Ty, Quant (FunValue, Type Ty))
      -> m (MacroTcResult tvar vvar)
    classify = \case
      (MacroTypeTy, quant) ->
        let toRes texpr = MacroTcTypeExpr $ CheckedMacroTypeExpr texpr quant
        in  toRes <$> T.fromExpr injectType injectTaggedType expr
      (_, quant)           ->
        let toRes vexpr = MacroTcValueExpr $ CheckedMacroValueExpr params vexpr quant
        in  toRes <$> V.fromExpr injectValue expr
