{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >=908
{-# LANGUAGE TypeAbstractions #-}
#endif

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
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1950>
      --
      -- We should not require 'FunValue's for value-like expressions.
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
data MacroTcResult var =
    MacroTcTypeExpr  (CheckedMacroTypeExpr  var)
  | MacroTcValueExpr (CheckedMacroValueExpr var)

deriving stock instance (Show var) => Show (MacroTcResult var)

-- | Typecheck a macro
tcMacro ::
     forall m ctx var.
     Monad m
  => TypeEnv
  -> (Name -> m var)              -- ^ Inject type
  -> (M.TagKind -> Name -> m var) -- ^ Inject tagged type
  -> (Name -> m var)              -- ^ Inject value
  -> Name                         -- ^ macro name
  -> Vec ctx Name                 -- ^ macro local params
  -> Expr ctx Ps                  -- ^ macro body
  -> m (Either MacroTcError (MacroTcResult var))
tcMacro tyEnv injectType injectTaggedType injectValue name params expr =
    case tcExpr tyEnv name params expr of
      Left  err -> pure $ Left err
      Right res -> classify res
  where
    classify ::
         (Type Ty, Quant (FunValue, Type Ty))
      -> m (Either MacroTcError (MacroTcResult var))
    classify = \case
      (MacroTypeTy, quant)
        | not (Vec.null params) ->
          pure $ Left $ TcUnsupportedTypeWithLocalParameters name (Vec.toList params)
        | otherwise -> do
          texpr <- T.fromExpr injectType injectTaggedType expr
          pure $ if isIncompleteType texpr then
            Left $ TcIncompleteTypeMacro name
          else
            Right $ MacroTcTypeExpr $ CheckedMacroTypeExpr texpr quant
      (_, quant) -> do
        vexpr <- V.fromExpr injectValue expr
        pure $ Right $ MacroTcValueExpr $ CheckedMacroValueExpr params vexpr quant

    -- | An incomplete type at the top level of a type-like macro: 'void' or
    -- 'const'-wrapped 'void'. Pointer indirection makes the type complete, so
    -- 'void *' (and 'const void *') are not flagged.
    isIncompleteType :: T.Expr var -> Bool
    isIncompleteType = \case
        T.TypeLit M.TypeVoid -> True
        T.App T.Const e      -> isIncompleteType e
        _                    -> False
