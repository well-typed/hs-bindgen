-- | Interface for typechecked macro value expressions
--
-- Intended for qualified import
--
-- @
-- import C.Expr.Typecheck.Interface.Value qualified as V
-- @
module C.Expr.Typecheck.Interface.Value (
    Expr(..)
  , ConversionError
  , fromExpr
  )
  where

import Control.Exception (Exception)
import Control.Monad.Except ( Except, MonadError (..), runExcept)
import Data.GADT.Compare (GEq (..))
import Data.Type.Equality ((:~:) (..))
import Data.Vec.Lazy (Vec)
import Data.Nat (Nat (..))

import C.Expr.Syntax qualified as M
import C.Expr.Syntax.Name

data Expr var =
    Literal M.ValueLit
  | Var var [Expr var]
  | forall n . App (M.VaFun (S n)) (Vec (S n) (Expr var))

deriving stock instance (Show var) => (Show (Expr var))
deriving stock instance Functor Expr
deriving stock instance Foldable Expr
deriving stock instance Traversable Expr

instance Eq var => Eq (Expr var) where
  Literal l1 == Literal l2 = l1 == l2
  Var v1 as1 == Var v2 as2 = v1 == v2 && as1 == as2
  App f1 xs1 == App f2 xs2 = case f1 `geq` f2 of
                               Just Refl -> xs1 == xs2
                               Nothing   -> False
  _ == _ = False

data ConversionError =
    -- | Unexpected type in a value expression (e.g., @int@)
    UnexpectedTypeInValue String
    -- | Unexpected function application on a type (not a value)
  | UnexpectedTypeFunctionApplicationInValue String
  deriving stock (Show)

instance Exception ConversionError

fromExpr :: M.Expr p -> Either ConversionError (Expr Name)
fromExpr = runExcept . go
  where
    go :: M.Expr p -> Except ConversionError (Expr Name)
    go = \case
      M.Term (M.Literal x) ->
        fromLit x
      M.Term (M.Var _ nm args) ->
        Var nm <$> mapM go args
      M.TyApp fun _ ->
        throwError $ UnexpectedTypeFunctionApplicationInValue (show fun)
      M.VaApp _ fun args ->
        App fun <$> mapM go args

    fromLit :: M.Literal -> Except ConversionError (Expr Name)
    fromLit = \case
      M.TypeLit x ->
        throwError $ UnexpectedTypeInValue (show x)
      M.ValueLit x -> pure . Literal $ case x of
        M.ValueInt y    -> M.ValueInt y
        M.ValueFloat y  -> M.ValueFloat y
        M.ValueChar y   -> M.ValueChar y
        M.ValueString y -> M.ValueString y
