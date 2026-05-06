-- | Interface for typechecked macro value expressions
--
-- Intended for qualified import
--
-- @
-- import C.Expr.Typecheck.Interface.Value qualified as V
-- @
module C.Expr.Typecheck.Interface.Value (
    Expr(..)
  , fromExpr
  )
  where

import Control.Exception (Exception)
import Data.GADT.Compare (GEq (..))
import Data.Nat (Nat (..))
import Data.Type.Equality ((:~:) (..))
import Data.Vec.Lazy (Vec)
import DeBruijn (Idx)

import C.Expr.Syntax qualified as M
import C.Expr.Syntax.Name
import C.Expr.Util.Panic

data Expr ctx var =
    Literal M.ValueLit
  | LocalParam (Idx ctx)
  | Var var [Expr ctx var]
  | forall n . App (M.VaFun (S n)) (Vec (S n) (Expr ctx var))

deriving stock instance (Show var) => (Show (Expr ctx var))
deriving stock instance Functor     (Expr ctx)
deriving stock instance Foldable    (Expr ctx)
deriving stock instance Traversable (Expr ctx)

instance Eq var => Eq (Expr ctx var) where
  Literal l1    == Literal l2    = l1 == l2
  LocalParam n1 == LocalParam n2 = n1 == n2
  Var v1 as1    == Var v2 as2    = v1 == v2 && as1 == as2
  App f1 xs1    == App f2 xs2    =
    case f1 `geq` f2 of
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

fromExpr ::
     forall ctx m var p. Applicative m
  => (Name -> m var)
  -> M.Expr ctx p
  -> m (Expr ctx var)
fromExpr injectValue = go
  where
    go :: M.Expr ctx p -> m (Expr ctx var)
    go = \case
      M.Term (M.Literal x) ->
        fromLit x
      M.Term (M.LocalParam i) ->
        pure $ LocalParam i
      M.Term (M.Var _ nm args) ->
        Var <$> injectValue nm <*> traverse go args
      M.TyApp fun _ ->
        panicPure $ show $ UnexpectedTypeFunctionApplicationInValue (show fun)
      M.VaApp _ fun args ->
        App fun <$> traverse go args

    fromLit :: M.Literal -> m (Expr ctx var)
    fromLit = \case
      M.TypeLit x ->
        panicPure $ show $ UnexpectedTypeInValue (show x)
      M.TypeTagged tag nm ->
        panicPure $ show $ UnexpectedTypeInValue (show (tag, nm))
      M.ValueLit x -> pure . Literal $ case x of
        M.ValueInt y    -> M.ValueInt y
        M.ValueFloat y  -> M.ValueFloat y
        M.ValueChar y   -> M.ValueChar y
        M.ValueString y -> M.ValueString y
