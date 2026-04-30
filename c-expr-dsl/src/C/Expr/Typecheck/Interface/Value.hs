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
import Data.Type.Equality ((:~:) (..))
import Data.Vec.Lazy (Vec)
import Data.Nat (Nat (..))

import C.Expr.Syntax qualified as M
import C.Expr.Syntax.Name
import C.Expr.Util.Panic

data Expr var =
    Literal M.ValueLit
  | LocalArg Name
  | Var var [Expr var]
  | forall n . App (M.VaFun (S n)) (Vec (S n) (Expr var))

deriving stock instance (Show var) => (Show (Expr var))
deriving stock instance Functor Expr
deriving stock instance Foldable Expr
deriving stock instance Traversable Expr

instance Eq var => Eq (Expr var) where
  Literal l1   == Literal l2   = l1 == l2
  LocalArg nm1 == LocalArg nm2 = nm1 == nm2
  Var v1 as1   == Var v2 as2   = v1 == v2 && as1 == as2
  App f1 xs1   == App f2 xs2   = case f1 `geq` f2 of
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
     forall m var p. Applicative m
  => (Name -> m var)
  -> M.Expr p
  -> m (Expr var)
fromExpr injectValue = go
  where
    go :: M.Expr p -> m (Expr var)
    go = \case
      M.Term (M.Literal x) ->
        fromLit x
      M.Term (M.LocalArg nm) ->
        pure $ LocalArg nm
      M.Term (M.Var _ nm args) ->
        Var <$> injectValue nm <*> traverse go args
      M.TyApp fun _ ->
        panicPure $ show $ UnexpectedTypeFunctionApplicationInValue (show fun)
      M.VaApp _ fun args ->
        App fun <$> traverse go args

    fromLit :: M.Literal -> m (Expr var)
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
