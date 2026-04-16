-- | Interface for typechecked macro type expressions
--
-- Intended for qualified import
--
-- @
-- import C.Expr.Typecheck.Interface.Type qualified as T
-- @
module C.Expr.Typecheck.Interface.Type (
    Expr(..)
  , Fun(..)
  , ConversionError(..)
  , fromExpr
  )
  where

import Control.Exception (Exception)
import Control.Monad.Except ( Except, MonadError (..), runExcept)
import Data.Vec.Lazy (Vec(..))
import Data.Nat (Nat (..))

import C.Expr.Syntax qualified as M
import C.Expr.Syntax.Name

data Expr var =
    TypeLit M.TypeLit
  | Var var
  -- TODO <https://github.com/well-typed/hs-bindgen/issues/1521>
  --
  -- Change how we represent @const@.
  | App Fun (Expr var)
  deriving stock (Show)

data Fun =
    Pointer
  | Const
  deriving stock (Show)

data ConversionError =
    -- | Unexpected value literal (e.g., the integer @42@)
    UnexpectedValueLiteralInType String
    -- | Unexpected named function call in type
  | UnexpectedFunctionCallInType Name
    -- | A unary type function received multiple arguments
  | UnexpectedMultipleArgumentsToUnaryTypeFunction
    -- | Unexpected function application on a value (not a type)
  | UnexpectedValueFunctionApplicationInType String
  deriving stock (Show)

instance Exception ConversionError

fromExpr :: M.Expr p -> Either ConversionError (Expr Name)
fromExpr =  runExcept . go
  where
    go :: M.Expr p -> Except ConversionError (Expr Name)
    go = \case
      M.Term (M.Literal x) ->
        fromLit x
      M.Term (M.Var _ nm []) ->
        pure $ Var nm
      M.Term (M.Var _ nm _ ) ->
        throwError $ UnexpectedFunctionCallInType nm
      M.TyApp fun args -> do
        arg <- myHead args
        case fun of
          M.Pointer -> App Pointer <$> (go arg)
          M.Const   -> App Const   <$> (go arg)
      M.VaApp _ fun _ ->
        throwError $ UnexpectedValueFunctionApplicationInType (show fun)

    fromLit :: M.Literal -> Except ConversionError (Expr Name)
    fromLit = \case
      M.TypeLit x -> pure $ TypeLit x
      M.ValueLit x -> throwError $ UnexpectedValueLiteralInType (show x)

    myHead :: Vec ('S n) a -> Except ConversionError a
    myHead = \case
      (x ::: VNil)    -> pure x
      (_ ::: _ ::: _) -> throwError $ UnexpectedMultipleArgumentsToUnaryTypeFunction
