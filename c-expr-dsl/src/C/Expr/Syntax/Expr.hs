{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >=908
{-# LANGUAGE TypeAbstractions #-}
#endif

module C.Expr.Syntax.Expr (
    -- * Expressions
    Expr(..)
  , TyFun(..)
  , VaFun(..)
  , ValueLit(..)
  , Literal(..)
  , Term(..)
  ) where

import Data.GADT.Compare (GEq (geq))
import Data.Kind
import Data.Nat (Nat (..))
import Data.Proxy
import Data.Type.Equality (type (:~:) (..))
import Data.Type.Nat (SNatI)
import Data.Type.Nat qualified as Nat
import Data.Vec.Lazy (Vec (..))
import Data.Vec.Lazy qualified as Vec
import DeBruijn (Idx, Ctx)
import GHC.Generics (Generic)

import C.Expr.Syntax.Literals
import C.Expr.Syntax.Name
import C.Expr.Syntax.TTG
import C.Expr.Syntax.Type
import C.Expr.Util.TestEquality

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

-- | Macro expression
--
-- For examples, see the extensive test suite "Test.CExpr.Parse".
type Expr :: Ctx -> Pass -> Type
data Expr ctx p
  -- | A term that is not a function application.
  = Term ( Term ctx p )
  -- | Exactly saturated non-nullary type-level function application.
  --
  -- We don't need an extension point here, because we do not need to evaluate
  -- type functions in Haskell. 'XApp' may be unnecessary if we can remove
  -- 'FunValue'.
  | forall n. TyApp             ( TyFun ( S n ) ) ( Vec ( S n ) ( Expr ctx p ) )
  -- | Exactly saturated non-nullary function application.
  | forall n. VaApp !( XApp p ) ( VaFun ( S n ) ) ( Vec ( S n ) ( Expr ctx p ) )
deriving stock instance ( Show ( XVar p ), Show ( XApp p ) ) => Show ( Expr ctx p )

instance ( Eq ( XApp p ), Eq ( XVar p ) ) => Eq ( Expr ctx p ) where
  Term m1 == Term m2 = m1 == m2
  TyApp f1 args1 == TyApp f2 args2
    | Just Refl <- f1 `equals1` f2
    = args1 == args2
    | otherwise
    = False
  VaApp x1 f1 args1 == VaApp x2 f2 args2
    | Just Refl <- f1 `equals1` f2
    = x1 == x2 && args1 == args2
    | otherwise
    = False
  _ == _ = False

instance ( Ord ( XApp p ), Ord ( XVar p ) ) => Ord ( Expr ctx p ) where
  compare ( Term m1 ) ( Term m2 ) = compare m1 m2
  compare ( TyApp @_ @_ @n1 f1 args1 ) ( TyApp @_ @_ @n2 f2 args2 ) =
    Vec.withDict args1 $ Vec.withDict args2 $
    case Nat.eqNat @( S n1 ) @( S n2 ) of
      Just Refl -> compare f1 f2 <> compare args1 args2
      Nothing ->
        compare ( Nat.reflect @( S n1 ) Proxy ) ( Nat.reflect @( S n2 ) Proxy )
  compare ( VaApp @_ @_ @n1 x1 f1 args1 ) ( VaApp @_ @_ @n2 x2 f2 args2 ) =
    Vec.withDict args1 $ Vec.withDict args2 $
    case Nat.eqNat @( S n1 ) @( S n2 ) of
      Just Refl -> compare f1 f2 <> compare x1 x2 <> compare args1 args2
      Nothing ->
        compare ( Nat.reflect @( S n1 ) Proxy ) ( Nat.reflect @( S n2 ) Proxy )
  compare (Term {}) (TyApp {}) = LT
  compare (Term {}) (VaApp {}) = LT
  compare (TyApp {}) (Term {}) = GT
  compare (VaApp {}) (Term {}) = GT
  compare (TyApp {}) (VaApp {}) = LT
  compare (VaApp {}) (TyApp {}) = GT

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

data TyFun arity where
  -- | Pointer
  Pointer :: TyFun ( S Z )
  -- | Const
  Const   :: TyFun ( S Z )

  -- NB: make sure to update 'instance GEq TyFun'
  -- when adding a new constructor.

deriving stock instance Show ( TyFun arity )
deriving stock instance Eq   ( TyFun arity )
deriving stock instance Ord  ( TyFun arity )

instance GEq TyFun where
  geq Pointer Pointer = Just Refl
  geq Const   Const   = Just Refl
  geq _        _        = Nothing

data VaFun arity where
  -- | @+@
  MUnaryPlus  :: VaFun ( S Z )
  -- | @-@
  MUnaryMinus :: VaFun ( S Z )
  -- | @!@
  MLogicalNot :: VaFun ( S Z )
  -- | @~@
  MBitwiseNot :: VaFun ( S Z )
  -- | @*@
  MMult       :: VaFun ( S ( S Z ) )
  -- | @/@
  MDiv        :: VaFun ( S ( S Z ) )
  -- | @%@
  MRem        :: VaFun ( S ( S Z ) )
  -- | @+@
  MAdd        :: VaFun ( S ( S Z ) )
  -- | @-@
  MSub        :: VaFun ( S ( S Z ) )
  -- | @<<@
  MShiftLeft  :: VaFun ( S ( S Z ) )
  -- | @>>@
  MShiftRight :: VaFun ( S ( S Z ) )
  -- | @<@
  MRelLT      :: VaFun ( S ( S Z ) )
  -- | @<=@
  MRelLE      :: VaFun ( S ( S Z ) )
  -- | @>@
  MRelGT      :: VaFun ( S ( S Z ) )
  -- | @>=@
  MRelGE      :: VaFun ( S ( S Z ) )
  -- | @==@
  MRelEQ      :: VaFun ( S ( S Z ) )
  -- | @!=@
  MRelNE      :: VaFun ( S ( S Z ) )
  -- | @&@
  MBitwiseAnd :: VaFun ( S ( S Z ) )
  -- | @^@
  MBitwiseXor :: VaFun ( S ( S Z ) )
  -- | @|@
  MBitwiseOr  :: VaFun ( S ( S Z ) )
  -- | @&&@
  MLogicalAnd :: VaFun ( S ( S Z ) )
  -- | @||@
  MLogicalOr  :: VaFun ( S ( S Z ) )
  -- | Tuples
  MTuple      :: SNatI n => VaFun ( S ( S n ) )

  -- NB: make sure to update 'instance GEq TyFun'
  -- when adding a new constructor.

deriving stock instance Show ( VaFun arity )
deriving stock instance Eq   ( VaFun arity )
deriving stock instance Ord  ( VaFun arity )

instance GEq VaFun where
  geq MUnaryPlus  MUnaryPlus  = Just Refl
  geq MUnaryMinus MUnaryMinus = Just Refl
  geq MLogicalNot MLogicalNot = Just Refl
  geq MBitwiseNot MBitwiseNot = Just Refl
  geq MMult       MMult       = Just Refl
  geq MDiv        MDiv        = Just Refl
  geq MRem        MRem        = Just Refl
  geq MAdd        MAdd        = Just Refl
  geq MSub        MSub        = Just Refl
  geq MShiftLeft  MShiftLeft  = Just Refl
  geq MShiftRight MShiftRight = Just Refl
  geq MRelLT      MRelLT      = Just Refl
  geq MRelLE      MRelLE      = Just Refl
  geq MRelGT      MRelGT      = Just Refl
  geq MRelGE      MRelGE      = Just Refl
  geq MRelEQ      MRelEQ      = Just Refl
  geq MRelNE      MRelNE      = Just Refl
  geq MBitwiseAnd MBitwiseAnd = Just Refl
  geq MBitwiseXor MBitwiseXor = Just Refl
  geq MBitwiseOr  MBitwiseOr  = Just Refl
  geq MLogicalAnd MLogicalAnd = Just Refl
  geq MLogicalOr  MLogicalOr  = Just Refl
  geq (MTuple @i) (MTuple @j)
    | Just Refl <- Nat.eqNat @i @j
    = Just Refl
  geq _           _           = Nothing

{-------------------------------------------------------------------------------
  Terms
-------------------------------------------------------------------------------}

-- | Value literal
data ValueLit =
    ValueInt    IntegerLiteral
  | ValueFloat  FloatingLiteral
  | ValueChar   CharLiteral
  | ValueString StringLiteral
  deriving stock (Eq, Ord, Show)

type Literal :: Type
data Literal =
    TypeLit  TypeLit

  | ValueLit ValueLit

    -- NB: We do not accept untagged identifiers here, since they may correspond
    -- to a value expression, not a type expression.

    -- | An elaborated type literal: @struct tag@, @union tag@, @enum tag@
  | TypeTagged !TagKind !Name
  deriving stock (Eq, Ord, Show)

type Term :: Ctx -> Pass -> Type
data Term ctx p =
    -- | Literal (i.e., constant) type or value
    Literal Literal

    -- | Reference to a function parameter
    --
    -- A parameter De Bruijn index and name of the enclosing function-like
    -- macro. For example, the second @X@ in @#define F(X) X + 1@, with index 0.
    --
    -- The language defined in @c-expr-dsl@ is a first-order language, so there
    -- is no need for arguments. For example, we do not support
    -- @#define MACRO(F,X) F(X)@.
  | LocalParam (Idx ctx)

    -- | Free variable: another macro or typedef
  | Var ( XVar p ) Name [Expr ctx p]
  deriving stock Generic
deriving stock instance ( Eq   ( XApp p ), Eq   ( XVar p ) ) => Eq   ( Term ctx p )
deriving stock instance ( Ord  ( XApp p ), Ord  ( XVar p ) ) => Ord  ( Term ctx p )
deriving stock instance ( Show ( XApp p ), Show ( XVar p ) ) => Show ( Term ctx p )
