{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >=908
{-# LANGUAGE TypeAbstractions #-}
#endif

module C.Expr.Syntax.Expr (
    -- ** Expressions
    MExpr(..)
  , MFun(..)
  , MTerm(..)
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
import GHC.Generics (Generic)

import C.Expr.Syntax.Literals
import C.Expr.Syntax.Name
import C.Expr.Syntax.TTG
import C.Expr.Util.TestEquality

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

-- | Macro expression
type MExpr :: Pass -> Type
data MExpr p
  -- | A term that is not a function application.
  = MTerm ( MTerm p )
  -- | Exactly saturated non-nullary function application.
  | forall n. MApp !( XApp p ) ( MFun ( S n ) ) ( Vec ( S n ) ( MExpr p ) )
deriving stock instance ( Show ( XVar p ), Show ( XApp p ) ) => Show ( MExpr p )

instance ( Eq ( XApp p ), Eq ( XVar p ) ) => Eq ( MExpr p ) where
  MTerm m1 == MTerm m2 = m1 == m2
  MApp x1 f1 args1 == MApp x2 f2 args2
    | Just Refl <- f1 `equals1` f2
    = x1 == x2 && args1 == args2
    | otherwise
    = False
  _ == _ = False
instance ( Ord ( XApp p ), Ord ( XVar p ) ) => Ord ( MExpr p ) where
  compare ( MTerm m1 ) ( MTerm m2 ) = compare m1 m2
  compare ( MApp @_ @n1 x1 f1 args1 ) ( MApp @_ @n2 x2 f2 args2 ) =
    Vec.withDict args1 $ Vec.withDict args2 $
    case Nat.eqNat @( S n1 ) @( S n2 ) of
      Just Refl -> compare f1 f2 <> compare x1 x2 <> compare args1 args2
      Nothing ->
        compare ( Nat.reflect @( S n1 ) Proxy ) ( Nat.reflect @( S n2 ) Proxy )
  compare (MTerm {}) (MApp {}) = LT
  compare (MApp {}) (MTerm {}) = GT

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

data MFun arity where
  -- | @+@
  MUnaryPlus  :: MFun ( S Z )
  -- | @-@
  MUnaryMinus :: MFun ( S Z )
  -- | @!@
  MLogicalNot :: MFun ( S Z )
  -- | @~@
  MBitwiseNot :: MFun ( S Z )
  -- | @*@
  MMult       :: MFun ( S ( S Z ) )
  -- | @/@
  MDiv        :: MFun ( S ( S Z ) )
  -- | @%@
  MRem        :: MFun ( S ( S Z ) )
  -- | @+@
  MAdd        :: MFun ( S ( S Z ) )
  -- | @-@
  MSub        :: MFun ( S ( S Z ) )
  -- | @<<@
  MShiftLeft  :: MFun ( S ( S Z ) )
  -- | @>>@
  MShiftRight :: MFun ( S ( S Z ) )
  -- | @<@
  MRelLT      :: MFun ( S ( S Z ) )
  -- | @<=@
  MRelLE      :: MFun ( S ( S Z ) )
  -- | @>@
  MRelGT      :: MFun ( S ( S Z ) )
  -- | @>=@
  MRelGE      :: MFun ( S ( S Z ) )
  -- | @==@
  MRelEQ      :: MFun ( S ( S Z ) )
  -- | @!=@
  MRelNE      :: MFun ( S ( S Z ) )
  -- | @&@
  MBitwiseAnd :: MFun ( S ( S Z ) )
  -- | @^@
  MBitwiseXor :: MFun ( S ( S Z ) )
  -- | @|@
  MBitwiseOr  :: MFun ( S ( S Z ) )
  -- | @&&@
  MLogicalAnd :: MFun ( S ( S Z ) )
  -- | @||@
  MLogicalOr  :: MFun ( S ( S Z ) )
  -- | Tuples
  MTuple      :: SNatI n => MFun ( S ( S n ) )

  -- NB: make sure to update 'instance GEq MFun'
  -- when adding a new constructor.

deriving stock instance Show ( MFun arity )
deriving stock instance Eq   ( MFun arity )
deriving stock instance Ord  ( MFun arity )

instance GEq MFun where
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

type MTerm :: Pass -> Type
data MTerm p =

    -- | Integer literal
    MInt IntegerLiteral

    -- | Floating-point literal
  | MFloat FloatingLiteral

    -- | Character literal
  | MChar CharLiteral

    -- | String literal
  | MString StringLiteral

    -- | Variable or function/macro call
    --
    -- This might be a macro argument, or another macro.
  | MVar ( XVar p ) Name [MExpr p]

    -- | Stringizing
    --
    -- See
    --
    -- * Section 6.10.3.2, "The # operator" of the spec
    -- * <https://gcc.gnu.org/onlinedocs/cpp/Stringizing.html>
  | MStringize Name

    -- | Concatenation
    --
    -- See
    --
    -- * Section 6.10.3.3, "The ## operator" of the spec
    -- * <https://gcc.gnu.org/onlinedocs/cpp/Concatenation.html>
  | MConcat ( MTerm p ) ( MTerm p )
  deriving stock Generic
deriving stock instance ( Eq ( XApp p ), Eq ( XVar p ) ) => Eq ( MTerm p )
deriving stock instance ( Ord ( XApp p ), Ord ( XVar p ) ) => Ord ( MTerm p )
deriving stock instance ( Show ( XApp p ), Show ( XVar p ) ) => Show ( MTerm p )
