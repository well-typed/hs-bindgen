module HsBindgen.C.AST.Macro (
    MExpr(..)
  , MFun(..)
  , MTerm(..)
  ) where

import Data.Kind qualified as Hs
import Data.GADT.Compare (GEq)
import Data.Nat (Nat(..))
import Data.Type.Nat (SNatI)
import Data.Vec.Lazy (Vec(..))
import GHC.Generics
import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Literal
import HsBindgen.C.Tc.Macro.Type

type MExpr :: Pass -> Hs.Type
data MExpr p
  = MTerm ( MTerm p )
  | forall n. MApp !( XApp p ) ( MFun ( 'S n ) ) ( Vec ( 'S n ) ( MExpr p ) )
instance ( Show ( XVar p ), Show ( XApp p ) ) => Show ( MExpr p )
instance ( Eq ( XApp p ), Eq ( XVar p ) ) => Eq ( MExpr p )
instance ( Ord ( XApp p ), Ord ( XVar p ) ) => Ord ( MExpr p )

type MFun :: Nat -> Hs.Type
data MFun arity where
  MUnaryPlus  :: MFun ( 'S Z )
  MUnaryMinus :: MFun ( 'S Z )
  MLogicalNot :: MFun ( 'S Z )
  MBitwiseNot :: MFun ( 'S Z )
  MMult       :: MFun ( 'S ( 'S Z ) )
  MDiv        :: MFun ( 'S ( 'S Z ) )
  MRem        :: MFun ( 'S ( 'S Z ) )
  MAdd        :: MFun ( 'S ( 'S Z ) )
  MSub        :: MFun ( 'S ( 'S Z ) )
  MShiftLeft  :: MFun ( 'S ( 'S Z ) )
  MShiftRight :: MFun ( 'S ( 'S Z ) )
  MRelLT      :: MFun ( 'S ( 'S Z ) )
  MRelLE      :: MFun ( 'S ( 'S Z ) )
  MRelGT      :: MFun ( 'S ( 'S Z ) )
  MRelGE      :: MFun ( 'S ( 'S Z ) )
  MRelEQ      :: MFun ( 'S ( 'S Z ) )
  MRelNE      :: MFun ( 'S ( 'S Z ) )
  MBitwiseAnd :: MFun ( 'S ( 'S Z ) )
  MBitwiseXor :: MFun ( 'S ( 'S Z ) )
  MBitwiseOr  :: MFun ( 'S ( 'S Z ) )
  MLogicalAnd :: MFun ( 'S ( 'S Z ) )
  MLogicalOr  :: MFun ( 'S ( 'S Z ) )
  MTuple      :: SNatI n => MFun ( 'S ( 'S n ) )

instance Show ( MFun arity )
instance Eq   ( MFun arity )
instance Ord  ( MFun arity )
instance GEq MFun

type MTerm :: Pass -> Hs.Type
data MTerm p
  = MInt IntegerLiteral
  | MFloat FloatingLiteral
  | MChar CharLiteral
  | MString StringLiteral
  | MVar (XVar p) CName [MExpr p]
  | MStringize CName
  | MConcat (MTerm p) (MTerm p)
instance ( Show ( XApp p ), Show ( XVar p ) ) => Show ( MTerm p )
instance ( Eq ( XApp p ), Eq ( XVar p ) ) => Eq ( MTerm p )
instance ( Ord ( XApp p ), Ord ( XVar p ) ) => Ord ( MTerm p )
instance Generic ( MTerm p )
