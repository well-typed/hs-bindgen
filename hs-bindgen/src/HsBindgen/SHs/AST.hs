-- | Simplified HS abstract syntax tree
module HsBindgen.SHs.AST (
    Global (..),
    ClosedExpr,
    SExpr (..),
    pattern EInt,
    SAlt (..),
    SDecl (..),
    ClosedType,
    SType (..),
    Instance (..),
    Record (..),
    Newtype (..),
) where

import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type

import DeBruijn

-- TODO: drop S prefix?

{-------------------------------------------------------------------------------
  Backend representation
-------------------------------------------------------------------------------}

data Global =
    Unit_type
  | Unit_constructor
  | Applicative_pure
  | Applicative_seq
  | Monad_return
  | Monad_seq
  | Storable_Storable
  | Storable_sizeOf
  | Storable_alignment
  | Storable_peekByteOff
  | Storable_pokeByteOff
  | Storable_peek
  | Storable_poke
  | Foreign_Ptr
  | ConstantArray

  | Eq_class
  | Ord_class
  | Num_class
  | Integral_class
  | Fractional_class
  | Bits_class
  | Div_class

  | Eq_eq
  | Eq_uneq
  | Ord_lt
  | Ord_le
  | Ord_gt
  | Ord_ge
  | Base_identity
  | Base_not
  | Base_and
  | Base_or
  | Bits_shiftL
  | Bits_shiftR
  | Bits_and
  | Bits_xor
  | Bits_or
  | Bits_complement
  | Num_negate
  | Num_add
  | Num_minus
  | Num_times
  | Div_div
  | Integral_rem

  | CFloat_constructor
  | CDouble_constructor
  | GHC_Float_castWord32ToFloat
  | GHC_Float_castWord64ToDouble

  | PrimType HsPrimType
  deriving stock (Eq, Show)

type ClosedExpr = SExpr EmptyCtx

-- | Simple expressions
type SExpr :: Ctx -> Star
data SExpr ctx =
    EGlobal Global
  | EBound (Idx ctx)
  | EFree (HsName NsVar)
  | ECon (HsName NsConstr)
  | EIntegral Integer HsPrimType
  | EFloat Float
  | EDouble Double
  | EApp (SExpr ctx) (SExpr ctx)
  | EInfix Global (SExpr ctx) (SExpr ctx)
  | ELam NameHint (SExpr (S ctx))
  | EUnusedLam (SExpr ctx)
  | ECase (SExpr ctx) [SAlt ctx]
  deriving stock (Show)

pattern EInt :: Int -> SExpr be
pattern EInt i <- EIntegral (fromInteger -> i) HsPrimCInt
  where
    EInt i = EIntegral (fromIntegral i) HsPrimCInt

-- | Case alternatives
data SAlt ctx where
    SAlt :: HsName NsConstr -> Add n ctx ctx' -> Vec n NameHint -> SExpr ctx' -> SAlt ctx

deriving stock instance Show (SAlt ctx)

-- | Simple declarations
data SDecl =
    DVar (HsName NsVar) (Maybe ClosedType) ClosedExpr
  | DInst Instance
  | DRecord Record
  | DNewtype Newtype
  | DDerivingNewtypeInstance ClosedType
  deriving stock (Show)

type ClosedType = SType EmptyCtx

-- | Simple types
type SType :: Ctx -> Star
data SType ctx =
    TGlobal Global
  | TCon (HsName NsTypeConstr)
  | TFun (SType ctx) (SType ctx)
  | TLit Natural
  | TBound (Idx ctx)
  | TApp (SType ctx) (SType ctx)
  | forall n ctx'. TForall (Vec n NameHint) (Add n ctx ctx') [SType ctx'] (SType ctx')

deriving stock instance Show (SType ctx)

data Instance  = Instance {
      instanceClass :: Global
    , instanceType  :: HsName NsTypeConstr
    , instanceDecs  :: [(Global, ClosedExpr)]
    }
  deriving stock (Show)

data Record = Record {
      dataType   :: HsName NsTypeConstr
    , dataCon    :: HsName NsConstr
    , dataFields :: [(HsName NsVar, ClosedType)]
    }
  deriving stock (Show)

data Newtype = Newtype {
      newtypeName   :: HsName NsTypeConstr
    , newtypeCon    :: HsName NsConstr
    , newtypeField  :: HsName NsVar
    , newtypeType   :: ClosedType
    }
  deriving stock (Show)
