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
  | Foreign_FunPtr
  | ConstantArray
  | IO_type

    -- | Primitive (unboxed) type equality
  | NomEq_class

  | CNot_class
  | CNot_resTyCon
  | CNot_not
  | CLogical_class
  | CLogical_resTyCon
  | CLogical_and
  | CLogical_or
  | CEq_class
  | CEq_eq
  | CEq_uneq
  | COrd_class
  | COrd_lt
  | COrd_le
  | COrd_gt
  | COrd_ge
  | CPlus_class
  | CPlus_resTyCon
  | CPlus_plus
  | CMinus_class
  | CMinus_resTyCon
  | CMinus_negate
  | CAdd_class
  | CAdd_resTyCon
  | CAdd_add
  | CSub_class
  | CSub_resTyCon
  | CSub_minus
  | CMult_class
  | CMult_resTyCon
  | CMult_mult
  | CDiv_class
  | CDiv_resTyCon
  | CDiv_div
  | CRem_class
  | CRem_resTyCon
  | CRem_rem
  | CComplement_class
  | CComplement_resTyCon
  | CComplement_complement
  | CBits_class
  | CBits_resTyCon
  | CBits_and
  | CBits_or
  | CBits_xor
  | CShift_class
  | CShift_resTyCon
  | CShift_shiftL
  | CShift_shiftR

  | IntLike_tycon
  | FloatLike_tycon

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
  | DEmptyData (HsName NsTypeConstr)
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

infixl 9 `TApp`

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
