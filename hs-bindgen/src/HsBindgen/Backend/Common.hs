{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Common backend functionality
module HsBindgen.Backend.Common (
    -- * Representation
    BackendRep(..)
  , Global(..)
  , SExpr(.., EInt)
  , SDecl(..)
  , SType(..)
  , Instance(..)
  , Record(..)
  , Newtype(..)
    -- * Full backend
  , Backend(..)
  , Fresh(..)
  ) where

import HsBindgen.Imports
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Util.PHOAS

{-------------------------------------------------------------------------------
  Backend representation
-------------------------------------------------------------------------------}

class BackendRep be where
  type Name be :: Star
  type Expr be :: Star
  type Decl be :: Star
  type Ty   be :: Star -- TOOD: rename Ty to Type

  -- | Resolve a global name.
  resolve      :: be -> Global -> Name be
  -- | Construct an expression.
  mkExpr       :: be -> SExpr be -> Expr be
  -- | Construct a declaration.
  mkDecl       :: be -> SDecl be -> Decl be
  -- | Construct a type.
  mkType       :: be -> SType be -> Ty   be

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
  | Div_class
  | Bits_class

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
  deriving stock (Eq)

-- | Simple expressions
data SExpr be =
    EGlobal Global
  | EVar (Fresh be Bound)
  | EFreeVar (HsName NsVar)
  | ECon (HsName NsConstr)
  | EIntegral Integer HsPrimType
  | EFloat Float
  | EDouble Double
  | EApp (SExpr be) (SExpr be)
  | EInfix Global (SExpr be) (SExpr be)
  | ELam (Maybe (Fresh be Bound)) (SExpr be)
  | ECase (SExpr be) [(HsName NsConstr, [Fresh be Bound], SExpr be)]
  | EInj (Expr be)

pattern EInt :: Int -> SExpr be
pattern EInt i <- EIntegral (fromInteger -> i) HsPrimCInt
  where
    EInt i = EIntegral (fromIntegral i) HsPrimCInt

-- | Simple declarations
data SDecl be =
    DVar (HsName NsVar) (Maybe (SType be)) (SExpr be)
  | DInst (Instance be)
  | DRecord (Record be)
  | DNewtype (Newtype be)
  | DDerivingNewtypeInstance (SType be)

-- | Simple types
data SType be =
    TGlobal Global
  | TCon (HsName NsTypeConstr)
  | TLit Natural
  | TFunTy (SType be) (SType be)
  | TApp (SType be) (SType be)
  | TTyVar (Fresh be Bound)
  | TForall
     { tforall_qtvs :: [Name be]
     , tforall_ctxt :: [SType be]
     , tforall_body :: SType be
     }

data Instance be = Instance {
      instanceClass :: Global
    , instanceType  :: HsName NsTypeConstr
    , instanceDecs  :: [(Global, SExpr be)]
    }

data Record be = Record {
      dataType   :: HsName NsTypeConstr
    , dataCon    :: HsName NsConstr
    , dataFields :: [(HsName NsVar, SType be)]
    }

data Newtype be = Newtype {
      newtypeName   :: HsName NsTypeConstr
    , newtypeCon    :: HsName NsConstr
    , newtypeField  :: HsName NsVar
    , newtypeType   :: SType be
    }

{-------------------------------------------------------------------------------
  Full backend
-------------------------------------------------------------------------------}

class (BackendRep be, Monad (M be)) => Backend be where
  data M be :: Star -> Star

  -- | Pick fresh variable
  --
  -- This is scoped because variables don't need to be /globally/ unique.
  fresh :: be -> HsName NsVar -> (Fresh be Bound -> M be a) -> M be a

newtype Fresh be a = Fresh { getFresh :: Name be }
