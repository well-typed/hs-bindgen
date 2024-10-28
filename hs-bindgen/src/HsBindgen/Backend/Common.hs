-- | Common backend functionality
module HsBindgen.Backend.Common (
    -- * Representation
    BackendRep(..)
  , Global(..)
  , SExpr(..)
  , SDecl(..)
  , SType(..)
  , Instance(..)
  , Data(..)
    -- * Full backend
  , Backend(..)
  , Fresh(..)
  ) where

import Data.Kind

import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type
import HsBindgen.Util.PHOAS

{-------------------------------------------------------------------------------
  Backend representation
-------------------------------------------------------------------------------}

class BackendRep be where
  type Name be :: Type
  type Expr be :: Type
  type Decl be :: Type
  type Ty   be :: Type

  resolve :: be -> Global   -> Name be  -- ^ Resolve name
  mkExpr  :: be -> SExpr be -> Expr be  -- ^ Construct expression
  mkDecl  :: be -> SDecl be -> Decl be  -- ^ Construct declaration
  mkType  :: be -> SType be -> Ty   be  -- ^ Construct type

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

  | PrimType HsPrimType

-- | Simple expressions
data SExpr be =
    EGlobal Global
  | EVar (Fresh be Bound)
  | ECon (HsName NsConstr)
  | EInt Int
  | EApp (SExpr be) (SExpr be)
  | EInfix Global (SExpr be) (SExpr be)
  | ELam (Maybe (Fresh be Bound)) (SExpr be)
  | ECase (SExpr be) [(HsName NsConstr, [Fresh be Bound], SExpr be)]
  | EInj (Expr be)

-- | Simple declarations
data SDecl be =
    DVar (Name be) (SExpr be)
  | DInst (Instance be)
  | DData (Data be) -- TOOD: rename to Record

-- | Simple types
data SType be =
    TGlobal Global
  | TCon (HsName NsTypeConstr)
  | TApp (SType be) (SType be)

data Instance be = Instance {
      instanceClass :: Global
    , instanceType  :: HsName NsTypeConstr
    , instanceDecs  :: [(Global, SExpr be)]
    }

-- TODO: rename to Record
data Data be = Data {
      dataType   :: HsName NsTypeConstr
    , dataCon    :: HsName NsConstr
    , dataFields :: [(HsName NsVar, SType be)]
    }

{-------------------------------------------------------------------------------
  Full backend
-------------------------------------------------------------------------------}

class (BackendRep be, Monad (M be)) => Backend be where
  data M be :: Type -> Type

  -- | Pick fresh variable
  --
  -- This is scoped because variables don't need to be /globally/ unique.
  fresh :: be -> HsName NsVar -> (Fresh be Bound -> M be a) -> M be a

newtype Fresh be a = Fresh { getFresh :: Name be }
