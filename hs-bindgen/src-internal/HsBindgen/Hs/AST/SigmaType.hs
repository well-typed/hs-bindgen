-- | Types used for translation of macro types.
--
-- These are completely separate from ones in "HsBindgen.Hs.AST.Type"
module HsBindgen.Hs.AST.SigmaType (
    SigmaType(..),
    PhiType(..),
    TauType(..),
    PredType(..),
    ATyCon(..),
    AClass(..),
) where

import DeBruijn (Ctx, Idx (..))

import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.Util.TestEquality

-- | A σ-type, of the form @forall tvs. ctxt => body@.
type SigmaType :: Star
data SigmaType where
  ForallTy ::
    { forallTyBinders :: Vec n NameHint
    , forallTy        :: PhiType n
    }
    -> SigmaType

deriving stock instance Show SigmaType

-- | A φ-type, of the form @ctxt => body@.
type PhiType :: Ctx -> Star
data PhiType ctx
  = QuantTy
  { quantTyCts  :: [PredType ctx]
  , quantTyBody :: TauType ctx
  }
  deriving stock (Generic, Show)

-- | A τ-type: no quantification or contexts (i.e. no @forall@, no @=>@ arrows).
type TauType :: Ctx -> Star
data TauType ctx
  = FunTy (TauType ctx) (TauType ctx)
  | TyVarTy (Idx ctx)
  | TyConAppTy ATyCon [TauType ctx]
  deriving stock (Eq, Generic, Show)

-- | A predicate/constraint τ-type.
type PredType :: Ctx -> Star
data PredType ctx
  = DictTy AClass [TauType ctx]
  | NomEqTy (TauType ctx) (TauType ctx)
  deriving stock Show

instance Eq (PredType ctx) where
  DictTy cls1 tys1 == DictTy cls2 tys2 =
    cls1 == cls2 && tys1 == tys2
  NomEqTy l1 r1 == NomEqTy l2 r2 =
    l1 == l2 && r1 == r2
  _ == _ = False

data ATyCon where
  ATyCon :: Macro.TyCon args Macro.Ty -> ATyCon
instance Show ATyCon where
  show ( ATyCon tc ) = show tc
instance Eq ATyCon where
  ATyCon tc1 == ATyCon tc2 =
    isJust $ equals2 tc1 tc2

data AClass where
  AClass :: Macro.TyCon args Macro.Ct -> AClass
instance Show AClass where
  show ( AClass tc ) = show tc
instance Eq AClass where
  AClass ( Macro.GenerativeTyCon ( Macro.ClassTyCon cls1 ) )
    ==
    AClass ( Macro.GenerativeTyCon ( Macro.ClassTyCon cls2 ) ) =
      isJust $ equals1 cls1 cls2
