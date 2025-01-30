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
    Field (..),
    Record (..),
    Newtype (..),
    ForeignImport (..),
    PatternSynonym (..),
) where

import HsBindgen.Imports
import HsBindgen.NameHint
import HsBindgen.Hs.AST qualified as Hs
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
  | HasFlexibleArrayMember_class
  | HasFlexibleArrayMember_offset

    -- | Primitive (unboxed) type equality
  | NomEq_class

  | Not_class
  | Not_not
  | Logical_class
  | Logical_and
  | Logical_or
  | RelEq_class
  | RelEq_eq
  | RelEq_uneq
  | RelOrd_class
  | RelOrd_lt
  | RelOrd_le
  | RelOrd_gt
  | RelOrd_ge
  | Plus_class
  | Plus_resTyCon
  | Plus_plus
  | Minus_class
  | Minus_resTyCon
  | Minus_negate
  | Add_class
  | Add_resTyCon
  | Add_add
  | Sub_class
  | Sub_resTyCon
  | Sub_minus
  | Mult_class
  | Mult_resTyCon
  | Mult_mult
  | Div_class
  | Div_div
  | Div_resTyCon
  | Rem_class
  | Rem_resTyCon
  | Rem_rem
  | Complement_class
  | Complement_resTyCon
  | Complement_complement
  | Bitwise_class
  | Bitwise_resTyCon
  | Bitwise_and
  | Bitwise_or
  | Bitwise_xor
  | Shift_class
  | Shift_resTyCon
  | Shift_shiftL
  | Shift_shiftR

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
  | EIntegral Integer (Maybe HsPrimType)
  | EFloat Float
  | EDouble Double
  | EApp (SExpr ctx) (SExpr ctx)
  | EInfix Global (SExpr ctx) (SExpr ctx)
  | ELam NameHint (SExpr (S ctx))
  | EUnusedLam (SExpr ctx)
  | ECase (SExpr ctx) [SAlt ctx]
  deriving stock (Show)

pattern EInt :: Int -> SExpr be
pattern EInt i <- EIntegral (fromInteger -> i) (Just HsPrimCInt)
  where
    EInt i = EIntegral (fromIntegral i) (Just HsPrimCInt)

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
  | DDerivingInstance Hs.Strategy ClosedType
  | DForeignImport ForeignImport
  | DPatternSynonym PatternSynonym
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
    , instanceArgs  :: [ClosedType]
    , instanceDecs  :: [(Global, ClosedExpr)]
    }
  deriving stock (Show)

data Field = Field {
      fieldName   :: HsName NsVar
    , fieldType   :: ClosedType
    , fieldOrigin :: Hs.FieldOrigin
    }
  deriving stock (Show)

data Record = Record {
      dataType   :: HsName NsTypeConstr
    , dataCon    :: HsName NsConstr
    , dataFields :: [Field]
    , dataOrigin :: Hs.StructOrigin
    }
  deriving stock (Show)

data Newtype = Newtype {
      newtypeName   :: HsName NsTypeConstr
    , newtypeCon    :: HsName NsConstr
    , newtypeField  :: Field
    , newtypeOrigin :: Hs.NewtypeOrigin
    }
  deriving stock (Show)

data ForeignImport = ForeignImport
    { foreignImportName     :: HsName NsVar
    , foreignImportType     :: ClosedType
    , foreignImportOrigName :: Text
    , foreignImportHeader   :: FilePath -- TODO: https://github.com/well-typed/hs-bindgen/issues/333
    , foreignImportOrigin   :: Hs.ForeignImportDeclOrigin
    }
  deriving stock (Show)

data PatternSynonym = PatternSynonym
    { patSynName   :: HsName NsConstr
    , patSynType   :: ClosedType
    , patSynRHS    :: ClosedExpr -- TODO: This should be Pat(tern)
    , patSynOrigin :: Hs.PatSynOrigin
    }
  deriving stock (Show)
