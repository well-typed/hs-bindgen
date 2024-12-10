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
  | DDerivingNewtypeInstance ClosedType
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

deriving stock instance Show (SType ctx)

data Instance  = Instance {
      instanceClass :: Global
    , instanceType  :: HsName NsTypeConstr
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
