-- | Simplified HS abstract syntax tree
module HsBindgen.SHs.AST (
-- TODO: drop S prefix?
    Global (..),
    ClosedExpr,
    SExpr (..),
    pattern EInt,
    SAlt (..),
    PatExpr (..),
    SDecl (..),
    ClosedType,
    SType (..),
    Instance (..),
    Field (..),
    Record (..),
    EmptyData (..),
    Newtype (..),
    ForeignImport (..),
    PatternSynonym (..),
) where

import DeBruijn (Ctx, EmptyCtx, Idx, Add)

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Hs.AST.Strategy qualified as Hs
import HsBindgen.Hs.AST.Type
import HsBindgen.Hs.Origin qualified as Origin
import HsBindgen.Imports
import HsBindgen.Language.Haskell
import HsBindgen.NameHint

import C.Char qualified

{-------------------------------------------------------------------------------
  Backend representation
-------------------------------------------------------------------------------}

data Global =
    Tuple_type Word
  | Tuple_constructor Word
  | Applicative_pure
  | Applicative_seq
  | Monad_return
  | Monad_seq
  | StaticSize_class
  | ReadRaw_class
  | WriteRaw_class
  | Storable_class
  | Storable_sizeOf
  | Storable_alignment
  | Storable_peekByteOff
  | Storable_pokeByteOff
  | Storable_peek
  | Storable_poke
  | Foreign_Ptr
  | Ptr_constructor
  | Foreign_FunPtr
  | ConstantArray
  | IO_type
  | HasFlexibleArrayMember_class
  | HasFlexibleArrayMember_offset
  | Bitfield_peekBitOffWidth
  | Bitfield_pokeBitOffWidth
  | CharValue_tycon
  | CharValue_constructor
  | CharValue_fromAddr
  | ByteArray_setUnionPayload
  | ByteArray_getUnionPayload
  | CAPI_with
  | CAPI_allocaAndPeek
  | ConstantArray_withPtr

    -- Other type classes
  | Bits_class
  | Bounded_class
  | Enum_class
  | Eq_class
  | FiniteBits_class
  | Floating_class
  | Fractional_class
  | Integral_class
  | Ix_class
  | Num_class
  | Ord_class
  | Read_class
  | Read_readPrec
  | Read_readList
  | Read_readListPrec
  | Real_class
  | RealFloat_class
  | RealFrac_class
  | Show_class
  | Show_showsPrec

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

  | NonEmpty_constructor
  | NonEmpty_singleton
  | Map_fromList
  | Read_readListDefault
  | Read_readListPrecDefault

  | CEnum_class
  | CEnumZ_tycon
  | CEnum_toCEnum
  | CEnum_fromCEnum
  | CEnum_declaredValues
  | CEnum_showsUndeclared
  | CEnum_readPrecUndeclared
  | CEnum_isDeclared
  | CEnum_mkDeclared
  | SequentialCEnum_class
  | SequentialCEnum_minDeclaredValue
  | SequentialCEnum_maxDeclaredValue
  | CEnum_declaredValuesFromList
  | CEnum_showsCEnum
  | CEnum_showsWrappedUndeclared
  | CEnum_readPrecCEnum
  | CEnum_readPrecWrappedUndeclared
  | CEnum_seqIsDeclared
  | CEnum_seqMkDeclared
  | AsCEnum_type
  | AsSequentialCEnum_type

  | ByteArray_type
  | SizedByteArray_type
  | PrimType HsPrimType
  deriving stock (Eq, Ord, Show)

type ClosedExpr = SExpr EmptyCtx

-- | Simple expressions
type SExpr :: Ctx -> Star
data SExpr ctx =
    EGlobal Global
  | EBound (Idx ctx)
  | EFree (HsName NsVar)
  | ECon (HsName NsConstr)
  | EIntegral Integer (Maybe HsPrimType)
  | EFloat Float HsPrimType -- ^ Type annotation to distinguish Float/CFLoat
  | EDouble Double HsPrimType
  | EChar C.Char.CharValue
  | EString String
  | ECString ByteArray
  | EApp (SExpr ctx) (SExpr ctx)
  | EInfix Global (SExpr ctx) (SExpr ctx)
  | ELam NameHint (SExpr (S ctx))
  | EUnusedLam (SExpr ctx)
  | ECase (SExpr ctx) [SAlt ctx]
  | ETup [SExpr ctx]
  | EList [SExpr ctx]
  deriving stock (Show)

-- | Pattern&Expressions
--
-- There is common sublanguage between term and pattern expressions.
-- This is that common sublanguage.
-- Expressions ('SExpr') and full patterns (may) have more cases,
-- but for bidirectional pattern synonyms only the common bases
-- can be used.
--
-- For now 'PatExpr' is quite small, as we don't need much.
type PatExpr :: Star
data PatExpr
  = PEApps (HsName NsConstr) [PatExpr] -- head of pattern application cannot be variable.
  | PELit Integer
  deriving stock (Show)

pattern EInt :: Int -> SExpr be
pattern EInt i <- EIntegral (fromInteger -> i) (Just HsPrimInt)
  where
    EInt i = EIntegral (fromIntegral i) (Just HsPrimInt)

-- | Case alternatives
data SAlt ctx where
    SAlt :: HsName NsConstr -> Add n ctx ctx' -> Vec n NameHint -> SExpr ctx' -> SAlt ctx

deriving stock instance Show (SAlt ctx)

-- | Simple declarations
data SDecl =
    DVar (HsName NsVar) ClosedType ClosedExpr
  | DInst Instance
  | DRecord Record
  | DNewtype Newtype
  | DEmptyData EmptyData
  | DDerivingInstance (Hs.Strategy ClosedType) ClosedType
  | DForeignImport ForeignImport
  | DPatternSynonym PatternSynonym
  | DComment String
  | DCSource String
  deriving stock (Show)

type ClosedType = SType EmptyCtx

-- | Simple types
type SType :: Ctx -> Star
data SType ctx =
    TGlobal Global
  | TCon (HsName NsTypeConstr)
  | TFun (SType ctx) (SType ctx)
  | TLit Natural
  | TExt ExtHsRef BindingSpec.TypeSpec
  | TBound (Idx ctx)
  | TApp (SType ctx) (SType ctx)
  | forall n ctx'. TForall (Vec n NameHint) (Add n ctx ctx') [SType ctx'] (SType ctx')

infixl 9 `TApp`

deriving stock instance Show (SType ctx)

data Instance  = Instance {
      instanceClass :: Global
    , instanceArgs  :: [ClosedType]
    , instanceTypes :: [(Global, ClosedType, ClosedType)]
    , instanceDecs  :: [(Global, ClosedExpr)]
    }
  deriving stock (Show)

data Field = Field {
      fieldName   :: HsName NsVar
    , fieldType   :: ClosedType
    , fieldOrigin :: Origin.Field
    }
  deriving stock (Show)

data Record = Record {
      dataType   :: HsName NsTypeConstr
    , dataCon    :: HsName NsConstr
    , dataFields :: [Field]
    , dataOrigin :: Origin.Decl Origin.Struct
    , dataDeriv  :: [(Hs.Strategy ClosedType, [Global])]
    }
  deriving stock (Show)

data EmptyData = EmptyData {
      emptyDataName   :: HsName NsTypeConstr
    , emptyDataOrigin :: Origin.Decl Origin.EmptyData
    }
  deriving stock (Show)

data Newtype = Newtype {
      newtypeName   :: HsName NsTypeConstr
    , newtypeCon    :: HsName NsConstr
    , newtypeField  :: Field
    , newtypeOrigin :: Origin.Decl Origin.Newtype
    , newtypeDeriv  :: [(Hs.Strategy ClosedType, [Global])]
    }
  deriving stock (Show)

data ForeignImport = ForeignImport
    { foreignImportName     :: HsName NsVar
    , foreignImportType     :: ClosedType
    , foreignImportOrigName :: Text
    , foreignImportHeader   :: FilePath -- TODO: https://github.com/well-typed/hs-bindgen/issues/333
    , foreignImportOrigin   :: Origin.ForeignImport
    }
  deriving stock (Show)

data PatternSynonym = PatternSynonym
    { patSynName   :: HsName NsConstr
    , patSynType   :: ClosedType
    , patSynRHS    :: PatExpr
    , patSynOrigin :: Origin.PatSyn
    }
  deriving stock (Show)
