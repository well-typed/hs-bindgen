{-# LANGUAGE MagicHash #-}

-- | Simplified HS abstract syntax tree
module HsBindgen.Backend.SHs.AST (
    Global (..)
  , ClosedExpr
  , SExpr (..)
  , pattern EInt
  , SAlt (..)
  , PatExpr (..)
    -- TODO: drop S prefix?
  , SDecl (..)
  , TypeSynonym (..)
  , Pragma (..)
  , ClosedType
  , SType (..)
  , Binding (..)
  , Instance (..)
  , Field (..)
  , Record (..)
  , EmptyData (..)
  , DerivingInstance (..)
  , Newtype (..)
  , ForeignImport (..)
  , Safety (..)
  , Parameter (..)
  , Result (..)
  , PatternSynonym (..)
  ) where

import Data.Type.Nat (Nat1)
import DeBruijn (Add, Ctx, EmptyCtx, Idx)

import C.Char qualified as CExpr.Runtime

import HsBindgen.Backend.Hs.AST.Strategy qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

{-------------------------------------------------------------------------------
  Backend representation
-------------------------------------------------------------------------------}

data Global =
    Tuple_type Word
  | Tuple_constructor Word
  | Applicative_pure
  | Applicative_seq
  | Maybe_just
  | Maybe_nothing
  | Monad_return
  | Monad_seq
  | StaticSize_class
  | ReadRaw_class
  | WriteRaw_class
  | ToFunPtr_class
  | ToFunPtr_toFunPtr
  | FromFunPtr_class
  | FromFunPtr_fromFunPtr
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
  | Foreign_plusPtr
  | Foreign_StablePtr
  | ConstantArray
  | IncompleteArray
  | IO_type
  | CharValue_tycon
  | CharValue_constructor
  | CharValue_fromAddr
  | ByteArray_setUnionPayload
  | ByteArray_getUnionPayload
  | CAPI_with
  | CAPI_allocaAndPeek
  | ConstantArray_withPtr
  | IncompleteArray_withPtr

    -- Flexible array members
  | FlexibleArrayMember_Offset_class
  | FlexibleArrayMember_Offset_offset
  | WithFlexibleArrayMember

    -- HasCField
  | HasCField_class
  | HasCField_CFieldType
  | HasCField_offset#
  | HasCField_ptrToCField
  | HasCField_pokeCField
  | HasCField_peekCField

    -- HasCBitfield
  | HasCBitfield_class
  | HasCBitfield_CBitfieldType
  | HasCBitfield_bitOffset#
  | HasCBitfield_bitWidth#
  | HasCBitfield_ptrToCBitfield
  | HasCBitfield_pokeCBitfield
  | HasCBitfield_peekCBitfield
  | HasCBitfield_BitfieldPtr

    -- HasField
  | HasField_class
  | HasField_getField

    -- Proxy
  | Proxy_type
  | Proxy_constructor

    -- HasBaseForeignType
  | HasBaseForeignType_class
  | HasBaseForeignType_fromBaseForeignType
  | HasBaseForeignType_toBaseForeignType
  | HasBaseForeignType_castFunPtrFromBaseForeignType
  | HasBaseForeignType_castFunPtrToBaseForeignType

    -- Functor
  | Functor_fmap

    -- Unsafe
  | IO_unsafePerformIO

    -- ConstPtr
  | ConstPtr_type
  | ConstPtr_constructor
  | ConstPtr_unConstPtr

    -- Prim
  | Prim_class
  | Prim_sizeOf#
  | Prim_alignment#
  | Prim_indexByteArray#
  | Prim_readByteArray#
  | Prim_writeByteArray#
  | Prim_indexOffAddr#
  | Prim_readOffAddr#
  | Prim_writeOffAddr#
  | Prim_add#
  | Prim_mul#

    -- Other type classes
  | Bitfield_class
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
  | Block_type
  | PrimType HsPrimType
  | ComplexType
  deriving stock (Eq, Ord, Show)

type ClosedExpr = SExpr EmptyCtx

-- | Simple expressions
type SExpr :: Ctx -> Star
data SExpr ctx =
    EGlobal Global
  | EBound (Idx ctx)
  | EFree (Hs.Name Hs.NsVar)
  | ECon (Hs.Name Hs.NsConstr)
  | EUnboxedIntegral Integer
  | EIntegral Integer (Maybe HsPrimType)
  | EFloat Float HsPrimType -- ^ Type annotation to distinguish Float/CFLoat
  | EDouble Double HsPrimType
  | EChar CExpr.Runtime.CharValue
  | EString String
  | ECString ByteArray
  | EApp (SExpr ctx) (SExpr ctx)
  | EInfix Global (SExpr ctx) (SExpr ctx)
  | ELam NameHint (SExpr (S ctx))
  | EUnusedLam (SExpr ctx)
  | ECase (SExpr ctx) [SAlt ctx]
  | ETup [SExpr ctx]
  | EUnboxedTup [SExpr ctx]
  | EList [SExpr ctx]
    -- | Type application using \@
  | ETypeApp (SExpr ctx) ClosedType
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
  = PEApps (Hs.Name Hs.NsConstr) [PatExpr] -- head of pattern application cannot be variable.
  | PELit Integer
  deriving stock (Show)

pattern EInt :: Int -> SExpr be
pattern EInt i <- EIntegral (fromInteger -> i) (Just HsPrimInt)
  where
    EInt i = EIntegral (fromIntegral i) (Just HsPrimInt)

-- | Case alternatives
--
-- Represents different kinds of pattern matching alternatives in case expressions.
--
-- Examples:
--
-- > case x of
-- >   Just y -> ...     -- SAlt with constructor "Just"
-- >   5# -> ...         -- SAltNoConstr for primitive literal
-- >   (# a, b #) -> ... -- SAltUnboxedTuple
--
data SAlt ctx where
    -- | Constructor pattern: @Con x y z -> body@
    --
    -- Pattern matches against a data constructor with fields.
    SAlt
      :: Hs.Name Hs.NsConstr -> Add n ctx ctx' -> Vec n NameHint -> SExpr ctx' -> SAlt ctx
    -- | Non-constructor pattern: @5# -> body@ or @x -> body@
    --
    -- Used for matching primitive values, literals, or catch-all variables.
    -- Always binds exactly one variable (hence Vec Nat1).
    SAltNoConstr
      :: Vec Nat1 NameHint -> SExpr (S ctx) -> SAlt ctx
    -- | Unboxed tuple pattern: @(# x, y #) -> body@
    --
    -- Pattern matches against unboxed tuples.
    SAltUnboxedTuple
      :: Add n ctx ctx' -> Vec n NameHint -> SExpr ctx' -> SAlt ctx

deriving stock instance Show (SAlt ctx)

-- | Simple declarations
data SDecl =
    DTypSyn TypeSynonym
  | DInst Instance
  | DRecord Record
  | DNewtype Newtype
  | DEmptyData EmptyData
  | DDerivingInstance DerivingInstance
  | DForeignImport ForeignImport
  | DBinding Binding
  | DPatternSynonym PatternSynonym
  deriving stock (Show)

type ClosedType = SType EmptyCtx

-- | Simple types
type SType :: Ctx -> Star
data SType ctx =
    TGlobal Global
  | TCon (Hs.Name Hs.NsTypeConstr)
  | TFun (SType ctx) (SType ctx)
  | TLit Natural
  | TStrLit String
  | TExt Hs.ExtRef BindingSpec.CTypeSpec BindingSpec.HsTypeSpec
  | TBound (Idx ctx)
  | TFree (Hs.Name Hs.NsVar)
  | TApp (SType ctx) (SType ctx)
  | forall n ctx'. TForall (Vec n NameHint) (Add n ctx ctx') [SType ctx'] (SType ctx')

data Pragma = NOINLINE
  deriving stock Show

infixl 9 `TApp`

deriving stock instance Show (SType ctx)

data TypeSynonym = TypeSynonym{
      name    :: Hs.Name Hs.NsTypeConstr
    , typ     :: ClosedType
    , origin  :: Origin.Decl Origin.EmptyData
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Instance = Instance{
      clss    :: Global
    , args    :: [ClosedType]
    , super   :: [(Global, [ClosedType])]
    , types   :: [(Global, [ClosedType], ClosedType)]
    , decs    :: [(Global, ClosedExpr)]
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Field = Field{
      name   :: Hs.Name Hs.NsVar
    , typ    :: ClosedType
    , origin :: Origin.Field
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Record = Record{
      typ     :: Hs.Name Hs.NsTypeConstr
    , con     :: Hs.Name Hs.NsConstr
    , fields  :: [Field]
    , origin  :: Origin.Decl Origin.Struct
    , deriv   :: [(Hs.Strategy ClosedType, [Global])]
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data EmptyData = EmptyData{
      name    :: Hs.Name Hs.NsTypeConstr
    , origin  :: Origin.Decl Origin.EmptyData
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data DerivingInstance = DerivingInstance{
      strategy :: Hs.Strategy ClosedType
    , typ      :: ClosedType
    , comment  :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Newtype = Newtype{
      name    :: Hs.Name Hs.NsTypeConstr
    , con     :: Hs.Name Hs.NsConstr
    , field   :: Field
    , origin  :: Origin.Decl Origin.Newtype
    , deriv   :: [(Hs.Strategy ClosedType, [Global])]
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

-- | We might want to reconsider the decision of 'foreignImportParameters' as
-- well as 'foreignImportResultType' being 'ClosedType's if we ever want to
-- generate polymorphic type signatures.
--
data ForeignImport = ForeignImport{
      name       :: Hs.Name Hs.NsVar
    , parameters :: [Parameter]
    , result     :: Result
    , origName   :: C.DeclName
    , callConv   :: CallConv
    , origin     :: Origin.ForeignImport
    , comment    :: Maybe HsDoc.Comment
    , safety     :: Safety
    }
  deriving stock (Show, Generic)

-- | Safety of foreign import declarations
data Safety = Safe | Unsafe
  deriving stock (Show, Eq, Generic)

instance Default Safety where
  def = Safe

data Binding = Binding{
      name       :: Hs.Name Hs.NsVar
    , parameters :: [Parameter]
    , result     :: Result
    , body       :: ClosedExpr
    , pragmas    :: [Pragma]
    , comment    :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Parameter = Parameter{
      name    :: Maybe (Hs.Name Hs.NsVar)
    , typ     :: ClosedType
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data Result = Result{
      typ     :: ClosedType
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)

data PatternSynonym = PatternSynonym{
      name    :: Hs.Name Hs.NsConstr
    , typ     :: ClosedType
    , rhs     :: PatExpr
    , origin  :: Origin.PatSyn
    , comment :: Maybe HsDoc.Comment
    }
  deriving stock (Show, Generic)
