-- | Simplified HS abstract syntax tree
module HsBindgen.Backend.SHs.AST (
-- TODO: drop S prefix?
    Global (..),
    ClosedExpr,
    SExpr (..),
    pattern EInt,
    SAlt (..),
    PatExpr (..),
    SDecl (..),
    ByCategory(..),
    BindingCategory(..),
    mapByCategory,
    displayBindingCategory,
    Pragma (..),
    ClosedType,
    SType (..),
    Var (..),
    Instance (..),
    Field (..),
    Record (..),
    EmptyData (..),
    DerivingInstance (..),
    Newtype (..),
    ForeignImport (..),
    Safety (..),
    FunctionParameter (..),
    PatternSynonym (..),
) where

import Data.Map qualified as Map

import C.Char qualified as CExpr.Runtime

import HsBindgen.Backend.Hs.AST.Strategy qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.NameHint

import DeBruijn (Add, Ctx, EmptyCtx, Idx)

{-------------------------------------------------------------------------------
  Backend representation
-------------------------------------------------------------------------------}

-- TODO_PR: Why do we need 'Ord'? The ordering is not particularly meaningful,
-- is it? (I guess only to use these as keys of maps?)

data Global =
    GTypeConst TypeConst
  | GTypeClass TypeClass
  | GTermVar TermVar
  | GTermConst TermConst
  deriving stock (Eq, Ord, Show)

data TypeConst =
    Tuple_type Word
  | Foreign_Ptr
  | Foreign_FunPtr
  | ConstantArray
  | IncompleteArray
  | IO_type
  | CharValue_tycon
  | AsCEnum_type
  | AsSequentialCEnum_type
  | ByteArray_type
  | SizedByteArray_type
  | Block_type
  | PrimType HsPrimType
  | ComplexType
  deriving stock (Eq, Ord, Show)

data TypeClass =
    StaticSize_class
  | ReadRaw_class
  | WriteRaw_class
  | ToFunPtr_class
  | FromFunPtr_class
  | Storable_class
  | HasFlexibleArrayMember_class

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
  | Real_class
  | RealFloat_class
  | RealFrac_class
  | Show_class
    -- | Primitive (unboxed) type equality
  | NomEq_class
  | Not_class
  | Logical_class
  | RelEq_class
  | RelOrd_class
  | Plus_class
  | Minus_class
  | Add_class
  | Sub_class
  | Mult_class
  | Div_class
  | Rem_class
  | Complement_class
  | Bitwise_class
  | Shift_class
  | CEnum_class

  | SequentialCEnum_class
  deriving stock (Eq, Ord, Show)

data TermVar =
    Applicative_pure
  | Applicative_seq
  | Monad_return
  | Monad_seq
  | ToFunPtr_toFunPtr
  | FromFunPtr_fromFunPtr
  | Storable_sizeOf
  | Storable_alignment
  | Storable_peekByteOff
  | Storable_pokeByteOff
  | Storable_peek
  | Storable_poke
  | HasFlexibleArrayMember_offset
  | Bitfield_peekBitOffWidth
  | Bitfield_pokeBitOffWidth
  | CharValue_fromAddr
  | ByteArray_setUnionPayload
  | ByteArray_getUnionPayload
  | CAPI_with
  | CAPI_allocaAndPeek
  | ConstantArray_withPtr
  | IncompleteArray_withPtr

    -- Unsafe
  | IO_unsafePerformIO

    -- Other type classes
  | Read_readPrec
  | Read_readList
  | Read_readListPrec
  | Show_showsPrec
  | Not_not
  | Logical_and
  | Logical_or
  | RelEq_eq
  | RelEq_uneq
  | RelOrd_lt
  | RelOrd_le
  | RelOrd_gt
  | RelOrd_ge
  | Plus_resTyCon
  | Plus_plus
  | Minus_resTyCon
  | Minus_negate
  | Add_resTyCon
  | Add_add
  | Sub_resTyCon
  | Sub_minus
  | Mult_resTyCon
  | Mult_mult
  | Div_div
  | Div_resTyCon
  | Rem_resTyCon
  | Rem_rem
  | Complement_resTyCon
  | Complement_complement
  | Bitwise_resTyCon
  | Bitwise_and
  | Bitwise_or
  | Bitwise_xor
  | Shift_resTyCon
  | Shift_shiftL
  | Shift_shiftR

  | GHC_Float_castWord32ToFloat
  | GHC_Float_castWord64ToDouble

  | NonEmpty_singleton
  | Map_fromList
  | Read_readListDefault
  | Read_readListPrecDefault

  | CEnum_toCEnum
  | CEnum_fromCEnum
  | CEnum_declaredValues
  | CEnum_showsUndeclared
  | CEnum_readPrecUndeclared
  | CEnum_isDeclared
  | CEnum_mkDeclared

  | SequentialCEnum_minDeclaredValue
  | SequentialCEnum_maxDeclaredValue
  | CEnum_declaredValuesFromList
  | CEnum_showsCEnum
  | CEnum_showsWrappedUndeclared
  | CEnum_readPrecCEnum
  | CEnum_readPrecWrappedUndeclared
  | CEnum_seqIsDeclared
  | CEnum_seqMkDeclared
  deriving stock (Eq, Ord, Show)

data TermConst =
    Tuple_constructor Word
  | Ptr_constructor
  | CharValue_constructor

  | CFloat_constructor
  | CDouble_constructor

  | NonEmpty_constructor

  -- TODO_PR: The name of the 'CEnumZ_tycon' data constructor is confusing, or
  -- there was/is a bug. 'CEnumZ_tycon' is a data constructor, but
  -- 'CharValue_tycon' is a type constructor?
  | CEnumZ_tycon
  deriving stock (Eq, Ord, Show)

type ClosedExpr = SExpr EmptyCtx

-- | Simple expressions
type SExpr :: Ctx -> Star
data SExpr ctx =
    EGlobal Global
  | EBound (Idx ctx)
  | EFree (Hs.Name Hs.NsVar)
  | ECon (Hs.Name Hs.NsConstr)
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
  = PEApps (Hs.Name Hs.NsConstr) [PatExpr] -- head of pattern application cannot be variable.
  | PELit Integer
  deriving stock (Show)

pattern EInt :: Int -> SExpr be
pattern EInt i <- EIntegral (fromInteger -> i) (Just HsPrimInt)
  where
    EInt i = EIntegral (fromIntegral i) (Just HsPrimInt)

-- | Case alternatives
data SAlt ctx where
    SAlt :: Hs.Name Hs.NsConstr -> Add n ctx ctx' -> Vec n NameHint -> SExpr ctx' -> SAlt ctx

deriving stock instance Show (SAlt ctx)

-- | Simple declarations
data SDecl =
    DVar Var
  | DInst Instance
  | DRecord Record
  | DNewtype Newtype
  | DEmptyData EmptyData
  | DDerivingInstance DerivingInstance
  | DForeignImport ForeignImport
  | DPatternSynonym PatternSynonym
  | DPragma Pragma
  deriving stock (Show)

newtype ByCategory a = ByCategory { unByCategory :: Map BindingCategory a }
  deriving newtype (Functor, Foldable, Show)

mapByCategory :: (BindingCategory -> a -> b) -> ByCategory a -> ByCategory b
mapByCategory f = ByCategory . Map.mapWithKey f . unByCategory

-- | Foreign import category.
data BindingCategory =
    -- | Types (top-level bindings).
    BType
    -- | Foreign import bindings with a @safe@ foreign import modifier.
  | BSafe
    -- | Foreign import bindings with an @unsafe@ foreign import modifier.
  | BUnsafe
    -- | Pointers to functions; generally @unsafe@.
  | BFunPtr
    -- | Temporary category for bindings to global variables or constants.
  | BGlobal
  deriving stock (Show, Eq, Ord, Enum, Bounded)

displayBindingCategory :: BindingCategory -> String
displayBindingCategory = \case
  BType   -> "Type"
  BSafe   -> "Safe"
  BUnsafe -> "Unsafe"
  BFunPtr -> "FunPtr"
  BGlobal -> "Global"

type ClosedType = SType EmptyCtx

-- | Simple types
type SType :: Ctx -> Star
data SType ctx =
    TGlobal Global
  | TCon (Hs.Name Hs.NsTypeConstr)
  | TFun (SType ctx) (SType ctx)
  | TLit Natural
  | TExt Hs.ExtRef BindingSpec.TypeSpec
  | TBound (Idx ctx)
  | TApp (SType ctx) (SType ctx)
  | forall n ctx'. TForall (Vec n NameHint) (Add n ctx ctx') [SType ctx'] (SType ctx')

data Pragma = NOINLINE (Hs.Name Hs.NsVar)
  deriving stock Show

infixl 9 `TApp`

deriving stock instance Show (SType ctx)

data Var = Var {
      varName    :: Hs.Name Hs.NsVar
    , varType    :: ClosedType
    , varExpr    :: ClosedExpr
    , varComment :: Maybe HsDoc.Comment
    }
  deriving stock (Show)

data Instance  = Instance {
      instanceClass   :: Global
    , instanceArgs    :: [ClosedType]
    , instanceTypes   :: [(Global, ClosedType, ClosedType)]
    , instanceDecs    :: [(Global, ClosedExpr)]
    , instanceComment :: Maybe HsDoc.Comment
    }
  deriving stock (Show)

data Field = Field {
      fieldName   :: Hs.Name Hs.NsVar
    , fieldType   :: ClosedType
    , fieldOrigin :: Origin.Field
    , fieldComment :: Maybe HsDoc.Comment
    }
  deriving stock (Show)

data Record = Record {
      dataType    :: Hs.Name Hs.NsTypeConstr
    , dataCon     :: Hs.Name Hs.NsConstr
    , dataFields  :: [Field]
    , dataOrigin  :: Origin.Decl Origin.Struct
    , dataDeriv   :: [(Hs.Strategy ClosedType, [Global])]
    , dataComment :: Maybe HsDoc.Comment
    }
  deriving stock (Show)

data EmptyData = EmptyData {
      emptyDataName    :: Hs.Name Hs.NsTypeConstr
    , emptyDataOrigin  :: Origin.Decl Origin.EmptyData
    , emptyDataComment :: Maybe HsDoc.Comment
    }
  deriving stock (Show)

data DerivingInstance = DerivingInstance {
      derivingInstanceStrategy :: Hs.Strategy ClosedType
    , derivingInstanceType     :: ClosedType
    , derivingInstanceComment  :: Maybe HsDoc.Comment
    }
  deriving stock (Show)

data Newtype = Newtype {
      newtypeName    :: Hs.Name Hs.NsTypeConstr
    , newtypeCon     :: Hs.Name Hs.NsConstr
    , newtypeField   :: Field
    , newtypeOrigin  :: Origin.Decl Origin.Newtype
    , newtypeDeriv   :: [(Hs.Strategy ClosedType, [Global])]
    , newtypeComment :: Maybe HsDoc.Comment
    }
  deriving stock (Show)

-- | We might want to reconsider the decision of having 'functionParameterType'
-- as well as 'foreignImportResultType' be a 'ClosedType' if we ever want to
-- generate polymorphic type signatures.
--
data ForeignImport = ForeignImport
    { foreignImportName       :: Hs.Name Hs.NsVar
    , foreignImportParameters :: [FunctionParameter]
    , foreignImportResultType :: ResultType ClosedType
    , foreignImportOrigName   :: Text
    , foreignImportCallConv   :: CallConv
    , foreignImportOrigin     :: Origin.ForeignImport
    , foreignImportComment    :: Maybe HsDoc.Comment
    , foreignImportSafety     :: Safety
    }
  deriving stock (Show)

-- | Safety of foreign import declarations
data Safety = Safe | Unsafe
  deriving stock (Show, Eq, Generic)

instance Default Safety where
  def = Safe

data FunctionParameter = FunctionParameter
  { functionParameterName    :: Maybe (Hs.Name Hs.NsVar)
  , functionParameterType    :: ClosedType
  , functionParameterComment :: Maybe HsDoc.Comment
  }
  deriving stock (Show)

data PatternSynonym = PatternSynonym
    { patSynName    :: Hs.Name Hs.NsConstr
    , patSynType    :: ClosedType
    , patSynRHS     :: PatExpr
    , patSynOrigin  :: Origin.PatSyn
    , patSynComment :: Maybe HsDoc.Comment
    }
  deriving stock (Show)
