{-# LANGUAGE MagicHash #-}

-- | Macro types: the types that we infer for macros.
module HsBindgen.Frontend.Macro.Tc.Type (
    -- * Names
    Name
  , FunName
    -- * Annotations
  , XVar(..)
  , XApp(..)
    -- * Pass
  , Pass(Ps, Tc)
    -- * Type inference
  , TypeEnv
  , VarEnv
    -- ** Type system
  , Type(..)
  , IntegralType(..)
  , Kind(Ty, Ct)
  , Quant(..)
  , QuantTyBody(..)
  , mkQuantTyBody
  , mkFunTy
    -- ** Type variables
  , MetaTyVar(..)
  , MetaOrigin(..)
  , InstOrigin(..)
  , TyVar(..)
  , SkolemTyVar(..)
  , Unique(..)
    -- ** Type constructors
  , TyCon(..)
  , GenerativeTyCon(..)
  , FamilyTyCon(..)
  , DataTyCon(..)
    -- ** Constraints
  , ClassTyCon(..)
  , CtOrigin(..)
    -- ** Pattern synonyms for types
  , pattern Class
  , pattern Data
  , pattern FamApp
  , pattern Not
  , pattern Plus
  , pattern Minus
  , pattern Complement
  , pattern Logical
  , pattern RelEq
  , pattern RelOrd
  , pattern Add
  , pattern Sub
  , pattern Mult
  , pattern Div
  , pattern Rem
  , pattern Bitwise
  , pattern Shift
  , pattern PrimIntInfoTy
  , pattern PrimFloatInfoTy
  , pattern IntLike
  , pattern FloatLike
  , pattern String
  , pattern Ptr
  , pattern Tuple
  , pattern PlusRes
  , pattern MinusRes
  , pattern AddRes
  , pattern SubRes
  , pattern MultRes
  , pattern DivRes
  , pattern RemRes
  , pattern ComplementRes
  , pattern BitsRes
  , pattern ShiftRes
  , pattern IntTy
  , pattern HsIntTy
  , pattern CharTy
  , pattern CharLitTy
    -- ** Query
  , tyVarName
  , tyVarNames
  , tyVarUnique
  , eqType
    -- ** Pretty-printing
  , pprCtOrigin
  , pprMetaOrigin
    -- * Evaluation
  , ValSType(..)
  , Value(..)
  , FunValue(..)
  ) where

-- base
import Data.Kind qualified as Hs
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Type.Equality (type (:~:) (..))
import Foreign.C.Types
import Foreign.Ptr qualified as Foreign
import GHC.Show (showSpace)

-- fin
import Data.Type.Nat qualified as Nat (SNatI, eqNat, reflectToNum,)

-- some
import Data.GADT.Compare (GEq (..), defaultEq)

-- text
import Data.Text qualified as Text

-- vec
import Data.Vec.Lazy qualified as Vec

-- c-expr
import C.Type qualified

-- hs-bindgen

import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.TestEquality (equals2)

{-------------------------------------------------------------------------------
  Type system for macros
-------------------------------------------------------------------------------}

type Name = Text
newtype Unique = Unique { uniqueInt :: Int }
  deriving newtype ( Enum, Eq, Show )
  deriving stock Generic

data Kind
  -- | The kind of types.
  = Ty
  -- | The kind of constraints.
  | Ct
  deriving stock ( Eq, Ord, Show )

type Type :: Kind -> Hs.Type
data Type ki where
  -- | A type variable.
  TyVarTy :: !TyVar -> Type Ty
  -- | A function type.
  FunTy :: !( NE.NonEmpty ( Type Ty ) ) -> !( Type Ty ) -> Type Ty
  -- | An (exactly saturated) application of a 'TyCon' to arguments.
  TyConAppTy :: !( TyCon nbArgs res ) -> !( Vec nbArgs ( Type Ty ) ) -> Type res
  -- | Nominal equality.
  NomEqPred :: !( Type Ty ) -> !( Type Ty ) -> Type Ct

mkFunTy :: Foldable f => f ( Type Ty ) -> Type Ty -> Type Ty
mkFunTy args = case toList args of
  [] -> id
  ( a : as ) -> \ res -> FunTy ( a NE.:| as ) res

-- | A qualified quantified type @forall tys. cts => args -> res@.
type Quant :: Hs.Type -> Hs.Type
data Quant res where
  Quant
    :: forall nbBinders res
    .  ( Nat.SNatI nbBinders )
    => { quantTyBodyFn :: !( Vec nbBinders ( Type Ty ) -> QuantTyBody res ) }
    -> Quant res
deriving stock instance Functor Quant

instance Eq ( Quant ( Type ki ) ) where
  qty1@( Quant @n1 _ ) == qty2@( Quant @n2 _ ) =
    case Nat.eqNat @n1 @n2 of
      Nothing -> False
      Just Refl ->
        mkQuantTyBody qty1 == mkQuantTyBody qty2

instance Eq ( Quant ( Vec n ( Type ki ) ) ) where
  qty1@( Quant @n1 _ ) == qty2@( Quant @n2 _ ) =
    case Nat.eqNat @n1 @n2 of
      Nothing -> False
      Just Refl ->
        mkQuantTyBody qty1 == mkQuantTyBody qty2

-- | The body of a quantified type (what's under the forall).
type QuantTyBody :: Hs.Type -> Hs.Type
data QuantTyBody body
  = QuantTyBody
  { quantTyQuant :: ![ Type Ct ]
  , quantTyBody  :: !body
  }
  deriving stock ( Show, Generic, Functor, Foldable, Traversable )

instance Eq ( QuantTyBody ( Type ki ) ) where
  QuantTyBody cts1 body1 == QuantTyBody cts2 body2 =
    and [ length cts1 == length cts2
        , all ( uncurry eqType ) ( zip cts1 cts2 )
        , eqType body1 body2
        ]
instance Eq ( QuantTyBody ( Vec n ( Type ki ) ) ) where
  QuantTyBody cts1 body1 == QuantTyBody cts2 body2 =
    and [ length cts1 == length cts2
        , all ( uncurry eqType ) ( zip cts1 cts2 )
        , eqTypes body1 body2
        ]

instance Show ( Type ki ) where
  showsPrec p = \case
    TyVarTy tv -> showString ( show tv )
    TyConAppTy tc tys ->
      showParen (p >= 10 && not (null tys)) $
          showsPrec ( if null tys then p else 11 ) tc
        . foldr ( \ a acc -> showSpace . showsPrec 11 a . acc ) id tys
    FunTy as r ->
      showParen (p >= 0) $
        foldr ( \ a acc -> showsPrec 0 a . showString " -> " . acc ) id as . showsPrec 0 r
    NomEqPred a b ->
      showParen (p >= 5) $
        showsPrec 5 a . showString " ~ " . showsPrec 5 b

instance Show body => Show ( Quant body ) where
  showsPrec p0 quantTy@( Quant @nbBinders _ ) =
    showParen ( p0 >= 0 && not ( null qtvs && null cts ) ) $
        ( if null qtvs
          then id else
            showString "forall"
          . foldr ( \ ( _, tv ) acc -> showSpace . showString ( Text.unpack tv ) . acc ) id qtvs
          . showString ". "
        )
      . foldr ( \ a acc -> showsPrec 0 a . showString " => " . acc ) id cts
      . showsPrec 0 body
    where
      qtvs :: [ ( Int, Text ) ]
      qtvs = toList $ tyVarNames @nbBinders
      QuantTyBody cts body = mkQuantTyBody quantTy

tyVarNames :: forall nbVars. Nat.SNatI nbVars => Vec nbVars ( Int, Text )
tyVarNames = fromJust $ Vec.fromListPrefix nms
  where
    n = Nat.reflectToNum @nbVars Proxy
    nms
      | n > 3
      = map ( \ i -> ( i, "a" <> Text.pack ( show i ) ) ) [ 1 .. n ]
      | otherwise
      = take n [ ( 1, "a" ), ( 2, "b" ), ( 3, "c" ) ]

mkQuantTyBody :: Quant body -> QuantTyBody body
mkQuantTyBody ( Quant @nbBinders body ) =
  body $ fmap ( uncurry mkSkol ) $ tyVarNames @nbBinders
  where
    mkSkol :: Int -> Name -> Type Ty
    mkSkol i tv = TyVarTy $ SkolemTv $ SkolemTyVar tv ( Unique i )

data TyVar
  = SkolemTv {-# UNPACK #-} !SkolemTyVar
  | MetaTv   {-# UNPACK #-} !MetaTyVar
  deriving stock Generic

tyVarName :: TyVar -> Name
tyVarName = \case
  SkolemTv sk  -> skolemTyVarName sk
  MetaTv   tau -> metaTyVarName tau

tyVarUnique :: TyVar -> Unique
tyVarUnique = \case
  SkolemTv sk  -> skolemTyVarUnique sk
  MetaTv   tau -> metaTyVarUnique   tau

instance Show TyVar where
  show tv =
    concat
      [ Text.unpack ( tyVarName tv )
      ++ case tv of
          MetaTv {} -> "_" ++ show u ++ "[tau]"
          SkolemTv {} -> "" -- assumes there is never shadowing in skolems
      ]
      where Unique u = tyVarUnique tv

data SkolemTyVar
  = SkolemTyVar
    { skolemTyVarName   :: !Name
    , skolemTyVarUnique :: !Unique
    }
  deriving stock Generic
instance Show SkolemTyVar where
  show sk = show ( SkolemTv sk )
instance Show MetaTyVar where
  show tau = show ( MetaTv tau )

data MetaTyVar
  = MetaTyVar
    { metaTyVarName   :: !Name
    , metaTyVarUnique :: !Unique
    , metaOrigin      :: !MetaOrigin
    }
  deriving stock Generic

type TyCon :: Nat -> Kind -> Hs.Type
data TyCon nbArgs res where
  GenerativeTyCon :: !( GenerativeTyCon nbArgs ki ) -> TyCon nbArgs ki
  FamilyTyCon     :: !( FamilyTyCon     nbArgs    ) -> TyCon nbArgs Ty
deriving stock instance Eq ( TyCon nbArgs res )

type GenerativeTyCon :: Nat -> Kind -> Hs.Type
data GenerativeTyCon nbArgs res where
  DataTyCon      :: !( DataTyCon   nbArgs ) -> GenerativeTyCon nbArgs Ty
  ClassTyCon     :: !( ClassTyCon  nbArgs ) -> GenerativeTyCon nbArgs Ct
deriving stock instance Eq ( GenerativeTyCon nbArgs res )

type DataTyCon :: Nat -> Hs.Type
data DataTyCon nbArgs where
  -- | Type constructor for 'Void'
  VoidTyCon      :: DataTyCon Z
  -- | Type constructor for character literals (different from the C @char@
  -- integral type)
  CharLitTyCon   :: DataTyCon Z
  -- | Unary type constructor for integral types, such as 'Int' or 'UShort'.
  IntLikeTyCon   :: DataTyCon ( S Z )
  -- | Unary type constructor for floating-point types, such as 'Float' or 'Double'.
  FloatLikeTyCon :: DataTyCon ( S Z )
  -- | Type constructor for pointers
  PtrTyCon       :: DataTyCon ( S Z )

  -- | Tuple type constructors
  TupleTyCon     :: !Word -> DataTyCon ( S ( S n ) )
    -- Invariant: the stored 'Word' matches the arity of the tuple

  -- | Family of nullary type constructors for arguments to 'IntLikeTyCon'.
  PrimIntInfoTyCon   :: !IntegralType -> DataTyCon Z
  -- | Family of nullary type constructors for arguments to 'FloatLikeTyCon'.
  PrimFloatInfoTyCon :: !C.Type.FloatingType -> DataTyCon Z

data IntegralType
  = CIntegralType !C.Type.IntegralType
  | HsIntType
  deriving stock ( Eq, Ord, Show, Generic )

deriving stock instance Eq  ( DataTyCon nbArgs )
deriving stock instance Ord ( DataTyCon nbArgs )

type FamilyTyCon :: Nat -> Hs.Type
data FamilyTyCon nbArgs where
  -- | Return type of unary addition.
  PlusResTyCon       :: FamilyTyCon ( S Z )
  -- | Return type of unary negation.
  MinusResTyCon      :: FamilyTyCon ( S Z )
  -- | Return type of binary addition.
  AddResTyCon        :: FamilyTyCon ( S ( S Z ) )
  -- | Return type of binary subtraction.
  SubResTyCon        :: FamilyTyCon ( S ( S Z ) )
  -- | Return type of binary multiplication.
  MultResTyCon       :: FamilyTyCon ( S ( S Z ) )
  -- | Return type of binary division/quotient.
  DivResTyCon        :: FamilyTyCon ( S ( S Z ) )
  -- | Return type of unary bitwise negation.
  ComplementResTyCon :: FamilyTyCon ( S Z )
  -- | Return type of binary remainder operation.
  RemResTyCon        :: FamilyTyCon ( S ( S Z ) )
  -- | Return type of binary bitwise logical operations.
  BitsResTyCon       :: FamilyTyCon ( S ( S Z ) )
  -- | Return type of binary bitwise shift operations,
  -- as a function of the type of the argument being shifted.
  ShiftResTyCon      :: FamilyTyCon ( S Z )

deriving stock instance Eq  ( FamilyTyCon nbArgs )
deriving stock instance Ord ( FamilyTyCon nbArgs )

type ClassTyCon :: Nat -> Hs.Type
data ClassTyCon nbArgs where
  -- | Class type constructor for @Not@
  NotTyCon        :: ClassTyCon ( S Z )
  -- | Class type constructor for @Logical@
  LogicalTyCon    :: ClassTyCon ( S ( S Z ) )
  -- | Class type constructor for @RelEq@
  RelEqTyCon      :: ClassTyCon ( S ( S Z ) )
  -- | Class type constructor for @RelOrd@
  RelOrdTyCon     :: ClassTyCon ( S ( S Z ) )
  -- | Class type constructor for @Plus@ (unary plus)
  PlusTyCon       :: ClassTyCon ( S Z )
  -- | Class type constructor for @Minus (unary minus)
  MinusTyCon      :: ClassTyCon ( S Z )
  -- | Class type constructor for @Add@
  AddTyCon        :: ClassTyCon ( S ( S Z ) )
  -- | Class type constructor for @Sub@
  SubTyCon        :: ClassTyCon ( S ( S Z ) )
  -- | Class type constructor for @Mult@
  MultTyCon       :: ClassTyCon ( S ( S Z ) )
  -- | Class type constructor for @Div@
  DivTyCon        :: ClassTyCon ( S ( S Z ) )
  -- | Class type constructor for @Rem@
  RemTyCon        :: ClassTyCon ( S ( S Z ) )
  -- | Class type constructor for @Complement@
  ComplementTyCon :: ClassTyCon ( S Z )
  -- | Class type constructor for @Bitwise@
  BitwiseTyCon    :: ClassTyCon ( S ( S Z ) )
  -- | Class type constructor for @Shift@
  ShiftTyCon      :: ClassTyCon ( S ( S Z ) )

deriving stock instance Eq  ( ClassTyCon nbArgs )
deriving stock instance Ord ( ClassTyCon nbArgs )

instance Show ( TyCon n ki ) where
  showsPrec p = \case
    GenerativeTyCon tc -> showsPrec p tc
    FamilyTyCon     tc -> showsPrec p tc

instance Show ( GenerativeTyCon n ki ) where
  showsPrec p = \case
    DataTyCon   tc -> showsPrec p tc
    ClassTyCon  tc -> showsPrec p tc

instance Show ( DataTyCon n ) where
  showsPrec p = \case
    VoidTyCon                 -> showString "Void"
    PtrTyCon                  -> showString "Ptr"
    CharLitTyCon              -> showString "CharLit"
    IntLikeTyCon              -> showString "IntLike"
    FloatLikeTyCon            -> showString "FloatLike"
    PrimIntInfoTyCon   inty   -> showsPrec p inty
    PrimFloatInfoTyCon floaty -> showsPrec p floaty
    TupleTyCon i              -> showString $ "Tuple" ++ show i
instance Show ( FamilyTyCon n ) where
  show = \case
    PlusResTyCon       -> "PlusRes"
    MinusResTyCon      -> "MinusRes"
    AddResTyCon        -> "AddRes"
    SubResTyCon        -> "SubRes"
    MultResTyCon       -> "MultRes"
    DivResTyCon        -> "DivRes"
    RemResTyCon        -> "RemRes"
    ComplementResTyCon -> "ComplementRes"
    BitsResTyCon       -> "BitsRes"
    ShiftResTyCon      -> "ShiftRes"

instance Show ( ClassTyCon n ) where
  show = \case
    NotTyCon        -> "Not"
    LogicalTyCon    -> "Logical"
    RelEqTyCon      -> "RelEq"
    RelOrdTyCon     -> "RelOrd"
    PlusTyCon       -> "Plus"
    MinusTyCon      -> "Minus"
    AddTyCon        -> "Add"
    SubTyCon        -> "Sub"
    MultTyCon       -> "Mult"
    DivTyCon        -> "Div"
    RemTyCon        -> "Rem"
    ComplementTyCon -> "Complement"
    BitwiseTyCon    -> "Bitwise"
    ShiftTyCon      -> "Shift"

{-------------------------------------------------------------------------------
  Type environment
-------------------------------------------------------------------------------}

type TypeEnv = Map C.Name ( Quant ( FunValue, Type Ty ) )
type VarEnv  = Map C.Name ( Type Ty )

data Pass = Ps | Tc

type XApp :: Pass -> Hs.Type
data family XApp p
data instance XApp Ps = NoXApp
  deriving stock ( Eq, Ord, Show, Generic )
newtype instance XApp Tc = XAppTc FunValue
  deriving stock ( Eq, Show, Generic )

type XVar :: Pass -> Hs.Type
data family XVar p
data instance XVar Ps = NoXVar
  deriving stock ( Eq, Ord, Show, Generic )
data instance XVar Tc = XVarTc FunValue
  deriving stock ( Eq, Show, Generic )

-- | A singleton for the type of a value, for use in evaluation of macros.
newtype ValSType ty = ValSType ( C.Type.SType ValSType ty )
  -- NB: this type ties the recursive knot of the open C.Type.SType type.
  --
  -- This type is defined here because it is tied to macro evaluation.
  -- In particular, if we decide to add support for evaluation macro tuples,
  -- we would need to add a constructor here to account for that.
 deriving newtype GEq
deriving stock instance Show ( ValSType ty )
instance Eq ( ValSType ty ) where
  ValSType ty1 == ValSType ty2 = defaultEq ty1 ty2
data Value where
  NoValue :: Value
  Value :: ValSType ty -> ty -> Value
instance Eq Value where
  NoValue == NoValue = True
  NoValue == Value {} = False
  Value {} == NoValue = False
  Value ty1 v1 == Value ty2 v2 =
    case geq ty1 ty2 of
      Nothing -> False
      Just Refl ->
        witnessValSType @Eq ty1 (v1 == v2)

-- | Produce class dictionaries for a class by matching on the singleton
-- for the type.
witnessValSType
  :: forall c ty r
  . ( forall x. c ( Foreign.Ptr x )
    , c CChar, c CSChar, c CUChar, c CShort, c CUShort, c CInt
    , c CUInt, c CLong, c CULong, c CLLong, c CULLong, c CPtrdiff
    , c CSize, c CBool, c CFloat, c CDouble, c () )
  => ValSType ty -> ( c ty => r ) -> r
witnessValSType ( ValSType ty ) f =
  C.Type.witnessType @c ( witnessValSType @c ) ty f

-- | A Haskell function that evaluates a macro function.
data FunValue where
  FunValue :: Nat.SNatI n => FunName -> ( Vec n Value -> Value ) -> FunValue
instance Eq FunValue where
  FunValue f1 _ == FunValue f2 _ = f1 == f2
instance Show FunValue where
  show ( FunValue nm _ ) = Text.unpack nm

{-------------------------------------------------------------------------------
  Constraints & errors
-------------------------------------------------------------------------------}

-- | The textual name of a macro function.
type FunName = Text

-- | Why did we emit a constraint?
data CtOrigin
  = AppOrigin !FunName
  | FunInstOrigin !FunName
  | ClassInstOrigin !( Quant ( Type Ct ) ) !CtOrigin
  | DefaultingOrigin !CtOrigin
  deriving stock ( Generic, Show )

pprCtOrigin :: CtOrigin -> Text
pprCtOrigin = \case
  AppOrigin fun ->
    "In an application of '" <> fun <> "'."
  FunInstOrigin fun ->
    "In the instantiation of '" <> fun <> "'."
  ClassInstOrigin qty orig ->
    Text.unlines
      [ "From the context of the class instance '" <> Text.pack ( show qty ) <> "'."
      , pprCtOrigin orig ]
  DefaultingOrigin ct ->
    Text.unlines
      [ "When defaulting a constraint."
      , pprCtOrigin ct
      ]

-- | Why did we create a new metavariable?
data MetaOrigin
  = ExpectedFunTyResTy !FunName
  | ExpectedVarTy !C.Name
  | Inst { instOrigin :: !InstOrigin, instPos :: !Int }
  | FunArg !C.Name !( C.Name, Int )
  | IntLitMeta !C.IntegerLiteral
  | FloatLitMeta !C.FloatingLiteral
  deriving stock ( Generic, Show )

data InstOrigin
  = FunInstMetaOrigin !FunName
  | ClassInstMetaOrigin !( Quant ( Type Ct ) )
  deriving stock ( Generic, Show )

pprMetaOrigin :: MetaOrigin -> Text
pprMetaOrigin = \case
  ExpectedFunTyResTy funNm ->
    "the result type of '" <> Text.pack ( show funNm ) <> "'"
  ExpectedVarTy ( C.Name varNm ) ->
    "the type of the identifier '" <> varNm <> "'"
  Inst funNm i ->
    "the " <> speakNth i <> " type argument in the instantiation of '" <> Text.pack ( show funNm ) <> "'"
  FunArg ( C.Name funNm ) ( _argNm, i ) ->
    "the type of the " <> speakNth i <> " argument of '" <> funNm <> "'"
  IntLitMeta i ->
    "the type of the integer literal '" <> Text.pack ( show i ) <> "'"
  FloatLitMeta f ->
    "the type of the floating-point literal '" <> Text.pack ( show f ) <> "'"

speakNth :: Int -> Text
speakNth n = Text.pack ( show n ) <> suffix
  where
    suffix
      | n >= 11 && n <= 13 = "th" -- 11, 12, 13 are non-standard
      | lastDigit == 1     = "st"
      | lastDigit == 2     = "nd"
      | lastDigit == 3     = "rd"
      | otherwise          = "th"
    lastDigit = n `rem` 10

{-------------------------------------------------------------------------------
  Syntactic type equality
-------------------------------------------------------------------------------}

-- | On-the-nose type equality.
eqType :: Type ki -> Type ki -> Bool
eqType ( TyVarTy tv1 ) ( TyVarTy tv2 ) = tyVarUnique tv1 == tyVarUnique tv2
eqType ( TyConAppTy tc1 args1 ) ( TyConAppTy tc2 args2 ) =
  case tc1 `equals2` tc2 of
    Nothing -> False
    Just Refl ->
      eqTypes args1 args2
eqType ( FunTy args1 res1 ) ( FunTy args2 res2 )
  =  length args1 == length args2
  && all ( uncurry eqType ) ( NE.zip args1 args2 )
  && eqType res1 res2
eqType _ _ = False

-- | 'eqType' for a vector of types.
eqTypes :: Vec n ( Type ki ) -> Vec n ( Type ki ) -> Bool
eqTypes = ( and . ) . Vec.zipWith eqType

{-------------------------------------------------------------------------------
  Pattern synonyms for types
-------------------------------------------------------------------------------}

pattern Class :: ClassTyCon nbArgs -> Vec nbArgs ( Type Ty ) -> Type Ct
pattern Class cls args = TyConAppTy ( GenerativeTyCon ( ClassTyCon cls ) ) args
{-# COMPLETE Class, NomEqPred #-}

pattern Data :: () => ki ~ Ty => DataTyCon nbArgs -> Vec nbArgs ( Type Ty ) -> Type ki
pattern Data tc args = TyConAppTy ( GenerativeTyCon ( DataTyCon tc ) ) args
pattern FamApp :: () => ki ~ Ty => FamilyTyCon nbArgs -> Vec nbArgs ( Type Ty ) -> Type ki
pattern FamApp tc args = TyConAppTy ( FamilyTyCon tc ) args

pattern Not :: Type Ty -> Type Ct
pattern Not a = Class NotTyCon ( a ::: VNil )
pattern Plus :: Type Ty -> Type Ct
pattern Plus a = Class PlusTyCon ( a ::: VNil )
pattern Minus :: Type Ty -> Type Ct
pattern Minus a = Class MinusTyCon ( a ::: VNil )
pattern Complement :: Type Ty -> Type Ct
pattern Complement a = Class ComplementTyCon ( a ::: VNil )

pattern Logical :: Type Ty -> Type Ty -> Type Ct
pattern Logical a b = Class LogicalTyCon ( a ::: b ::: VNil )
pattern RelEq :: Type Ty -> Type Ty -> Type Ct
pattern RelEq a b = Class RelEqTyCon ( a ::: b ::: VNil )
pattern RelOrd :: Type Ty -> Type Ty -> Type Ct
pattern RelOrd a b = Class RelOrdTyCon ( a ::: b ::: VNil )
pattern Add :: Type Ty -> Type Ty -> Type Ct
pattern Add a b = Class AddTyCon ( a ::: b ::: VNil )
pattern Sub :: Type Ty -> Type Ty -> Type Ct
pattern Sub a b = Class SubTyCon ( a ::: b ::: VNil )
pattern Mult :: Type Ty -> Type Ty -> Type Ct
pattern Mult a b = Class MultTyCon ( a ::: b ::: VNil )
pattern Div :: Type Ty -> Type Ty -> Type Ct
pattern Div a b = Class DivTyCon ( a ::: b ::: VNil )
pattern Rem :: Type Ty -> Type Ty -> Type Ct
pattern Rem a b = Class RemTyCon ( a ::: b ::: VNil )
pattern Bitwise :: Type Ty -> Type Ty -> Type Ct
pattern Bitwise a b = Class BitwiseTyCon ( a ::: b ::: VNil )
pattern Shift :: Type Ty -> Type Ty -> Type Ct
pattern Shift a b = Class ShiftTyCon ( a ::: b ::: VNil )

pattern PrimIntInfoTy :: IntegralType -> Type Ty
pattern PrimIntInfoTy inty = Data (PrimIntInfoTyCon inty) VNil
pattern PrimFloatInfoTy :: C.Type.FloatingType -> Type Ty
pattern PrimFloatInfoTy floaty = Data (PrimFloatInfoTyCon floaty) VNil
pattern IntLike :: Type Ty -> Type Ty
pattern IntLike intLike = Data IntLikeTyCon (intLike ::: VNil)
pattern FloatLike :: Type Ty -> Type Ty
pattern FloatLike floatLike = Data FloatLikeTyCon (floatLike ::: VNil)
pattern String :: Type Ty
pattern String = Tuple 2 (Ptr CharTy ::: HsIntTy ::: VNil)
pattern Ptr :: Type Ty -> Type Ty
pattern Ptr ty = Data PtrTyCon (ty ::: VNil)

pattern Tuple :: () => ( nbArgs ~ S (S n) ) => Word -> Vec nbArgs (Type Ty) -> Type Ty
pattern Tuple l as = Data ( TupleTyCon l ) as

pattern PlusRes :: Type Ty -> Type Ty
pattern PlusRes a = FamApp PlusResTyCon ( a ::: VNil )
pattern MinusRes :: Type Ty -> Type Ty
pattern MinusRes a = FamApp MinusResTyCon ( a ::: VNil )
pattern AddRes :: Type Ty -> Type Ty -> Type Ty
pattern AddRes a b = FamApp AddResTyCon ( a ::: b ::: VNil )
pattern SubRes :: Type Ty -> Type Ty -> Type Ty
pattern SubRes a b = FamApp SubResTyCon ( a ::: b ::: VNil )
pattern MultRes :: Type Ty -> Type Ty -> Type Ty
pattern MultRes a b = FamApp MultResTyCon ( a ::: b ::: VNil )
pattern DivRes :: Type Ty -> Type Ty -> Type Ty
pattern DivRes a b = FamApp DivResTyCon ( a ::: b ::: VNil )
pattern RemRes :: Type Ty -> Type Ty -> Type Ty
pattern RemRes a b = FamApp RemResTyCon ( a ::: b ::: VNil )
pattern ComplementRes :: Type Ty -> Type Ty
pattern ComplementRes a = FamApp ComplementResTyCon ( a ::: VNil )
pattern BitsRes :: Type Ty -> Type Ty -> Type Ty
pattern BitsRes a b = FamApp BitsResTyCon ( a ::: b ::: VNil )
pattern ShiftRes :: Type Ty -> Type Ty
pattern ShiftRes a = FamApp ShiftResTyCon ( a ::: VNil )


pattern IntTy :: Type Ty
pattern IntTy = IntLike ( PrimIntInfoTy ( CIntegralType ( C.Type.IntLike ( C.Type.Int C.Type.Signed ) ) ) )
pattern HsIntTy :: Type Ty
pattern HsIntTy = IntLike ( PrimIntInfoTy ( HsIntType ) )
pattern CharTy :: Type Ty
pattern CharTy = IntLike ( PrimIntInfoTy ( CIntegralType ( C.Type.CharLike C.Type.Char ) ) )

pattern CharLitTy :: Type Ty
pattern CharLitTy = Data CharLitTyCon VNil
