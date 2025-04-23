{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

-- | Type inference for simple function-like C macros.
module HsBindgen.C.Tc.Macro
  (
    -- * Typechecking macros
    tcMacro
  , TcMacroError(..)
  , pprTcMacroError

  , TypeEnv(..)
  , MacroTypes

    -- ** Macro type-system
  , Type(..), Kind(..)
  , TyCon(..), GenerativeTyCon(..), DataTyCon(..), ClassTyCon(..)
  , FamilyTyCon(..)
  , IntegralType(..)
  , Quant(..), QuantTyBody(..)
  , tyVarName, tyVarNames, mkQuantTyBody
  , isPrimTy

    -- ** Macro typechecking errors
  , TcError(..), CtOrigin(..), MetaOrigin(..), CouldNotUnifyReason(..)
  , pprTcError, pprCtOrigin, pprMetaOrigin, pprCouldNotUnifyReason

  )
  where

-- base
import Control.Monad.ST
  ( ST, runST )
import Data.Either
  ( partitionEithers )
import Data.Kind qualified as Hs
import Data.List
  ( intercalate )
import Data.List.NonEmpty qualified as NE
import Data.Foldable
  ( for_ )
import Data.Functor
  ( (<&>) )
import Data.Maybe
  ( fromJust )
import Data.Monoid
  ( Endo(..) )
import Data.Proxy
  ( Proxy(..) )
import Data.STRef
  ( STRef, newSTRef, readSTRef )
import Data.Traversable
  ( for )
import Data.Typeable
  ( Typeable, eqT )
import Data.Type.Equality
  ( type (:~:)(..) )
import GHC.Show
  ( showSpace )
import GHC.Exts
  ( Int(I#), dataToTag# )

-- containers
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map

-- fin
import Data.Fin qualified as Fin
  ( toNatural )
import Data.Type.Nat qualified as Nat
  ( SNat(..), SNatI, snat, eqNat, reflectToNum )

-- mtl
import Control.Monad.Except
  ( ExceptT )
import Control.Monad.Except qualified as Except
import Control.Monad.State.Strict
  ( StateT(..), State )
import Control.Monad.State.Strict qualified as State
import Control.Monad.Writer
  ( WriterT )
import Control.Monad.Writer qualified as Writer
import Control.Monad.Trans
  ( lift )

-- text
import Data.Text qualified as Text

-- vec
import Data.Vec.Lazy qualified as Vec

-- c-expr
import C.Type qualified
import C.Operators qualified as C.Op

-- hs-bindgen
import HsBindgen.Imports
import HsBindgen.Errors
import HsBindgen.C.AST.Literal
  ( IntegerLiteral(..), FloatingLiteral(..) )
import HsBindgen.C.AST.Macro
  ( MExpr(..), MFun(..), MTerm(..) )
import HsBindgen.C.AST.Name
  ( CName(..) )
import HsBindgen.C.AST.Type
  ( PrimIntType(..), PrimSign(..), PrimFloatType(..) )
import HsBindgen.Util.TestEquality
  ( equals2 )

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

-- | A qualified quantified type @forall tys. cts => args -> res@.
type Quant :: Hs.Type -> Hs.Type
data Quant res where
  Quant
    :: forall nbBinders res
    .  ( Nat.SNatI nbBinders )
    => { quantTyBodyFn :: !( Vec nbBinders ( Type Ty ) -> QuantTyBody res ) }
    -> Quant res

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

  -- | Family of nullary type constructors for arguments to 'IntLikeTyCon'.
  PrimIntInfoTyCon   :: !IntegralType -> DataTyCon Z
  -- | Family of nullary type constructors for arguments to 'FloatLikeTyCon'.
  PrimFloatInfoTyCon :: !C.Type.FloatingType -> DataTyCon Z

  -- | Type constructor for the type of a 'PrimType' value.
  PrimTyTyCon    :: DataTyCon Z
  -- | Type constructor for the type of an empty macro.
  EmptyTyCon     :: DataTyCon Z

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
    PrimTyTyCon               -> showString "PrimTy"
    EmptyTyCon                -> showString "Empty"
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

eqTypes :: Vec n ( Type ki ) -> Vec n ( Type ki ) -> Bool
eqTypes = ( and . ) . Vec.zipWith eqType

{-------------------------------------------------------------------------------
  Free type variables and substitution
-------------------------------------------------------------------------------}

data FVs where
  FVs
    :: { boundTvs       :: IntSet
       , seenTvs        :: IntSet
       , seenTvsRevList :: [ TyVar ]
       }
    -> FVs

insertFV :: TyVar -> FVs -> FVs
insertFV tv fvs@( FVs { boundTvs = bound, seenTvs = seen, seenTvsRevList = revTvs } )
  | u `IntSet.member` bound || u `IntSet.member` seen
  = fvs
  | otherwise
  = fvs
      { seenTvs        = IntSet.insert u seen
      , seenTvsRevList = tv : revTvs
      }
  where
    u = uniqueInt $ tyVarUnique tv

getFVs :: IntSet -> State FVs () -> FVs
getFVs bound = ( `State.execState` ( FVs bound IntSet.empty [] ) )

noBoundVars :: IntSet
noBoundVars = IntSet.empty

freeTyVarsOfType :: Type ki -> State FVs ()
freeTyVarsOfType = \case
  TyVarTy tv -> State.modify' $ insertFV tv
  FunTy args res -> goFunTy args res
  TyConAppTy _tc tys -> freeTyVarsOfTypes tys
  NomEqPred a b -> freeTyVarsOfTypes ( a ::: b ::: VNil )

  where
    goFunTy :: NE.NonEmpty ( Type Ty ) -> Type Ty -> State FVs ()
    goFunTy (argTy NE.:| mbArgTys) resTy = do
      freeTyVarsOfType argTy
      case NE.nonEmpty mbArgTys of
        Nothing     -> freeTyVarsOfType        resTy
        Just argTys -> goFunTy          argTys resTy

freeTyVarsOfTypes :: Traversable t => t ( Type ki ) -> State FVs ()
freeTyVarsOfTypes = traverse_ freeTyVarsOfType
{-# INLINEABLE freeTyVarsOfType #-}

newtype Subst tv = Subst ( IntMap ( tv, Type Ty ) )
instance Functor Subst where
  fmap f ( Subst s ) = Subst $ fmap ( first f ) s

-- | Combine two substitutions, applying the first substitution over
-- the range of the second:
--
-- @applySubst s1 ( applySubst s2 ty ) == applySubst ( s1 <> s2 ) ty@
instance Show tv => Semigroup ( Subst tv ) where
  sub1@( Subst s1 ) <> ( Subst s2 ) =
    Subst $ IntMap.unionWithKey ( substClashErr "Semigroup Subst" ) s1
              ( IntMap.map ( \ ( nm, ty ) -> ( nm, applySubst sub1 ty ) ) s2 )
instance Show tv => Monoid ( Subst tv ) where
  mempty = Subst IntMap.empty
instance Show tv => Show ( Subst tv ) where
  show ( Subst s ) = "{ " ++ intercalate ", " ( map f $ IntMap.elems s ) ++ " }"
    where
      f ( tv, ty ) = show tv ++ " |-> " ++ show ty

isEmptySubst :: Subst tv -> Bool
isEmptySubst ( Subst s ) = IntMap.null s

domain :: Subst tv -> IntSet
domain ( Subst s ) = IntMap.keysSet s

--range :: IntSet -> Subst tv -> [ Type Ty ]
--range tvs ( Subst s ) = IntMap.elems $ fmap snd $ IntMap.restrictKeys s tvs

addOneToSubst :: HasCallStack => TyVar -> Type Ty -> Subst TyVar -> Subst TyVar
addOneToSubst tv ty s = mkSubst [ ( tv, ty ) ] <> s

mkSubst :: HasCallStack => [ ( TyVar, Type Ty ) ] -> Subst TyVar
mkSubst = Subst
        . IntMap.fromListWithKey ( substClashErr "mkSubst" )
        . map ( \ ( tv, ty ) -> ( uniqueInt ( tyVarUnique tv ), ( tv, ty ) ) )

substClashErr :: ( Show a, HasCallStack ) => String -> Int -> a -> a -> a
substClashErr str i ty1 ty2 =
  panicPure $
    unlines
      [ str ++ ": incoherent substitution"
      , "TyVar with unique " ++ show ( Unique i ) ++ " mapped to two different types"
      , "ty1: " ++ show ty1
      , "ty2: " ++ show ty2
      ]

lookupSubst :: TyVar -> Subst tv -> Maybe ( Type Ty )
lookupSubst tv ( Subst s ) =
  fmap snd $ IntMap.lookup ( uniqueInt $ tyVarUnique tv ) s

applySubst :: forall ki tv. Subst tv -> Type ki -> Type ki
applySubst subst = goTy
  where
    goTy :: forall ki'. Type ki' -> Type ki'
    goTy = \case
      ty@( TyVarTy tv ) ->
        case lookupSubst tv subst of
          Nothing  -> ty
          Just ty' -> ty'
      FunTy args res ->
        FunTy ( fmap goTy args ) ( goTy res )
      TyConAppTy tc tys ->
        TyConAppTy tc $ fmap goTy tys
      NomEqPred a b ->
        NomEqPred ( goTy a ) ( goTy b )

{-------------------------------------------------------------------------------
  Constraints & errors
-------------------------------------------------------------------------------}

data FunName = forall arity. FunName ( Either CName ( MFun arity ) )
instance Show FunName where
  show ( FunName funName ) =
    case funName of
      Left ( CName f ) -> Text.unpack f
      Right f -> show f

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
    "In an application of '" <> Text.pack ( show fun ) <> "'."
  FunInstOrigin fun ->
    "In the instantiation of '" <> Text.pack ( show fun ) <> "'."
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
  | ExpectedVarTy !CName
  | Inst { instOrigin :: !InstOrigin, instPos :: !Int }
  | FunArg !CName !( CName, Int )
  | IntLitMeta !IntegerLiteral
  | FloatLitMeta !FloatingLiteral
  deriving stock ( Generic, Show )

data InstOrigin
  = FunInstMetaOrigin !FunName
  | ClassInstMetaOrigin !( Quant ( Type Ct ) )
  deriving stock ( Generic, Show )

pprMetaOrigin :: MetaOrigin -> Text
pprMetaOrigin = \case
  ExpectedFunTyResTy funNm ->
    "the result type of '" <> Text.pack ( show funNm ) <> "'"
  ExpectedVarTy ( CName varNm ) ->
    "the type of the identifier '" <> varNm <> "'"
  Inst funNm i ->
    "the " <> speakNth i <> " type argument in the instantiation of '" <> Text.pack ( show funNm ) <> "'"
  FunArg ( CName funNm ) ( _argNm, i ) ->
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

data TcError
  = UnificationError !UnificationError
  | UnexpectedMTerm  !MTerm
  | UnboundVariable  !CName
  deriving stock Show

data UnificationError
  = forall k. Typeable k => CouldNotUnify !CouldNotUnifyReason !CtOrigin !( Type k ) !( Type k )
deriving stock instance Show UnificationError

pprTcError :: TcError -> Text
pprTcError = \case
  UnificationError err ->
    pprUnificationError err
  UnexpectedMTerm tm ->
    "Unexpected MTerm: " <> Text.pack ( show tm )
  UnboundVariable ( CName nm ) ->
    "Unbound variable: '" <> nm <> "'"

pprUnificationError :: UnificationError -> Text
pprUnificationError = \case
  CouldNotUnify rea orig ty1 ty2 ->
    Text.unlines
      [ "Could not unify:"
      , "  - " <> Text.pack ( show ty1 )
      , "  - " <> Text.pack ( show ty2 )
      , "because " <> pprCouldNotUnifyReason rea <> "."
      , pprCtOrigin orig ]

data CouldNotUnifyReason
  -- | Trying to unify incompatible types, e.g. a 'PiTy' with a 'TyConAppTy'.
  = IncompatibleTypes
  -- | Trying to unify two TyConApps of different lengths.
  | TyConAppUnequalLength
  -- | Trying to unify two TyConApps with different head TyCons.
  | TyConAppDifferentTyCon
  -- | Trying to unify a type variable with a type mentiong this type variable.
  | OccursCheck !TyVar
  -- | Trying to unify a skolem variable with another type.
  | RigidSkolem !SkolemTyVar
  deriving stock ( Generic, Show )

pprCouldNotUnifyReason :: CouldNotUnifyReason -> Text
pprCouldNotUnifyReason = \case
  IncompatibleTypes ->
    "the types are incompatible"
  TyConAppUnequalLength ->
    "the type constructors are applied to different numbers of arguments"
  TyConAppDifferentTyCon ->
    "the type constructors are different"
  OccursCheck tv ->
    "of an occurs-check in the variable '" <> tyVarName tv <> "'"
  RigidSkolem sk ->
    "'" <> skolemTyVarName sk <> "' is a rigid skolem variable"

{-------------------------------------------------------------------------------
  Typechecking macros: typechecker environment
-------------------------------------------------------------------------------}

data SrcSpan = SrcSpan
  deriving stock ( Eq, Ord, Generic )
instance Show SrcSpan where
  show _ = "<<noSrcSpan>>"

data TcEnv s =
  TcEnv
    { tcGblEnv :: !( TcGblEnv s )
    , tcLclEnv :: !TcLclEnv
    }

data TypeEnv =
   TypeEnv
     { typeEnvMacros   :: MacroTypes
     , typeEnvTypedefs :: Set CName
     }
  deriving stock Show

type MacroTypes = Map CName ( Quant ( Type Ty ) )
type VarEnv     = Map CName ( Type Ty )

data TcGblEnv s
  = TcGblEnv
      { tcTypeEnv  :: !( STRef s TypeEnv )
      , tcPlatform :: !C.Type.Platform
      }

data TcLclEnv
  = TcLclEnv
      { tcSrcSpan :: !SrcSpan
      , tcVarEnv  :: !VarEnv
      }

newtype TcPureM a = TcPureM ( forall s. TcEnv s -> ST s a )
instance Functor TcPureM where
  fmap f ( TcPureM g ) = TcPureM ( fmap f . g )
instance Applicative TcPureM where
  pure f = TcPureM \ _ -> pure f
  (<*>) = ap
instance Monad TcPureM where
  TcPureM ma >>= f = TcPureM \ env -> do
    !a <- ma env
    case f a of
      TcPureM g -> g env

runTcM :: C.Type.Platform -> TypeEnv -> TcPureM a -> ( a, [ ( TcError, SrcSpan ) ] )
runTcM plat initTyEnv ( TcPureM f ) = runST do
  tcErrs    <- newSTRef []
  tcTypeEnv <- newSTRef initTyEnv
  let
    tcGblEnv = TcGblEnv { tcTypeEnv, tcPlatform = plat }
    tcLclEnv = TcLclEnv { tcSrcSpan = SrcSpan, tcVarEnv = Map.empty }
  res <- f ( TcEnv { tcGblEnv, tcLclEnv } )
  errs <- readSTRef tcErrs
  return ( res, errs )

getSrcSpan :: TcPureM SrcSpan
getSrcSpan =
  TcPureM \ ( TcEnv _gbl ( TcLclEnv { tcSrcSpan } ) ) ->
    return tcSrcSpan

getPlatform :: TcPureM C.Type.Platform
getPlatform =
  TcPureM \ ( TcEnv ( TcGblEnv { tcPlatform = plat } ) _ ) ->
    pure plat

lookupTyEnv :: CName -> TcPureM ( Maybe ( Quant ( Type Ty ) ) )
lookupTyEnv varNm = TcPureM \ ( TcEnv ( TcGblEnv { tcTypeEnv } ) _ ) -> do
  tyEnv <- readSTRef tcTypeEnv
  return $ Map.lookup varNm (typeEnvMacros tyEnv)

lookupVarType :: CName -> TcPureM ( Maybe ( Type Ty ) )
lookupVarType varNm = TcPureM \ ( TcEnv _ lcl ) ->
  return $ Map.lookup varNm ( tcVarEnv lcl )

declareLocalVars :: Map CName ( Type Ty ) -> TcPureM a -> TcPureM a
declareLocalVars vs ( TcPureM f ) = TcPureM \ ( TcEnv gbl lcl ) ->
  f ( TcEnv gbl ( lcl { tcVarEnv = tcVarEnv lcl <> vs } ) )

{-------------------------------------------------------------------------------
  Typechecking macros: constraint generation monad
-------------------------------------------------------------------------------}

-- | Monad for unique generation.
type TcUniqueM = StateT Unique TcPureM

-- | Monad for unification.
type TcUnifyM = WriterT UnifyResult ( StateT ( Subst TyVar ) TcPureM )

-- | A collection of constraints (with their origin).
type Cts = [ ( Type Ct, CtOrigin ) ]

-- | Monad for generating constraints.
type TcGenM = WriterT ( Cts, [ ( TcError, SrcSpan ) ] ) ( StateT ( Subst TyVar ) TcUniqueM )

liftTcPureM :: TcPureM a -> TcGenM a
liftTcPureM = lift . lift . lift

newUnique :: Monoid w => WriterT w ( StateT s TcUniqueM ) Unique
newUnique = lift $ do
  u <- lift $ State.get
  let !u' = succ u
  lift $ State.put u'
  return u'
{-# INLINEABLE newUnique #-}

newMetaTyVarTy :: MetaOrigin -> Name -> TcGenM ( Type Ty )
newMetaTyVarTy metaOrigin metaTyVarName = do
  metaTyVarUnique <- newUnique
  return $
    TyVarTy $
      MetaTv $
        MetaTyVar
          { metaTyVarUnique
          , metaTyVarName
          , metaOrigin
          }

-- | 'Control.Monad.Trans.Control.liftBaseWith' for 'TcPureM' and 'TcGenM'.
liftBaseTcM :: ( forall x. TcPureM x -> TcPureM x ) -> TcGenM a -> TcGenM a
liftBaseTcM morph g = do
  s0 <- lift $ State.get
  u  <- lift $ lift $ State.get
  ( ( ( a, ctsErrs ), subst ), u' ) <-
    liftTcPureM
      $ morph
      $ ( `State.runStateT` u )
      $ ( `State.runStateT` s0 )
      $ Writer.runWriterT g
  lift $ State.put subst
  lift $ lift $ State.put u'
  Writer.tell ctsErrs
  return a

liftUnifyM :: TcUnifyM a -> TcGenM a
liftUnifyM = Writer.mapWriterT ( fmap ( second deferredEqs ) . State.mapStateT lift )
  where
    deferredEqs :: UnifyResult -> ( Cts, [ ( TcError, SrcSpan ) ] )
    deferredEqs ( UnifyResult { deferredEqualities = eqs, unifyErrors = errs } ) =
      ( eqs, map ( first UnificationError ) errs )

addErrTcGenM :: TcError -> TcGenM ()
addErrTcGenM err = do
  srcSpan <- liftTcPureM getSrcSpan
  Writer.tell ( [], [ ( err, srcSpan ) ] )

runTcGenMTcM :: TcGenM a -> TcUniqueM ( ( a, ( Cts, [ ( TcError, SrcSpan ) ] ) ), Subst TyVar )
runTcGenMTcM = aux . Writer.runWriterT
  where
    aux :: StateT ( Subst TyVar ) TcUniqueM x -> TcUniqueM ( x, Subst TyVar )
    aux ( State.StateT f ) =
      State.StateT \ u ->
        ( `State.runStateT` u ) $ f mempty

-- | Run a 'TcUnifyM' action and retrieve the underlying 'Subst'
-- when unification succeeded without deferring any equalities.
runTcUnifyMSubst :: forall a. Subst TyVar -> TcUnifyM a -> TcPureM ( Maybe ( a, Subst TyVar ) )
runTcUnifyMSubst subst0 =
  fmap unifySuccess . ( `State.runStateT` subst0 ) . Writer.runWriterT
    where
      unifySuccess ( ( a, UnifyResult { deferredEqualities = eqs, unifyErrors = errs } ), subst )
        | null eqs && null errs
        = Just ( a, subst )
        | otherwise
        = Nothing

-- | Run a 'TcGenM' action and retrieve the underlying 'Subst'
-- when there were no errors.
runTcGenMSubst :: TcGenM a -> TcUniqueM ( Maybe ( ( Cts, Subst TyVar ), a ) )
runTcGenMSubst = fmap noErrs . runTcGenMTcM
  where
    noErrs ( ( a, ( cts, mbErrs ) ), subst ) =
      if null mbErrs
      then Just ( ( cts, subst ), a )
      else Nothing

{-------------------------------------------------------------------------------
  Typechecking macros: unification
-------------------------------------------------------------------------------}

data UnifyResult =
  UnifyResult
    { deferredEqualities :: [ ( Type Ct, CtOrigin ) ]
    , unifyErrors        :: [ ( UnificationError, SrcSpan ) ] }
  deriving stock Show
instance Semigroup UnifyResult where
  UnifyResult d1 e1 <> UnifyResult d2 e2 =
    UnifyResult ( d1 ++ d2 ) ( e1 ++ e2 )
instance Monoid UnifyResult where
  mempty = UnifyResult [] []

data SwapFlag = NotSwapped | Swapped
  deriving stock ( Eq, Ord, Show )

swap :: SwapFlag -> SwapFlag
swap = \case
  NotSwapped -> Swapped
  Swapped -> NotSwapped

unifyType :: CtOrigin -> SwapFlag -> Type Ty -> Type Ty -> TcUnifyM ()
unifyType orig swapped ty1 ty2
  | TyVarTy tv1 <- ty1
  = unifyTyVar orig swapped tv1 ty2
  | TyVarTy tv2 <- ty2
  = unifyTyVar orig ( swap swapped ) tv2 ty1
  | FunTy args1 res1 <- ty1
  , FunTy args2 res2 <- ty2
  = unifyFunTys orig swapped args1 res1 args2 res2
  | FamApp {} <- ty1
  = defer
  | FamApp {} <- ty2
  = defer
  | TyConAppTy ( GenerativeTyCon tc1 ) as1 <- ty1
  , TyConAppTy ( GenerativeTyCon tc2 ) as2 <- ty2
  = unifyTyConApp orig swapped ( tc1, as1 ) ( tc2, as2 )
  | otherwise
  = couldNotUnify IncompatibleTypes orig swapped ty1 ty2
  where
    eq :: Type Ct
    eq = case swapped of
      NotSwapped -> NomEqPred ty1 ty2
      Swapped    -> NomEqPred ty2 ty1
    defer :: TcUnifyM ()
    defer =
      Writer.tell $
        UnifyResult
          { deferredEqualities = [ ( eq, orig ) ]
          , unifyErrors = []
          }

unifyTyConApp
  :: forall nbArgs1 nbArgs2 resKi
  .  Typeable resKi
  => CtOrigin
  -> SwapFlag
  -> ( GenerativeTyCon nbArgs1 resKi, Vec nbArgs1 ( Type Ty ) )
  -> ( GenerativeTyCon nbArgs2 resKi, Vec nbArgs2 ( Type Ty ) )
  -> TcUnifyM ()
unifyTyConApp orig swapped ( tc1, args1 ) ( tc2, args2 )
  | Just Refl <- tcOK
  = unifyTypes orig swapped args1 args2
  | otherwise
  = couldNotUnify TyConAppDifferentTyCon orig swapped
      ( TyConAppTy ( GenerativeTyCon tc1 ) args1 )
      ( TyConAppTy ( GenerativeTyCon tc2 ) args2 )
  where
    tcOK :: Maybe ( nbArgs1 :~: nbArgs2 )
    tcOK = fmap ( \ Refl -> Refl ) $ tc1 `equals2` tc2

unifyTypes :: CtOrigin -> SwapFlag -> Vec n ( Type Ty ) -> Vec n ( Type Ty ) -> TcUnifyM ()
unifyTypes orig swapped as bs = sequence_ $ Vec.zipWith ( unifyType orig swapped ) as bs
{-# INLINEABLE unifyTypes #-}

unifyTyVar :: CtOrigin -> SwapFlag -> TyVar -> Type Ty -> TcUnifyM ()
unifyTyVar _ _ tv1 ( TyVarTy tv2 )
  | tyVarUnique tv1 == tyVarUnique tv2
  = return ()
unifyTyVar orig swapped tv1 ty2' = do
  plat <- lift $ lift $ getPlatform
  subst <- State.get
  let ty2 = normaliseType plat $ applySubst subst ty2'
  case lookupSubst tv1 subst of
    Just ty1 ->
      unifyType orig swapped ty1 ty2
    Nothing
      | TyVarTy tv2 <- ty2
      , tyVarUnique tv1 == tyVarUnique tv2
      -> return ()
      | SkolemTv {} <- tv1
      , TyVarTy ( tv2@( MetaTv {} ) ) <- ty2
      -> unifyTyVar orig ( swap swapped ) tv2 ( TyVarTy tv1 )
      | IntSet.member ( uniqueInt $ tyVarUnique tv1 ) $ seenTvs $ getFVs noBoundVars $ freeTyVarsOfType ty2
      -> couldNotUnify ( OccursCheck tv1 ) orig swapped ( TyVarTy tv1 ) ty2
      | otherwise
      -> case tv1 of
          MetaTv tau1 ->
            State.put $ addOneToSubst ( MetaTv tau1 ) ty2 subst
          SkolemTv sk1 ->
            couldNotUnify ( RigidSkolem sk1 ) orig swapped ( TyVarTy tv1 ) ty2

unifyFunTys :: CtOrigin -> SwapFlag -> NE.NonEmpty ( Type Ty ) -> Type Ty -> NE.NonEmpty ( Type Ty )  -> Type Ty -> TcUnifyM ()
unifyFunTys orig swapped ( arg1 NE.:| args1 ) res1 ( arg2 NE.:| args2 ) res2 = do
  unifyType orig swapped arg1 arg2
  if | argTy1 : rest1 <- args1
     , argTy2 : rest2 <- args2
     -> unifyFunTys orig swapped ( argTy1 NE.:| rest1 ) res1 ( argTy2 NE.:| rest2 ) res2
     | argTy1 : rest1 <- args1
     -> unifyType orig swapped ( FunTy ( argTy1 NE.:| rest1 ) res1 ) res2
     | argTy2 : rest2 <- args2
     -> unifyType orig swapped res1 ( FunTy ( argTy2 NE.:| rest2 ) res2 )
     | otherwise
     -> unifyType orig swapped res1 res2

couldNotUnify :: Typeable ki => CouldNotUnifyReason -> CtOrigin -> SwapFlag -> Type ki -> Type ki -> TcUnifyM ()
couldNotUnify rea orig swapped ty1 ty2 = do
  srcSpan <- lift $ lift getSrcSpan
  let
    oneErrorHere :: UnificationError -> UnifyResult
    oneErrorHere err = UnifyResult [] [ ( err, srcSpan ) ]
  Writer.tell $ oneErrorHere $
    case swapped of
      NotSwapped -> CouldNotUnify rea orig ty1 ty2
      Swapped    -> CouldNotUnify rea orig ty2 ty1

{-------------------------------------------------------------------------------
  Typechecking macros: normalisation
-------------------------------------------------------------------------------}

-- | Normalise a type by reducing reducible type-family applications.
normaliseType :: C.Type.Platform -> Type ki -> Type ki
normaliseType plat ty =
  case ty of
    TyVarTy {} -> ty
    FunTy args res ->
      FunTy ( fmap ( normaliseType plat ) args ) ( normaliseType plat res )
    NomEqPred lhs rhs ->
      NomEqPred ( normaliseType plat lhs ) ( normaliseType plat rhs )
    TyConAppTy tc args ->
      let
        args'  = fmap ( normaliseType plat ) args
        tcApp' = TyConAppTy tc args'
      in
        case tc of
          FamilyTyCon fam ->
            fromMaybe tcApp' $ reduceTyFamApp plat fam args'
          GenerativeTyCon {} ->
            tcApp'

reduceTyFamApp :: C.Type.Platform -> FamilyTyCon n -> Vec n ( Type Ty ) -> Maybe ( Type Ty )
reduceTyFamApp platform = \case
  PlusResTyCon       -> adapt $ C.Op.opResType platform $ C.Op.UnaryOp  C.Op.UnaryPlus
  MinusResTyCon      -> adapt $ C.Op.opResType platform $ C.Op.UnaryOp  C.Op.UnaryMinus
  AddResTyCon        -> adapt $ C.Op.opResType platform $ C.Op.BinaryOp C.Op.Add
  SubResTyCon        -> adapt $ C.Op.opResType platform $ C.Op.BinaryOp C.Op.Sub
  MultResTyCon       -> adapt $ C.Op.opResType platform $ C.Op.BinaryOp C.Op.Mult
  DivResTyCon        -> adapt $ C.Op.opResType platform $ C.Op.BinaryOp C.Op.Div
  RemResTyCon        -> adapt $ C.Op.opResType platform $ C.Op.BinaryOp C.Op.Rem
  ComplementResTyCon -> adapt $ C.Op.opResType platform $ C.Op.UnaryOp  C.Op.BitwiseNot
  BitsResTyCon       -> adapt $ C.Op.opResType platform $ C.Op.BinaryOp C.Op.BitwiseAnd
  ShiftResTyCon      -> adapt $ \ ( ty ::: VNil ) ->
                          -- NB: need to adapt to the fact that bit shift operators
                          -- are binary, but the result type family only cases on
                          -- the first argument (the shiftee) and not the
                          -- second argument (the shift amount).
                                C.Op.opResType platform ( C.Op.BinaryOp C.Op.ShiftLeft )
                                  ( ty ::: cIntTy ::: VNil )

  where
    cIntTy :: C.Type.Type CType
    cIntTy = C.Type.Arithmetic ( C.Type.Integral $ C.Type.IntLike $ C.Type.Int C.Type.Signed )
    adapt :: ( Vec n ( C.Type.Type CType ) -> Maybe ( C.Type.Type CType ) )
          -> Vec n ( Type Ty ) -> Maybe ( Type Ty )
    adapt f args = do
      args' <- traverse fromMacroType args
      res   <- f args'
      toMacroType res

-- | A recursive newtype, which instantiates the v'C.Type.Ptr' constructor of
-- t'C.Type.Type' to t'C.Type.Type' itself.
newtype CType = CType ( C.Type.Type CType )
  deriving stock Eq

toMacroType :: C.Type.Type CType -> Maybe ( Type Ty )
toMacroType = \case
  -- TODO: the below should probably be throwPure_TODO,
  -- but I can't figure a way to trigger this codepath; maybe it isn't possible yet.
  -- Explicit casts would be one way to introduce `void`, but they don't work (yet).
  -- https://github.com/well-typed/hs-bindgen/issues/441
  C.Type.Void          -> panicPure "C macro typechecker does not support 'void' (yet)"
  C.Type.Arithmetic a  ->
    case a of
      C.Type.Integral  i -> Just $ IntLike   $ PrimIntInfoTy   $ CIntegralType i
      C.Type.FloatLike f -> Just $ FloatLike $ PrimFloatInfoTy f
  C.Type.Ptr ( CType a ) -> Ptr <$> toMacroType a

fromMacroType :: Type Ty -> Maybe ( C.Type.Type CType )
fromMacroType = \case
  TyVarTy {} -> Nothing
  FunTy {} -> Nothing
  TyConAppTy tc args ->
    case tc of
      FamilyTyCon {} -> Nothing
      GenerativeTyCon ( DataTyCon dat ) ->
        case dat of
          TupleTyCon {} -> Nothing
          VoidTyCon -> Just $ C.Type.Void
          CharLitTyCon -> Nothing
          IntLikeTyCon ->
            case args of
              ( a ::: VNil ) ->
                case a of
                  PrimIntInfoTy (CIntegralType inty) ->
                    Just $ C.Type.Arithmetic $ C.Type.Integral inty
                  _ -> Nothing
          FloatLikeTyCon ->
            case args of
              ( a ::: VNil ) ->
                case a of
                  PrimFloatInfoTy floaty ->
                    Just $ C.Type.Arithmetic $ C.Type.FloatLike floaty
                  _ -> Nothing
          PtrTyCon       ->
            case args of
              ( a ::: VNil ) ->
                C.Type.Ptr . CType <$> fromMacroType a

          PrimIntInfoTyCon {} -> panicPure "fromMacroType: 'PrimIntInfoTyCon'"
          PrimFloatInfoTyCon {} -> panicPure "fromMacroType: 'PrimFloatInfoTyCon'"
          PrimTyTyCon -> panicPure "fromMacroType: 'PrimTyTyCon'"
          EmptyTyCon  -> panicPure "fromMacroType: 'EmptyTyCon'"

{-------------------------------------------------------------------------------
  Typechecking macros: instantiation
-------------------------------------------------------------------------------}

instantiate
  :: forall nbBinders body
  .  Nat.SNatI nbBinders
  => CtOrigin -> InstOrigin
  -> ( Vec nbBinders ( Type Ty ) -> QuantTyBody body )
  -> TcGenM ( Vec nbBinders ( Type Ty ), body )
instantiate ctOrig instOrig body = do
  tvs <-
    for ( tyVarNames @nbBinders ) \ ( i, tvName ) ->
      newMetaTyVarTy ( Inst { instOrigin = instOrig, instPos = i } ) tvName
  let QuantTyBody cts bodyTy = body tvs
  Writer.tell $ ( map (, ctOrig ) cts, mempty )
  return ( tvs, bodyTy )

{-------------------------------------------------------------------------------
  Typechecking macros: type inference
-------------------------------------------------------------------------------}

applySubstNormalise :: C.Type.Platform -> Subst tv -> Type ki -> Type ki
applySubstNormalise plat subst = normaliseType plat . applySubst subst

-- | Infer the type of a macro declaration (before constraint solving and generalisation).
inferTop :: CName -> [ CName ] -> MExpr -> TcUniqueM ( ( Type Ty, Cts ), [ ( TcError, SrcSpan ) ] )
inferTop funNm argsList body =
  Vec.reifyList argsList \ args -> do
    plat <- lift getPlatform
    ( ( ( argTys, bodyTy ), ( cts, mbErrs ) ), subst ) <- runTcGenMTcM ( inferLam funNm args body )
    let macroTy =
          case NE.nonEmpty $ toList argTys of
            Nothing -> bodyTy
            Just argTysNE -> FunTy argTysNE bodyTy
        macroTy' = applySubstNormalise plat subst macroTy
        cts' = map ( first ( applySubstNormalise plat subst ) ) cts
    debugTraceM $ unlines
      [ "inferTop " ++ show funNm
      , "ty: " ++ show macroTy'
      , "cts: " ++ show cts'
      , "final subst: " ++ show subst
      ]
    return ( ( macroTy', cts' ), mbErrs )

inferExpr :: MExpr -> TcGenM ( Type Ty )
inferExpr = \case
  MEmpty -> return Empty
  MTerm tm -> inferTerm tm
  MApp fun args -> inferApp ( FunName $ Right fun ) ( Vec.toList args )

inferTerm :: MTerm -> TcGenM ( Type Ty )
inferTerm = \case
  MInt i@( IntegerLiteral { integerLiteralType = mbIntyTy } ) ->
    IntLike <$>
      case mbIntyTy of
        Just intyTy ->
          return $ PrimIntInfoTy $ CIntegralType $ fromPrimIntTy intyTy
        Nothing ->
          newMetaTyVarTy (IntLitMeta i) "i"
  MFloat f@( FloatingLiteral { floatingLiteralType = mbFloatyTy }) ->
    FloatLike <$>
      case mbFloatyTy of
        Just floatyTy ->
          return $ PrimFloatInfoTy $ fromPrimFloatTy floatyTy
        Nothing ->
          newMetaTyVarTy (FloatLitMeta f) "f"
  MChar {} ->
    return CharLitTy
  MString {} ->
    return String
  MVar fun args -> inferApp ( FunName $ Left fun ) args
  MType {} -> return PrimTy
  MAttr _attr mbTerm ->
    case mbTerm of
      Nothing -> return Empty
      Just tm -> inferTerm tm
  MStringize {} -> return String
  MConcat a1 a2 -> do
    ty1 <- inferTerm a1
    ty2 <- inferTerm a2
    let orig = AppOrigin ( FunName ( Left "##" ) )
    liftUnifyM $ unifyType orig NotSwapped ty1 String
    liftUnifyM $ unifyType orig NotSwapped ty2 String
    return String

-- TODO: these functions would go away if we change the type we store
-- in the v'MFloat'/v'MInt' constructors to re-use the @c-expr@ library.
fromPrimIntTy :: ( PrimIntType, PrimSign ) -> C.Type.IntegralType
fromPrimIntTy ( i, s ) = C.Type.IntLike $
  case i of
    PrimShort    -> C.Type.Short    s'
    PrimInt      -> C.Type.Int      s'
    PrimLong     -> C.Type.Long     s'
    PrimLongLong -> C.Type.LongLong s'
  where
    s' = fromMacroSign s
fromMacroSign :: PrimSign -> C.Type.Sign
fromMacroSign = \case
  Signed   -> C.Type.Signed
  Unsigned -> C.Type.Unsigned
fromPrimFloatTy :: PrimFloatType -> C.Type.FloatingType
fromPrimFloatTy = \case
  PrimFloat      -> C.Type.FloatType
  PrimDouble     -> C.Type.DoubleType
  PrimLongDouble -> throwPure_TODO 349 "tcMacro: long double not supported"

inferApp :: FunName -> [ MExpr ] -> TcGenM ( Type Ty )
inferApp fun mbArgs = do
  funTy <- inferFun fun
  case NE.nonEmpty mbArgs of
    Nothing ->
      return funTy
    Just args -> do
      argTys <- traverse inferExpr args
      resTy <- newMetaTyVarTy ( ExpectedFunTyResTy fun ) "r"
      let actualTy = FunTy argTys resTy
      liftUnifyM $ unifyType ( AppOrigin fun ) NotSwapped actualTy funTy
      return resTy

inferFun :: FunName -> TcGenM ( Type Ty )
inferFun funNm@( FunName fun ) =
  case fun of
    Left varNm@( CName varStr ) -> do
      mbTy <- liftTcPureM $ lookupVarType varNm
      case mbTy of
        Just varTy -> return varTy
        Nothing -> do
          mbQTy <- liftTcPureM $ lookupTyEnv varNm
          case mbQTy of
            Just ( Quant funQTy ) ->
              snd <$>
                instantiate ( FunInstOrigin funNm ) ( FunInstMetaOrigin funNm ) funQTy
            Nothing -> do
              addErrTcGenM $ UnboundVariable varNm
              newMetaTyVarTy ( ExpectedVarTy varNm ) ( varStr <> "_ty" )
    Right mFun  ->
      case inferMFun mFun of
        Quant funQTy ->
          snd <$>
            instantiate ( FunInstOrigin funNm ) ( FunInstMetaOrigin funNm ) funQTy

inferLam :: forall n. CName -> Vec n CName -> MExpr -> TcGenM ( Vec n ( Type Ty ), Type Ty )
inferLam _ VNil body = ( VNil, ) <$> inferExpr body
inferLam funNm argNms@( _ ::: _ ) body = do
  let is = Vec.imap ( \ i _ -> fromIntegral ( Fin.toNatural i ) + 1 ) argNms
  argTys <-
    for ( Vec.zipWith (,) is argNms ) \ ( i, argNm@( CName argStr ) ) ->
      newMetaTyVarTy ( FunArg funNm ( argNm, i ) ) ( "ty_" <> argStr )
  liftBaseTcM ( declareLocalVars ( Map.fromList $ toList $ Vec.zipWith (,) argNms argTys ) ) $
    ( argTys, ) <$> inferExpr body

inferMFun :: MFun arity -> Quant ( Type Ty )
inferMFun = \case

  -- Tuple
  MTuple @n -> Quant @(S (S n)) \ as ->
    QuantTyBody [] $ funTy (Vec.toList as) (Tuple (fromIntegral $ length as ) as)

  -- Logical operators
  MLogicalNot -> q1 \ a   -> QuantTyBody [Not  a]      $ funTy [a]   IntTy
  MLogicalAnd -> q2 \ a b -> QuantTyBody [Logical a b] $ funTy [a,b] IntTy
  MLogicalOr  -> q2 \ a b -> QuantTyBody [Logical a b] $ funTy [a,b] IntTy

  -- Comparison operators
  MRelEQ      -> q2 \ a b -> QuantTyBody [RelEq a b]    $ funTy [a,b] IntTy
  MRelNE      -> q2 \ a b -> QuantTyBody [RelEq a b]    $ funTy [a,b] IntTy
  MRelLT      -> q2 \ a b -> QuantTyBody [RelOrd a b]   $ funTy [a,b] IntTy
  MRelLE      -> q2 \ a b -> QuantTyBody [RelOrd a b]   $ funTy [a,b] IntTy
  MRelGT      -> q2 \ a b -> QuantTyBody [RelOrd a b]   $ funTy [a,b] IntTy
  MRelGE      -> q2 \ a b -> QuantTyBody [RelOrd a b]   $ funTy [a,b] IntTy

  -- Arithmetic operators

    -- Unary
  MUnaryPlus  -> q1 \ a   -> QuantTyBody [Plus  a] $ funTy [a] ( PlusRes  a )
  MUnaryMinus -> q1 \ a   -> QuantTyBody [Minus a] $ funTy [a] ( MinusRes a )

    -- Additive
  MAdd        -> q2 \ a b -> QuantTyBody [Add a b] $ funTy [a,b] ( AddRes a b )
  MSub        -> q2 \ a b -> QuantTyBody [Sub a b] $ funTy [a,b] ( SubRes a b )

    -- Multiplicative
  MMult       -> q2 \ a b -> QuantTyBody [Mult a b] $ funTy [a,b] ( MultRes a b )
  MDiv        -> q2 \ a b -> QuantTyBody [Div  a b] $ funTy [a,b] ( DivRes  a b )
  MRem        -> q2 \ a b -> QuantTyBody [Rem  a b] $ funTy [a,b] ( RemRes  a b )

    -- Bit
  MBitwiseNot -> q1 \ a   -> QuantTyBody [Complement a] $ funTy [a]   ( ComplementRes a )
  MBitwiseAnd -> q2 \ a b -> QuantTyBody [Bitwise a b]  $ funTy [a,b] ( BitsRes a b )
  MBitwiseXor -> q2 \ a b -> QuantTyBody [Bitwise a b]  $ funTy [a,b] ( BitsRes a b )
  MBitwiseOr  -> q2 \ a b -> QuantTyBody [Bitwise a b]  $ funTy [a,b] ( BitsRes a b )

    -- Bit shift
  MShiftLeft  -> q2 \ a i -> QuantTyBody [Shift a i] $ funTy [a,i] ( ShiftRes a )
  MShiftRight -> q2 \ a i -> QuantTyBody [Shift a i] $ funTy [a,i] ( ShiftRes a )
  where
    q1 body = Quant @( S Z )       \ (a ::: VNil) -> body a
    q2 body = Quant @( S ( S Z ) ) \ (a ::: i ::: VNil) -> body a i
    funTy mbArgs res =
      case NE.nonEmpty mbArgs of
        Just args -> FunTy args res
        Nothing   -> res

pattern Class :: ClassTyCon nbArgs -> Vec nbArgs ( Type Ty ) -> Type Ct
pattern Class cls args = TyConAppTy ( GenerativeTyCon ( ClassTyCon cls ) ) args
{-# COMPLETE Class, NomEqPred #-}

pattern Data :: DataTyCon nbArgs -> Vec nbArgs ( Type Ty ) -> Type Ty
pattern Data tc args = TyConAppTy ( GenerativeTyCon ( DataTyCon tc ) ) args
pattern FamApp :: FamilyTyCon nbArgs -> Vec nbArgs ( Type Ty ) -> Type Ty
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
pattern PrimTy :: Type Ty
pattern PrimTy = Data PrimTyTyCon VNil
pattern Empty :: Type Ty
pattern Empty = Data EmptyTyCon VNil
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

-- Convenient synonyms
pattern IntTy :: Type Ty
pattern IntTy = IntLike ( PrimIntInfoTy ( CIntegralType ( C.Type.IntLike ( C.Type.Int C.Type.Signed ) ) ) )
pattern HsIntTy :: Type Ty
pattern HsIntTy = IntLike ( PrimIntInfoTy ( HsIntType ) )
pattern CharTy :: Type Ty
pattern CharTy = IntLike ( PrimIntInfoTy ( CIntegralType ( C.Type.CharLike C.Type.Char ) ) )

pattern CharLitTy :: Type Ty
pattern CharLitTy = Data CharLitTyCon VNil

isPrimTy :: forall n. Nat.SNatI n => (Vec n (Type Ty) -> QuantTyBody ( Type Ty ) ) -> Bool
isPrimTy bf = case Nat.snat @n of
    Nat.SZ -> isPrimTy' (bf VNil)
    Nat.SS -> False

isPrimTy' :: QuantTyBody ( Type Ty ) -> Bool
isPrimTy' (QuantTyBody [] PrimTy) = True
isPrimTy' _                       = False

{-------------------------------------------------------------------------------
  Typechecking macros: classes
--------------------------------------------------------------------------------

The following pieces of information determine how class constraints are solved:

  1. The superclass implication structure, as specified by the function
     'classSuperclasses'.

  2. Class instances, as specified by the function
     'classInstancesWithDefaults'.
-}

-- | The type constructor tag of a 'DataTyCon' or 'ClassTyCon'.
type TyConTag :: Kind -> Hs.Type
newtype TyConTag ki = TyConTag Int
  deriving stock ( Eq, Ord, Show )

-- | The type constructor tag of a 'DataTyCon'.
--
--See Note [Class instances and DataTyConTag].
type DataTyConTag  = TyConTag Ty
-- | The type constructor tag of a 'ClassTyCon'.
type ClassTyConTag = TyConTag Ct

-- | What heads this type?
--
-- Used for class instance matching.
data TypeHead
  -- | The type is headed by the function type constructor.
  = FunTyHead
  -- | The type is headed by the type constructor with the given 'DataTyConTag'.
  | TyConHead !DataTyConTag
  deriving stock ( Eq, Ord, Show )

-- | A defaulting proposal, returning a collection of additional equalities
-- between the input types.
type DefaultingProposal nbBinders =
  Vec nbBinders ( Type Ty ) -> NE.NonEmpty ( Type Ty, Type Ty )

-- | An instance for a class constraint, corresponding to the quantified
-- type at the head of the instance.
--
-- For example, the instance
--
-- @instance forall x y. ( x ~ y ) => Cls x y Int@
--
-- is represented by the quantified type
--
-- @forall x y. ( x ~ y ) => Cls x y Int@.
data Instance where
  Instance
    :: forall nbArgs nbBinders
    .  ( Nat.SNatI nbArgs, Nat.SNatI nbBinders )
    => { instanceQuantTy  :: !( Vec nbBinders ( Type Ty ) -> QuantTyBody ( Vec nbArgs ( Type Ty ) ) )
       , instanceDefaults :: !( [ DefaultingProposal nbBinders ] )
       }
    -> Instance

-- | A trie, used to look up class instances.
data TrieMap k a =
  Trie
    { value    :: [ a ]
    , children :: Map ( Maybe k ) ( TrieMap k a )
    }
  deriving stock ( Eq, Show, Functor, Foldable, Traversable )

instance Ord k => Semigroup ( TrieMap k a ) where
  Trie v1 c1 <> Trie v2 c2 = Trie ( v1 ++ v2 ) ( c1 <> c2 )
instance Ord k => Monoid ( TrieMap k a ) where
  mempty = Trie [] Map.empty

insertTrie :: Ord k => [ Maybe k ] -> a -> TrieMap k a -> TrieMap k a
insertTrie []         v ( Trie vs cs ) = Trie ( v : vs ) cs
insertTrie ( p : ps ) v ( Trie vs cs ) =
  Trie vs $
    let child = Map.findWithDefault mempty p cs
        newChild = insertTrie ps v child
     in Map.insert p newChild cs

lookupTrie :: Ord k => [ Maybe k ] -> TrieMap k a -> [ a ]
lookupTrie []         ( Trie vs _ ) = vs
lookupTrie ( p : ps ) ( Trie _ cs ) =
  case p of
   Nothing ->
     concatMap   ( lookupTrie ps ) ( Map.elems cs )
   _         ->
        maybe [] ( lookupTrie ps ) ( Map.lookup p       cs )
     ++ maybe [] ( lookupTrie ps ) ( Map.lookup Nothing cs )

isEmptyTrie :: TrieMap k a -> Bool
isEmptyTrie ( Trie vs cs ) = null vs && null cs

trieFromList :: Ord k => [ ( [ Maybe k ], v ) ] -> TrieMap k v
trieFromList = foldl' ( \ t ( k, v ) -> insertTrie k v t ) mempty

mapMaybeATrie :: ( Ord k, Applicative f ) => ( a -> f ( Maybe b ) ) -> TrieMap k a -> f ( TrieMap k b )
mapMaybeATrie f ( Trie vs cs ) =
  Trie
    <$> mapMaybeA f vs
    <*> ( `Map.traverseMaybeWithKey` cs )
         ( \ _key -> fmap ( guarded ( not . isEmptyTrie ) ) . mapMaybeATrie f )

type InstanceKey = [ Maybe TypeHead ]

instanceKey :: Instance -> InstanceKey
instanceKey ( Instance { instanceQuantTy = qty } ) =
  map typeHead $ Vec.toList $ quantTyBody ( mkQuantTyBody ( Quant qty ) )

argsTypeHeads :: Vec n ( Type Ty ) -> [ Maybe TypeHead ]
argsTypeHeads = toList . fmap typeHead

typeHead :: Type Ty -> Maybe TypeHead
typeHead = \case
  FunTy {} ->
    Just FunTyHead
  TyConAppTy tc _ ->
    case tc of
      GenerativeTyCon ( DataTyCon dc ) ->
        Just $ TyConHead $ dataTyConTag dc
      FamilyTyCon {} ->
        Nothing
  TyVarTy {} ->
    Nothing

-- | An instance environment.
type InstEnv = forall nbArgs. ClassTyCon nbArgs -> TrieMap TypeHead Instance

-- | The superclass structure of built-in classes.
classSuperclasses :: forall nbArgs. ClassTyCon nbArgs -> ( Vec nbArgs ( Type Ty ) -> [ Type Ct ] )
classSuperclasses cls =
  case cls of
    NotTyCon        -> noSCs
    LogicalTyCon    -> noSCs
    RelEqTyCon      -> noSCs
    RelOrdTyCon     -> \ ( a ::: b ::: VNil ) -> [ RelEq a b ]
    PlusTyCon       -> noSCs
    MinusTyCon      -> noSCs
    AddTyCon        -> noSCs
    SubTyCon        -> noSCs
    MultTyCon       -> noSCs
    DivTyCon        -> noSCs
    RemTyCon        -> noSCs
    ComplementTyCon -> noSCs
    BitwiseTyCon    -> noSCs
    ShiftTyCon      -> noSCs
  where
    noSCs = const []

-- | Built-in top-level class instances, with associated defaulting assignments.
classInstancesWithDefaults :: forall nbClsArgs. ClassTyCon nbClsArgs -> TrieMap TypeHead Instance
classInstancesWithDefaults cls =
  trieFromList . map ( \ i -> ( instanceKey i, i ) ) $
    case cls of
      NotTyCon        -> [i1]
      LogicalTyCon    -> [ii2]
      RelEqTyCon      -> [ii2, ff2, if2, fi2, str2]
      RelOrdTyCon     -> [ii2, ff2, if2, fi2, str2]
      PlusTyCon       -> [i1, f1]
      MinusTyCon      -> [i1, f1]
      AddTyCon        -> [ii2, ff2, if2, fi2]
      SubTyCon        -> [ii2, ff2, if2, fi2]
      MultTyCon       -> [ii2, ff2, if2, fi2]
      DivTyCon        -> [ii2, ff2, if2, fi2]
      RemTyCon        -> [ii2]
      ComplementTyCon -> [i1]
      BitwiseTyCon    -> [ii2]
      ShiftTyCon      -> [ii2] -- TODO: default two arguments separately
  where

    primIntTy    = PrimIntInfoTy $ CIntegralType $ C.Type.IntLike $ C.Type.Int C.Type.Signed
    primDoubleTy = PrimFloatInfoTy C.Type.DoubleType

    dfltToInt, dfltToDouble :: DefaultingProposal ( S Z )
    dfltToInt    ( a ::: VNil ) = NE.singleton ( a, primIntTy    )
    dfltToDouble ( a ::: VNil ) = NE.singleton ( a, primDoubleTy )

    dfltToEqual :: DefaultingProposal ( S ( S Z ) )
    dfltToEqual ( a ::: b ::: VNil ) = NE.singleton ( a, b )

    i1, f1, ii2, ff2, if2, fi2, str2 :: Instance
    i1   = mkNAry [ dfltToInt    ] ( IntLike   ::: VNil )
    f1   = mkNAry [ dfltToDouble ] ( FloatLike ::: VNil )
    ii2  = mkNAry [ dfltToEqual ] ( IntLike   ::: IntLike   ::: VNil )
    ff2  = mkNAry [ dfltToEqual ] ( FloatLike ::: FloatLike ::: VNil )
    if2  = mkNAry [ ] ( IntLike   ::: FloatLike ::: VNil )
    fi2  = mkNAry [ ] ( FloatLike ::: IntLike   ::: VNil )
    str2 = mkNAryNoForall ( String ::: String ::: VNil )

    mkNAryNoForall :: forall nbArgs. Vec nbArgs ( Type Ty ) -> Instance
    mkNAryNoForall tys =
      let qty :: Vec Z ( Type Ty ) -> QuantTyBody ( Vec nbArgs ( Type Ty ) )
          qty _ = QuantTyBody [] tys
      in
        Vec.withDict tys $
          Instance
            { instanceQuantTy  = qty
            , instanceDefaults = []
            }

    mkNAry :: [ DefaultingProposal nbArgs ] -> Vec nbArgs ( Type Ty -> Type Ty ) -> Instance
    mkNAry dflts tcs =
      Vec.withDict tcs $
        Instance
          { instanceQuantTy  = \ args -> QuantTyBody [] ( Vec.zipWith ($) tcs args )
          , instanceDefaults = dflts
          }

-- | Get the 'DataTyConTag' associated with a type constructor.
--
-- See Note [Class instances and DataTyConTag].
dataTyConTag :: DataTyCon args -> DataTyConTag
dataTyConTag tc = TyConTag $ I# ( dataToTag# tc )

-- | Get the 'ClassTyConTag' associated with a class.
classTyConTag :: ClassTyCon args -> ClassTyConTag
classTyConTag cls = TyConTag $ I# ( dataToTag# cls )

{-------------------------------------------------------------------------------
  Typechecking macros: constraint solving monad
-------------------------------------------------------------------------------}

data Solubility
  = Soluble
  | Insoluble
  deriving stock ( Eq, Ord, Show )
data InertSet =
  InertSet
    { inertDicts :: !( Map ClassTyConTag ( TrieMap TypeHead ( ( Type Ct, CtOrigin ), Solubility ) ) )
    , inertEqs   :: ![ ( ( Type Ct, CtOrigin ), Solubility ) ]
    }
  deriving stock Show

{-
liftTcGenM :: TcGenM a -> TcUniqueM ( Maybe ( a, ( Subst TyVar, Cts ) ) )
liftTcGenM ( Writer.WriterT ( State.StateT f ) ) =
  State.StateT \ ( u, st ) ->
    let
      aux :: ( ( a, ( Cts, [ ( TcError, SrcSpan ) ] ) ), ( Unique, Subst TyVar ) )
          -> ( Maybe ( a, ( Subst TyVar, Cts ) ), ( Unique, SolverState ) )
      aux ( ( a, ( cts, errs ) ), ( u', subst' ) ) =
        if null errs
        then ( Just ( a, ( subst', cts ) ), ( u', st ) )
        else ( Nothing, ( u, st ) )
    in
    fmap aux $ f ( u, solverSubst st )
-}

emptyInertSet :: InertSet
emptyInertSet =
  InertSet { inertDicts = Map.empty, inertEqs = [] }

modifyingInerts :: ( InertSet -> InertSet ) -> TcSolveM ()
modifyingInerts f =
  State.modify' $
    \ st@( SolverState { solverInerts = inerts } ) ->
       st { solverInerts = f inerts }

inertCts :: InertSet -> ( Cts, Cts )
inertCts ( InertSet { inertDicts = dicts, inertEqs = eqs } ) =
  partitionEithers ( concatMap ( fmap classify . toList ) $ dicts )
    <>
  partitionEithers ( map classify eqs )
  where
    classify ( a, sol ) =
      case sol of
        Soluble   -> Right a
        Insoluble -> Left a

mapMaybeInerts :: ( Type Ct -> Maybe ( Type Ct ) ) -> InertSet -> ( InertSet, Cts )
mapMaybeInerts f inerts@( InertSet { inertDicts = dicts, inertEqs = eqs } ) =
  let kick :: ( ( Type Ct, CtOrigin ), Solubility ) -> Writer.Writer Cts ( Maybe ( ( Type Ct, CtOrigin ), Solubility ) )
      kick ct@( ( ctPred, ctOrig ), _ ) =
        case f ctPred of
          Just ctPred' -> do
            Writer.tell [ ( ctPred', ctOrig ) ]
            return Nothing
          Nothing ->
            return $ Just ct
      ( keptDicts, kickedDicts ) =
        Writer.runWriter $
          ( `Map.traverseMaybeWithKey` dicts ) \ _key ->
            fmap ( guarded ( not . isEmptyTrie ) ) . mapMaybeATrie kick
      ( keptEqs  , kickedEqs   ) = Writer.runWriter $ mapMaybeA kick eqs
   in ( inerts { inertDicts = keptDicts, inertEqs = keptEqs }
      , kickedDicts ++ kickedEqs
      )

-- | State for the 'TcSolveM' constraint solving monad.
data SolverState
  = SolverState
  { solverSubst    :: !( Subst TyVar )
  , solverInerts   :: !InertSet
  , solverWorkList :: !Cts
  }
  deriving stock Show

-- | Monad for solving constraints.
type TcSolveM = StateT SolverState TcUniqueM

initSolverState :: Cts -> SolverState
initSolverState cts0 =
  SolverState
    { solverSubst    = mempty
    , solverInerts   = emptyInertSet
    , solverWorkList = cts0
    }

emitWork :: Subst TyVar -> Cts -> TcSolveM ()
emitWork subst newCts = do
  unless ( null newCts ) $
    debugTraceM $
      unlines $
        "emitting new work" : map ( ( " - " ++ ) . show ) newCts
  State.modify' $
    \ st@( SolverState
          { solverSubst    = subst0
          , solverWorkList = wl0 } ) ->
        st
          { solverSubst    = subst0 <> subst
          , solverWorkList = wl0 ++ newCts
          }
  kickOut subst

addInertDict :: Solubility
             -> ( ( ClassTyCon nbArgs, Vec nbArgs ( Type Ty ) ), CtOrigin )
             -> InertSet -> InertSet
addInertDict sol ( ( cls, args ), ctOrig ) inerts@( InertSet { inertDicts = dicts } ) =
  inerts { inertDicts = Map.alter doInsert ( classTyConTag cls ) dicts }
    where
      ct = Class cls args
      key = argsTypeHeads args
      doInsert = Just . insertTrie key ( ( ct, ctOrig ), sol ) . fromMaybe mempty

addInertEq :: Solubility -> ( Type Ct, CtOrigin ) -> InertSet -> InertSet
addInertEq sol eq inerts@( InertSet { inertEqs = eqs } ) =
  inerts { inertEqs = eqs ++ [ ( eq, sol ) ] }

nextWorkItem :: TcSolveM ( Maybe ( Type Ct, CtOrigin ) )
nextWorkItem = do
  st@( SolverState { solverWorkList = wl } ) <- State.get
  case wl of
    [] -> return Nothing
    ct : others -> do
      State.put $ st { solverWorkList = others }
      return $ Just ct

solvingLoop :: ( ( Type Ct, CtOrigin ) -> TcSolveM () ) -> TcSolveM ()
solvingLoop solveOne = loop 1
  where
    loop :: Int -> TcSolveM ()
    loop !iter = do
      mbWorkItem <- nextWorkItem
      for_ mbWorkItem \ workItem -> do
        debugTraceM $
          unlines
            [ "solvingLoop: iteration #" ++ show iter
            , "work item: " ++ show workItem
            ]
        solveOne workItem
        loop ( iter + 1 )

runTcSolveM :: Cts -> TcSolveM a -> TcUniqueM ( a, ( Subst TyVar, ( Cts, Cts ) ) )
runTcSolveM cts ( State.StateT f ) =
  fmap aux $ f ( initSolverState cts )

  where
    aux :: ( a, SolverState ) -> ( a, ( Subst TyVar, ( Cts, Cts ) ) )
    aux ( a, st ) =
      ( a, ( solverSubst st , inertCts ( solverInerts st ) ) )

{-------------------------------------------------------------------------------
  Typechecking macros: constraint solving
-------------------------------------------------------------------------------}

-- | Solve a constraint, either directly or by matching against
-- the provided instance environment.
solveCt :: HandleOverlap -> Defaulting -> InstEnv -> ( Type Ct, CtOrigin ) -> TcSolveM ()
solveCt handleOverlap defaulting instEnv ( ct, ctOrig ) =
  case ct of
    NomEqPred a b ->
      solveEqCt ctOrig a b
    Class cls args ->
      solveDictCt handleOverlap defaulting ctOrig cls ( instEnv cls ) args

-- | Solve an equality constraint.
solveEqCt :: CtOrigin -> Type Ty -> Type Ty -> TcSolveM ()
solveEqCt ctOrig lhs rhs = do
  ( ( (), UnifyResult eqs errs ), innerSubst ) <-
    lift $ lift $ ( `State.runStateT` mempty ) $ Writer.runWriterT $
      unifyType ctOrig NotSwapped lhs rhs
  let
    sameOld other =
      case other of
        NomEqPred lhs' rhs'
          |  lhs `eqType` lhs' && rhs `eqType` rhs'
          || lhs `eqType` rhs' && rhs `eqType` lhs'
          -> Left ()
        _ -> Right other
    ( noProgress, progress ) =
      partitionEithers $
        map
          ( \ ( ct, orig ) -> ( , orig ) <$> sameOld ct )
          eqs
    mkInsol :: UnificationError -> Maybe ( Type Ct, CtOrigin )
    mkInsol ( CouldNotUnify @ki _rea ctOrig' lhs' rhs' ) =
      ( eqT @ki @Ty ) <&> \ Refl ->
        ( NomEqPred lhs' rhs', ctOrig' )

  modifyingInerts $
      ( appEndo $ foldMap ( Endo . addInertEq Insoluble ) $ mapMaybe ( mkInsol . fst ) errs )
    . ( if null noProgress then id else addInertEq Soluble ( NomEqPred lhs rhs, ctOrig ) )
  emitWork innerSubst progress

-- | Look up a constraint in the inert set of the solver.
lookupCt :: Type Ct -> TcSolveM ( Maybe Bool )
lookupCt ct = do
  SolverState { solverInerts = inerts } <- State.get
  return $
    case ct of
      Class cls args -> do
        dicts <- Map.lookup ( classTyConTag cls ) $ inertDicts inerts
        finish $ mapMaybe ( matchWithSCs . first fst )
               $ lookupTrie ( argsTypeHeads args ) dicts
      NomEqPred {} ->
        finish $ mapMaybe ( matchEq . first fst )
               $ inertEqs inerts
  where
    finish :: [ Solubility ] -> Maybe Bool
    finish []   = Nothing
    finish sols = Just $ any ( == Soluble ) sols
    matchEq :: ( Type Ct, Solubility ) -> Maybe Solubility
    matchEq ( pty, sol ) =
      case pty of
        TyConAppTy {} -> Nothing
        NomEqPred lhs rhs -> do
          guard $
            any ( eqType ct ) [ pty, NomEqPred rhs lhs ]
          return sol
    matchWithSCs :: ( Type Ct, Solubility ) -> Maybe Solubility
    matchWithSCs ( pty, sol ) =
      case pty of
        NomEqPred {} -> Nothing
        Class cls' args' -> do
          guard $
            any ( eqType ct ) ( pty : classSuperclasses cls' args' )
          return sol

-- | Kick out constraints which mention variables from the domain of the
-- new substitution.
kickOut :: Subst TyVar -> TcSolveM ()
kickOut subst =
  unless ( isEmptySubst subst ) do
    plat <- lift $ lift getPlatform
    st@( SolverState { solverInerts = inerts, solverWorkList = wl0 } ) <- State.get
    let ( okInerts, kickedInerts ) = mapMaybeInerts ( mbKickOut plat ) inerts
    unless ( null kickedInerts ) do
      debugTraceM $ unlines
        [ "kickOut"
        , "subst: " ++ show subst
        , "inerts kicked out: " ++ show kickedInerts
        ]
      State.put $
        st { solverInerts = okInerts, solverWorkList = wl0 ++ kickedInerts }
  where
    mbKickOut :: C.Type.Platform -> Type Ct -> Maybe ( Type Ct )
    mbKickOut plat ct =
      let
        ctFVs = getFVs noBoundVars $ freeTyVarsOfType ct
      in
        if IntSet.null $ seenTvs ctFVs `IntSet.intersection` domain subst
        then
          Nothing
        else
          Just $ applySubstNormalise plat subst ct

-- | Whether to do defaulting or not.
data Defaulting
  = DefaultTyVarsExcept !IntSet
  | Don'tDefault
  deriving stock ( Eq, Show )

-- | How to handle the situation in which multiple instances match?
data HandleOverlap
  -- | Arbitrarily pick the first instance.
  = PickFirst
  -- | Defer the decision.
  | Defer
  deriving stock ( Eq, Show )

-- | Solve a class constraint by looking up in the provided instance environment
-- for this class.
solveDictCt
  :: HandleOverlap
      -- ^ How to handle the presence of multiple matching instances
  -> Defaulting
      -- ^ Do defaulting as well (if possible)?
  -> CtOrigin
  -> ClassTyCon nbArgs
  -> TrieMap TypeHead Instance
  -> Vec nbArgs ( Type Ty )
  -> TcSolveM ()
solveDictCt handleOverlap doDefault ctOrig cls instEnv args = do
  matchingDict <- lookupCt ct
  case matchingDict of
    Just {} -> do
      debugTraceM $ unlines
        [ "solveDictCt: constraint discharged (matching inert)"
        , "ct: " ++ show ct ]
      return ()
    Nothing -> do
      matches <- lift $ mapMaybeA matcher $ lookupTrie ( argsTypeHeads args ) instEnv
      case matches of
        [] -> do
          debugTraceM $ unlines
            [ "solveDictCt: insoluble; adding constraint to inert set"
            , "ct: " ++ show ct ]
          modifyingInerts $
            addInertDict Insoluble ( ( cls, args ), ctOrig )
        ( newCts, subst ) : rest
          | null rest || handleOverlap == PickFirst
          -> do
            debugTraceM $ unlines
              [ "solveDictCt: solved constraint"
              , "ct: " ++ show ct
              , "context: " ++ show newCts
              , "subst: " ++ show subst ]
            emitWork subst newCts
        _ -> do
          debugTraceM $ unlines
            [ "solveDictCt: multiple solutions; adding constraint to inert set"
            , "ct: " ++ show ct ]
          modifyingInerts $
            addInertDict Soluble ( ( cls, args ), ctOrig )
    where
      ct = Class cls args
      matcher :: Instance -> TcUniqueM ( Maybe ( Cts, Subst TyVar ) )
      matcher inst = do
        matchRes <- matchOneInst ctOrig cls inst args
        case matchRes of
          Nothing ->
            do debugTraceM $
                 unlines
                    [ "solveDictCt: matchOne FAILURE"
                    , "ct: " ++ show ct
                    ]
               return Nothing
          Just ( ( newCts, matchSubst ), dfltCands ) -> fmap ( newCts, ) <$> do
            case doDefault of
              Don'tDefault -> do
                debugTraceM $
                  unlines
                    [ "solveDictCt: matchOne SUCCESS (not defaulting)"
                    , "ct: " ++ show ct
                    , "subst: " ++ show matchSubst
                    ]
                return $ Just matchSubst
              DefaultTyVarsExcept qtvs -> do
                candSubsts <- lift dfltCands
                  -- Only do defaulting when no candidate type variables
                  -- for quantification are involved.
                  -- (Alternatively we could choose to default only
                  -- a subset of the type variables, but we don't do so for now.)
                case filter ( doesNotRefine qtvs matchSubst ) candSubsts of
                  [] -> do
                    debugTraceM $
                      unlines
                        [ "solveDictCt: matchOne SUCCESS (no defaulting)"
                        , "qtvs: " ++ show qtvs
                        , "ct: " ++ show ct
                        , "subst: " ++ show matchSubst
                        ]
                    return $ Just matchSubst
                  dfltSubst1 : _ -> do
                    debugTraceM $
                      unlines
                        [ "solveDictCt: matchOne SUCCESS (defaulting)"
                        , "qtvs: " ++ show qtvs
                        , "ct: " ++ show ct
                        , "matchSubst: " ++ show matchSubst
                        , "dfltSubst: " ++ show dfltSubst1
                        ]
                    return $ Just dfltSubst1

doesNotRefine :: IntSet -> Subst tv -> Subst tv -> Bool
doesNotRefine qtvs ( Subst matchSubst ) ( Subst dfltSubst ) =
  and $
    IntMap.intersectionWith ( \ ( _, ty1 ) ( _, ty2 ) -> ty1 `eqType` ty2 )
      ( matchSubst `IntMap.restrictKeys` qtvs )
      ( dfltSubst  `IntMap.restrictKeys` qtvs )

{-
-- | Combine two substitutions, if they are compatible.
combineSubsts :: Subst tv -> Subst tv -> Maybe ( Subst tv )
combineSubsts ( Subst s1 ) ( Subst s2 ) =
  case IntMap.foldMapWithKey go s1 s2 of
    ( Any errs, subst ) ->
      if errs
      then Nothing
      else Just $ Subst subst
  where
    go :: Int -> ( tv, Type Ty ) -> IntMap ( tv, Type Ty )
       -> ( Any, IntMap ( tv, Type Ty ) )
    go k tys1 m2 =
      case IntMap.alterF ( fmap Just . doIntersect k tys1 ) k m2 of
        Nothing -> ( Any True , mempty )
        Just m3 -> ( Any False, m3 )

    doIntersect :: Int
                -> ( tv, Type Ty ) -> Maybe ( tv, Type Ty )
                -> Maybe ( tv, Type Ty )
    doIntersect _ tvTy1 Nothing = Just tvTy1
    doIntersect _ tvTy1@( _, ty1 ) ( Just ( _, ty2 ) ) =
      if eqType ty1 ty2
      then Just tvTy1
      else Nothing
-}

-- | Match a constraint against an instance.
--
-- The returned first substitution does the matching, if that was possible.
-- The second substitution is an optional defaulting substitution.
matchOneInst
  :: forall nbArgs
  .  CtOrigin
  -> ClassTyCon nbArgs
  -> Instance
  -> Vec nbArgs ( Type Ty )
  -> TcUniqueM ( Maybe ( ( Cts, Subst TyVar ), TcPureM [ Subst TyVar ] ) )
matchOneInst ctOrig cls
  ( Instance
    { instanceQuantTy = ( iqty :: Vec nbBinders ( Type Ty ) -> QuantTyBody ( Vec instNbArgs ( Type Ty ) ) )
    , instanceDefaults = mbDflt }
    ) args
    | Just Refl <- Vec.withDict args $ Nat.eqNat @nbArgs @instNbArgs
    =
  runTcGenMSubst do
    let orig = ClassInstMetaOrigin $ Quant $ fmap ( fmap ( Class cls ) ) iqty
    ( instBndrs, instArgTys ) <- instantiate ctOrig orig iqty
    liftUnifyM $
      unifyTypes ctOrig NotSwapped instArgTys args
    matchSubst <- State.get
    return $
      mapMaybeA ( tryDefault ctOrig matchSubst . ( $ instBndrs ) ) mbDflt
  | otherwise
  = panicPure $ unlines
      [ "matchOneInst: incorrect class arity"
      , "class: " ++ show cls
      ]

tryDefault :: CtOrigin -> Subst TyVar -> NE.NonEmpty ( Type Ty, Type Ty ) -> TcPureM ( Maybe ( Subst TyVar ) )
tryDefault ctOrig matchSubst dfltEqs =
  fmap ( fmap snd ) $ runTcUnifyMSubst matchSubst $
    traverse ( uncurry $ unifyType ( DefaultingOrigin ctOrig ) NotSwapped ) dfltEqs

{-------------------------------------------------------------------------------
  Typechecking macros: top-level entry point to constraint solving
-------------------------------------------------------------------------------}

-- | Top-level type-checking monad.
type TcTopM = ExceptT TcMacroError TcUniqueM

simplifyAndDefault :: IntSet -> Cts -> TcTopM ( Subst TyVar, Cts )
simplifyAndDefault quantTvs cts =
  do
    ( (), ( subst, ( insols, inerts ) ) ) <- lift $ runTcSolveM cts $ solvingLoop solveOne
    for_ ( NE.nonEmpty insols ) \ errs ->
      Except.throwError ( TcInconsistentConstraints $ NE.singleton ( NE.toList errs ) )
    return ( subst, inerts )

  where
    solveOne = solveCt Defer ( DefaultTyVarsExcept quantTvs ) classInstancesWithDefaults

{-------------------------------------------------------------------------------
  Typechecking macros: generalisation
-------------------------------------------------------------------------------}

tcMacro :: C.Type.Platform -> TypeEnv -> CName -> [ CName ] -> MExpr -> Either TcMacroError ( Quant ( Type Ty ) )
tcMacro plat tyEnv macroNm args body =
  throwErrors $ runTcM plat tyEnv $ ( `State.evalStateT` Unique 0 ) $ Except.runExceptT do

    -- Step 1: infer the type.
    ( ( ty, ctsOrigs ), mbErrs ) <- lift $ inferTop macroNm args body
    traverse_ ( Except.throwError . TcErrors ) ( NE.nonEmpty mbErrs )

    -- Step 2: compute the set of metavariables that are candidates for quantification.
    let
      freeTvs = seenTvs $ getFVs noBoundVars $ freeTyVarsOfType ty

    -- Step 3: simplify and default constraints.
    ( ctSubst, simpleCts ) <- simplifyAndDefault freeTvs ctsOrigs

    -- Step 4: generalise.
    let
      qtvsList = reverse $ seenTvsRevList $ getFVs noBoundVars $
                   freeTyVarsOfType ( applySubstNormalise plat ctSubst ty )

    debugTraceM $
      unlines
        [ "tcMacro"
        , "ty: " ++ show ty
        , "freeTvs: " ++ show freeTvs
        , "ctSubst: " ++ show ctSubst
        , "simpleCts: " ++ show simpleCts
        , "qtvs: " ++ show qtvsList
        ]
    return $
      Vec.reifyList qtvsList \ qtvs ->
        Quant \ tys ->
          let quantSubst = mkSubst $ toList $ Vec.zipWith (,) qtvs tys
              finalSubst = quantSubst <> ctSubst
          in QuantTyBody
              { quantTyQuant = map ( applySubstNormalise plat finalSubst ) ( fmap fst simpleCts )
              , quantTyBody  =       applySubstNormalise plat finalSubst   ty
              }
  where
    throwErrors ( _, ( err : errs ) ) = Left $ TcErrors ( err NE.:| errs )
    throwErrors ( res, [] ) = res

data TcMacroError
  -- | Errors in the constraint-generation phase,
  -- e.g. we failed to unify some types.
  = TcErrors !( NE.NonEmpty ( TcError, SrcSpan ) )
  -- | A collection of class constraints was inconsistent.
  | TcInconsistentConstraints !( NE.NonEmpty Cts )
  deriving stock ( Show, Generic )

instance Eq TcMacroError where
  _ == _ = True

pprTcMacroError :: TcMacroError -> Text
pprTcMacroError tcMacroErr = Text.unlines . ( "Failed to typecheck macro:" : ) $
  case tcMacroErr of
    TcErrors errs ->
      map ( \ ( err, _srcSpan ) -> pprTcError err ) ( NE.toList errs )
    TcInconsistentConstraints ctss ->
      "Constraints are inconsistent:"
      : concat
        [ ( "  - " <> Text.pack ( show i ) <> ":" )
        : map ( \ ( ct, _orig ) -> "    '" <> Text.pack ( show ct ) <> "'" ) cts
        | cts <- NE.toList ctss
        | i <- [ ( 1 :: Int ) .. ]
        ]

mapMaybeA :: Applicative m => ( a -> m ( Maybe b ) ) -> [ a ] -> m [ b ]
mapMaybeA f =
  foldr ( liftA2 ( maybe id (:) ) . f ) ( pure [] )
{-# INLINEABLE mapMaybeA #-}

guarded :: ( m -> Bool ) -> m -> Maybe m
guarded cond m = do
  guard $ cond m
  return m

{-------------------------------------------------------------------------------
  Quick & dirty testing framework
-------------------------------------------------------------------------------}

debugTraceM :: Applicative f => String -> f ()
debugTraceM
  | debug
  = traceM
  | otherwise
  = const $ pure ()
{-# INLINE debugTraceM #-}

debug :: Bool
debug = False

{-
var :: CName -> MExpr
var v = MTerm ( MVar v [] )

instance Num MExpr where
  fromInteger i = MTerm $ MInt $ IntegerLiteral (Text.pack $ show i) Nothing i
  negate a = MApp MUnaryMinus ( a ::: VNil )
  a + b = MApp MAdd ( a ::: b ::: VNil )
  a * b = MApp MMult ( a ::: b ::: VNil )
  abs = panicPure "no"
  signum = panicPure "no"

testMacro :: forall n. Nat.SNatI n => ( Vec n MExpr -> MExpr ) -> Either TcMacroError ( Quant ( Type Ty ) )
testMacro f = tcMacro Map.empty "TEST" ( toList vars ) ( f $ fmap var vars )
  where
    vars :: Vec n CName
    vars = fromJust $ Vec.fromList @n [ fromString ("x" ++ show i) | i <- [1..n] ]
    n :: Int
    n = Nat.reflectToNum @n Proxy

test1, test2, test3, test4 :: Either TcMacroError ( Quant ( Type Ty ) )
test1 = testMacro @(S (S Z)) \ ( a ::: b ::: VNil ) -> a + b
test2 = testMacro @(S Z) \ ( a ::: VNil ) -> a + 1
test3 = testMacro @Z \ VNil -> 1
test4 = testMacro @(S (S Z)) \ ( x ::: y ::: VNil ) -> x + ( 12 * y )
-}
