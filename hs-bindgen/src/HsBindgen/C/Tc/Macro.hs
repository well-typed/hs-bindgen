{-# LANGUAGE CPP #-}

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

    -- ** Macro type-system
  , Type(..), Kind(..)
  , TyCon(..), DataTyCon(..), ClassTyCon(..)
  , Quant(..), QuantTyBody(..)
  , tyVarName, tyVarNames, mkQuantTyBody
  , isPrimTy

    -- ** Macro typechecking monad
  , TcM, runTcM, TypeEnv

    -- ** Macro typechecking errors
  , TcError(..), CtOrigin(..), MetaOrigin(..), CouldNotUnifyReason(..)
  , pprTcError, pprCtOrigin, pprMetaOrigin, pprCouldNotUnifyReason
  )
  where

-- base
#if !MIN_VERSION_base(4,18,0)
import Control.Applicative
  ( liftA2 )
#endif
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
  ( Any(..), Endo(..) )
import Data.Proxy
  ( Proxy(..) )
import Data.STRef
  ( STRef, newSTRef, readSTRef, writeSTRef )
import Data.Traversable
  ( for )
import Data.Typeable
  ( Typeable, eqT )
import Data.Type.Equality
  ( type (:~:)(..) )
import GHC.Generics
  ( Generic )
import GHC.Show
  ( showSpace )
import GHC.Stack
  ( HasCallStack )
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

-- pretty-show
import Text.Show.Pretty
  ( PrettyVal(..) )
import Text.Show.Pretty qualified as Pretty

-- text
import Data.Text qualified as Text

-- vec
import Data.Vec.Lazy qualified as Vec

-- hs-bindgen
import HsBindgen.Imports
import HsBindgen.C.AST.Literal
  ( IntegerLiteral(..), FloatingLiteral(..) )
import HsBindgen.C.AST.Macro
  ( MExpr(..), MFun(..), MTerm(..) )
import HsBindgen.C.AST.Name
  ( CName(..) )
import HsBindgen.C.AST.Type
  ( PrimIntType(..), PrimSign(..), PrimFloatType(..) )
import HsBindgen.Pretty.Orphans
  ()
import HsBindgen.Util.TestEquality
  ( equals )

{-------------------------------------------------------------------------------
  Type system for macros
-------------------------------------------------------------------------------}

type Name = Text
newtype Unique = Unique { uniqueInt :: Int }
  deriving newtype ( Enum, Eq, Show )
  deriving stock Generic
  deriving anyclass PrettyVal

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
data Quant resKi where
  Quant
    :: forall nbBinders resKi
    .  ( Nat.SNatI nbBinders )
    => { quantTyBodyFn :: !( Vec nbBinders ( Type Ty ) -> QuantTyBody resKi ) }
    -> Quant resKi

instance Eq ( Quant resKi ) where
  qty1@( Quant @n1 _ ) == qty2@( Quant @n2 _ ) =
    case Nat.eqNat @n1 @n2 of
      Nothing -> False
      Just Refl ->
        mkQuantTyBody qty1 == mkQuantTyBody qty2

-- | The body of a quantified type (what's under the forall).
data QuantTyBody ki
  = QuantTyBody
  { quantTyQuant :: ![ Type Ct ]
  , quantTyBody  :: !( Type ki )
  }
  deriving stock ( Show, Generic )
  deriving anyclass PrettyVal

instance Eq ( QuantTyBody ki ) where
  QuantTyBody cts1 body1 == QuantTyBody cts2 body2 =
    and [ length cts1 == length cts2
        , all ( uncurry eqType ) ( zip cts1 cts2 )
        , eqType body1 body2
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

instance PrettyVal ( Type ki ) where
  prettyVal = \case
    TyVarTy tv -> Pretty.Con "TyVarTy" [ prettyVal tv ]
    TyConAppTy tc tys -> Pretty.Con "TyConAppTy" [ prettyVal tc, prettyVal tys ]
    FunTy args res -> Pretty.Con "FunTy" [ prettyVal args, prettyVal res ]
    NomEqPred a b -> Pretty.Con "NomEqPred" [ prettyVal a, prettyVal b ]

instance Show ( Quant ki ) where
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

mkQuantTyBody :: Quant ki -> QuantTyBody ki
mkQuantTyBody ( Quant @nbBinders body ) =
  body $ fmap ( uncurry mkSkol ) $ tyVarNames @nbBinders
  where
    mkSkol :: Int -> Name -> Type Ty
    mkSkol i tv = TyVarTy $ SkolemTv $ SkolemTyVar tv ( Unique i )

instance PrettyVal ( Quant ki ) where
  prettyVal quantTy =
    Pretty.Con "Quant" [ prettyVal $ mkQuantTyBody quantTy ]

data TyVar
  = SkolemTv {-# UNPACK #-} !SkolemTyVar
  | MetaTv   {-# UNPACK #-} !MetaTyVar
  deriving stock Generic
  deriving anyclass PrettyVal


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
  deriving anyclass PrettyVal
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
  deriving anyclass PrettyVal

type TyCon :: Nat -> Kind -> Hs.Type
data TyCon nbArgs res where
  DataTyCon      :: !( DataTyCon   nbArgs ) -> TyCon nbArgs Ty
  ClassTyCon     :: !( ClassTyCon  nbArgs ) -> TyCon nbArgs Ct
deriving stock instance Eq ( TyCon nbArgs res )

instance PrettyVal ( TyCon nbArgs k ) where
  prettyVal ( DataTyCon   dc  ) = Pretty.Con   "DataTyCon" [ prettyVal dc     ]
  prettyVal ( ClassTyCon  cls ) = Pretty.Con  "ClassTyCon" [ prettyVal cls    ]

type DataTyCon :: Nat -> Hs.Type
data DataTyCon nbArgs where
  -- | Type constructor for Bool
  BoolTyCon      :: DataTyCon Z
  -- | Type constructor forString
  StringTyCon    :: DataTyCon Z
  -- | Type constructor for integral types, such as 'Int' or 'UShort'.
  IntLikeTyCon   :: DataTyCon ( S Z )
  -- | Type constructor for floating-point types, such as 'Float' or 'Double'.
  FloatLikeTyCon :: DataTyCon ( S Z )

  -- | Type constructor for the type of primitive integral types,
  -- which appear as arguments to 'IntLikeTyCon'.
  PrimIntTyCon   :: PrimIntType -> DataTyCon Z
  -- | Type constructor for the type of primitive  floating-point types,
  -- which appear as arguments to 'FloatLikeTyCon'.
  PrimFloatTyCon :: PrimFloatType -> DataTyCon Z

  -- | Type constructor for the type of a 'PrimType' value.
  PrimTyTyCon    :: DataTyCon Z
  -- | Type constructor for the type of an empty macro.
  EmptyTyCon     :: DataTyCon Z

deriving stock instance Eq  ( DataTyCon nbArgs )
deriving stock instance Ord ( DataTyCon nbArgs )

instance PrettyVal ( DataTyCon nbArgs ) where
  prettyVal c = Pretty.Con ( show c ) []

type ClassTyCon :: Nat -> Hs.Type
data ClassTyCon nbArgs where
  -- | Class type constructor for @Eq@
  EqTyCon         :: ClassTyCon ( S Z )
  -- | Class type constructor for @Ord@
  OrdTyCon        :: ClassTyCon ( S Z )
  -- | Class type constructor for @Num@
  NumTyCon        :: ClassTyCon ( S Z )
  -- | Class type constructor for @Integral@
  IntegralTyCon   :: ClassTyCon ( S Z )
  -- | Class type constructor for @Div@
  DivTyCon        :: ClassTyCon ( S Z )
  -- | Class type constructor for @Bits@
  BitsTyCon       :: ClassTyCon ( S Z )
deriving stock instance Eq  ( ClassTyCon nbArgs )
deriving stock instance Ord ( ClassTyCon nbArgs )

instance PrettyVal ( ClassTyCon nbArgs ) where
  prettyVal = \case
    EqTyCon         -> Pretty.Con "EqTyCon"         []
    OrdTyCon        -> Pretty.Con "OrdTyCon"        []
    NumTyCon        -> Pretty.Con "NumTyCon"        []
    IntegralTyCon   -> Pretty.Con "IntegralTyCon"   []
    DivTyCon        -> Pretty.Con "DivTyCon"        []
    BitsTyCon       -> Pretty.Con "BitsTyCon"       []

instance Show ( TyCon n ki ) where
  showsPrec p = \case
    DataTyCon   tc -> showsPrec p tc
    ClassTyCon  tc -> showsPrec p tc

instance Show ( DataTyCon n ) where
  showsPrec p = \case
    BoolTyCon             -> showString "Bool"
    StringTyCon           -> showString "String"
    IntLikeTyCon          -> showString "IntLike"
    FloatLikeTyCon        -> showString "FloatLike"
    PrimIntTyCon   inty   -> showsPrec p inty
    PrimFloatTyCon floaty -> showsPrec p floaty
    PrimTyTyCon           -> showString "PrimTy"
    EmptyTyCon            -> showString "Empty"
instance Show ( ClassTyCon n ) where
  show = \case
    EqTyCon         -> "Eq"
    OrdTyCon        -> "Ord"
    NumTyCon        -> "Num"
    IntegralTyCon   -> "Integral"
    DivTyCon        -> "Div"
    BitsTyCon       -> "Bits"

-- | On-the-nose type equality.
eqType :: Type ki -> Type ki -> Bool
eqType ( TyVarTy tv1 ) ( TyVarTy tv2 ) = tyVarUnique tv1 == tyVarUnique tv2
eqType ( TyConAppTy ( DataTyCon tc1 ) args1 ) ( TyConAppTy ( DataTyCon tc2 ) args2 ) =
  case tc1 `equals` tc2 of
    Nothing -> False
    Just Refl ->
      eqTypes args1 args2
eqType ( TyConAppTy ( ClassTyCon cls1 ) args1 ) ( TyConAppTy ( ClassTyCon cls2 ) args2 ) =
  case cls1 `equals` cls2 of
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
  error $
    unlines
      [ str ++ ": incoherent substitution"
      , "TyVar with unique " ++ show ( Unique i ) ++ " mapped to two different types"
      , "ty1: " ++ show ty1
      , "ty2: " ++ show ty2
      ]

lookupSubst :: TyVar -> Subst tv -> Maybe ( Type Ty )
lookupSubst tv ( Subst s ) =
  fmap snd $ IntMap.lookup ( uniqueInt $ tyVarUnique tv ) s

applySubst :: forall ki tv. HasCallStack => Subst tv -> Type ki -> Type ki
applySubst = goTy
  where
    goTy :: forall ki'. Subst tv -> Type ki' -> Type ki'
    goTy subst = \case
      ty@( TyVarTy tv ) ->
        case lookupSubst tv subst of
          Nothing  -> ty
          Just ty' -> ty'
      FunTy args res ->
        FunTy ( fmap ( applySubst subst ) args ) ( applySubst subst res )
      TyConAppTy tc tys ->
        TyConAppTy tc $ fmap ( applySubst subst ) tys
      NomEqPred a b ->
        NomEqPred ( applySubst subst a ) ( applySubst subst b )

{-------------------------------------------------------------------------------
  Constraints & errors
-------------------------------------------------------------------------------}

data FunName = forall arity. FunName ( Either CName ( MFun arity ) )
instance Show FunName where
  show ( FunName funName ) =
    case funName of
      Left ( CName f ) -> Text.unpack f
      Right f -> show f
instance PrettyVal FunName where
  prettyVal f = prettyVal ( show f )

-- | Why did we emit a constraint?
data CtOrigin
  = AppOrigin !FunName
  | FunInstOrigin !FunName
  | ClassInstOrigin !(Quant Ct) !CtOrigin
  | DefaultingOrigin !CtOrigin
  deriving stock ( Generic, Show )
  deriving anyclass PrettyVal

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
  deriving anyclass PrettyVal

data InstOrigin
  = FunInstMetaOrigin !FunName
  | ClassInstMetaOrigin !( Quant Ct )
  deriving stock ( Generic, Show )
  deriving anyclass PrettyVal

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

instance PrettyVal TcError where
  prettyVal = \case
    UnificationError err ->
      Pretty.Con "UnificationError" [ prettyVal err ]
    UnexpectedMTerm mTerm ->
      Pretty.Con "UnexpectedMTerm" [ prettyVal mTerm ]
    UnboundVariable nm ->
      Pretty.Con "UnboundVariable" [ prettyVal nm ]
instance PrettyVal UnificationError where
  prettyVal = \case
    CouldNotUnify rea orig ty1 ty2 ->
      Pretty.Con "CouldNotUnify" [ prettyVal rea, prettyVal orig, prettyVal ty1, prettyVal ty2 ]

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
  deriving anyclass PrettyVal

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
  deriving anyclass PrettyVal
instance Show SrcSpan where
  show _ = "<<noSrcSpan>>"

data TcEnv s =
  TcEnv
    { tcGblEnv :: !( TcGblEnv s )
    , tcLclEnv :: !TcLclEnv
    }

type TypeEnv = Map CName ( Quant Ty )
type VarEnv  = Map CName ( Type Ty )

data TcGblEnv s
  = TcGblEnv
      { tcUnique  :: !( STRef s Unique )
      , tcTypeEnv :: !( STRef s TypeEnv )
      }

data TcLclEnv
  = TcLclEnv
      { tcSrcSpan :: !SrcSpan
      , tcVarEnv  :: !VarEnv
      }

newtype TcM a = TcM ( forall s. TcEnv s -> ST s a )
instance Functor TcM where
  fmap f ( TcM g ) = TcM ( fmap f . g )
instance Applicative TcM where
  pure f = TcM $ \ _ -> pure f
  (<*>) = ap
instance Monad TcM where
  TcM ma >>= f = TcM $ \ env -> do
    !a <- ma env
    case f a of
      TcM g -> g env

runTcM :: Map CName ( Quant Ty ) -> TcM a -> ( a, [ ( TcError, SrcSpan ) ] )
runTcM initTyEnv ( TcM f ) = runST $ do
  tcUnique  <- newSTRef ( Unique 0 )
  tcErrs    <- newSTRef []
  tcTypeEnv <- newSTRef initTyEnv
  let
    tcGblEnv = TcGblEnv { tcUnique, tcTypeEnv }
    tcLclEnv = TcLclEnv { tcSrcSpan = SrcSpan, tcVarEnv = Map.empty }
  res <- f ( TcEnv { tcGblEnv, tcLclEnv } )
  errs <- readSTRef tcErrs
  return ( res, errs )

getSrcSpan :: TcM SrcSpan
getSrcSpan =
  TcM $ \ ( TcEnv _gbl ( TcLclEnv { tcSrcSpan } ) ) ->
    return tcSrcSpan

stateSTRef :: STRef s a -> ( a -> ( b, a ) ) -> ST s b
stateSTRef ref f = do
  a <- readSTRef ref
  let !( !b, !a' ) = f a
  writeSTRef ref a'
  return b

newUnique :: TcM Unique
newUnique = TcM $ \ ( TcEnv ( TcGblEnv { tcUnique } ) _ ) ->
  stateSTRef tcUnique ( \ old -> (old, succ old) )

newMetaTyVarTy :: MetaOrigin -> Name -> TcM ( Type Ty )
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

lookupTyEnv :: CName -> TcM ( Maybe ( Quant Ty ) )
lookupTyEnv varNm = TcM $ \ ( TcEnv ( TcGblEnv { tcTypeEnv } ) _ ) -> do
  tyEnv <- readSTRef tcTypeEnv
  return $ Map.lookup varNm tyEnv

lookupVarType :: CName -> TcM ( Maybe ( Type Ty ) )
lookupVarType varNm = TcM $ \ ( TcEnv _ lcl ) ->
  return $ Map.lookup varNm ( tcVarEnv lcl )

declareLocalVars :: Map CName ( Type Ty ) -> TcM a -> TcM a
declareLocalVars vs ( TcM f ) = TcM $ \ ( TcEnv gbl lcl ) ->
  f ( TcEnv gbl ( lcl { tcVarEnv = tcVarEnv lcl <> vs } ) )

{-------------------------------------------------------------------------------
  Typechecking macros: constraint generation monad
-------------------------------------------------------------------------------}

-- | Monad for unification.
type TcUnifyM = WriterT UnifyResult ( StateT ( Subst TyVar ) TcM )

-- | A collection of constraints (with their origin).
type Cts = [ ( Type Ct, CtOrigin ) ]

-- | Monad for generating constraints.
type TcGenM = WriterT ( Cts, [ ( TcError, SrcSpan ) ] ) ( StateT ( Subst TyVar ) TcM )

liftTcM :: TcM a -> TcGenM a
liftTcM = lift . lift

-- | 'Control.Monad.Trans.Control.liftBaseWith' for 'TcM' and 'TcGenM'.
liftBaseTcM :: ( forall x. TcM x -> TcM x ) -> TcGenM a -> TcGenM a
liftBaseTcM morph g = do
  s0 <- State.get
  ( ( a, cts ), subst ) <- liftTcM $ morph $ ( `State.runStateT` s0 ) $ Writer.runWriterT g
  State.put subst
  Writer.tell cts
  return a

liftUnifyM :: TcUnifyM a -> TcGenM a
liftUnifyM = Writer.mapWriterT ( fmap ( second deferredEqs ) )
  where
    deferredEqs :: UnifyResult -> ( Cts, [ ( TcError, SrcSpan ) ] )
    deferredEqs ( UnifyResult { deferredEqualities = eqs, unifyErrors = errs } ) =
      ( eqs, map ( first UnificationError ) errs )

addErrTcGenM :: TcError -> TcGenM ()
addErrTcGenM err = do
  srcSpan <- lift $ lift getSrcSpan
  Writer.tell ( [], [ ( err, srcSpan ) ] )


runTcGenMTcM :: TcGenM a -> TcM ( ( a, ( Cts, [ ( TcError, SrcSpan ) ] ) ), Subst TyVar )
runTcGenMTcM = ( `State.runStateT` mempty ) . Writer.runWriterT


-- | Run a 'TcUnifyM' action and retrieve the underlying 'Subst'
-- when unification succeeded without deferring any equalities.
runTcUnifyMSubst :: forall a. Subst TyVar -> TcUnifyM a -> TcM ( Maybe ( a, Subst TyVar ) )
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
runTcGenMSubst :: TcGenM a -> TcM ( Maybe ( ( Cts, Subst TyVar ), a ) )
runTcGenMSubst = fmap noErrs . runTcGenMTcM
  where
    noErrs ( ( a, ( cts, mbErrs ) ), subst ) =
      if null mbErrs
      then Just ( ( cts, subst ), a )
      else Nothing

{-------------------------------------------------------------------------------
  Typechecking macros: unification & constraint generation
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

unifyType :: Typeable ki => CtOrigin -> SwapFlag -> Type ki -> Type ki -> TcUnifyM ()
unifyType orig swapped ty1 ty2
  | TyVarTy tv1 <- ty1
  = unifyTyVar orig swapped tv1 ty2
  | TyVarTy tv2 <- ty2
  = unifyTyVar orig ( swap swapped ) tv2 ty1
  | FunTy args1 res1 <- ty1
  , FunTy args2 res2 <- ty2
  = unifyFunTys orig swapped args1 res1 args2 res2
  | TyConAppTy tc1 as1 <- ty1
  , TyConAppTy tc2 as2 <- ty2
  = unifyTyConApp orig swapped ( tc1, as1 ) ( tc2, as2 )
  | otherwise
  = couldNotUnify IncompatibleTypes orig swapped ty1 ty2

unifyTyConApp
  :: forall nbArgs1 nbArgs2 resKi
  .  Typeable resKi
  => CtOrigin
  -> SwapFlag
  -> ( TyCon nbArgs1 resKi, Vec nbArgs1 ( Type Ty ) )
  -> ( TyCon nbArgs2 resKi, Vec nbArgs2 ( Type Ty ) )
  -> TcUnifyM ()
unifyTyConApp orig swapped ( tc1, args1 ) ( tc2, args2 )
  | Just Refl <- tcOK
  = unifyTypes orig swapped args1 args2
  | otherwise
  = couldNotUnify TyConAppDifferentTyCon orig swapped ( TyConAppTy tc1 args1 ) ( TyConAppTy tc2 args2 )
  where
    tcOK :: Maybe ( nbArgs1 :~: nbArgs2 )
    tcOK =
      case ( tc1, tc2 ) of
        ( DataTyCon  dc1 , DataTyCon  dc2  ) ->
          case dc1 `equals` dc2  of
            Just Refl -> Just Refl
            Nothing   -> Nothing
        ( ClassTyCon cls1, ClassTyCon cls2 ) ->
          case cls1 `equals` cls2 of
            Just Refl -> Just Refl
            Nothing   -> Nothing

unifyTypes :: Typeable ki => CtOrigin -> SwapFlag -> Vec n ( Type ki ) -> Vec n ( Type ki ) -> TcUnifyM ()
unifyTypes orig swapped as bs = sequence_ $ Vec.zipWith ( unifyType orig swapped ) as bs
{-# INLINEABLE unifyTypes #-}

unifyTyVar :: CtOrigin -> SwapFlag -> TyVar -> Type Ty -> TcUnifyM ()
unifyTyVar _ _ tv1 ( TyVarTy tv2 )
  | tyVarUnique tv1 == tyVarUnique tv2
  = return ()
unifyTyVar orig swapped tv1 ty2' = do
  subst <- State.get
  let ty2 = applySubst subst ty2'
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
  Typechecking macros: instantiation
-------------------------------------------------------------------------------}

instantiate :: CtOrigin -> InstOrigin -> Quant ki -> TcGenM ( Type ki )
instantiate ctOrig instOrig ( Quant @nbBinders body ) = do
  tvs <-
    liftTcM $
      for ( tyVarNames @nbBinders ) $ \ ( i, tvName ) ->
        newMetaTyVarTy ( Inst { instOrigin = instOrig, instPos = i } ) tvName
  let QuantTyBody cts bodyTy = body tvs
  Writer.tell $ ( map (, ctOrig ) cts, mempty )
  return bodyTy

{-------------------------------------------------------------------------------
  Typechecking macros: type inference
-------------------------------------------------------------------------------}

-- | Infer the type of a macro declaration (before constraint solving and generalisation).
inferTop :: CName -> [ CName ] -> MExpr -> TcM ( ( Type Ty, Cts ), [ ( TcError, SrcSpan ) ] )
inferTop funNm argsList body =
  Vec.reifyList argsList $ \ args -> do
    ( ( ( argTys, bodyTy ), ( cts, mbErrs ) ), subst ) <- runTcGenMTcM ( inferLam funNm args body )
    let macroTy =
          case NE.nonEmpty $ toList argTys of
            Nothing -> bodyTy
            Just argTysNE -> FunTy argTysNE bodyTy
        macroTy' = applySubst subst macroTy
        cts' = map ( first ( applySubst subst ) ) cts
    debugTraceM $ unlines
      [ "inferTop " ++ show funNm
      , "ty: " ++ show macroTy'
      , "cts: " ++ show cts'
      , "final subst: " ++ show subst
      ]
    return ( ( macroTy', cts' ), mbErrs )

inferExpr :: MExpr -> TcGenM ( Type Ty )
inferExpr = \case
  MTerm tm -> inferTerm tm
  MApp fun args -> inferApp ( FunName $ Right fun ) ( Vec.toList args )

inferTerm :: MTerm -> TcGenM ( Type Ty )
inferTerm = \case
  MEmpty -> return Empty
  MInt i@( IntegerLiteral { integerLiteralType = mbIntyTy } ) ->
    IntLike <$>
      case mbIntyTy of
        Just intyTy ->
          return $ PrimIntTy intyTy
        Nothing -> do
          m <- liftTcM $ newMetaTyVarTy (IntLitMeta i) "i"
          return m
  MFloat f@( FloatingLiteral { floatingLiteralType = mbFloatyTy }) ->
    FloatLike <$>
      case mbFloatyTy of
        Just floatyTy ->
          return $ PrimFloatTy floatyTy
        Nothing -> do
          m <- liftTcM $ newMetaTyVarTy (FloatLitMeta f) "f"
          return m
  MVar fun args -> inferApp ( FunName $ Left fun ) args
  MType {} -> return PrimTy
  MAttr _attr tm -> inferTerm tm
  MStringize {} -> return String
  MConcat a1 a2 -> do
    ty1 <- inferTerm a1
    ty2 <- inferTerm a2
    let orig = AppOrigin ( FunName ( Left "##" ) )
    liftUnifyM $ unifyType orig NotSwapped ty1 String
    liftUnifyM $ unifyType orig NotSwapped ty2 String
    return String

inferApp :: FunName -> [ MExpr ] -> TcGenM ( Type Ty )
inferApp fun mbArgs = do
  funTy <- inferFun fun
  case NE.nonEmpty mbArgs of
    Nothing ->
      return funTy
    Just args -> do
      argTys <- traverse inferExpr args
      resTy <- liftTcM $ newMetaTyVarTy ( ExpectedFunTyResTy fun ) "r"
      let actualTy = FunTy argTys resTy
      liftUnifyM $ unifyType ( AppOrigin fun ) NotSwapped actualTy funTy
      return resTy

inferFun :: FunName -> TcGenM ( Type Ty )
inferFun funNm@( FunName fun ) =
  case fun of
    Left varNm@( CName varStr ) -> do
      mbTy <- liftTcM $ lookupVarType varNm
      case mbTy of
        Just varTy -> return varTy
        Nothing -> do
          mbQTy <- liftTcM $ lookupTyEnv varNm
          case mbQTy of
            Just funQTy ->
              instantiate ( FunInstOrigin funNm ) ( FunInstMetaOrigin funNm ) funQTy
            Nothing -> do
              addErrTcGenM $ UnboundVariable varNm
              liftTcM $ newMetaTyVarTy ( ExpectedVarTy varNm ) ( varStr <> "_ty" )
    Right mFun  -> do
      let funQTy = inferMFun mFun
      instantiate ( FunInstOrigin funNm ) ( FunInstMetaOrigin funNm ) funQTy

inferLam :: forall n. CName -> Vec n CName -> MExpr -> TcGenM ( Vec n ( Type Ty ), Type Ty )
inferLam _ VNil body = ( VNil, ) <$> inferExpr body
inferLam funNm argNms@( _ ::: _ ) body = do
  let is = Vec.imap ( \ i _ -> fromIntegral ( Fin.toNatural i ) + 1 ) argNms
  argTys <- liftTcM $
    for ( Vec.zipWith (,) is argNms ) $ \ ( i, argNm@( CName argStr ) ) ->
      newMetaTyVarTy ( FunArg funNm ( argNm, i ) ) ( "ty_" <> argStr )
  liftBaseTcM ( declareLocalVars ( Map.fromList $ toList $ Vec.zipWith (,) argNms argTys ) ) $
    ( argTys, ) <$> inferExpr body

inferMFun :: MFun arity -> Quant Ty
inferMFun = \case
  MUnaryPlus  -> q1 $ \ a   -> QuantTyBody [Num a]              ( funTy [a]          a )
  MUnaryMinus -> q1 $ \ a   -> QuantTyBody [Num a]              ( funTy [a]          a )
  MLogicalNot -> q0 $          QuantTyBody []                   ( funTy [Bool]       Bool )
  MBitwiseNot -> q1 $ \ a   -> QuantTyBody [Bits a]             ( funTy [a]          a )
  MMult       -> q1 $ \ a   -> QuantTyBody [Num a]              ( funTy [a,a]        a )
  MDiv        -> q1 $ \ a   -> QuantTyBody [Div a]              ( funTy [a,a]        a )
  MRem        -> q1 $ \ a   -> QuantTyBody [Integral a]         ( funTy [a,a]        a )
  MAdd        -> q1 $ \ a   -> QuantTyBody [Num a]              ( funTy [a,a]        a )
  MSub        -> q1 $ \ a   -> QuantTyBody [Num a]              ( funTy [a,a]        a )
  MShiftLeft  -> q2 $ \ a i -> QuantTyBody [Bits a, Integral i] ( funTy [a,i]        a )
  MShiftRight -> q2 $ \ a i -> QuantTyBody [Bits a, Integral i] ( funTy [a,i]        a )
  MRelLT      -> q1 $ \ a   -> QuantTyBody [Ord a]              ( funTy [a,a]        Bool )
  MRelLE      -> q1 $ \ a   -> QuantTyBody [Ord a]              ( funTy [a,a]        Bool )
  MRelGT      -> q1 $ \ a   -> QuantTyBody [Ord a]              ( funTy [a,a]        Bool )
  MRelGE      -> q1 $ \ a   -> QuantTyBody [Ord a]              ( funTy [a,a]        Bool )
  MRelEQ      -> q1 $ \ a   -> QuantTyBody [Eq a]               ( funTy [a,a]        Bool )
  MRelNE      -> q1 $ \ a   -> QuantTyBody [Eq a]               ( funTy [a,a]        Bool )
  MBitwiseAnd -> q1 $ \ a   -> QuantTyBody [Bits a]             ( funTy [a,a]        a )
  MBitwiseXor -> q1 $ \ a   -> QuantTyBody [Bits a]             ( funTy [a,a]        a )
  MBitwiseOr  -> q1 $ \ a   -> QuantTyBody [Bits a]             ( funTy [a,a]        a )
  MLogicalAnd -> q0 $          QuantTyBody []                   ( funTy [Bool, Bool] Bool )
  MLogicalOr  -> q0 $          QuantTyBody []                   ( funTy [Bool, Bool] Bool )
  where
    q0 body = Quant @Z             $ \ VNil -> body
    q1 body = Quant @( S Z )       $ \ (a ::: VNil) -> body a
    q2 body = Quant @( S ( S Z ) ) $ \ (a ::: i ::: VNil) -> body a i
    funTy mbArgs res =
      case NE.nonEmpty mbArgs of
        Just args -> FunTy args res
        Nothing   -> res

pattern Eq :: Type Ty -> Type Ct
pattern Eq a = TyConAppTy (ClassTyCon EqTyCon) ( a ::: VNil )
pattern Ord :: Type Ty -> Type Ct
pattern Ord a = TyConAppTy (ClassTyCon OrdTyCon) ( a ::: VNil )
pattern Num :: Type Ty -> Type Ct
pattern Num a = TyConAppTy (ClassTyCon NumTyCon) ( a ::: VNil )
pattern Integral :: Type Ty -> Type Ct
pattern Integral a = TyConAppTy (ClassTyCon IntegralTyCon) ( a ::: VNil )
pattern Div :: Type Ty -> Type Ct
pattern Div a = TyConAppTy (ClassTyCon DivTyCon) ( a ::: VNil )
pattern Bits :: Type Ty -> Type Ct
pattern Bits a = TyConAppTy (ClassTyCon BitsTyCon) ( a ::: VNil )

pattern Bool :: Type Ty
pattern Bool = TyConAppTy (DataTyCon BoolTyCon) VNil
pattern PrimIntTy :: PrimIntType -> Type Ty
pattern PrimIntTy inty = TyConAppTy (DataTyCon (PrimIntTyCon inty)) VNil
pattern PrimFloatTy :: PrimFloatType -> Type Ty
pattern PrimFloatTy floaty = TyConAppTy (DataTyCon (PrimFloatTyCon floaty)) VNil
pattern IntLike :: Type Ty -> Type Ty
pattern IntLike intLike = TyConAppTy (DataTyCon IntLikeTyCon) (intLike ::: VNil)
pattern FloatLike :: Type Ty -> Type Ty
pattern FloatLike floatLike = TyConAppTy (DataTyCon FloatLikeTyCon) (floatLike ::: VNil)
pattern String :: Type Ty
pattern String = TyConAppTy (DataTyCon StringTyCon) VNil
pattern PrimTy :: Type Ty
pattern PrimTy = TyConAppTy (DataTyCon PrimTyTyCon) VNil
pattern Empty :: Type Ty
pattern Empty = TyConAppTy (DataTyCon EmptyTyCon) VNil

isPrimTy :: forall n. Nat.SNatI n => (Vec n (Type Ty) -> QuantTyBody Ty) -> Bool
isPrimTy bf = case Nat.snat @n of
    Nat.SZ -> isPrimTy' (bf VNil)
    Nat.SS -> False

isPrimTy' :: QuantTyBody Ty -> Bool
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
    :: forall nbBinders
    .  ( Nat.SNatI nbBinders )
    => { instanceQuantTy :: !( Vec nbBinders ( Type Ty ) -> QuantTyBody Ct )
       , instanceDefaults :: !( Maybe ( Vec nbBinders ( Type Ty ) ) )
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

instanceKey :: HasCallStack => Instance -> InstanceKey
instanceKey ( Instance { instanceQuantTy = qty } ) =
  case quantTyBody ( mkQuantTyBody ( Quant qty ) ) of
    TyConAppTy _cls args ->
      argsTypeHeads args
    NomEqPred {} ->
      error "instanceKey called on 'NomEqPred'"

argsTypeHeads :: Vec n ( Type Ty ) -> [ Maybe TypeHead ]
argsTypeHeads = toList . fmap typeHead

typeHead :: Type Ty -> Maybe TypeHead
typeHead = \case
  FunTy {} ->
    Just FunTyHead
  TyConAppTy tc _ ->
    case tc of
      DataTyCon dc ->
        Just $ TyConHead $ dataTyConTag dc
  TyVarTy {} ->
    Nothing

-- | An instance environment.
type InstEnv = forall nbArgs. ClassTyCon nbArgs -> TrieMap TypeHead Instance

-- | The superclass structure of built-in classes.
classSuperclasses :: forall nbArgs. ClassTyCon nbArgs -> ( Vec nbArgs ( Type Ty ) -> [ Type Ct ] )
classSuperclasses cls =
  case cls of
    EqTyCon         -> noSCs
    OrdTyCon        -> \ ( a ::: VNil ) -> [ Eq a ]
    NumTyCon        -> noSCs
    IntegralTyCon   -> \ ( a ::: VNil ) -> [ Num a, Div a ]
    DivTyCon        -> \ ( a ::: VNil ) -> [ Num a ]
    BitsTyCon       -> noSCs
  where
    noSCs = const []

-- | Built-in top-level class instances, with associated defaulting assignments.
classInstancesWithDefaults :: forall nbClsArgs. ClassTyCon nbClsArgs -> TrieMap TypeHead Instance
classInstancesWithDefaults cls =
  trieFromList . map ( \ i -> ( instanceKey i, i ) ) $
    case cls of
      EqTyCon         -> [inty1, floaty1] ++ map mkUnaryNoForall [ QuantTyBody [] String, QuantTyBody [] Bool ]
      OrdTyCon        -> [inty1, floaty1] ++ map mkUnaryNoForall [ QuantTyBody [] String, QuantTyBody [] Bool ]
      NumTyCon        -> [inty1, floaty1]
      IntegralTyCon   -> [inty1]
      DivTyCon        -> [inty1, floaty1]
      BitsTyCon       -> [inty1]
  where

    intTy    = PrimIntTy $ PrimInt Signed
    doubleTy = PrimFloatTy PrimDouble

    -- An instance of the form @forall inty. C (IntLike inty)@,
    -- with defaulting assignment @inty := PrimInt Signed@.
    inty1 :: nbClsArgs ~ S Z => Instance
    inty1 = mkUnary @( S Z ) ( Just ( intTy ::: VNil ) ) $ \ ( inty ::: VNil ) ->
      QuantTyBody [] $ IntLike inty

    -- An instance of the form @forall floaty. C (FloatLike float)@,
    -- with defaulting assignment @floaty := PrimDouble@.
    floaty1 :: nbClsArgs ~ S Z => Instance
    floaty1 = mkUnary @( S Z ) ( Just ( doubleTy ::: VNil ) ) $ \ ( floaty ::: VNil ) ->
      QuantTyBody [] $ FloatLike floaty

    mkUnary :: forall instNbBinders
            .  ( Nat.SNatI instNbBinders, nbClsArgs ~ S Z )
            => Maybe ( Vec instNbBinders ( Type Ty ) )
            -> ( Vec instNbBinders ( Type Ty ) -> QuantTyBody Ty )
            -> Instance
    mkUnary mbDflt unaryInst =
      Instance
        { instanceQuantTy  = mkUnaryQTy . unaryInst
        , instanceDefaults = mbDflt
        }
    mkUnaryNoForall :: nbClsArgs ~ S Z => QuantTyBody Ty -> Instance
    mkUnaryNoForall = mkUnary @Z ( Just VNil ) . const

    mkUnaryQTy :: nbClsArgs ~ S Z => QuantTyBody Ty -> QuantTyBody Ct
    mkUnaryQTy ( QuantTyBody ctxt param ) =
      QuantTyBody ctxt ( TyConAppTy ( ClassTyCon cls ) ( param ::: VNil ) )

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
          ( `Map.traverseMaybeWithKey` dicts ) $ \ _key ->
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
type TcSolveM = StateT SolverState TcM

initSolverState :: Cts -> SolverState
initSolverState cts0 =
  SolverState
    { solverSubst    = mempty
    , solverInerts   = emptyInertSet
    , solverWorkList = cts0
    }

emitWork :: Subst TyVar -> Cts -> TcSolveM ()
emitWork subst newCts = do
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
      ct = TyConAppTy ( ClassTyCon cls ) args
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

solvingLoop :: ( ( Type Ct, CtOrigin ) -> TcSolveM () ) -> Cts -> TcM ( Subst TyVar, ( Cts, Cts ) )
solvingLoop solveOne cts = finish <$> ( `State.execStateT` initSolverState cts ) ( loop 1 )
  where
    loop :: Int -> TcSolveM ()
    loop !iter = do
      mbWorkItem <- nextWorkItem
      for_ mbWorkItem $ \ workItem -> do
        debugTraceM $
          unlines [ "solvingLoop: iteration #" ++ show iter
                  , "work item: " ++ show workItem
                  ]
        solveOne workItem
        loop ( iter + 1 )
    finish :: SolverState -> ( Subst TyVar, ( Cts, Cts ) )
    finish st =
      ( solverSubst st, inertCts ( solverInerts st ) )

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
    TyConAppTy ( ClassTyCon cls ) args ->
      solveDictCt handleOverlap defaulting ctOrig cls ( instEnv cls ) args

-- | Solve an equality constraint.
solveEqCt :: CtOrigin -> Type Ty -> Type Ty -> TcSolveM ()
solveEqCt ctOrig lhs rhs = do
  ( ( (), UnifyResult eqs errs ), innerSubst ) <-
    lift $ ( `State.runStateT` mempty ) $ Writer.runWriterT $
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
      TyConAppTy ( ClassTyCon cls ) args -> do
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
        TyConAppTy ( ClassTyCon cls' ) args' -> do
          guard $
            any ( eqType ct ) ( pty : classSuperclasses cls' args' )
          return sol

-- | Kick out constraints which mention variables from the domain of the
-- new substitution.
kickOut :: Subst TyVar -> TcSolveM ()
kickOut subst = do
  st@( SolverState { solverInerts = inerts, solverWorkList = wl0 } ) <- State.get
  let ( okInerts, kickedInerts ) = mapMaybeInerts mbKickOut inerts
  debugTraceM $ unlines
    [ "kickOut"
    , "subst: " ++ show subst
    , "inerts kicked out: " ++ show kickedInerts
    ]
  State.put $
    st { solverInerts = okInerts, solverWorkList = wl0 ++ kickedInerts }
  where
    mbKickOut :: Type Ct -> Maybe ( Type Ct )
    mbKickOut ct =
      let
        ctFVs = getFVs noBoundVars $ freeTyVarsOfType ct
      in
        if IntSet.null $ seenTvs ctFVs `IntSet.intersection` domain subst
        then
          Nothing
        else
          Just $ applySubst subst ct

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
              [ "solveDictCt: solved constraint, emitting context"
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
      ct = TyConAppTy ( ClassTyCon cls ) args
      matcher :: Instance -> TcM ( Maybe ( Cts, Subst TyVar ) )
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
          Just ( ( newCts, matchSubst ), doMbDflt ) -> do
            let
              matchNoDefault =
                do
                  debugTraceM $
                    unlines
                      [ "solveDictCt: matchOne SUCCESS (no defaulting)"
                      , "ct: " ++ show ct
                      ]
                  return $ Just ( newCts, matchSubst )
            if
              | DefaultTyVarsExcept qtvs <- doDefault
              -> do mbDflt <- doMbDflt
                    case mbDflt of
                      Nothing ->
                        matchNoDefault
                      Just dfltSubst -> do
                        let mbSubst
                              -- Only do defaulting when no candidate type variables
                              -- for quantification are involved.
                              -- (Alternatively we could choose to default only
                              -- a subset of the type variables, but we don't do so for now.)
                              | IntSet.null $ IntSet.intersection ( domain dfltSubst ) qtvs
                              = combineSubsts matchSubst dfltSubst
                              | otherwise
                              = Just matchSubst
                        debugTraceM $
                          unlines
                            [ "solveDictCt: matchOne with default"
                            , "qtvs: " ++ show qtvs
                            , "ct: " ++ show ct
                            , "matchSubst: " ++ show matchSubst
                            , "dfltSubst: " ++ show dfltSubst
                            , "mbSubst: " ++ show mbSubst
                            ]
                        for mbSubst $ \ subst ->
                          return
                            ( map ( first $ applySubst subst ) newCts, subst )
              | otherwise
              -> matchNoDefault

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

-- | Match a constraint against an instance.
--
-- The returned first substitution does the matching, if that was possible.
-- The second substitution is an optional defaulting substitution.
matchOneInst
  :: CtOrigin
  -> ClassTyCon nbArgs
  -> Instance
  -> Vec nbArgs ( Type Ty )
  -> TcM ( Maybe ( ( Cts, Subst TyVar ), TcM ( Maybe ( Subst TyVar ) ) ) )
matchOneInst ctOrig cls ( Instance { instanceQuantTy = iqty, instanceDefaults = mbDflt } ) args =
  runTcGenMSubst $ do
    let qty :: Quant Ct
        qty = Quant iqty
        orig = ClassInstMetaOrigin qty
    instTy <- instantiate ctOrig orig qty
    liftUnifyM $ unifyType ctOrig NotSwapped
      instTy
      ( TyConAppTy ( ClassTyCon cls ) args )
    subst0 <- State.get
    return $
      case mbDflt of
        Nothing ->
          return Nothing
        Just dflt ->
          fmap ( fmap snd ) $ runTcUnifyMSubst subst0 $ do
            unifyType ( DefaultingOrigin ctOrig ) NotSwapped
              instTy
              ( quantTyBody $ iqty dflt )

{-------------------------------------------------------------------------------
  Typechecking macros: top-level entry point to constraint solving
-------------------------------------------------------------------------------}

-- | Top-level type-checking monad.
type TcTopM = ExceptT TcMacroError TcM

simplifyAndDefault :: IntSet -> Cts -> TcTopM ( Subst TyVar, Cts )
simplifyAndDefault quantTvs cts0 =
  do
    ( subst, ( insols, inerts ) ) <- lift $ solvingLoop solveOne cts0
    for_ ( NE.nonEmpty insols ) $ \ errs ->
      Except.throwError ( TcInconsistentConstraints $ NE.singleton ( NE.toList errs ) )
    return ( subst, inerts )

  where
    solveOne = solveCt Defer ( DefaultTyVarsExcept quantTvs ) classInstancesWithDefaults

{-------------------------------------------------------------------------------
  Typechecking macros: generalisation
-------------------------------------------------------------------------------}

tcMacro :: Map CName ( Quant Ty ) -> CName -> [ CName ] -> MExpr -> Either TcMacroError ( Quant Ty )
tcMacro tyEnv macroNm args body = throwErrors $ runTcM tyEnv $ Except.runExceptT $ do

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
    qtvsList = reverse $ seenTvsRevList $ getFVs noBoundVars $ freeTyVarsOfType ( applySubst ctSubst ty )

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
    Vec.reifyList qtvsList  $ \ qtvs ->
      Quant $ \ tys ->
        let quantSubst = mkSubst $ toList $ Vec.zipWith (,) qtvs tys
            finalSubst = quantSubst <> ctSubst
        in QuantTyBody
            { quantTyQuant = map ( applySubst finalSubst ) ( fmap fst simpleCts )
            , quantTyBody  =       applySubst finalSubst   ty
            }
  where
    throwErrors ( _, ( err : errs ) ) = Left $ TcErrors ( err NE.:| errs )
    throwErrors ( res, [] ) = res

data TcMacroError
  -- | Errors in the constraint-generation phase,
  -- e.g. we failed to unify some types.
  = TcErrors !( NE.NonEmpty ( TcError, SrcSpan ) )
  -- | A collection of class constraints was inconsistent,
  -- based on model-checking.
  | TcInconsistentConstraints !( NE.NonEmpty Cts )
  deriving stock ( Show, Generic )
  deriving anyclass PrettyVal

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
  abs = error "no"
  signum = error "no"

testMacro :: forall n. Nat.SNatI n => ( Vec n MExpr -> MExpr ) -> Either TcMacroError ( Quant Ty )
testMacro f = tcMacro Map.empty "TEST" ( toList vars ) ( f $ fmap var vars )
  where
    vars :: Vec n CName
    vars = fromJust $ Vec.fromList @n [ fromString ("x" ++ show i) | i <- [1..n] ]
    n :: Int
    n = Nat.reflectToNum @n Proxy

test1, test2, test3, test4 :: Either TcMacroError ( Quant Ty )
test1 = testMacro @(S (S Z)) $ \ ( a ::: b ::: VNil ) -> a + b
test2 = testMacro @(S Z) $ \ ( a ::: VNil ) -> a + 1
test3 = testMacro @Z $ \ VNil -> 1
test4 = testMacro @(S (S Z)) $ \ ( x ::: y ::: VNil ) -> x + ( 12 * y )
-}
