{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
  , QuantTy(..)
  , isPrimTy

    -- ** Macro typechecking monads
  , TcM, runTcM, TcGenM
  , TypeEnv

  -- ** Macro typechecking errors
  , TcError(..), CtOrigin(..), MetaOrigin(..), CouldNotUnifyReason(..)
  , pprTcError, pprCtOrigin, pprMetaOrigin, pprCouldNotUnifyReason
  )
  where

-- base
import Control.Monad.ST
  ( ST, runST )
import Data.Kind qualified as Hs
import Data.List
  ( intercalate, intersect )
import Data.List.NonEmpty qualified as NE
import Data.Maybe
  ( fromJust )
import Data.Monoid
  ( Any (..), Ap(..) )
import Data.STRef
import Data.Traversable
  ( for )
import Data.Type.Equality
  ( type (:~:)(..) )
import GHC.Generics
  ( Generic )
import GHC.Show
  ( showSpace )
import GHC.Stack
import GHC.Exts
  ( Int(I#), dataToTag# )

-- containers
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map

-- fin
import Data.Fin qualified as Fin
  ( toNatural )
import Data.Nat
  ( Nat(..) )
import Data.Type.Nat qualified as Nat

-- mtl
import Control.Monad.State.Strict
  ( StateT(..), State )
import Control.Monad.State.Strict qualified as State
import Control.Monad.Writer
  ( WriterT )
import Control.Monad.Writer qualified as Writer
  ( tell, runWriterT )
import Control.Monad.Trans
  ( lift )

-- pretty-show
import Text.Show.Pretty
  ( PrettyVal(..) )
import Text.Show.Pretty qualified as Pretty

-- text
import Data.Text qualified as Text

-- vec
import Data.Vec.Lazy
  ( Vec(..) )
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

data Kind = Ty | Ct
type Type :: Kind -> Hs.Type
data Type ki where
  -- | A type variable.
  TyVarTy :: !TyVar -> Type Ty
  -- | A function type.
  FunTy :: !( NE.NonEmpty ( Type Ty ) ) -> !( Type Ty ) -> Type Ty
  -- | An (exactly saturated) application of a 'TyCon' to arguments.
  TyConAppTy :: !( TyCon n ki ) -> !( Vec n ( Type Ty ) ) -> Type ki

-- | A qualified quantified type @forall tys. cts => args -> res@.
data QuantTy where
  QuantTy
    :: forall n
    .  Nat.SNatI n
    => { quantTyBodyFn :: !( Vec n ( Type Ty ) -> QuantTyBody ) }
    -> QuantTy

instance Eq QuantTy where
  qty1@( QuantTy ( _ :: ( Vec n ( Type Ty ) -> QuantTyBody ) ) )
    ==
      qty2@( QuantTy ( _ :: ( Vec m ( Type Ty ) -> QuantTyBody ) ) ) =
    case Nat.eqNat @n @m of
      Nothing -> False
      Just Refl ->
        mkQuantTyBody qty1 == mkQuantTyBody qty2

-- | The body of a quantified type (what's under the forall).
data QuantTyBody
  = QuantTyBody
  { quantTyQuant :: ![ Type Ct ]
  , quantTyBody  :: !( Type Ty )
  }
  deriving stock ( Show, Generic )
  deriving anyclass PrettyVal

instance Eq QuantTyBody where
  QuantTyBody cts1 body1 == QuantTyBody cts2 body2 =
    and [ length cts1 == length cts2
        , eqTypes ( zip cts1 cts2 )
        , eqType body1 body2
        ]

instance Show ( Type ki ) where
  showsPrec p0 = \case
    TyVarTy tv -> showString ( show tv )
    TyConAppTy tc tys ->
      showParen (p0 >= 10) $
        showString (show tc) . foldr (\ a acc -> showSpace . showsPrec 11 a . acc) id tys
    FunTy as r ->
      showParen (p0 >= 0) $
        foldr ( \ a acc -> showsPrec 0 a . showString " -> " . acc ) id as . showsPrec 0 r

instance PrettyVal ( Type ki ) where
  prettyVal = \case
    TyVarTy tv -> Pretty.Con "TyVarTy" [ prettyVal tv ]
    TyConAppTy tc tys -> Pretty.Con "TyConAppTy" [ prettyVal tc, prettyVal tys ]
    FunTy args res -> Pretty.Con "FunTy" [ prettyVal args, prettyVal res ]

instance Show QuantTy where
  showsPrec p0 quantTy@(QuantTy ( _ :: ( Vec n ( Type Ty ) -> QuantTyBody ) ) ) =
    showParen ( p0 >= 0 && not ( null qtvs && null cts ) ) $
        ( if null qtvs
          then id else
            showString "forall"
          . foldr ( \ tv acc -> showSpace . showString ( Text.unpack tv ) . acc ) id qtvs
          . showString ". "
        )
      . foldr ( \ a acc -> showsPrec 0 a . showString " => " . acc ) id cts
      . showsPrec 0 body
    where
      qtvs = tyVarNames @n
      QuantTyBody cts body = mkQuantTyBody quantTy

tyVarNames :: forall n. Nat.SNatI n => Vec n ( Text )
tyVarNames = fromJust $ Vec.fromListPrefix nms
  where
    n = Nat.snatToNat ( Nat.snat @n )
    nms
      | n > 3
      = map ( \ i -> "a" <> Text.pack ( show i ) ) [ ( 1 :: Int ) .. ]
      | otherwise
      = [ "a", "b", "c" ]

mkQuantTyBody :: QuantTy -> QuantTyBody
mkQuantTyBody ( QuantTy body ) =
  body $ Vec.imap ( \ i tv -> mkSkol tv ( fromIntegral $ Fin.toNatural i ) ) tyVarNames
  where
    mkSkol tv i = TyVarTy $ SkolemTv $ SkolemTyVar tv (Unique i)

instance PrettyVal QuantTy where
  prettyVal quantTy =
    Pretty.Con "QuantTy" [ prettyVal $ mkQuantTyBody quantTy ]

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
data TyCon n k where
  DataTyCon  :: !( DataTyCon  n ) -> TyCon n Ty
  ClassTyCon :: !( ClassTyCon n ) -> TyCon n Ct

instance PrettyVal ( TyCon n k ) where
  prettyVal ( DataTyCon dc ) = Pretty.Con "DataTyCon" [ prettyVal dc ]
  prettyVal ( ClassTyCon cls ) = Pretty.Con "ClassTyCon" [ prettyVal cls ]

type DataTyCon :: Nat -> Hs.Type
data DataTyCon n where
  -- | Type constructor for Bool
  BoolTyCon      :: DataTyCon Z
  -- | Type constructor forString
  StringTyCon    :: DataTyCon Z
  -- | Type constructors for integral types, such as 'Int' or 'UShort'.
  IntLikeTyCon   :: !PrimIntType -> DataTyCon Z
  -- | Type constructor for floating-point types, such as 'Float' or 'Double'.
  FloatLikeTyCon :: !PrimFloatType -> DataTyCon Z
  -- | Type constructor for the type of a 'PrimType' value.
  PrimTyTyCon    :: DataTyCon Z
  -- | Type constructor for the type of an empty macro.
  EmptyTyCon     :: DataTyCon Z

deriving stock instance Eq  ( DataTyCon n )
deriving stock instance Ord ( DataTyCon n )

instance PrettyVal ( DataTyCon n ) where
  prettyVal c = Pretty.Con ( show c ) []

type ClassTyCon :: Nat -> Hs.Type
data ClassTyCon n where
  -- | Class type constructor for Eq
  EqTyCon         :: ClassTyCon ( S Z )
  -- | Class type constructor for Ord
  OrdTyCon        :: ClassTyCon ( S Z )
  -- | Class type constructor for Num
  NumTyCon        :: ClassTyCon ( S Z )
  -- | Class type constructor for Integral
  IntegralTyCon   :: ClassTyCon ( S Z )
  -- | Class type constructor for Fractional
  FractionalTyCon :: ClassTyCon ( S Z )
  -- | Class type constructor for Bits
  BitsTyCon       :: ClassTyCon ( S Z )
deriving stock instance Eq  ( ClassTyCon n )
deriving stock instance Ord ( ClassTyCon n )

instance PrettyVal ( ClassTyCon n ) where
  prettyVal = \case
    EqTyCon         -> Pretty.Con "EqTyCon"         []
    OrdTyCon        -> Pretty.Con "OrdTyCon"        []
    NumTyCon        -> Pretty.Con "NumTyCon"        []
    IntegralTyCon   -> Pretty.Con "IntegralTyCon"   []
    FractionalTyCon -> Pretty.Con "FractionalTyCon" []
    BitsTyCon       -> Pretty.Con "BitsTyCon"       []

instance Show (TyCon n ki) where
  show = \case
    DataTyCon  tc -> show tc
    ClassTyCon tc -> show tc

instance Show (DataTyCon n) where
  show = \case
    BoolTyCon -> "Bool"
    StringTyCon -> "String"
    IntLikeTyCon inty ->
      case inty of
        PrimShort s ->
          case s of
            Signed -> "Short"
            Unsigned -> "UShort"
        PrimInt s ->
          case s of
            Signed -> "Int"
            Unsigned -> "UInt"
        PrimLong s ->
          case s of
            Signed -> "Long"
            Unsigned -> "ULong"
        PrimLongLong s ->
          case s of
            Signed -> "LLong"
            Unsigned -> "ULLong"
    FloatLikeTyCon floaty ->
      case floaty of
        PrimFloat      -> "Float"
        PrimDouble     -> "Double"
        PrimLongDouble -> "LDouble"
    PrimTyTyCon -> "PrimTy"
    EmptyTyCon -> "Empty"
instance Show (ClassTyCon n) where
  show = \case
    EqTyCon         -> "Eq"
    OrdTyCon        -> "Ord"
    NumTyCon        -> "Num"
    IntegralTyCon   -> "Integral"
    FractionalTyCon -> "Fractional"
    BitsTyCon       -> "Bits"

-- | On-the-nose type equality.
eqType :: Type ki -> Type ki -> Bool
eqType ( TyVarTy tv1 ) ( TyVarTy tv2 ) = tyVarUnique tv1 == tyVarUnique tv2
eqType ( TyConAppTy ( DataTyCon tc1 ) args1 ) ( TyConAppTy ( DataTyCon tc2 ) args2 ) =
  case tc1 `equals` tc2 of
    Nothing -> False
    Just Refl ->
      eqTypes ( Vec.zipWith (,) args1 args2 )
eqType ( TyConAppTy ( ClassTyCon cls1 ) args1 ) ( TyConAppTy ( ClassTyCon cls2 ) args2 ) =
  case cls1 `equals` cls2 of
    Nothing -> False
    Just Refl ->
      eqTypes ( Vec.zipWith (,) args1 args2 )
eqType ( FunTy args1 res1 ) ( FunTy args2 res2 )
  =  length args1 == length args2
  && eqTypes ( NE.zip args1 args2 )
  && eqType res1 res2
eqType _ _ = False

eqTypes :: ( Functor f, Foldable f ) => f ( Type ki, Type ki ) -> Bool
eqTypes = and . fmap ( uncurry eqType )

{-------------------------------------------------------------------------------
  Free type variables and substitution
-------------------------------------------------------------------------------}

data FVs
  = FVs { boundTvs :: IntSet
        , seenTvs  :: IntSet
        , seenTvsRevList :: [ TyVar ]
        }

insertFV :: TyVar -> FVs -> FVs
insertFV tv fvs@( FVs { boundTvs = bound, seenTvs = seen, seenTvsRevList = tvs } )
  | u `IntSet.member` bound || u `IntSet.member` seen
  = fvs
  | otherwise
  = fvs { seenTvs = IntSet.insert u seen
        , seenTvsRevList = tv : tvs
        }
  where
    u = uniqueInt $ tyVarUnique tv

freeTyVarsOfType :: IntSet -> Type ki -> [ TyVar ]
freeTyVarsOfType bound0 = reverse . seenTvsRevList . ( `State.execState` ( FVs bound0 IntSet.empty [] ) ) . goTy
  where
    goTy :: forall ki'. Type ki' -> State FVs ()
    goTy = \case
      TyVarTy tv ->
        State.modify' ( insertFV tv )
      FunTy args res -> goFunTy args res
      TyConAppTy _tc tys -> traverse_ goTy tys

    goFunTy :: forall ki'. NE.NonEmpty ( Type Ty ) -> Type ki' -> State FVs ()
    goFunTy (argTy NE.:| mbArgTys) resTy = do
      goTy argTy
      case NE.nonEmpty mbArgTys of
        Nothing     -> goTy    resTy
        Just argTys -> goFunTy argTys resTy

tyVarSet :: [ TyVar ] -> IntSet
tyVarSet = IntSet.fromList . map ( uniqueInt . tyVarUnique )

newtype Subst tv = Subst { substMap :: IntMap ( tv, Type Ty ) }
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
lookupSubst tv ( Subst s ) = fmap snd $ IntMap.lookup ( uniqueInt $ tyVarUnique tv ) s

applySubst :: forall ki tv. HasCallStack => Subst tv -> Type ki -> Type ki
applySubst = goTy
  where
    goTy :: forall ki'. Subst tv -> Type ki' -> Type ki'
    goTy subst = \case
      ty@( TyVarTy tv ) ->
        case IntMap.lookup ( uniqueInt $ tyVarUnique tv ) ( substMap subst ) of
          Nothing  -> ty
          Just ( _, ty' ) -> ty'
      FunTy args res ->
        FunTy ( fmap ( applySubst subst ) args ) ( applySubst subst res )
      TyConAppTy tc tys ->
        TyConAppTy tc $ fmap ( applySubst subst ) tys

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
  | InstOrigin !FunName
  | IntLitOrigin !IntegerLiteral
  | FloatLitOrigin !FloatingLiteral
  | DefaultingOrigin
  deriving stock ( Generic, Show )
  deriving anyclass PrettyVal

pprCtOrigin :: CtOrigin -> Text
pprCtOrigin = \case
  AppOrigin fun ->
    "In an application of '" <> Text.pack ( show fun ) <> "'."
  InstOrigin fun ->
    "In the instantiation of '" <> Text.pack ( show fun ) <> "'."
  IntLitOrigin i ->
    "From the integer literal '" <> Text.pack (show i) <> "'."
  FloatLitOrigin f ->
    "From the floating-point literal '" <> Text.pack (show f) <> "'."
  DefaultingOrigin {} ->
    "From class defaulting."

-- | Why did we create a new metavariable?
data MetaOrigin
  = ExpectedFunTyResTy !FunName
  | ExpectedVarTy !CName
  | Inst { instFunName :: !FunName, instPos :: !Int }
  | FunArg !CName !( CName, Int )
  | IntLitMeta !IntegerLiteral
  | FloatLitMeta !FloatingLiteral
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
  = forall k1 k2
  . CouldNotUnify CouldNotUnifyReason CtOrigin ( Type k1 ) ( Type k2 )
  | UnexpectedMTerm !MTerm
  | UnboundVariable !CName
deriving stock instance Show TcError

instance PrettyVal TcError where
  prettyVal = \case
    CouldNotUnify rea orig ty1 ty2 ->
      Pretty.Con "CouldNotUnify" [ prettyVal rea, prettyVal orig, prettyVal ty1, prettyVal ty2 ]
    UnexpectedMTerm mTerm ->
      Pretty.Con "UnexpectedMTerm" [ prettyVal mTerm ]
    UnboundVariable nm ->
      Pretty.Con "UnboundVariable" [ prettyVal nm ]

pprTcError :: TcError -> Text
pprTcError = \case
  CouldNotUnify rea orig ty1 ty2 ->
    Text.unlines
      [ "Could not unify:"
      , "  - " <> Text.pack ( show ty1 )
      , "  - " <> Text.pack ( show ty2 )
      , "because " <> pprCouldNotUnifyReason rea <> "."
      , pprCtOrigin orig ]
  UnexpectedMTerm tm ->
    "Unexpected MTerm: " <> Text.pack ( show tm )
  UnboundVariable ( CName nm ) ->
    "Unbound variable: '" <> nm <> "'"

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

type TypeEnv = Map CName QuantTy
type VarEnv  = Map CName ( Type Ty )

data TcGblEnv s
  = TcGblEnv
      { tcUnique  :: !( STRef s Unique )
      , tcErrs    :: !( STRef s [ ( TcError, SrcSpan ) ] )
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

runTcM :: Map CName QuantTy -> TcM a -> ( a, [ ( TcError, SrcSpan ) ] )
runTcM initTyEnv ( TcM f ) = runST $ do
  tcUnique  <- newSTRef ( Unique 0 )
  tcErrs    <- newSTRef []
  tcTypeEnv <- newSTRef initTyEnv
  let
    tcGblEnv = TcGblEnv { tcUnique, tcErrs, tcTypeEnv }
    tcLclEnv = TcLclEnv { tcSrcSpan = SrcSpan, tcVarEnv = Map.empty }
  res <- f ( TcEnv { tcGblEnv, tcLclEnv } )
  errs <- readSTRef tcErrs
  return ( res, errs )

addErrTcM :: TcError -> TcM ()
addErrTcM err = TcM $ \ ( TcEnv ( TcGblEnv { tcErrs } ) ( TcLclEnv { tcSrcSpan } ) ) ->
  modifySTRef' tcErrs ( \ errs -> ( ( err, tcSrcSpan ) : errs ) )

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

lookupTyEnv :: CName -> TcM ( Maybe QuantTy )
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

-- | A collection of constraints (with their origin).
type Cts = [ ( Type Ct, CtOrigin ) ]

-- | Monad for generating constraints.
type TcGenM = WriterT Cts ( StateT ( Subst TyVar ) TcM )

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

runTcGenMTcM :: TcGenM a -> TcM ( ( a, Cts ), Subst TyVar )
runTcGenMTcM x = ( `State.runStateT` mempty ) $ Writer.runWriterT x

-- | Run a 'TcGenM' action and retrieve the underlying 'Subst'
-- when there were no errors.
runTcGenMSubst :: TcGenM () -> Maybe ( Subst TyVar )
runTcGenMSubst = go . runTcM mempty . runTcGenMTcM
  where
    go ( ( _, subst ), errs ) =
      if null errs
      then Just subst
      else Nothing

{-------------------------------------------------------------------------------
  Typechecking macros: unification & constraint generation
-------------------------------------------------------------------------------}

unifyType :: CtOrigin -> Bool -> Type ki -> Type ki -> TcGenM ()
unifyType orig swapped ty1 ty2
  | TyVarTy tv1 <- ty1
  = unifyTyVar orig swapped tv1 ty2
  | TyVarTy tv2 <- ty2
  = unifyTyVar orig ( not swapped ) tv2 ty1
  | FunTy args1 res1 <- ty1
  , FunTy args2 res2 <- ty2
  = unifyFunTys orig swapped args1 res1 args2 res2
  | TyConAppTy tc1 as1 <- ty1
  , TyConAppTy tc2 as2 <- ty2
  = unifyTyConApp orig swapped ( tc1, as1 ) ( tc2, as2 )
  | otherwise
  = couldNotUnify IncompatibleTypes orig swapped ty1 ty2

unifyTyConApp
  :: forall n1 ki1 n2 ki2
  .  CtOrigin
  -> Bool
  -> ( TyCon n1 ki1, Vec n1 ( Type Ty ) )
  -> ( TyCon n2 ki2, Vec n2 ( Type Ty ) )
  -> TcGenM ()
unifyTyConApp orig swapped ( tc1, args1 ) ( tc2, args2 )
  | Just Refl <- tcOK
  = unifyTypes orig swapped ( Vec.zipWith (,) args1 args2 )
  | otherwise
  = couldNotUnify TyConAppDifferentTyCon orig swapped ( TyConAppTy tc1 args1 ) ( TyConAppTy tc2 args2 )
  where
    tcOK :: Maybe ( '( n1, ki1 ) :~: '( n2, ki2 ) )
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
        _ -> Nothing

unifyTypes :: Traversable t => CtOrigin -> Bool -> t ( Type Ty, Type Ty ) -> TcGenM ()
unifyTypes orig swapped = traverse_ ( uncurry $ unifyType orig swapped )
{-# INLINEABLE unifyTypes #-}

unifyTyVar :: CtOrigin -> Bool -> TyVar -> Type Ty -> TcGenM ()
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
      -> unifyTyVar orig ( not swapped ) tv2 ( TyVarTy tv1 )
      | any ( ( == tyVarUnique tv1 ) . tyVarUnique ) $ freeTyVarsOfType IntSet.empty ty2
      -> couldNotUnify ( OccursCheck tv1 ) orig swapped ( TyVarTy tv1 ) ty2
      | otherwise
      -> case tv1 of
          MetaTv tau1 ->
            State.put $ addOneToSubst ( MetaTv tau1 ) ty2 subst
          SkolemTv sk1 ->
            couldNotUnify ( RigidSkolem sk1 ) orig swapped ( TyVarTy tv1 ) ty2

unifyFunTys :: CtOrigin -> Bool -> NE.NonEmpty ( Type Ty ) -> Type Ty -> NE.NonEmpty ( Type Ty )  -> Type Ty -> TcGenM ()
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

couldNotUnify :: CouldNotUnifyReason -> CtOrigin -> Bool -> Type k1 -> Type k2 -> TcGenM ()
couldNotUnify rea orig swapped ty1 ty2 =
  liftTcM . addErrTcM $
    if swapped
    then CouldNotUnify rea orig ty2 ty1
    else CouldNotUnify rea orig ty1 ty2

{-------------------------------------------------------------------------------
  Typechecking macros: instantiation
-------------------------------------------------------------------------------}

instantiate :: FunName -> QuantTy -> TcGenM ( Type Ty )
instantiate funNm@( FunName fn ) ( QuantTy { quantTyBodyFn = body } ) = do
  let
    -- Use the built-in type variable names for 'MFun's,
    -- but use 'a1', 'a2', 'a3'... for user-defined function macros.
    mkTvNm i tvNm = case fn of
      Left _var -> "a" <> Text.pack ( show i )
      Right {}  -> tvNm

    tvNms' = Vec.imap ( \ i tvNm -> ( fromIntegral ( Fin.toNatural i ) + 1, mkTvNm i tvNm ) ) tyVarNames
  tvs <- liftTcM $ for tvNms' $ \ ( i, tvName ) ->
    newMetaTyVarTy ( Inst { instFunName = funNm, instPos = i } ) tvName
  let QuantTyBody cts bodyTy = body tvs
  Writer.tell $ map (, InstOrigin funNm ) cts
  return bodyTy

{-------------------------------------------------------------------------------
  Typechecking macros: type inference
-------------------------------------------------------------------------------}

-- | Infer the type of a macro declaration (before constraint solving and generalisation).
inferTop :: CName -> [ CName ] -> MExpr -> TcM ( Type Ty, Cts )
inferTop funNm argsList body = Vec.reifyList argsList $ \ args -> do
  ( ( ( argTys, bodyTy ), cts ), subst ) <- runTcGenMTcM ( inferLam funNm args body )
  let macroTy = case NE.nonEmpty $ toList argTys of
        Nothing -> bodyTy
        Just argTysNE -> FunTy argTysNE bodyTy
      macroTy' = applySubst subst macroTy
      cts' = map ( first ( applySubst subst ) ) cts
  return ( macroTy', cts' )

inferExpr :: MExpr -> TcGenM ( Type Ty )
inferExpr = \case
  MTerm tm -> inferTerm tm
  MApp fun args -> inferApp ( FunName $ Right fun ) ( Vec.toList args )

inferTerm :: MTerm -> TcGenM ( Type Ty )
inferTerm = \case
  MEmpty -> return Empty
  MInt i@( IntegerLiteral { integerLiteralType = mbIntyTy } ) ->
    case mbIntyTy of
      Just intyTy ->
        return $ IntLike intyTy
      Nothing -> do
        m <- liftTcM $ newMetaTyVarTy (IntLitMeta i) "i"
        Writer.tell [ ( Integral m, IntLitOrigin i ) ]
        return m
  MFloat f@( FloatingLiteral { floatingLiteralType = mbFloatyTy }) ->
    case mbFloatyTy of
      Just floatyTy ->
        return $ FloatLike floatyTy
      Nothing -> do
        m <- liftTcM $ newMetaTyVarTy (FloatLitMeta f) "f"
        Writer.tell [ ( Fractional m , FloatLitOrigin f ) ]
        return m
  MVar fun args -> inferApp ( FunName $ Left fun ) args
  MType {} -> return PrimTy
  MAttr _attr tm -> inferTerm tm
  MStringize {} -> return String
  MConcat a1 a2 -> do
    ty1 <- inferTerm a1
    ty2 <- inferTerm a2
    let orig = AppOrigin ( FunName ( Left "##" ) )
    unifyType orig False ty1 String
    unifyType orig False ty2 String
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
      unifyType ( AppOrigin fun ) False actualTy funTy
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
              instantiate funNm funQTy
            Nothing -> liftTcM $ do
              addErrTcM $ UnboundVariable varNm
              newMetaTyVarTy ( ExpectedVarTy varNm ) ( varStr <> "_ty" )
    Right mFun  -> do
      let funQTy = inferMFun mFun
      instantiate funNm funQTy

inferLam :: forall n. CName -> Vec n CName -> MExpr -> TcGenM ( Vec n ( Type Ty ), Type Ty )
inferLam _ VNil body = ( VNil, ) <$> inferExpr body
inferLam funNm argNms@( _ ::: _ ) body = do
  let is = Vec.imap ( \ i _ -> fromIntegral ( Fin.toNatural i ) + 1 ) argNms
  argTys <- liftTcM $
    for ( Vec.zipWith (,) is argNms ) $ \ ( i, argNm@( CName argStr ) ) ->
      newMetaTyVarTy ( FunArg funNm ( argNm, i ) ) ( "ty_" <> argStr )
  liftBaseTcM ( declareLocalVars ( Map.fromList $ toList $ Vec.zipWith (,) argNms argTys ) ) $
    ( argTys, ) <$> inferExpr body

inferMFun :: MFun arity -> QuantTy
inferMFun = \case
  MUnaryPlus  -> q1 $ \ a   -> QuantTyBody [Num a]              ( funTy [a]          a )
  MUnaryMinus -> q1 $ \ a   -> QuantTyBody [Num a]              ( funTy [a]          a )
  MLogicalNot -> q0 $          QuantTyBody []                   ( funTy [Bool]       Bool )
  MBitwiseNot -> q1 $ \ a   -> QuantTyBody [Bits a]             ( funTy [a]          a )
  MMult       -> q1 $ \ a   -> QuantTyBody [Num a]              ( funTy [a,a]        a )
  MDiv        -> q1 $ \ a   -> QuantTyBody [Fractional a]       ( funTy [a,a]        a )
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
    q0 body = QuantTy @Z             $ \ VNil -> body
    q1 body = QuantTy @( S Z )       $ \ (a ::: VNil) -> body a
    q2 body = QuantTy @( S ( S Z ) ) $ \ (a ::: i ::: VNil) -> body a i
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
pattern Fractional :: Type Ty -> Type Ct
pattern Fractional a = TyConAppTy (ClassTyCon FractionalTyCon) ( a ::: VNil )
pattern Bits :: Type Ty -> Type Ct
pattern Bits a = TyConAppTy (ClassTyCon BitsTyCon) ( a ::: VNil )

pattern Bool :: Type Ty
pattern Bool = TyConAppTy (DataTyCon BoolTyCon) VNil
pattern IntLike :: PrimIntType -> Type Ty
pattern IntLike intLike = TyConAppTy (DataTyCon (IntLikeTyCon intLike)) VNil
pattern FloatLike :: PrimFloatType -> Type Ty
pattern FloatLike floatLike = TyConAppTy (DataTyCon (FloatLikeTyCon floatLike)) VNil
pattern String :: Type Ty
pattern String = TyConAppTy (DataTyCon StringTyCon) VNil
pattern PrimTy :: Type Ty
pattern PrimTy = TyConAppTy (DataTyCon PrimTyTyCon) VNil
pattern Empty :: Type Ty
pattern Empty = TyConAppTy (DataTyCon EmptyTyCon) VNil

isPrimTy :: forall n. Nat.SNatI n => (Vec n (Type Ty) -> QuantTyBody) -> Bool
isPrimTy bf = case Nat.snat @n of
    Nat.SZ -> isPrimTy' (bf VNil)
    Nat.SS -> False

isPrimTy' :: QuantTyBody -> Bool
isPrimTy' (QuantTyBody [] PrimTy) = True
isPrimTy' _                       = False

{-------------------------------------------------------------------------------
  Typechecking macros: classes
--------------------------------------------------------------------------------

The following pieces of information determine how class constraints are solved.

  1. The superclass implication structure, as specified by the function
     'classImplies'.

  2. Class instances, as specified by the function
     'classInstancesWithDefaults'.

    Each instance comes with a collection of default types that match that
    instance.

    Instances are uniquely specified by a vector of DataTyConTag, as explained
    in Note [Class instances and DataTyConTag].
-}

-- | Does one class (of a given arity) imply another (of the same arity),
-- e.g. does @cls1 a1 a2@ imply @cls2 a1 a2@?
classImplies :: ClassTyCon n -> ClassTyCon n -> Bool
classImplies OrdTyCon EqTyCon = True
classImplies IntegralTyCon NumTyCon = True
classImplies FractionalTyCon NumTyCon = True
classImplies cls1 cls2 = cls1 == cls2

-- | The data constructor tag of a 'DataTyCon', used as an identifier for
-- a class instance.
--
--See Note [Class instances and DataTyConTag].
newtype DataTyConTag = DataTyConTag Int
  deriving stock ( Eq, Ord )

{- Note [Class instances and DataTyConTag]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the simple type system used in type inference of macros, we assume that
all instances are of the form

  instance forall tc1Args tc2Args ... . ctxt => Cls ( Tc1 tc1Args ) ( Tc2 tc2Args ) ...

e.g.

  instance forall primIntTy. Num ( IntLike primIntTy )
  instance Eq Bool

This means that instances for a class are uniquely described by a vector
of the constructor tags of the type constructors at the head of the instance.
-}

-- | A list of possible defaulting assignments for a class.
--
-- Why is the result keyed by a vector of 'DataTyConTag's?
-- See Note [Class instances and DataTyConTag].
classInstancesWithDefaults :: ClassTyCon n -> Map ( Vec n DataTyConTag ) ( Vec n ( Type Ty ) )
classInstancesWithDefaults = Map.fromList . \case
  EqTyCon         -> mkUnaryInsts [ intTy, doubleTy, String, Bool ]
  OrdTyCon        -> mkUnaryInsts [ intTy, doubleTy, String, Bool ]
  NumTyCon        -> mkUnaryInsts [ intTy, doubleTy ]
  IntegralTyCon   -> mkUnaryInsts [ intTy ]
  FractionalTyCon -> mkUnaryInsts [ doubleTy ]
  BitsTyCon       -> mkUnaryInsts [ intTy ]
  where
    intTy = IntLike ( PrimInt Signed )
    doubleTy = FloatLike PrimDouble
    mkUnaryInsts :: [ Type Ty ] -> [ ( Vec ( S Z ) DataTyConTag, Vec ( S Z ) ( Type Ty ) ) ]
    mkUnaryInsts = map ( \ ty -> ( getTag ty ::: VNil, ty ::: VNil ) )
    getTag :: Type Ty -> DataTyConTag
    getTag ( TyConAppTy ( DataTyCon dc ) args )
      | Just Refl <- sameLength args VNil
      = DataTyConTag $ I# ( dataToTag# dc )
    getTag ty = error $ "classDefaults getTag: unexpected type" ++ show ty

-- | All instances of a class. Keyed using 'DataTyConTag's due to the
-- assumptions laid out in Note [Class instances and DataTyConTag].
classInstances :: ClassTyCon n -> [ Vec n DataTyConTag ]
classInstances = Map.keys . classInstancesWithDefaults

-- | Does the given constraint match some top-level instance?
matchesSomeTopLevelInstance :: Type Ct -> Bool
matchesSomeTopLevelInstance ( TyConAppTy ( ClassTyCon cls ) args ) =
  any ( and . Vec.zipWith ( flip matchesInstance ) args ) ( classInstances cls )

-- | Does the given type match an instance, as referred to using its
-- 'DataTyConTag' (see Note [Class instances and DataTyConTag])?
matchesInstance :: DataTyConTag -> Type Ty -> Bool
matchesInstance ( DataTyConTag tyConTag ) arg
  | TyConAppTy ( DataTyCon tc ) _tcArgs <- arg
  = I# ( dataToTag# tc ) == tyConTag
  | otherwise
  = False

{-------------------------------------------------------------------------------
  Typechecking macros: constraint solving/simplification
-------------------------------------------------------------------------------}

data SolveOneFromTheOther
  = Inconsistent
  | KeepLeft
  | KeepRight
  | KeepBoth
  deriving stock ( Eq, Ord, Show )

wantLeft :: SolveOneFromTheOther -> Bool
wantLeft KeepLeft = True
wantLeft KeepBoth = True
wantLeft Inconsistent = False
wantLeft KeepRight = False

solveOneFromTheOther :: Type Ct -> Type Ct -> SolveOneFromTheOther
solveOneFromTheOther
  ( TyConAppTy ( ClassTyCon cls1 ) args1 )
  ( TyConAppTy ( ClassTyCon cls2 ) args2 )
    | Just Refl <- sameLength args1 args2
    , eqTypes $ Vec.zipWith (,) args1 args2
    =  if | null $ intersect ( classInstances cls1 ) ( classInstances cls2 )
          -> Inconsistent
          | cls1 `classImplies` cls2
          -> KeepLeft
          | cls2 `classImplies` cls1
          -> KeepRight
          | otherwise
          -> KeepBoth
    | otherwise
    = KeepBoth

sameLength :: forall n1 n2 a1 a2. Vec n1 a1 -> Vec n2 a2 -> Maybe ( n1 :~: n2 )
sameLength v1 v2 = Vec.withDict v1 $ Vec.withDict v2 $ Nat.eqNat @n1 @n2

type InconsistentPair  = ( ( Type Ct, CtOrigin ), ( Type Ct, CtOrigin ) )
type InconsistentPairs = [ InconsistentPair ]

simplifyCts :: Cts -> ( Cts, InconsistentPairs )
simplifyCts = go [] []
  where
    go :: InconsistentPairs -> Cts -> Cts -> ( Cts, InconsistentPairs )
    go errs _ [] = ( [], errs )
    go errs prev (ct:cts) =
      let ( keepCt, incons, keptCts ) = partitionCts ct cts
          wantCt = not ( matchesSomeTopLevelInstance ( fst ct ) )
                && keepCt
                && all wantLeft ( map ( solveOneFromTheOther ( fst ct ) . fst ) ( prev ++ cts ) )
          rest = go ( addPairs errs incons ) prev keptCts
      in if wantCt
         then first ( ct : ) rest
         else                rest

    addPairs :: InconsistentPairs -> [ InconsistentPair ] -> InconsistentPairs
    addPairs errs [] = errs
    addPairs errs (pair:pairs) = addPairs (addPair errs pair) pairs

    addPair :: InconsistentPairs -> InconsistentPair -> InconsistentPairs
    addPair errs pair
      | any ( eqPair pair ) errs
      = errs
      | otherwise
      = pair : errs

    eqPair :: InconsistentPair -> InconsistentPair -> Bool
    eqPair ( ( ct1, _ ), ( ct2, _ ) ) ( ( ct3, _ ), ( ct4, _ ) )
      =  ( eqType ct1 ct3 && eqType ct2 ct4 )
      || ( eqType ct1 ct4 && eqType ct2 ct3 )


partitionCts :: ( Type Ct, CtOrigin ) -> Cts -> ( Bool, InconsistentPairs, Cts )
partitionCts ct = go True [] []
  where
    go wantCt errs acc [] = ( wantCt, errs, acc )
    go wantCt errs acc (other:others) =
      case solveOneFromTheOther ( fst ct ) ( fst other ) of
        KeepLeft ->
          go wantCt errs acc others
        KeepRight ->
          go False errs (other:acc) others
        KeepBoth ->
          go wantCt errs (other:acc) others
        Inconsistent ->
          go False ((ct, other) : errs) acc others

{-------------------------------------------------------------------------------
  Typechecking macros: defaulting
-------------------------------------------------------------------------------}

-- | A model for a collection of constraints is a substitution, assigning
-- to each metavariable a type such that the collection of constraints is
-- satisfied.
newtype Models = Models [ Subst TyVar ]
  deriving stock Show

instance Semigroup Models where
  Models ms1 <> Models ms2 =
    Models $
      catMaybes $
        zipWith combineSubsts ms1 ms2

instance Monoid Models where
  mempty = Models [ mempty ]

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

-- | Default a constraint, returning a collection of possible 'Models'.
defaultCt :: IntSet -> Type Ct -> Models
defaultCt quantTvs ct@( TyConAppTy ( ClassTyCon cls ) ( args :: Vec n ( Type ty ) ) )
  | null $ freeTyVarsOfType quantTvs ct
  = mempty
  | otherwise
  = Models $ Map.elems $ Map.mapMaybeWithKey ( defaultingSubst args ) ( classInstancesWithDefaults cls )
  where
    defaultingSubst :: Vec n ( Type Ty ) -> Vec n DataTyConTag -> Vec n ( Type Ty ) -> Maybe ( Subst TyVar )
    defaultingSubst argTys instTag candTys =
      coerce $
        foldMap ( Ap . uncurry defaultOne ) $
          Vec.zipWith (,) argTys $
            Vec.zipWith (,) instTag candTys

    defaultOne :: Type Ty -> ( DataTyConTag, Type Ty ) -> Maybe ( Subst TyVar )
    defaultOne ty ( instTag, candTy )
      -- Type is already an instance of the class; nothing to do.
      | matchesInstance instTag ty
      = Just mempty
      | otherwise
        -- Otherwise, try to unify the type with the candidate.
      = let mbDefSubst = runTcGenMSubst $ unifyType DefaultingOrigin False ty candTy
        in
        -- Take care not to default any bound variables; these will get quantified over instead.
          fmap
            ( \ ( Subst defTvs ) -> Subst ( defTvs `IntMap.withoutKeys` quantTvs ) )
            mbDefSubst

-- | Simplify constraints, then do defaulting.
--
-- Do this in a loop until no more defaulting happens.
simplifyAndDefault :: IntSet -> Cts -> Either TcMacroError Cts
simplifyAndDefault quantTvs ctsOrigs = do
  let ( simpleCts, mbIncons ) = simplifyCts ctsOrigs
  throwAnyErrs TcInconsistentPairs mbIncons
  let Models mbModels = foldMap ( defaultCt quantTvs . fst ) simpleCts
  case NE.nonEmpty mbModels of
    Nothing -> do
      Left ( TcInconsistentConstraints simpleCts )
    Just ( defaultSubst@( Subst assigs ) NE.:| _otherSubsts ) -> do
      if null assigs
      then
        return simpleCts
      else do
        let defltCts = map ( first ( applySubst defaultSubst ) ) simpleCts
        simplifyAndDefault quantTvs defltCts

{-------------------------------------------------------------------------------
  Typechecking macros: generalisation
-------------------------------------------------------------------------------}

tcMacro :: Map CName QuantTy -> CName -> [ CName ] -> MExpr -> Either TcMacroError QuantTy
tcMacro tyEnv macroNm args body = do

  -- Step 1: infer the type.
  let
    ( ( ty, ctsOrigs ), mbErrs ) = runTcM tyEnv ( inferTop macroNm args body )
  throwAnyErrs TcErrors mbErrs

  -- Step 2: compute the set of metavariables we will quantify over.
  let
    quantTvs = freeTyVarsOfType mempty ty

  -- Step 3: simplify and default constraints.
  simpleCts <- simplifyAndDefault ( tyVarSet quantTvs ) ctsOrigs

  -- Step 4: generalise.
  Vec.reifyList quantTvs $ \ ( quantTvsVec :: Vec n TyVar ) -> do
    Right $ QuantTy @n $ \ tys ->
      let quantSubst = mkSubst ( toList $ Vec.zipWith (,) quantTvsVec tys )
      in QuantTyBody
          { quantTyQuant = map ( applySubst quantSubst ) ( fmap fst simpleCts )
          , quantTyBody  =       applySubst quantSubst   ty
          }

data TcMacroError
  -- | Errors in the constraint-generation phase,
  -- e.g. we failed to unify some types.
  = TcErrors !( NE.NonEmpty ( TcError, SrcSpan ) )
  -- | Some class constraints were pairwise inconsistent,
  -- based on a simple syntactic check.
  | TcInconsistentPairs !( NE.NonEmpty InconsistentPair )
  -- | A collection of class constraints was inconsistent,
  -- based on model-checking.
  | TcInconsistentConstraints !Cts
  deriving stock ( Show, Generic )
  deriving anyclass PrettyVal

instance Eq TcMacroError where
  _ == _ = True

pprTcMacroError :: TcMacroError -> Text
pprTcMacroError tcMacroErr = Text.unlines . ( "Failed to typecheck macro:" : ) $
  case tcMacroErr of
    TcErrors errs ->
      map ( \ ( err, _srcSpan ) -> pprTcError err ) ( NE.toList errs )
    TcInconsistentPairs cts ->
      ( "Inconsistent pairs of constraints:" : ) $
      map
        ( \ ( ( ct1, _orig1 ), ( ct2, _orig2 ) ) ->
          "  - '" <> Text.pack ( show ct1 ) <> "' and " <> Text.pack ( show ct2 )
        )
        ( NE.toList cts )
    TcInconsistentConstraints cts ->
      [ "Constraints are inconsistent:" ]
      ++ map ( \ ( ct, _orig ) -> "    '" <> Text.pack ( show ct ) <> "'" ) cts

throwAnyErrs :: ( NE.NonEmpty err -> TcMacroError ) -> [ err ] -> Either TcMacroError ()
throwAnyErrs f mbErrs =
  case NE.nonEmpty mbErrs of
    Just errs -> Left ( f errs )
    Nothing -> Right ()

--------------------------------------------------------------------------------
