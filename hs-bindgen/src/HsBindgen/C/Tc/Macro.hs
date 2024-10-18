{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Type inference for simple function-like C macros.
module HsBindgen.C.Tc.Macro
  ( Type(..), Kind(..)
  , TyCon(..), DataTyCon(..), ClassTyCon(..)
  , TcM, runTcM
  , QuantTy(..)
  , TcMacroError(..)
  , tcMacro
  )
  where

-- base
import Control.Arrow
  ( first )
import Control.Monad
  ( ap )
import Control.Monad.ST
  ( ST, runST )
import Data.Coerce
  ( coerce )
import Data.Either
  ( partitionEithers )
import Data.Foldable
  ( toList, traverse_, foldrM )
import Data.Function
  ( on )
import Data.Kind qualified as Hs
import Data.List
  ( nubBy, intercalate, intersectBy ) --, sort )
import Data.List.NonEmpty qualified as NE
--import Data.Maybe
--  ( mapMaybe )
import Data.Semigroup
  ( First(..) )
import Data.STRef
import Data.Traversable
  ( for )
import Data.Type.Equality
  ( type (:~:)(..), TestEquality(..) )
import GHC.Generics
  ( Generic )
import GHC.Show
  ( showSpace )
import GHC.Stack

-- containers
import Data.IntMap.Strict
  ( IntMap )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map )
import Data.Map.Strict qualified as Map

-- fin
import Data.Fin qualified as Fin
  ( toNatural )
import Data.Nat
  ( Nat(..) )
import Data.Type.Nat qualified as Nat
  ( eqNat )

-- mtl
import Control.Monad.State.Strict
  ( StateT(..) )
import Control.Monad.State.Strict qualified as State
  ( get, put, runStateT )
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
import Data.Text
  ( Text )
import Data.Text qualified as Text

-- vec
import Data.Vec.Lazy
  ( Vec(..) )
import Data.Vec.Lazy qualified as Vec

-- hs-bindgen
import HsBindgen.C.AST.Literal
  ( Literal(..) )
import HsBindgen.C.AST.Macro
  ( MExpr(..), MFun(..), MTerm(..) )
import HsBindgen.C.AST.Name
  ( CName(..) )
import HsBindgen.Pretty.Orphans
  ()

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
  FunTy :: !( NE.NonEmpty ( Type Ty ) ) -> Type Ty -> Type Ty
  -- | An (exactly saturated) application of a 'TyCon' to arguments.
  TyConAppTy :: TyCon n ki -> Vec n ( Type Ty ) -> Type ki

-- | A qualified quantified type @forall tys. cts => args -> res@.
data QuantTy =
  forall n. QuantTy
    { quantTyBinders  :: !( Vec n Name )
    , quantTyBodyFn   :: !( Vec n ( Type Ty ) -> QuantTyBody )
    }

-- | The body of a quantified type (what's under the forall).
data QuantTyBody
  = QuantTyBody
  { quantTyQuant :: ![ Type Ct ]
  , quantTyBody  :: !( Type Ty )
  }
  deriving stock Show

instance Show (Type ki) where
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
  showsPrec p0 (QuantTy tvs f) =
    showParen ( p0 >= 0 && not ( null tvs && null cts ) ) $
        ( if null tvs
          then id else
            showString "forall"
          . foldr ( \ tv acc -> showSpace . showString ( Text.unpack tv ) . acc ) id tvs
          . showString ". "
        )
      . foldr ( \ a acc -> showsPrec 0 a . showString " => " . acc ) id cts
      . showsPrec 0 body
    where
      QuantTyBody cts body = f $ fmap mkSkol tvs
      mkSkol tv = TyVarTy $ SkolemTv $ SkolemTyVar tv (Unique 0)

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
        ++ case tyVarUnique tv of
             Unique 0 -> ""
             u        -> "_" ++ show u
      , case tv of { MetaTv {} -> "[tau]"; SkolemTv {} -> "" }
      ]

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
  BoolTyCon   :: DataTyCon Z
  StringTyCon :: DataTyCon Z
  IntTyCon    :: DataTyCon Z
  DoubleTyCon :: DataTyCon Z

deriving stock instance Eq  ( DataTyCon n )
deriving stock instance Ord ( DataTyCon n )
instance TestEquality DataTyCon where
  testEquality BoolTyCon   BoolTyCon   = Just Refl
  testEquality StringTyCon StringTyCon = Just Refl
  testEquality IntTyCon    IntTyCon    = Just Refl
  testEquality DoubleTyCon DoubleTyCon = Just Refl
  testEquality _ _ = Nothing

instance PrettyVal ( DataTyCon n ) where
  prettyVal = \case
    BoolTyCon   -> Pretty.Con "BoolTyCon"   []
    StringTyCon -> Pretty.Con "StringTyCon" []
    IntTyCon    -> Pretty.Con "IntTyCon"    []
    DoubleTyCon -> Pretty.Con "DoubleTyCon" []

type ClassTyCon :: Nat -> Hs.Type
data ClassTyCon n where
  EqTyCon         :: ClassTyCon ( S Z )
  OrdTyCon        :: ClassTyCon ( S Z )
  NumTyCon        :: ClassTyCon ( S Z )
  IntegralTyCon   :: ClassTyCon ( S Z )
  FractionalTyCon :: ClassTyCon ( S Z )
  BitsTyCon       :: ClassTyCon ( S Z )
deriving stock instance Eq  ( ClassTyCon n )
deriving stock instance Ord ( ClassTyCon n )
instance TestEquality ClassTyCon where
  testEquality EqTyCon         EqTyCon         = Just Refl
  testEquality OrdTyCon        OrdTyCon        = Just Refl
  testEquality NumTyCon        NumTyCon        = Just Refl
  testEquality IntegralTyCon   IntegralTyCon   = Just Refl
  testEquality FractionalTyCon FractionalTyCon = Just Refl
  testEquality BitsTyCon       BitsTyCon       = Just Refl
  testEquality _ _ = Nothing

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
    BoolTyCon   -> "Bool"
    StringTyCon -> "String"
    IntTyCon    -> "Int"
    DoubleTyCon -> "Double"
instance Show (ClassTyCon n) where
  show = \case
    EqTyCon         -> "Eq"
    OrdTyCon        -> "Ord"
    NumTyCon        -> "Num"
    IntegralTyCon   -> "Integral"
    FractionalTyCon -> "Fractional"
    BitsTyCon       -> "Bits"

-- | On-the-nose type equality.
eqType :: Type Ty -> Type Ty -> Bool
eqType ( TyVarTy tv1 ) ( TyVarTy tv2 ) = tyVarUnique tv1 == tyVarUnique tv2
eqType ( TyConAppTy ( DataTyCon tc1 ) args1 ) ( TyConAppTy ( DataTyCon tc2 ) args2 )
  = case testEquality tc1 tc2 of
      Nothing -> False
      Just Refl ->
        eqTypes ( Vec.zipWith (,) args1 args2 )
eqType ( FunTy args1 res1 ) ( FunTy args2 res2 )
  =  length args1 == length args2
  && eqTypes ( NE.zip args1 args2 )
  && eqType res1 res2
eqType _ _ = False

eqTypes :: ( Functor f, Foldable f ) => f ( Type Ty, Type Ty ) -> Bool
eqTypes = and . fmap ( uncurry eqType )

{-------------------------------------------------------------------------------
  Free type variables and substitution
-------------------------------------------------------------------------------}

freeTyVarsOfType :: forall ki. HasCallStack => IntSet -> Type ki -> [ TyVar ]
freeTyVarsOfType bound0 = nubBy ( (==) `on` tyVarUnique ) . reverse . goTy bound0 []
  where
    goTy :: forall ki'. IntSet -> [ TyVar ] -> Type ki' -> [ TyVar ]
    goTy bound !tvs = \case
      TyVarTy tv ->
        let u = uniqueInt $ tyVarUnique tv
        in if u `IntSet.member` bound
           then tvs
           else tv : tvs
      FunTy args res -> goFunTy bound tvs args res
      TyConAppTy _tc tys -> concatMap (freeTyVarsOfType bound) tys

    goFunTy :: forall ki'. IntSet -> [ TyVar ] -> NE.NonEmpty ( Type Ty ) -> Type ki' -> [ TyVar ]
    goFunTy bound tvs (argTy NE.:| mbArgTys) resTy =
      let !tvs' = goTy bound tvs argTy
      in case NE.nonEmpty mbArgTys of
        Nothing     -> goTy bound tvs resTy
        Just argTys -> goFunTy bound tvs' argTys resTy

tyVarSet :: [ TyVar ] -> IntSet
tyVarSet = IntSet.fromList . map ( uniqueInt . tyVarUnique )

newtype Subst tv = Subst { substMap :: IntMap ( tv, Type Ty ) }
  deriving newtype Monoid
instance Functor Subst where
  fmap f ( Subst s ) = Subst $ fmap ( first f ) s
instance Show k => Semigroup ( Subst k ) where
  ( Subst s1 ) <> sub2@( Subst s2 ) =
    Subst $ IntMap.unionWithKey ( substClashErr "Semigroup Subst" )
              s2
              ( IntMap.map ( \ ( nm, ty ) -> ( nm, applySubst sub2 ty ) ) s1 )
instance Show k => Show ( Subst k ) where
  show ( Subst s ) = "{ " ++ intercalate ", " ( map f $ IntMap.elems s ) ++ " }"
    where
      f ( tv, ty ) = show tv ++ " |-> " ++ show ty


addOneToSubst :: HasCallStack => TyVar -> Type Ty -> Subst TyVar -> Subst TyVar
addOneToSubst tv ty s = s <> mkSubst [ ( tv, ty ) ]

mkSubst :: HasCallStack => [ ( TyVar, Type Ty ) ] -> Subst TyVar
mkSubst = Subst
        . IntMap.fromListWithKey ( substClashErr "mkSubst " )
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

type FunName = Either CName MFun
showFunName :: FunName -> String
showFunName = \case
  Left ( CName f ) -> Text.unpack f
  Right f -> show f

-- | Why did we emit a constraint?
data CtOrigin
  = AppOrigin !FunName
  | InstOrigin !FunName
  | IntLitOrigin !( Literal Integer )
  | FloatLitOrigin !Double
  deriving stock Generic
  deriving anyclass PrettyVal

instance Show CtOrigin where
  show = \case
    AppOrigin fun ->
      "In an application of '" ++ showFunName fun ++ "'."
    InstOrigin fun ->
      "In the instantiation of '" ++ showFunName fun ++ "'."
    IntLitOrigin i ->
      "From the integer literal '" ++ show i ++ "'."
    FloatLitOrigin f ->
      "From the floating-point literal '" ++ show f ++ "'."

-- | Why did we create a new metavariable?
data MetaOrigin
  = ExpectedFunTyResTy !FunName
  | ExpectedVarTy !CName
  | Inst { instFunName :: !FunName, instPos :: !Int }
  | FunArg !CName !( CName, Int )
  | IntLitMeta !( Literal Integer )
  | FloatLitMeta !Double
  deriving stock Generic
  deriving anyclass PrettyVal

instance Show MetaOrigin where
  show = \case
    ExpectedFunTyResTy funNm ->
      "the result type of " ++ show funNm
    ExpectedVarTy varNm ->
      "the type of the identifier " ++ show varNm
    Inst funNm i ->
      "the " ++ speakNth i ++ " type argument in the instantiation of " ++ show funNm
    FunArg funNm ( _argNm, i ) ->
      "the type of the " ++ speakNth i ++ " argument of " ++ show funNm
    IntLitMeta i ->
      "the type of the integer literal " ++ show i
    FloatLitMeta f ->
      "the type of the floating-point literal " ++ show f

speakNth :: Int -> String
speakNth n = show n ++ suffix
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

instance PrettyVal TcError where
  prettyVal = \case
    CouldNotUnify rea orig ty1 ty2 ->
      Pretty.Con "CouldNotUnify" [ prettyVal rea, prettyVal orig, prettyVal ty1, prettyVal ty2 ]
    UnexpectedMTerm mTerm ->
      Pretty.Con "UnexpectedMTerm" [ prettyVal mTerm ]
    UnboundVariable nm ->
      Pretty.Con "UnboundVariable" [ prettyVal nm ]

instance Show TcError where
  show = \case
    CouldNotUnify rea orig ty1 ty2 ->
      unlines
        [ "Could not unify '" ++ show ty1 ++ "' and '" ++ show ty2 ++ "'"
        , "because " ++ show rea ++ "."
        , show orig ]
    UnexpectedMTerm tm ->
      "Unexpected MTerm: " ++ show tm
    UnboundVariable nm ->
      "Unbound variable: " ++ show nm

data CouldNotUnifyReason
  -- | Trying to unify incompatible types, e.g. a 'PiTy' with a 'TyConAppTy'.
  = IncompatibleTypes
  -- | Trying to unify two TyConApps of different lengths.
  | TyConAppUnequalLength
  -- | Trying to unify two TyConApps with different head TyCons.
  | TyConAppDifferentTyCon
  -- | Trying to unify a type variable with a type mentiong this type variable.
  | OccursCheck !Name
  -- | Trying to unify a skolem variable with another type.
  | RigidSkolem !Name
  deriving stock Generic
  deriving anyclass PrettyVal
instance Show CouldNotUnifyReason where
  show = \case
    IncompatibleTypes ->
      "the types are incompatible"
    TyConAppUnequalLength ->
      "the type constructors are applied to different numbers of arguments"
    TyConAppDifferentTyCon ->
      "the type constructors are different"
    OccursCheck tv ->
      "of an occurs-check in the variable '" ++ Text.unpack tv ++ "'"
    RigidSkolem tv ->
      "'" ++ Text.unpack tv ++ "' is a rigid skolem variable"

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

data TcGblEnv s
  = TcGblEnv
      { tcUnique  :: !( STRef s Unique )
      , tcErrs    :: !( STRef s [ ( TcError, SrcSpan ) ] )
      , tcTypeEnv :: !( STRef s ( Map CName QuantTy ) )
      }

data TcLclEnv
  = TcLclEnv
      { tcSrcSpan :: !SrcSpan
      , tcVars    :: !( Map CName ( Type Ty ) )
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
    tcLclEnv = TcLclEnv { tcSrcSpan = SrcSpan, tcVars = Map.empty }
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

errTcM :: TcError -> TcM a
errTcM err = TcM $ \ ( TcEnv ( TcGblEnv { tcErrs } ) ( TcLclEnv { tcSrcSpan } ) ) -> do
  allErrs <- stateSTRef tcErrs ( \ errs -> let allErrs = ( err, tcSrcSpan ) : errs in ( allErrs , allErrs ) )
  error $ concatMap ( \ ( msg, src ) -> unlines [ show msg, "At: " ++ show src ] )  allErrs

newUnique :: TcM Unique
newUnique = TcM $ \ ( TcEnv ( TcGblEnv { tcUnique } ) _ ) ->
  stateSTRef tcUnique ( \ old -> (old, succ old) )

newMetaTyVarTy :: MetaOrigin -> Name -> TcM ( Type Ty )
newMetaTyVarTy metaOrigin metaTyVarName  = do
  metaTyVarUnique <- newUnique
  return $
    TyVarTy $
      MetaTv $
        MetaTyVar
          { metaTyVarUnique
          , metaTyVarName
          , metaOrigin
          }

lookupVarType :: CName -> TcM ( Maybe ( Type Ty ) )
lookupVarType varNm = TcM $ \ ( TcEnv _ lcl ) ->
  return $ Map.lookup varNm ( tcVars lcl )

withVars :: Map CName ( Type Ty ) -> TcM a -> TcM a
withVars vs ( TcM f ) = TcM $ \ ( TcEnv gbl lcl ) ->
  f ( TcEnv gbl ( lcl { tcVars = tcVars lcl <> vs } ) )

{-------------------------------------------------------------------------------
  Typechecking macros: constraint generation monad
-------------------------------------------------------------------------------}

-- | A collection of constraints (with their origin).
type Cts = [ ( Type Ct, CtOrigin ) ]

-- | Monad for generating constraints.
type TcGenM = WriterT Cts ( StateT ( Subst TyVar ) TcM )

liftTcM :: TcM a -> TcGenM a
liftTcM = lift . lift

liftBaseTcM :: ( forall x. TcM x -> TcM x ) -> TcGenM a -> TcGenM a
liftBaseTcM morph g = do
  s0 <- State.get
  ( ( a, cts ), subst ) <- liftTcM $ morph $ ( `State.runStateT` s0 ) $ Writer.runWriterT g
  State.put subst
  Writer.tell cts
  return a

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
  | TyConAppTy tc1  as1 <- ty1
  , TyConAppTy tc2  as2 <- ty2
  = unifyTyConApp orig swapped ( tc1, as1 ) ( tc2, as2 )
  | otherwise
  = couldNotUnify IncompatibleTypes orig swapped ty1 ty2

unifyTyConApp :: forall n1 ki1 n2 ki2. CtOrigin -> Bool
              -> ( TyCon n1 ki1, Vec n1 ( Type Ty ) ) -> ( TyCon n2 ki2, Vec n2 ( Type Ty ) )
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
        ( DataTyCon  dc1 , DataTyCon  dc2  ) -> case testEquality dc1  dc2  of { Just Refl -> Just Refl; Nothing -> Nothing }
        ( ClassTyCon cls1, ClassTyCon cls2 ) -> case testEquality cls1 cls2 of { Just Refl -> Just Refl; Nothing -> Nothing }
        _                                    -> Nothing

unifyTypes :: Traversable t => CtOrigin -> Bool -> t ( Type Ty, Type Ty ) -> TcGenM ()
unifyTypes orig swapped = traverse_ ( uncurry $ unifyType orig swapped )
{-# INLINEABLE unifyTypes #-}

unifyTyVar :: CtOrigin -> Bool -> TyVar -> Type Ty -> TcGenM ()
unifyTyVar orig swapped tv ty' = do
  subst <- State.get
  case ty' of
    _
      | Just ty <- lookupSubst tv subst
      -> unifyType orig swapped ty ty'
    TyVarTy tv'
      | tyVarUnique tv == tyVarUnique tv'
      -> return ()
      | SkolemTv {} <- tv
      , MetaTv {} <- tv'
      -> unifyTyVar orig ( not swapped ) tv' ( TyVarTy tv )
    _
      | any ( ( == tyVarUnique tv ) . tyVarUnique ) $ freeTyVarsOfType IntSet.empty ty'
      -> couldNotUnify (OccursCheck (tyVarName tv)) orig swapped ( TyVarTy tv ) ty'
      | MetaTv tau <- tv
      -> State.put $ addOneToSubst ( MetaTv tau ) ty' subst
      | otherwise
      -> couldNotUnify (RigidSkolem (tyVarName tv)) orig swapped ( TyVarTy tv ) ty'

unifyFunTys :: CtOrigin -> Bool -> NE.NonEmpty ( Type Ty ) -> Type Ty -> NE.NonEmpty ( Type Ty )  -> Type Ty -> TcGenM ()
unifyFunTys orig swapped ( arg1 NE.:| args1 ) res1 ( arg2 NE.:| args2 ) res2
  = do unifyType orig swapped arg1 arg2
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
instantiate funNm ( QuantTy { quantTyBinders = tvNames, quantTyBodyFn = body } ) = do
  let is = Vec.imap ( \ i _ -> fromIntegral ( Fin.toNatural i ) + 1 ) tvNames
  tvs <- liftTcM $ for ( Vec.zipWith (,) is tvNames ) $ \ ( i, tvName ) ->
    newMetaTyVarTy ( Inst { instFunName = funNm, instPos = i } ) tvName
  let QuantTyBody cts bodyTy = body tvs
  Writer.tell $ map (, InstOrigin funNm ) cts
  return bodyTy

{-------------------------------------------------------------------------------
  Typechecking macros: type inference
-------------------------------------------------------------------------------}

-- | Infer the type of a macro declaration (before constraint solving and generalisation).
inferTop :: CName -> [ CName ] -> MExpr -> TcM ( Type Ty, ( Cts, InconsistentPairs ) )
inferTop funNm argsList body = Vec.reifyList argsList $ \ args ->
  do ( ( ( argTys, bodyTy ), cts ), subst ) <- ( `State.runStateT` mempty ) $ Writer.runWriterT ( inferLam funNm args body )
     let macroTy = case NE.nonEmpty $ toList argTys of
           Nothing -> bodyTy
           Just argTysNE -> FunTy argTysNE bodyTy
     return ( applySubst subst macroTy, simplifyCts $ map ( first ( applySubst subst ) ) cts )

inferExpr :: MExpr -> TcGenM ( Type Ty )
inferExpr = \case
  MTerm tm -> inferTerm tm
  MApp fun args -> inferApp ( Right fun ) args

inferTerm :: MTerm -> TcGenM ( Type Ty )
inferTerm = \case
  tm@MEmpty -> liftTcM . errTcM $ UnexpectedMTerm tm
  MInt i -> do
    m <- liftTcM $ newMetaTyVarTy (IntLitMeta i) "i"
    Writer.tell [ ( Integral m, IntLitOrigin i ) ]
    return m
  MFloat f -> do
    m <- liftTcM $ newMetaTyVarTy (FloatLitMeta f) "f"
    Writer.tell [ ( Fractional m , FloatLitOrigin f ) ]
    return m
  MVar fun args -> inferApp ( Left fun ) args
  tm@( MType {} ) -> liftTcM . errTcM $ UnexpectedMTerm tm
  MAttr _attr tm -> inferTerm tm
  MStringize {} -> return String
  MConcat {} -> return String

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
      unifyType ( AppOrigin fun ) False funTy actualTy
      return resTy

inferFun :: FunName -> TcGenM ( Type Ty )
inferFun fun = case fun of
  Left varNm@( CName varStr ) -> liftTcM $ do
    mbTy <- lookupVarType varNm
    case mbTy of
      Nothing -> do
        addErrTcM $ UnboundVariable varNm
        newMetaTyVarTy ( ExpectedVarTy varNm ) ( varStr <> "_ty" )
      Just varTy -> return varTy
  Right mFun  -> do
    let funQTy = inferMFun mFun
    instantiate fun funQTy

inferLam :: forall n. CName -> Vec n CName -> MExpr -> TcGenM ( Vec n ( Type Ty ), Type Ty )
inferLam _ VNil body = ( VNil, ) <$> inferExpr body
inferLam funNm argNms@( _ ::: _ ) body = do
  let is = Vec.imap ( \ i _ -> fromIntegral ( Fin.toNatural i ) + 1 ) argNms
  argTys <- liftTcM $
    for ( Vec.zipWith (,) is argNms ) $ \ ( i, argNm@( CName argStr ) ) ->
      newMetaTyVarTy ( FunArg funNm ( argNm, i ) ) argStr
  liftBaseTcM ( withVars ( Map.fromList $ toList $ Vec.zipWith (,) argNms argTys ) ) $
    ( argTys, ) <$> inferExpr body

inferMFun :: MFun -> QuantTy
inferMFun = \case
  MUnaryPlus  -> q1 $ \ a -> QuantTyBody [Num a]        ( funTy [a]          a )
  MUnaryMinus -> q1 $ \ a -> QuantTyBody [Num a]        ( funTy [a]          a )
  MLogicalNot -> q0 $        QuantTyBody []             ( funTy [Bool]       Bool )
  MBitwiseNot -> q1 $ \ a -> QuantTyBody [Bits a]       ( funTy [a]          a )
  MMult       -> q1 $ \ a -> QuantTyBody [Num a]        ( funTy [a,a]        a )
  MDiv        -> q1 $ \ a -> QuantTyBody [Fractional a] ( funTy [a,a]        a )
  MRem        -> q1 $ \ a -> QuantTyBody [Integral a]   ( funTy [a,a]        a )
  MAdd        -> q1 $ \ a -> QuantTyBody [Num a]        ( funTy [a,a]        a )
  MSub        -> q1 $ \ a -> QuantTyBody [Num a]        ( funTy [a,a]        a )
  MShiftLeft  -> q1 $ \ a -> QuantTyBody [Bits a]       ( funTy [a,Int]      a )
  MShiftRight -> q1 $ \ a -> QuantTyBody [Bits a]       ( funTy [a,Int]      a )
  MRelLT      -> q1 $ \ a -> QuantTyBody [Ord a]        ( funTy [a,a]        Bool )
  MRelLE      -> q1 $ \ a -> QuantTyBody [Ord a]        ( funTy [a,a]        Bool )
  MRelGT      -> q1 $ \ a -> QuantTyBody [Ord a]        ( funTy [a,a]        Bool )
  MRelGE      -> q1 $ \ a -> QuantTyBody [Ord a]        ( funTy [a,a]        Bool )
  MRelEQ      -> q1 $ \ a -> QuantTyBody [Eq a]         ( funTy [a,a]        Bool )
  MRelNE      -> q1 $ \ a -> QuantTyBody [Eq a]         ( funTy [a,a]        Bool )
  MBitwiseAnd -> q1 $ \ a -> QuantTyBody [Bits a]       ( funTy [a,a]        a )
  MBitwiseXor -> q1 $ \ a -> QuantTyBody [Bits a]       ( funTy [a,a]        a )
  MBitwiseOr  -> q1 $ \ a -> QuantTyBody [Bits a]       ( funTy [a,a]        a )
  MLogicalAnd -> q0 $        QuantTyBody []             ( funTy [Bool, Bool] Bool )
  MLogicalOr  -> q0 $        QuantTyBody []             ( funTy [Bool, Bool] Bool )
  where
    q0 body = QuantTy VNil $ \ VNil -> body
    q1 body = QuantTy ("a" ::: VNil) $ \ (a ::: VNil) -> body a
    funTy mbArgs res = case NE.nonEmpty mbArgs of
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
pattern Int :: Type Ty
pattern Int = TyConAppTy (DataTyCon IntTyCon) VNil
pattern Double :: Type Ty
pattern Double = TyConAppTy (DataTyCon DoubleTyCon) VNil
pattern String :: Type Ty
pattern String = TyConAppTy (DataTyCon StringTyCon) VNil

{-------------------------------------------------------------------------------
  Typechecking macros: constraint solving
-------------------------------------------------------------------------------}

classImplies :: ClassTyCon n -> ClassTyCon n -> Bool
classImplies OrdTyCon EqTyCon = True
classImplies IntegralTyCon NumTyCon = True
classImplies FractionalTyCon NumTyCon = True
classImplies cls1 cls2 = cls1 == cls2

data Models
  = NoModels ![ Cts ] !( IntMap ( First TyVar, Cts ) )
  | Models !( NE.NonEmpty ( Subst ( First TyVar, Cts ) ) )

instance Semigroup Models where
  NoModels cts1 tvs1 <> NoModels cts2 tvs2 = NoModels ( cts1 <> cts2 ) ( tvs1 <> tvs2 )
  no@( NoModels {}) <> _ = no
  _ <> no@( NoModels {} ) = no
  Models ms1 <> Models ms2 =
    let ( bads, mbOk ) = partitionEithers [ combineSubsts m1 m2 | m1 <- NE.toList ms1, m2 <- NE.toList ms2 ]
    in case NE.nonEmpty mbOk of
        Just ok ->
          Models ok
        Nothing ->
          NoModels [] ( IntMap.unions bads )

instance Monoid Models where
  mempty = Models ( NE.singleton mempty )

-- | Combine two substitutions, if they are compatible.
combineSubsts :: forall tv. Semigroup tv => Subst tv -> Subst tv -> Either ( IntMap tv ) ( Subst tv )
combineSubsts ( Subst mods1 ) ( Subst mods2 ) =
  case IntMap.foldMapWithKey go mods1 mods2 of
    ( i3, m3 ) ->
      if IntMap.null i3
      then Right $ Subst m3
      else Left i3
  where
    go :: Int -> ( tv, Type Ty ) -> IntMap ( tv, Type Ty )
       -> ( IntMap tv, IntMap ( tv, Type Ty ) )
    go k tys1 m2 =
      case IntMap.alterF @( Either ( IntMap tv ) ) ( doIntersect k tys1 ) k m2 of
        Left  i  -> ( i, IntMap.delete k m2 )
        Right m3 -> ( IntMap.empty, m3 )

    doIntersect :: Int
                -> ( tv, Type Ty ) -> Maybe ( tv, Type Ty )
                -> Either ( IntMap tv ) ( Maybe ( tv, Type Ty ) )
    doIntersect _ tys1 Nothing = Right ( Just tys1 )
    doIntersect k ( tv1, ty1 ) ( Just ( tv2, ty2 ) )
      = if eqType ty1 ty2
        then Right ( Just ( tv1 <> tv2, ty1 ) )
        else Left ( IntMap.singleton k ( tv1 <> tv2 ) )


classInstances :: ClassTyCon n -> [ Vec n ( Type Ty ) ]
classInstances = \case
  EqTyCon         -> map ( ::: VNil ) [ Int, Double, String, Bool ]
  OrdTyCon        -> map ( ::: VNil ) [ Int, Double, String, Bool ]
  NumTyCon        -> map ( ::: VNil ) [ Int, Double ]
  IntegralTyCon   -> map ( ::: VNil ) [ Int ]
  FractionalTyCon -> map ( ::: VNil ) [ Double ]
  BitsTyCon       -> map ( ::: VNil ) [ Int ]

matchesInstance :: Type Ct -> Bool
matchesInstance ( TyConAppTy ( ClassTyCon cls ) args ) =
  any ( eqTypes . Vec.zipWith (,) args ) ( classInstances cls )

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
solveOneFromTheOther ( TyConAppTy ( ClassTyCon cls1 ) args1 ) ( TyConAppTy ( ClassTyCon cls2 ) args2 )
  | Just Refl <- sameLength args1 args2
  , eqTypes $ Vec.zipWith (,) args1 args2
  =  if | null $ intersectBy ( ( eqTypes . ) . Vec.zipWith (,) ) ( classInstances cls1 ) ( classInstances cls2 )
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
          wantCt = not ( matchesInstance ( fst ct ) )
                && keepCt
                && all wantLeft ( map ( solveOneFromTheOther ( fst ct ) . fst ) ( prev ++ cts ) )
          rest = go ( errs ++ incons ) prev keptCts
      in if wantCt
         then first ( ct : ) rest
         else                rest

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
  Typechecking macros: defaulting and quantification
-------------------------------------------------------------------------------}

tcMacro :: Map CName QuantTy -> CName -> [ CName ] -> MExpr -> Either TcMacroError QuantTy
tcMacro tyEnv macroNm args body = do
  let
    -- Step 1: infer the type.
    ( ( ty, ( cts, mbIncons ) ), mbErrs ) = runTcM tyEnv ( inferTop macroNm args body )
  throwAnyErrs TcErrors            mbErrs
  throwAnyErrs TcInconsistentPairs mbIncons
  let
    -- Step 2: compute the set of metavariables we will quantify over.
    quantTvs = freeTyVarsOfType mempty ty

    -- Step 3: try to solve the remaining constraints by defaulting.
    models = foldMap ( defaultCt ( tyVarSet quantTvs ) ) cts
  case models of
    NoModels nm1 nm2 -> do
      Left ( TcInconsistentSets nm1 ( coerce nm2 ) )
    Models ( defaultSubst NE.:| _otherSubsts ) -> do
      let
        -- Step 4: re-simplify constraints after defaulting.
        -- NB: no need to go around in a loop for such simple constraints.
        ( ctsAfterDefaulting , mbIncons2 ) =
          simplifyCts ( map ( first ( applySubst defaultSubst ) ) cts )

      throwAnyErrs TcInconsistentPairs mbIncons2

      -- Step 5: generalise.
      Vec.reifyList quantTvs $ \ quantTvsVec ->
        Right $ QuantTy ( fmap tyVarName quantTvsVec ) $ \ tys ->
          let quantSubst = mkSubst ( toList $ Vec.zipWith (,) quantTvsVec tys )
          in QuantTyBody
              { quantTyQuant = map ( applySubst quantSubst ) ( fmap fst ctsAfterDefaulting )
              , quantTyBody  =       applySubst quantSubst   ty
              }

defaultCt :: IntSet -> ( Type Ct, CtOrigin ) -> Models
defaultCt quantTvs ct@( TyConAppTy ( ClassTyCon cls ) ( args :: Vec n ( Type ty ) ), _ ) =
  modelsForInstances ( classInstances cls )
  where

    notate :: First TyVar -> ( First TyVar, Cts )
    notate tv = ( tv, [ ct ] )

    modelsForInstances :: [ Vec n ( Type Ty ) ] -> Models
    modelsForInstances [] = NoModels [ [ ct ] ] IntMap.empty
    modelsForInstances ( instArgs : instArgss ) =
      case modelForInstance instArgs of
        Left badTvs ->
          case modelsForInstances instArgss of
            NoModels {}     -> NoModels [ ] ( fmap notate badTvs )
            ms@( Models {}) -> ms
        Right m ->
          case modelsForInstances instArgss of
            NoModels {} -> Models ( NE.singleton $ fmap notate m  )
            Models ms   -> Models ( fmap notate m `NE.cons` ms )

    modelForInstance :: Vec n ( Type Ty ) -> Either ( IntMap ( First TyVar ) ) ( Subst ( First TyVar ) )
    modelForInstance instArgs =
      case sequence $ Vec.zipWith defaultArg args instArgs of
        Nothing -> Left IntMap.empty
        Just ms ->
          foldrM combineSubsts ( Subst IntMap.empty ) ms

    defaultArg :: Type Ty -> Type Ty -> Maybe ( Subst ( First TyVar ) )
    defaultArg ( TyVarTy ( MetaTv tv ) ) instTy
      | let u = uniqueInt $ metaTyVarUnique tv
      = if u `IntSet.member` quantTvs
        then Just $ Subst IntMap.empty
        else Just $ Subst ( IntMap.singleton u ( First $ MetaTv tv, instTy ) )
    defaultArg ty candTy =
      if eqType ty candTy
      then Just $ Subst IntMap.empty
      else Nothing

data TcMacroError
  = TcErrors !( NE.NonEmpty (TcError, SrcSpan) )
  | TcInconsistentPairs !( NE.NonEmpty InconsistentPair )
  | TcInconsistentSets ![ Cts ] !( IntMap ( TyVar, Cts ) )
  deriving stock (Show, Generic)
  deriving anyclass (PrettyVal)
instance Eq TcMacroError where
  _ == _ = True

throwAnyErrs :: ( NE.NonEmpty err -> TcMacroError ) -> [ err ] -> Either TcMacroError ()
throwAnyErrs f mbErrs =
  case NE.nonEmpty mbErrs of
    Just errs -> Left ( f errs )
    Nothing -> Right ()

--------------------------------------------------------------------------------
-- Testing

{-
int :: Integer -> MExpr
int i = MTerm (MInt (Literal (Text.pack $ show i) i))
double :: Double -> MExpr
double f = MTerm (MFloat f)
var :: Text -> MExpr
var x = MTerm ( MVar ( CName x ) [] )

-- 1 + 2
expr1:: MExpr
expr1 = MApp MAdd [int 1, int 2]

-- ( 7 * 7 ) <= ( 1 << 8 )
expr2 :: MExpr
expr2 = MApp MRelLE [MApp MMult [int 7, int 7], MApp MShiftLeft [int 1, int 8]]

-- 1 + 1.0
expr3 :: MExpr
expr3 = MApp MAdd [int 1, double 1]

-- a + b
expr4 :: MExpr -> MExpr -> MExpr
expr4 a b = MApp MAdd [ a, b ]


testFun :: Int -> Vec n Text -> ( Vec n MExpr -> MExpr ) -> Either TcMacroError QuantTy
testFun i args f =
  case NE.nonEmpty mbDups of
    Nothing ->
      tcMacro mempty ( CName $ "testFn_" <> Text.pack ( show i ) ) ( toList $ fmap CName args ) body
    Just dups ->

      let s = NE.length dups > 1
      in
        error $ unlines $
          [ "internal error in 'testFun':"
          , "  the following variable" ++ (if s then "s" else "") ++ " appear" ++ (if s then "" else "s") ++ " more than once:"
          ] ++ map ( ( "    - " <> ) . show ) ( NE.toList dups )
  where
    mbDups = mapMaybe ( \ case ( a NE.:| ( _ : _ ) ) -> Just a; _ -> Nothing ) $ NE.group $ sort $ toList args
    body = f $ fmap ( \ nm -> MTerm ( MVar ( CName nm ) [] ) ) args

test1, test2, test3, test4 :: Either TcMacroError QuantTy
test1 = testFun 1 VNil ( \ VNil -> expr1 )
test2 = testFun 2 VNil ( \ VNil -> expr2 )
test3 = testFun 3 VNil ( \ VNil -> expr3 )
test4 = testFun 4 ( "a" ::: "b" ::: VNil ) ( \ ( a ::: b ::: VNil ) -> expr4 a b )
-}
--------------------------------------------------------------------------------
