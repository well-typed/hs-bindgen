{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module C.Typing.Arithmetic.TH
  ( hostPlatform
  , genTyFam, genUnaryTyFam, genBinaryTyFam
  ) where

-- base
import Data.Foldable
  ( toList )
import Data.Functor
  ( (<&>) )
import Data.Kind qualified as Hs
import Data.Maybe
  ( fromJust, maybeToList )
import Data.Proxy
  ( Proxy(..) )
import Data.Void
  ( Void )
import Foreign
  ( Ptr )
import Foreign.C

-- fin
import Data.Type.Nat qualified as Fin
import Data.Type.Nat
  ( Nat(..) )

-- template-haskell
import Language.Haskell.TH qualified as TH

-- vec
import Data.Vec.Lazy
  ( Vec(..) )
import Data.Vec.Lazy qualified as Vec

-- typing-c
import C.Type

--------------------------------------------------------------------------------

hostPlatform :: TH.Q Platform
hostPlatform = return $ Platform { platformWordWidth = WordWidth64 }

genUnaryTyFam :: String -> ( Platform -> Type -> Maybe Type ) -> TH.Q [ TH.Dec ]
genUnaryTyFam str f = genTyFam @( S Z ) str g
  where
    g :: Platform -> Vec ( S Z ) Type -> Maybe Type
    g p ( a ::: VNil ) = f p a

genBinaryTyFam :: String -> ( Platform -> Type -> Type -> Maybe Type ) -> TH.Q [ TH.Dec ]
genBinaryTyFam str f = genTyFam @( S ( S Z ) ) str g
  where
    g :: Platform -> Vec ( S ( S Z ) ) Type -> Maybe Type
    g p ( a ::: b ::: VNil ) = f p a b


genTyFam :: forall n. Fin.SNatI n => String -> ( Platform -> Vec n Type -> Maybe Type ) -> TH.Q [ TH.Dec ]
genTyFam famName impl = do
  let hsTy :: TH.Type
      hsTy = TH.ConT ''Hs.Type
      n :: Int
      n = Fin.reflectToNum @n Proxy
      fam = TH.mkName famName
      args = fmap TH.mkName $ fromJust $ Vec.fromListPrefix @n [ "t" ++ show i | i <- [(1 :: Int)..]]
  plat <- hostPlatform
  return
    [ TH.KiSigD fam ( foldr ( \ arg acc -> TH.AppT ( TH.AppT TH.ArrowT arg ) acc ) hsTy ( replicate n hsTy ) )
    , TH.ClosedTypeFamilyD
      ( TH.TypeFamilyHead fam [ TH.PlainTV a TH.BndrReq | a <- toList args ] TH.NoSig Nothing )
      ( mkTyFamEqs fam ( impl plat ) )
    ]

mkTyFamEqs :: forall n. Fin.SNatI n => TH.Name -> ( Vec n Type -> Maybe Type ) -> [ TH.TySynEqn ]
mkTyFamEqs fam impl =
  [ TH.TySynEqn Nothing ( foldl' TH.AppT ( TH.ConT fam ) ( toList $ fmap mkType args ) ) ( mkType res )
  | ( args :: Vec n Type ) <- enumerateTypes @n
  , res <- maybeToList $ impl args
  ]

newtype F n = F { unF :: [ ( Vec n Type, Maybe Int ) ] }

enumerateTypes :: forall n. Fin.SNatI n => [ Vec n Type ]
enumerateTypes = fmap fst $ unF $
  Fin.induction
    ( F [ ( VNil, Nothing ) ] )
    ( \ ( ( F prev ) :: F m ) -> F $ do
      ( tys, mbLastUsedTyVarNumber ) <- prev
      let m = case mbLastUsedTyVarNumber of
                Nothing -> 1
                Just i  -> i + 1
      ( ty, mbNextUsedTyVar ) <- allTypes m
      let mbUsedTv = maxMaybe mbLastUsedTyVarNumber mbNextUsedTyVar
      return ( Vec.snoc tys ty, mbUsedTv )
    )

maxMaybe :: Maybe Int -> Maybe Int -> Maybe Int
maxMaybe ( Just i ) ( Just j ) = Just $ max i j
maxMaybe j@( Just {} ) Nothing = j
maxMaybe Nothing r = r


mkType :: Type -> TH.Type
mkType = \case
  Void -> TH.ConT ''Void
  Ptr ty -> TH.AppT ( TH.ConT ''Ptr ) $ mkType ty
  Arithmetic a ->
    mkArithmeticType a
  Struct nm ->
    -- NB: we're piggy-backing on structs for type family equations,
    -- turning a named struct into a type variable, for example:
    --
    --   SubRes (Ptr a) (Ptr a) = PtrDiff
    TH.VarT ( TH.mkName nm )

mkArithmeticType :: ArithmeticType -> TH.Type
mkArithmeticType ( Integral i ) = mkIntegralType i
mkArithmeticType ( FloatLike f ) =
  TH.ConT $
    case f of
      FloatType  -> ''CFloat
      DoubleType -> ''CDouble

mkIntegralType :: IntegralType -> TH.Type
mkIntegralType = \case
  Bool -> TH.ConT ''CBool
  CharLike c -> TH.ConT $
    case c of
      Char  -> ''CChar
      SChar -> ''CSChar
      UChar -> ''CUChar
  IntLike i ->
    mkIntLikeType i

mkIntLikeType :: IntLikeType -> TH.Type
mkIntLikeType = TH.ConT . \case
  Short    s ->
    case s of
      Signed   -> ''CShort
      Unsigned -> ''CUShort
  Int      s ->
    case s of
      Signed   -> ''CInt
      Unsigned -> ''CUInt
  Long     s ->
    case s of
      Signed   -> ''CLong
      Unsigned -> ''CULong
  LongLong s ->
    case s of
      Signed   -> ''CLLong
      Unsigned -> ''CULLong
  PtrDiff -> ''CPtrdiff


allTypes :: Int -> [ ( Type, Maybe Int ) ]
allTypes n =
  fmap ( ( , Nothing ) . Arithmetic ) allArithmeticTypes ++ ( Void, Nothing ) :
    [ ( Ptr ( Struct ( "ty_" ++ show i ) ), Just i ) | i <- [ 1 .. n ] ]
  -- For unary functions, just have a single pointer type "Ptr a".
  -- For binary functions, the first argument has "Ptr a1",
  -- while the second argument has both "Ptr a1" and "Ptr a2".
  -- etc

allArithmeticTypes :: [ ArithmeticType ]
allArithmeticTypes = fmap FloatLike [ FloatType, DoubleType ]
                  ++ fmap Integral  allIntegralTypes

allIntegralTypes :: [ IntegralType ]
allIntegralTypes = Bool : fmap CharLike [ Char, SChar, UChar ] ++ fmap IntLike allIntLikeTypes

allIntLikeTypes :: [ IntLikeType ]
allIntLikeTypes = concatMap ( [ Signed, Unsigned ] <&> ) [ Short, Int, Long, LongLong ] ++ [ PtrDiff ]
