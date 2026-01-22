{-# LANGUAGE ScopedTypeVariables #-}

-- | **Internal** module listing out all n-tuples of types
-- for the purposes of generating code with Template Haskell
-- and for testing.
module C.Type.Internal.Universe
  ( OpaqueTy(..)
  , enumerateTypeTuples
  , allArithmeticTypes, allIntegralTypes, allIntLikeTypes
  ) where

-- base
import Data.Functor
  ( (<&>) )
import GHC.Generics
  ( Generic )

-- fin
import qualified Data.Type.Nat as Fin

-- vec
import Data.Vec.Lazy ( Vec(..) )
import Data.Vec.Lazy qualified as Vec
  ( snoc )

-- c-expr
import C.Type

--------------------------------------------------------------------------------

-- | Helper type to use 'Fin.induction' on.
newtype F n = F { unF :: [ ( Vec n ( Type OpaqueTy ), Maybe Int ) ] }

-- | An opaque named type with a unique identifier.
newtype OpaqueTy = OpaqueTy Int
  deriving stock ( Eq, Ord, Show, Generic )

-- | Enumerate all tuples of types.
--
-- For example, for @n = 2@, this will list out pairs of types.
-- This list will include all pairwise combination of primitive types,
-- and also two pairs of pointer types:
--
--  - @( Ptr ty_1, Ptr ty_2 )@ – pointers to different types,
--  - @( Ptr ty_1, Ptr ty_1 )@ – pointers to the same type.
--
-- This is used for generating type family and class instances.
enumerateTypeTuples :: forall n. Fin.SNatI n => [ Vec n ( Type OpaqueTy ) ]
enumerateTypeTuples = fmap fst $ unF $
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

maxMaybe :: Ord i => Maybe i -> Maybe i -> Maybe i
maxMaybe ( Just i ) ( Just j ) = Just $ max i j
maxMaybe j@( Just {} ) Nothing = j
maxMaybe Nothing r = r

allTypes :: Int -> [ ( Type OpaqueTy, Maybe Int ) ]
allTypes n =
  fmap ( ( , Nothing ) . Arithmetic ) allArithmeticTypes ++ ( Void, Nothing ) :
    [ ( Ptr ( OpaqueTy i ), Just i ) | i <- [ 1 .. n ] ]
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
allIntLikeTypes =
  concatMap ( [ Signed, Unsigned ] <&> ) [ Short, Int, Long, LongLong ]
    ++ [ PtrDiff ]
