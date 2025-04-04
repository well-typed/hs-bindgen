{-# LANGUAGE TypeFamilies #-}

module HsBindgen.Runtime.CEnum.General (
    -- * Type Class
    GeneralCEnum(..)
    -- * Bounded API
  , generalCEnumMinBound
  , generalCEnumMaxBound
    -- * Enum API
  , generalCEnumSucc
  , generalCEnumPred
  , generalCEnumToEnum
  , generalCEnumFromEnum
  , generalCEnumEnumFrom
  , generalCEnumEnumFromThen
  , generalCEnumEnumFromTo
  , generalCEnumEnumFromThenTo
    -- * Instance Deriving
  , GenCEnum(..)
  ) where

import Control.Exception (throw)
import Data.Bifunctor (Bifunctor(first))
import Data.List qualified as List
import Data.List.Compat qualified as ListCompat
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy(Proxy))

import HsBindgen.Runtime.CEnum.Exception

{-------------------------------------------------------------------------------
  Type Class
-------------------------------------------------------------------------------}

-- | C enumeration with support for non-sequential values
--
-- This class helps implement 'Bounded' and 'Enum' instances for C enumerations
-- represented using `newtype` wrappers around integral types, where the
-- enumeration values may not be sequential.
--
-- Instances must be defined to maintain the following invariants.
--
-- @toGeneralCEnum@ and @fromGeneralCEnum@ must be inverses of each other:
--
-- prop> toGeneralCEnum . fromGeneralCEnum === id
--
-- prop> fromGeneralCEnum . toGeneralCEnum === id
--
-- @generalCEnumValues@ must be non-empty:
--
-- prop> length generalCEnumValues > 0
--
-- @generalCEnumValues@ must be monotonically increasing:
--
-- prop> sort generalCEnumValues == generalCEnumValues
--
-- prop> nub generalCEnumValues == generalCEnumValues
class Integral (GeneralCEnumZ a) => GeneralCEnum a where
  -- | Wrapped integral type
  type GeneralCEnumZ a

  -- | Wrap the integral type
  toGeneralCEnum :: GeneralCEnumZ a -> a

  -- | Unwrap the integral type
  fromGeneralCEnum :: a -> GeneralCEnumZ a

  -- | List of values
  generalCEnumValues :: Proxy a -> [GeneralCEnumZ a]

{-------------------------------------------------------------------------------
  Bounded API
-------------------------------------------------------------------------------}

-- | Get the lower bound
--
-- This is an implementation of 'minBound'.
--
-- A 'CEnumException' is thrown if there are no values (an invariant violation).
generalCEnumMinBound :: forall a. GeneralCEnum a => a
generalCEnumMinBound = case List.uncons values of
    Just (i, _) -> toGeneralCEnum i
    Nothing     -> throw CEnumEmpty
  where
    values :: [GeneralCEnumZ a]
    values = generalCEnumValues (Proxy :: Proxy a)

-- | Get the upper bound
--
-- This is an implementation of 'maxBound'.
--
-- A 'CEnumException' is thrown if there are no values (an invariant violation).
generalCEnumMaxBound :: forall a. GeneralCEnum a => a
generalCEnumMaxBound = case ListCompat.unsnoc values of
    Just (_, z) -> toGeneralCEnum z
    Nothing     -> throw CEnumEmpty
  where
    values :: [GeneralCEnumZ a]
    values = generalCEnumValues (Proxy :: Proxy a)

{-------------------------------------------------------------------------------
  Enum API
-------------------------------------------------------------------------------}

-- | Get the successor of a value
--
-- This is an implementation of 'succ'.
--
-- This is a partial function.  A 'CEnumException' is thrown if the passed value
-- has no successor or is invalid.
generalCEnumSucc :: forall a. GeneralCEnum a => a -> a
generalCEnumSucc x =
    case List.dropWhile (/= i) values of
      _i:j:_rest -> toGeneralCEnum j
      []         -> throw $ CEnumInvalid     (fromIntegral i)
      _otherwise -> throw $ CEnumNoSuccessor (fromIntegral i)
  where
    i :: GeneralCEnumZ a
    i = fromGeneralCEnum x

    values :: [GeneralCEnumZ a]
    values = generalCEnumValues (Proxy :: Proxy a)

-- | Get the predecessor of a value
--
-- This is an implementation of 'pred'.
--
-- This is a partial function.  A 'CEnumException' is thrown if the passed value
-- has no precessor or is invalid.
generalCEnumPred :: forall a. GeneralCEnum a => a -> a
generalCEnumPred y =
    case List.dropWhile (/= j) (reverse values) of
      _j:i:_rest -> toGeneralCEnum i
      []         -> throw $ CEnumInvalid       (fromIntegral j)
      _otherwise -> throw $ CEnumNoPredecessor (fromIntegral j)
  where
    j :: GeneralCEnumZ a
    j = fromGeneralCEnum y

    values :: [GeneralCEnumZ a]
    values = generalCEnumValues (Proxy :: Proxy a)

-- | Convert from an 'Int'
--
-- This is an implementation of 'toEnum'.
--
-- This is a partial function.  A 'CEnumException' is thrown if the passed value
-- is invalid.
generalCEnumToEnum :: forall a. GeneralCEnum a => Int -> a
generalCEnumToEnum i'
    | i `elem` values = toGeneralCEnum i
    | otherwise       = throw $ CEnumInvalid (fromIntegral i)
  where
    i :: GeneralCEnumZ a
    i = fromIntegral i'

    values :: [GeneralCEnumZ a]
    values = generalCEnumValues (Proxy :: Proxy a)

-- | Convert to an 'Int'
--
-- This is an implementation of 'fromEnum'.
--
-- This is a partial function.  A 'CEnumException' is thrown if the passed value
-- is invalid.
generalCEnumFromEnum :: forall a. GeneralCEnum a => a -> Int
generalCEnumFromEnum x
    | i `elem` values = fromIntegral i
    | otherwise       = throw $ CEnumInvalid (fromIntegral i)
  where
    i :: GeneralCEnumZ a
    i = fromGeneralCEnum x

    values :: [GeneralCEnumZ a]
    values = generalCEnumValues (Proxy :: Proxy a)

-- | Create a list of enumeration values like @[x ..]@
--
-- This is an implementation of 'enumFrom'.
--
-- This is a partial function.  A 'CEnumException' is thrown if the passed value
-- is invalid.
generalCEnumEnumFrom :: forall a. GeneralCEnum a => a -> [a]
generalCEnumEnumFrom x =
    case List.dropWhile (/= i) values of
      _i:rest -> x : map toGeneralCEnum rest
      []      -> throw $ CEnumInvalid (fromIntegral i)
  where
    i :: GeneralCEnumZ a
    i = fromGeneralCEnum x

    values :: [GeneralCEnumZ a]
    values = generalCEnumValues (Proxy :: Proxy a)

-- | Create a list of enumeration values like @[x, y ..]@
--
-- This is an implementation of 'enumFromThen'.
--
-- This is a partial function.  A 'CEnumException' is thrown if a passed value
-- is invalid or if the from and then values are equal.
generalCEnumEnumFromThen :: forall a. GeneralCEnum a => a -> a -> [a]
generalCEnumEnumFromThen x y
    | i == j    = throw $ CEnumFromEqThen (fromIntegral i)
    | otherwise = case List.dropWhile (/= i) values of
        _i:rest -> case first ((+ 1) . length) (List.break (== j) rest) of
          (_w, []) -> throw $ CEnumInvalid (fromIntegral j)
          (w,  js) -> x : map (toGeneralCEnum . NonEmpty.head) (chunksOf w js)
        []      -> throw $ CEnumInvalid (fromIntegral i)
  where
    i, j :: GeneralCEnumZ a
    i = fromGeneralCEnum x
    j = fromGeneralCEnum y

    values :: [GeneralCEnumZ a]
    values = (if i < j then id else reverse) $
      generalCEnumValues (Proxy :: Proxy a)

-- | Create a list of enumeration values like @[x .. z]@
--
-- This is an implementation of 'enumFromTo'.
--
-- This is a partial function.  A 'CEnumException' is thrown if a passed value
-- is invalid.
generalCEnumEnumFromTo :: forall a. GeneralCEnum a => a -> a -> [a]
generalCEnumEnumFromTo x z =
    case List.dropWhile (/= i) values of
      _i:rest -> case List.break (== k) rest of
        (js,  _k:_) -> x : map toGeneralCEnum js ++ [z]
        (_js, [])   -> throw $ CEnumInvalid (fromIntegral k)
      []      -> throw $ CEnumInvalid (fromIntegral i)
  where
    i, k :: GeneralCEnumZ a
    i = fromGeneralCEnum x
    k = fromGeneralCEnum z

    values :: [GeneralCEnumZ a]
    values = generalCEnumValues (Proxy :: Proxy a)

-- | Create a list of enumeration values like @[x, y .. z]@
--
-- This is an implementation of 'enumFromThenTo'.
--
-- This is a partial function.  A 'CEnumException' is thrown if a passed value
-- is invalid or if the from and then values are equal.
generalCEnumEnumFromThenTo :: forall a. GeneralCEnum a => a -> a -> a -> [a]
generalCEnumEnumFromThenTo x y z
    | i == j    = throw $ CEnumFromEqThen (fromIntegral i)
    | otherwise = case List.dropWhile (/= i) values of
        _i:rest -> case List.break (== k) rest of
          (is,  _k:_) ->
            case first ((+ 1) . length) (List.break (== j) (is++[k])) of
              (_w, []) -> throw $ CEnumInvalid (fromIntegral j)
              (w,  js) ->
                x : map (toGeneralCEnum . NonEmpty.head) (chunksOf w js)
          (_js, [])   -> throw $ CEnumInvalid (fromIntegral k)
        []      -> throw $ CEnumInvalid (fromIntegral i)
  where
    i, j, k :: GeneralCEnumZ a
    i = fromGeneralCEnum x
    j = fromGeneralCEnum y
    k = fromGeneralCEnum z

    values :: [GeneralCEnumZ a]
    values = (if i < j then id else reverse) $
      generalCEnumValues (Proxy :: Proxy a)

{-------------------------------------------------------------------------------
  Instance Deriving
-------------------------------------------------------------------------------}

-- | Type used to derive 'Bounded' and 'Enum' instances when a type has a
-- 'GeneralCEnum' instance
newtype GenCEnum a = GenCEnum a

instance GeneralCEnum a => Bounded (GenCEnum a) where
  minBound = GenCEnum generalCEnumMinBound
  maxBound = GenCEnum generalCEnumMaxBound

instance GeneralCEnum a => Enum (GenCEnum a) where
  succ     (GenCEnum x) = GenCEnum $ generalCEnumSucc x
  pred     (GenCEnum y) = GenCEnum $ generalCEnumPred y
  toEnum                = GenCEnum . generalCEnumToEnum
  fromEnum (GenCEnum x) = generalCEnumFromEnum x
  enumFrom (GenCEnum x) = GenCEnum <$> generalCEnumEnumFrom x
  enumFromThen (GenCEnum x) (GenCEnum y) =
    GenCEnum <$> generalCEnumEnumFromThen x y
  enumFromTo (GenCEnum x) (GenCEnum z) =
    GenCEnum <$> generalCEnumEnumFromTo x z
  enumFromThenTo (GenCEnum x) (GenCEnum y) (GenCEnum z) =
    GenCEnum <$> generalCEnumEnumFromThenTo x y z

{-------------------------------------------------------------------------------
  Auxiliary Functions
-------------------------------------------------------------------------------}

chunksOf :: Int -> [a] -> [NonEmpty a]
chunksOf n xs
    | n > 0     = aux xs
    | otherwise = error $ "chunksOf: n must be positive, got " ++ show n
  where
    aux :: [a] -> [NonEmpty a]
    aux xs' = case first NonEmpty.nonEmpty (List.splitAt n xs') of
      (Just ne, rest)  -> ne : aux rest
      (Nothing, _rest) -> []
