{-# LANGUAGE TypeFamilies #-}

module HsBindgen.Runtime.CEnum.Sequential (
    -- * Type Class
    SequentialCEnum(..)
    -- * Bounded API
  , sequentialCEnumMinBound
  , sequentialCEnumMaxBound
    -- * Enum API
  , sequentialCEnumSucc
  , sequentialCEnumPred
  , sequentialCEnumToEnum
  , sequentialCEnumFromEnum
  , sequentialCEnumEnumFrom
  , sequentialCEnumEnumFromThen
  , sequentialCEnumEnumFromTo
  , sequentialCEnumEnumFromThenTo
    -- * Instance Deriving
  , SeqCEnum(..)
  ) where

import Control.Exception (throw)
import Data.Proxy (Proxy(Proxy))

import HsBindgen.Runtime.CEnum.Exception

{-------------------------------------------------------------------------------
  Type Class
-------------------------------------------------------------------------------}

-- | C enumeration with sequential values
--
-- This class helps implement 'Bounded' and 'Enum' instances for C enumerations
-- represented using `newtype` wrappers around integral types, where the
-- enumeration values are sequential.
--
-- Instances must be defined to maintain the following invariants.
--
-- @toSequentialCEnum@ and @fromSequentialCEnum@ must be inverses of each other:
--
-- prop> toSequentialCEnum . fromSequentialCEnum === id
--
-- prop> fromSequentialCEnum . toSequentialCEnum === id
--
-- @sequentialCEnumMin@ must be less than or equal to @sequentialCEnumMax@:
--
-- prop> sequentialCEnumMin <= sequentialCEnumMax
class Integral (SequentialCEnumZ a) => SequentialCEnum a where
  -- | Wrapped integral type
  type SequentialCEnumZ a

  -- | Wrap the integral type
  toSequentialCEnum   :: SequentialCEnumZ a -> a

  -- | Unwrap the integral type
  fromSequentialCEnum :: a -> SequentialCEnumZ a

  -- | Lower bound
  sequentialCEnumMin  :: Proxy a -> SequentialCEnumZ a

  -- | Upper bound
  sequentialCEnumMax  :: Proxy a -> SequentialCEnumZ a

{-------------------------------------------------------------------------------
  Bounded API
-------------------------------------------------------------------------------}

-- | Get the lower bound
--
-- This is an implementation of 'minBound'.
sequentialCEnumMinBound :: forall a. SequentialCEnum a => a
sequentialCEnumMinBound =
    toSequentialCEnum $ sequentialCEnumMin (Proxy :: Proxy a)

-- | Get the upper bound
--
-- This is an implementation of 'maxBound'.
sequentialCEnumMaxBound :: forall a. SequentialCEnum a => a
sequentialCEnumMaxBound =
    toSequentialCEnum $ sequentialCEnumMax (Proxy :: Proxy a)

{-------------------------------------------------------------------------------
  Enum API
-------------------------------------------------------------------------------}

-- | Get the successor of a value
--
-- This is an implementation of 'succ'.
--
-- This is a partial function.  A 'CEnumException' is thrown if the passed value
-- has no successor or is invalid.
sequentialCEnumSucc :: forall a. SequentialCEnum a => a -> a
sequentialCEnumSucc x
    | i >= minValue && i < maxValue = toSequentialCEnum $ i + 1
    | i == maxValue = throw $ CEnumNoSuccessor (fromIntegral i)
    | otherwise     = throw $ CEnumInvalid     (fromIntegral i)
  where
    i :: SequentialCEnumZ a
    i = fromSequentialCEnum x

    proxy :: Proxy a
    proxy = Proxy

    minValue, maxValue :: SequentialCEnumZ a
    minValue = sequentialCEnumMin proxy
    maxValue = sequentialCEnumMax proxy

-- | Get the predecessor of a value
--
-- This is an implementation of 'pred'.
--
-- This is a partial function.  A 'CEnumException' is thrown if the passed value
-- has no precessor or is invalid.
sequentialCEnumPred :: forall a. SequentialCEnum a => a -> a
sequentialCEnumPred y
    | j > minValue && j <= maxValue = toSequentialCEnum $ j - 1
    | j == minValue = throw $ CEnumNoPredecessor (fromIntegral j)
    | otherwise     = throw $ CEnumInvalid       (fromIntegral j)
  where
    j :: SequentialCEnumZ a
    j = fromSequentialCEnum y

    proxy :: Proxy a
    proxy = Proxy

    minValue, maxValue :: SequentialCEnumZ a
    minValue = sequentialCEnumMin proxy
    maxValue = sequentialCEnumMax proxy

-- | Convert from an 'Int'
--
-- This is an implementation of 'toEnum'.
--
-- This is a partial function.  A 'CEnumException' is thrown if the passed value
-- is invalid.
sequentialCEnumToEnum :: forall a. SequentialCEnum a => Int -> a
sequentialCEnumToEnum i'
    | i >= minValue && i <= maxValue = toSequentialCEnum i
    | otherwise                      = throw $ CEnumInvalid (fromIntegral i)
  where
    i :: SequentialCEnumZ a
    i = fromIntegral i'

    proxy :: Proxy a
    proxy = Proxy

    minValue, maxValue :: SequentialCEnumZ a
    minValue = sequentialCEnumMin proxy
    maxValue = sequentialCEnumMax proxy

-- | Convert to an 'Int'
--
-- This is an implementation of 'fromEnum'.
--
-- This is a partial function.  A 'CEnumException' is thrown if the passed value
-- is invalid.
sequentialCEnumFromEnum :: forall a. SequentialCEnum a => a -> Int
sequentialCEnumFromEnum x
    | i >= minValue && i <= maxValue = fromIntegral i
    | otherwise                      = throw $ CEnumInvalid (fromIntegral i)
  where
    i :: SequentialCEnumZ a
    i = fromSequentialCEnum x

    proxy :: Proxy a
    proxy = Proxy

    minValue, maxValue :: SequentialCEnumZ a
    minValue = sequentialCEnumMin proxy
    maxValue = sequentialCEnumMax proxy

-- | Create a list of enumeration values like @[x ..]@
--
-- This is an implementation of 'enumFrom'.
--
-- This is a partial function.  A 'CEnumException' is thrown if the passed value
-- is invalid.
sequentialCEnumEnumFrom :: forall a. SequentialCEnum a => a -> [a]
sequentialCEnumEnumFrom x
    | i >= minValue && i <= maxValue = map toSequentialCEnum [i .. maxValue]
    | otherwise                      = throw $ CEnumInvalid (fromIntegral i)
  where
    i :: SequentialCEnumZ a
    i = fromSequentialCEnum x

    proxy :: Proxy a
    proxy = Proxy

    minValue, maxValue :: SequentialCEnumZ a
    minValue = sequentialCEnumMin proxy
    maxValue = sequentialCEnumMax proxy

-- | Create a list of enumeration values like @[x, y ..]@
--
-- This is an implementation of 'enumFromThen'.
--
-- This is a partial function.  A 'CEnumException' is thrown if a passed value
-- is invalid or if the from and then values are equal.
sequentialCEnumEnumFromThen :: forall a. SequentialCEnum a => a -> a -> [a]
sequentialCEnumEnumFromThen x y
    | i < minValue || i > maxValue = throw $ CEnumInvalid (fromIntegral i)
    | j < minValue || j > maxValue = throw $ CEnumInvalid (fromIntegral j)
    | i < j                        = map toSequentialCEnum [i, j .. maxValue]
    | i > j                        = map toSequentialCEnum [i, j .. minValue]
    | otherwise                    = throw $ CEnumFromEqThen (fromIntegral i)
  where
    i, j :: SequentialCEnumZ a
    i = fromSequentialCEnum x
    j = fromSequentialCEnum y

    proxy :: Proxy a
    proxy = Proxy

    minValue, maxValue :: SequentialCEnumZ a
    minValue = sequentialCEnumMin proxy
    maxValue = sequentialCEnumMax proxy

-- | Create a list of enumeration values like @[x .. z]@
--
-- This is an implementation of 'enumFromTo'.
--
-- This is a partial function.  A 'CEnumException' is thrown if a passed value
-- is invalid.
sequentialCEnumEnumFromTo :: forall a. SequentialCEnum a => a -> a -> [a]
sequentialCEnumEnumFromTo x z
    | i < minValue || i > maxValue = throw $ CEnumInvalid (fromIntegral i)
    | k < minValue || k > maxValue = throw $ CEnumInvalid (fromIntegral k)
    | otherwise                    = map toSequentialCEnum [i .. k]
  where
    i, k :: SequentialCEnumZ a
    i = fromSequentialCEnum x
    k = fromSequentialCEnum z

    proxy :: Proxy a
    proxy = Proxy

    minValue, maxValue :: SequentialCEnumZ a
    minValue = sequentialCEnumMin proxy
    maxValue = sequentialCEnumMax proxy

-- | Create a list of enumeration values like @[x, y .. z]@
--
-- This is an implementation of 'enumFromThenTo'.
--
-- This is a partial function.  A 'CEnumException' is thrown if a passed value
-- is invalid or if the from and then values are equal.
sequentialCEnumEnumFromThenTo :: forall a.
     SequentialCEnum a
  => a
  -> a
  -> a
  -> [a]
sequentialCEnumEnumFromThenTo x y z
    | i < minValue || i > maxValue = throw $ CEnumInvalid (fromIntegral i)
    | j < minValue || j > maxValue = throw $ CEnumInvalid (fromIntegral j)
    | k < minValue || k > maxValue = throw $ CEnumInvalid (fromIntegral k)
    | i == j                       = throw $ CEnumFromEqThen (fromIntegral i)
    | otherwise                    = map toSequentialCEnum [i, j .. k]
  where
    i, j, k :: SequentialCEnumZ a
    i = fromSequentialCEnum x
    j = fromSequentialCEnum y
    k = fromSequentialCEnum z

    proxy :: Proxy a
    proxy = Proxy

    minValue, maxValue :: SequentialCEnumZ a
    minValue = sequentialCEnumMin proxy
    maxValue = sequentialCEnumMax proxy

{-------------------------------------------------------------------------------
  Instance Deriving
-------------------------------------------------------------------------------}

-- | Type used to derive 'Bounded' and 'Enum' instances when a type has a
-- 'SequentialCEnum' instance
newtype SeqCEnum a = SeqCEnum a

instance SequentialCEnum a => Bounded (SeqCEnum a) where
  minBound = SeqCEnum sequentialCEnumMinBound
  maxBound = SeqCEnum sequentialCEnumMaxBound

instance SequentialCEnum a => Enum (SeqCEnum a) where
  succ     (SeqCEnum x) = SeqCEnum $ sequentialCEnumSucc x
  pred     (SeqCEnum y) = SeqCEnum $ sequentialCEnumPred y
  toEnum                = SeqCEnum . sequentialCEnumToEnum
  fromEnum (SeqCEnum x) = sequentialCEnumFromEnum x
  enumFrom (SeqCEnum x) = SeqCEnum <$> sequentialCEnumEnumFrom x
  enumFromThen (SeqCEnum x) (SeqCEnum y) =
    SeqCEnum <$> sequentialCEnumEnumFromThen x y
  enumFromTo (SeqCEnum x) (SeqCEnum z) =
    SeqCEnum <$> sequentialCEnumEnumFromTo x z
  enumFromThenTo (SeqCEnum x) (SeqCEnum y) (SeqCEnum z) =
    SeqCEnum <$> sequentialCEnumEnumFromThenTo x y z
