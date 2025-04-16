{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | C enumerations
--
-- This module is intended to be imported qualified.
module HsBindgen.Runtime.CEnum (
    -- * Type class
    CEnum(..)
    -- * API
  , isDeclared
  , mkDeclared
  , getNames
    -- * Show instance support
  , showCEnum
    -- * Deriving via support
  , AsCEnum(..)
    -- ** Exceptions
  , CEnumException(..)
  ) where

import Control.Exception (Exception(displayException), throw)
import Data.Bifunctor (Bifunctor(first))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy(Proxy))

{-------------------------------------------------------------------------------
  Type class
-------------------------------------------------------------------------------}

-- | C enumeration
--
-- This class implements an API for Haskell representations of C enumerations.
class Integral (CEnumZ a) => CEnum a where
  -- | Integral representation type
  type CEnumZ a

  -- | Construct a value from the integral representation
  --
  -- prop> toCEnumZ . fromCEnumZ === id
  fromCEnumZ :: CEnumZ a -> a

  -- | Get the integral representation for a value
  --
  -- prop> fromCEnumZ . toCEnumZ === id
  toCEnumZ :: a -> CEnumZ a

  -- | Declared values and associated names
  declaredValues :: proxy a -> Map (CEnumZ a) (NonEmpty String)

  -- | Lower and upper bounds when declared values are sequential
  --
  -- This is an optimization.  When set, the first value must be be the minimum
  -- key in 'declaredValues' and the second value must be the maximum key in
  -- 'declaredValues'.
  rangeIsSequential :: proxy a -> Maybe (CEnumZ a, CEnumZ a)

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Determine if the specified value is declared
isDeclared :: forall a. CEnum a => a -> Bool
isDeclared x = case rangeIsSequential (Proxy :: Proxy a) of
    Just (minZ, maxZ) -> i >= minZ && i <= maxZ
    Nothing -> i `Map.member` declaredValues (Proxy :: Proxy a)
  where
    i :: CEnumZ a
    i = toCEnumZ x

-- | Construct a value only if it is declared
mkDeclared :: forall a. CEnum a => CEnumZ a -> Maybe a
mkDeclared i = case rangeIsSequential (Proxy :: Proxy a) of
    Just (minZ, maxZ)
      | i >= minZ && i <= maxZ -> Just (fromCEnumZ i)
      | otherwise -> Nothing
    Nothing
      | i `Map.member` declaredValues (Proxy :: Proxy a) ->
          Just (fromCEnumZ i)
      | otherwise -> Nothing

-- | Get all names associated with a value
--
-- An empty list is returned when the specified value is not declared.
getNames :: forall a. CEnum a => a -> [String]
getNames x = maybe [] NonEmpty.toList $
    Map.lookup (toCEnumZ x) (declaredValues (Proxy :: Proxy a))

{-------------------------------------------------------------------------------
  Show instance support
-------------------------------------------------------------------------------}

-- | Show the specified value
--
-- This function may be used in the definition of a 'Show' instance for a
-- @newtype@ representation of a C enumeration.
--
-- When the value is declared, a corresponding name is returned.  Otherwise,
-- a string consisting of the constructor name and the integral value separated
-- by a space is returned.  Examples for a hypothetical enumeration type:
--
-- > showName "StatusCode" StatusOK == "StatusOK"
--
-- > showName "StatusCode" (StatusCode 418) == "StatusCode 418"
showCEnum :: forall a. CEnum a => String -> a -> String
showCEnum constructorName x =
    case Map.lookup i (declaredValues (Proxy :: Proxy a)) of
      Just (name :| _names) -> name
      Nothing -> constructorName ++ ' ' : show (toInteger i)
  where
    i :: CEnumZ a
    i = toCEnumZ x

{-------------------------------------------------------------------------------
  Deriving via support
-------------------------------------------------------------------------------}

-- | Type used to derive classes using @DerivingVia@
--
-- The following classes may be derived:
--
-- * 'Bounded' may be derived using the bounds of the declared values.  This is
--   /not/ derived by default.
-- * 'Enum` may be derived using the bounds of the declared values.  This
--   instance assumes that only the declared values are valid and throws a
--   'CEnumException' if passed a value that is not declared.  This is /not/
--   derived by default.
newtype AsCEnum a = AsCEnum a

instance CEnum a => Bounded (AsCEnum a) where
  minBound = AsCEnum minBound'
  maxBound = AsCEnum maxBound'

instance CEnum a => Enum (AsCEnum a) where
  succ (AsCEnum x)                     = AsCEnum $ succ' x
  pred (AsCEnum y)                     = AsCEnum $ pred' y
  toEnum                               = AsCEnum . toEnum'
  fromEnum (AsCEnum x)                 = fromEnum' x
  enumFrom (AsCEnum x)                 = AsCEnum <$> enumFrom' x
  enumFromThen (AsCEnum x) (AsCEnum y) = AsCEnum <$> enumFromThen' x y
  enumFromTo (AsCEnum x) (AsCEnum z)   = AsCEnum <$> enumFromTo' x z
  enumFromThenTo (AsCEnum x) (AsCEnum y) (AsCEnum z) =
    AsCEnum <$> enumFromThenTo' x y z

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- Exceptions used by optional C enumeration instances
data CEnumException
  = CEnumNotDeclared Integer
  | CEnumNoSuccessor Integer
  | CEnumNoPredecessor Integer
  | CEnumEmpty
  | CEnumFromEqThen Integer
  deriving (Eq, Show)

instance Exception CEnumException where
  displayException = \case
    CEnumNotDeclared i -> "C enumeration value not declared: " ++ show i
    CEnumNoSuccessor i ->
      "C enumeration value has no declared successor: " ++ show i
    CEnumNoPredecessor i ->
      "C enumeration value has no declared predecessor: " ++ show i
    CEnumEmpty -> "C enumeration has no declared values"
    CEnumFromEqThen i -> "enumeration from and then values equal: " ++ show i

{-------------------------------------------------------------------------------
  Bounded instance implementation
-------------------------------------------------------------------------------}

minBound' :: forall a. CEnum a => a
minBound' = case rangeIsSequential (Proxy :: Proxy a) of
    Just (minZ, _maxZ) -> fromCEnumZ minZ
    Nothing -> case Map.lookupMin (declaredValues (Proxy :: Proxy a)) of
      Just (i, _names) -> fromCEnumZ i
      Nothing -> throw CEnumEmpty

maxBound' :: forall a. CEnum a => a
maxBound' = case rangeIsSequential (Proxy :: Proxy a) of
    Just (_minZ, maxZ) -> fromCEnumZ maxZ
    Nothing -> case Map.lookupMax (declaredValues (Proxy :: Proxy a)) of
      Just (k, _names) -> fromCEnumZ k
      Nothing -> throw CEnumEmpty

{-------------------------------------------------------------------------------
  Enum instance implementation
-------------------------------------------------------------------------------}

succ' :: forall a. CEnum a => a -> a
succ' x = case rangeIsSequential (Proxy :: Proxy a) of
    Just (minZ, maxZ)
      | i >= minZ && i < maxZ -> fromCEnumZ (i + 1)
      | i == maxZ -> throw $ CEnumNoSuccessor (toInteger i)
      | otherwise -> throw $ CEnumNotDeclared (toInteger i)
    Nothing -> either (throw . CEnumNotDeclared) id $ do
      (_ltMap, gtMap) <- splitMap i (declaredValues (Proxy :: Proxy a))
      case Map.lookupMin gtMap of
        Just (j, _names) -> return $ fromCEnumZ j
        Nothing -> throw $ CEnumNoSuccessor (toInteger i)
  where
    i :: CEnumZ a
    i = toCEnumZ x

pred' :: forall a. CEnum a => a -> a
pred' y = case rangeIsSequential (Proxy :: Proxy a) of
    Just (minZ, maxZ)
      | j > minZ && j <= maxZ -> fromCEnumZ (j - 1)
      | j == minZ -> throw $ CEnumNoPredecessor (toInteger j)
      | otherwise -> throw $ CEnumNotDeclared (toInteger j)
    Nothing -> either (throw . CEnumNotDeclared) id $ do
      (ltMap, _gtMap) <- splitMap j (declaredValues (Proxy :: Proxy a))
      case Map.lookupMax ltMap of
        Just (i, _names) -> return $ fromCEnumZ i
        Nothing -> throw $ CEnumNoPredecessor (toInteger j)
  where
    j :: CEnumZ a
    j = toCEnumZ y

toEnum' :: CEnum a => Int -> a
toEnum' i = case mkDeclared (fromIntegral i) of
    Just x  -> x
    Nothing -> throw $ CEnumNotDeclared (toInteger i)

fromEnum' :: forall a. CEnum a => a -> Int
fromEnum' x = case rangeIsSequential (Proxy :: Proxy a) of
    Just (minZ, maxZ)
      | i >= minZ && i <= maxZ -> fromIntegral i
      | otherwise -> throw $ CEnumNotDeclared (toInteger i)
    Nothing
      | i `Map.member` declaredValues (Proxy :: Proxy a) -> fromIntegral i
      | otherwise -> throw $ CEnumNotDeclared (toInteger i)
  where
    i :: CEnumZ a
    i = toCEnumZ x

enumFrom' :: forall a. CEnum a => a -> [a]
enumFrom' x = case rangeIsSequential (Proxy :: Proxy a) of
    Just (minZ, maxZ)
      | i >= minZ && i <= maxZ -> map fromCEnumZ [i .. maxZ]
      | otherwise -> throw $ CEnumNotDeclared (toInteger i)
    Nothing -> either (throw . CEnumNotDeclared) id $ do
      (_ltMap, gtMap) <- splitMap i (declaredValues (Proxy :: Proxy a))
      return $ x : map fromCEnumZ (Map.keys gtMap)
  where
    i :: CEnumZ a
    i = toCEnumZ x

enumFromThen' :: forall a. CEnum a => a -> a -> [a]
enumFromThen' x y = case compare i j of
    LT -> case rangeIsSequential (Proxy :: Proxy a) of
      Just (minZ, maxZ)
        | i < minZ || i > maxZ -> throw $ CEnumNotDeclared (toInteger i)
        | j < minZ || j > maxZ -> throw $ CEnumNotDeclared (toInteger j)
        | otherwise -> map fromCEnumZ [i, j .. maxZ]
      Nothing -> either (throw . CEnumNotDeclared) id $ do
        (_ltIMap, gtIMap) <- splitMap i (declaredValues (Proxy :: Proxy a))
        (ltJMap,  gtJMap) <- splitMap j gtIMap
        let w  = Map.size ltJMap + 1
            js = j : Map.keys gtJMap
        return $ x : map (fromCEnumZ . NonEmpty.head) (nonEmptyChunksOf w js)
    GT -> case rangeIsSequential (Proxy :: Proxy a) of
      Just (minZ, maxZ)
        | i < minZ || i > maxZ -> throw $ CEnumNotDeclared (toInteger i)
        | j < minZ || j > maxZ -> throw $ CEnumNotDeclared (toInteger j)
        | otherwise -> map fromCEnumZ [i, j .. minZ]
      Nothing -> either (throw . CEnumNotDeclared) id $ do
        (ltIMap, _gtIMap) <- splitMap i (declaredValues (Proxy :: Proxy a))
        (ltJMap, gtJMap)  <- splitMap j ltIMap
        let w  = Map.size gtJMap + 1
            js = j : reverse (Map.keys ltJMap)
        return $ x : map (fromCEnumZ . NonEmpty.head) (nonEmptyChunksOf w js)
    -- consistently prioritized over CEnumNotDeclared errors
    EQ -> throw $ CEnumFromEqThen (toInteger i)
  where
    i, j :: CEnumZ a
    i = toCEnumZ x
    j = toCEnumZ y

enumFromTo' :: forall a. CEnum a => a -> a -> [a]
enumFromTo' x z = case rangeIsSequential (Proxy :: Proxy a) of
    Just (minZ, maxZ)
      | i < minZ || i > maxZ -> throw $ CEnumNotDeclared (toInteger i)
      | k < minZ || k > maxZ -> throw $ CEnumNotDeclared (toInteger k)
      | otherwise -> map fromCEnumZ [i .. k]
    Nothing -> either (throw . CEnumNotDeclared) id $ do
      (_ltIMap, gtIMap)  <- splitMap i (declaredValues (Proxy :: Proxy a))
      (ltKMap,  _gtKMap) <- splitMap k gtIMap
      return $ x : map fromCEnumZ (Map.keys ltKMap) ++ [z]
  where
    i, k :: CEnumZ a
    i = toCEnumZ x
    k = toCEnumZ z

enumFromThenTo' :: forall a. CEnum a => a -> a -> a -> [a]
enumFromThenTo' x y z = case compare i j of
    LT -> case rangeIsSequential (Proxy :: Proxy a) of
      Just (minZ, maxZ)
        | i < minZ || i > maxZ -> throw $ CEnumNotDeclared (toInteger i)
        | j < minZ || j > maxZ -> throw $ CEnumNotDeclared (toInteger j)
        | k < minZ || k > maxZ -> throw $ CEnumNotDeclared (toInteger k)
        | otherwise -> map fromCEnumZ [i, j .. k]
      Nothing -> either (throw . CEnumNotDeclared) id $ do
        (_ltIMap, gtIMap)  <- splitMap i (declaredValues (Proxy :: Proxy a))
        (ltJMap,  gtJMap)  <- splitMap j gtIMap
        (ltKMap,  _gtKMap) <- splitMap k gtJMap
        let w  = Map.size ltJMap + 1
            js = j : Map.keys ltKMap ++ [k]
        return $ x : map (fromCEnumZ . NonEmpty.head) (nonEmptyChunksOf w js)
    GT -> case rangeIsSequential (Proxy :: Proxy a) of
      Just (minZ, maxZ)
        | i < minZ || i > maxZ -> throw $ CEnumNotDeclared (toInteger i)
        | j < minZ || j > maxZ -> throw $ CEnumNotDeclared (toInteger j)
        | k < minZ || k > maxZ -> throw $ CEnumNotDeclared (toInteger k)
        | otherwise -> map fromCEnumZ [i, j .. k]
      Nothing -> either (throw . CEnumNotDeclared) id $ do
        (ltIMap,  _gtIMap) <- splitMap i (declaredValues (Proxy :: Proxy a))
        (ltJMap,  gtJMap)  <- splitMap j ltIMap
        (_ltKMap, gtKMap)  <- splitMap k ltJMap
        let w  = Map.size gtJMap + 1
            js = j : reverse (k : Map.keys gtKMap)
        return $ x : map (fromCEnumZ . NonEmpty.head) (nonEmptyChunksOf w js)
    -- consistently prioritized over CEnumNotDeclared errors
    EQ -> throw $ CEnumFromEqThen (toInteger i)
  where
    i, j, k :: CEnumZ a
    i = toCEnumZ x
    j = toCEnumZ y
    k = toCEnumZ z

{-------------------------------------------------------------------------------
  Auxiliary Functions
-------------------------------------------------------------------------------}

nonEmptyChunksOf :: Int -> [a] -> [NonEmpty a]
nonEmptyChunksOf n xs
    | n > 0     = aux xs
    | otherwise = error $ "nonEmptyChunksOf: n must be positive, got " ++ show n
  where
    aux :: [a] -> [NonEmpty a]
    aux xs' = case first NonEmpty.nonEmpty (List.splitAt n xs') of
      (Just ne, rest)  -> ne : aux rest
      (Nothing, _rest) -> []

splitMap :: Integral k => k -> Map k v -> Either Integer (Map k v, Map k v)
splitMap n m = case Map.splitLookup n m of
    (ltMap,  Just{},  gtMap)  -> Right (ltMap, gtMap)
    (_ltMap, Nothing, _gtMap) -> Left (toInteger n)
