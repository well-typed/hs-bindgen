{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Internal definitions of the 'Example.Alphabet.Internal.Alphabet' data-type.
-}
module Example.Alphabet.Internal (
    -- * Data-type
    Alphabet (..),

    -- ** Constructors
    fromSymbols,
    fromSymbolsWithStateNames,

    -- ** Accessors
    alphabetStateNames,
    alphabetSymbols,
    gapIndex,
    gapSymbol,
    symbolSet,

    -- ** Subsetting
    getSubsetIndex,
    getSubsetIndices,
) where

import Control.DeepSeq (NFData)
import Control.Monad (filterM)
import Control.Monad.State.Strict
import Example.Alphabet.Gap (gapIndex)
import Data.Bifunctor (bimap)
import Data.Bits
import Data.Data
import Data.Foldable hiding (foldl1, foldr1)
import Data.Foldable qualified as F
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..))
import Data.Hashable (Hashable (..))
import Data.Hashable.Lifted (Hashable1 (..))
import Data.IntSet (IntSet)
import Data.IntSet qualified as Int
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Word
import GHC.Exts (IsList (fromList), Item)
import GHC.Generics (Generic)
import Example.Measure
import Numeric.Natural


{- |
A collection of symbols and optional corresponding state names.
-}
data Alphabet a = Alphabet
    { isSorted ∷ !Bool
    , semanticGap ∷ !Bool
    , symbolVector ∷ {-# UNPACK #-} !(Vector a)
    , stateNames ∷ [a]
    }
    deriving anyclass (NFData)
    deriving stock (Data, Functor, Generic)


type role Alphabet representational


-- Newtypes for corecing and consolidation of alphabet input processing logic
newtype AlphabetInputSingle a = ASI {toSingle ∷ a}
    deriving anyclass (NFData)
    deriving stock (Data, Eq, Generic, Ord)


type role AlphabetInputSingle representational


newtype AlphabetInputTuple a = ASNI {toTuple ∷ (a, a)}
    deriving anyclass (NFData)
    deriving stock (Data, Eq, Generic, Ord)


type role AlphabetInputTuple representational


{-
-- -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
-- -   Supporting code and data structures:
-- -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
-}

newtype UnnamedSymbol a
    = Unnamed a
    deriving anyclass (NFData)
    deriving stock (Generic)


type role UnnamedSymbol representational


newtype NamedSymbol a
    = Named (a, a)
    deriving anyclass (NFData)
    deriving stock (Generic)


type role NamedSymbol representational


class InternalClass a where
    gapSymbol' ∷ a


    isGapSymboled ∷ a → Bool


    isMissingSymboled ∷ a → Bool


instance (Eq a) ⇒ Eq (Alphabet a) where
    lhs == rhs =
        length lhs == length rhs && symbolVector lhs == symbolVector rhs


instance Eq1 Alphabet where
    liftEq f lhs rhs =
        length lhs == length rhs && liftEq f (symbolVector lhs) (symbolVector rhs)


instance Foldable Alphabet where
    {-# INLINE toList #-}
    toList = toList . symbolVector


    {-# INLINE foldMap #-}
    foldMap f = foldMap f . symbolVector


    {-# INLINE foldr #-}
    foldr f e = foldr f e . symbolVector


    {-# INLINE foldl #-}
    foldl f e = foldl f e . symbolVector


    {-# INLINE foldr1 #-}
    foldr1 f = F.foldr1 f . symbolVector


    {-# INLINE foldl1 #-}
    foldl1 f = F.foldl1 f . symbolVector


    {-# INLINE length #-}
    length = length . symbolVector


instance (Hashable a) ⇒ Hashable (Alphabet a) where
    hashWithSalt salt = hashWithSalt salt . toList


instance Hashable1 Alphabet where
    liftHashWithSalt f salt = liftHashWithSalt f salt . toList


instance HasSymbolCount (Alphabet a) where
    symbolCount = SymbolCount . toEnum . length . symbolVector


instance (Eq a, IsString a) ⇒ InternalClass (AlphabetInputSingle a) where
    gapSymbol' = ASI $ fromString "-"


    isGapSymboled = (gapSymbol' ==)


    isMissingSymboled = (ASI (fromString "?") ==)


instance (Eq a, IsString a) ⇒ InternalClass (AlphabetInputTuple a) where
    gapSymbol' = ASNI (fromString "-", fromString "-")


    isGapSymboled (ASNI (x, _)) = x == fromString "-"


    isMissingSymboled (ASNI (x, _)) = x == fromString "?"


instance (Ord a) ⇒ Ord (Alphabet a) where
    compare = comparing length `thenBy` comparing symbolVector


instance Ord1 Alphabet where
    liftCompare f =
        let lengthComparison ∷ Alphabet a → Alphabet b → Ordering
            lengthComparison x = compare (length x) . length
            liftedComparison x = liftCompare f (symbolVector x) . symbolVector
        in  lengthComparison `thenBy` liftedComparison


instance (Show a) ⇒ Show (Alphabet a) where
    show x = fold ["Alphabet: {", intercalate ", " $ show <$> toList x, "}"]


instance Show1 Alphabet where
    liftShowsPrec shwP _shwL p input =
        let showList0 ∷ (a → ShowS) → [a] → ShowS
            showList0 f list suffix =
                case list of
                    [] → "{}" <> suffix
                    x : xs →
                        let showList1 [] = '}' : suffix
                            showList1 (y : ys) = ',' : ' ' : f y (showList1 ys)
                        in  '{' : f x (showList1 xs)
        in  showParen (p > 5) $
                showString "Alphabet: "
                    . showList0 (shwP p) (toList input)


{- |
\( \mathcal{O} \left(\, \lvert\Sigma\rvert \,\right) \)

Retrieves the state names for the symbols of the 'Example.Alphabet.Internal.Alphabet'.

If the symbols of the 'Example.Alphabet.Internal.Alphabet' were not given state names during
construction then an empty list is returned.
-}
alphabetStateNames ∷ (IsList (f a), Item (f a) ~ a) ⇒ Alphabet a → f a
alphabetStateNames = fromList . toList . stateNames


{- |
\( \mathcal{O} \left(\, \lvert\Sigma\rvert \,\right) \)

Retrieves the symbols of the 'Example.Alphabet.Internal.Alphabet'. Synonym for 'toList'.
-}
{-# INLINE [1] alphabetSymbols #-}


{-# RULES "alphabetSymbols/Set" ∀ (x ∷ (Ord a) ⇒ Alphabet a). alphabetSymbols x = symbolSet x #-}


{-# RULES "alphabetSymbols/Vector" alphabetSymbols = symbolVector #-}


alphabetSymbols ∷ (IsList (f a), Item (f a) ~ a) ⇒ Alphabet a → f a
alphabetSymbols = fromList . toList


{- |
 \( \mathcal{O} \left(\, 1 \,\right) \)

Retrieves the "gap character" from the alphabet.
-}
{-# INLINE gapSymbol #-}
gapSymbol ∷ Alphabet a → Maybe a
gapSymbol input
    | semanticGap input = Just . (! fromEnum gapIndex) $ symbolVector input
    | otherwise = Nothing


{- |
\( \mathcal{O} \left(\, \lvert\Sigma\rvert \right) \,\) for a sorted alphabet.

\( \mathcal{O} \left(\, \lvert\Sigma\rvert * log_{2}\lvert\Sigma\rvert \right) \,\) for a /non-sorted/ alphabet.

Retrieves the set of all symbols from the alphabet.
-}
symbolSet ∷ (Ord a) ⇒ Alphabet a → Set a
symbolSet a = case toList $ symbolVector a of
    [] → mempty
    -- First element is 'gap'
    g : xs → Set.insert g $ f xs
    where
        f
            | isSorted a = Set.fromDistinctAscList
            | otherwise = Set.fromList


{- |
For a given subset of symbols, this function returns a positive 'Natural' number
in the range $\( \left[\; 0,\, 2^{\lvert\Sigma\rvert} - 1 \;\right]\).
This number is the unique index of the given subset in the powerset of the alphabet.
-}
{-# INLINE getSubsetIndices #-}
{-# SPECIALIZE getSubsetIndices ∷ Alphabet String → Set String → IntSet #-}
getSubsetIndices ∷ (Ord a) ⇒ Alphabet a → Set a → IntSet
getSubsetIndices a s
    | isSorted a = produceSet . go low $ consumeSet s
    | otherwise = produceSet . mo low $ consumeSet s
    where
        -- NOTE:
        -- sorted:   /O(log a + n)/, a >= n
        -- unsorted: /O(a)/

        vec = symbolVector a
        gap = gapSymbol a
        idx = fromEnum gapIndex
        low = idx + 1

        inputHadGap = isJust $ gap >>= (`Set.lookupIndex` s)
        consumeSet = Set.toAscList . maybe id Set.delete gap
        produceSet = addGapVal . Int.fromDistinctAscList

        addGapVal
            | inputHadGap = Int.insert idx
            | otherwise = id

        -- Faster binary search for a sorted alphabet
        go _ [] = []
        go !lo (x : xs) = case withinVec vec x lo of
            Right i → i : go (i + 1) xs
            Left i → go i xs

        -- Slower version for an unsorted alphabet
        mo _ [] = []
        mo i (x : xs)
            | i > length vec = []
            | x == (vec ! i) = i : mo 0 xs
            | otherwise = mo (i + 1) (x : xs)


{- |
For a given subset of symbols, this function returns a positive 'Natural' number
in the range $\( \left[\; 0,\, 2^{\lvert\Sigma\rvert} - 1 \;\right]\).
This number is the unique index of the given subset in the powerset of the alphabet.
-}
{-# INLINE getSubsetIndex #-}
{-# SPECIALIZE getSubsetIndex ∷ Alphabet String → Set String → Word → Word #-}
{-# SPECIALIZE getSubsetIndex ∷ Alphabet String → Set String → Word8 → Word8 #-}
{-# SPECIALIZE getSubsetIndex ∷ Alphabet String → Set String → Word16 → Word16 #-}
{-# SPECIALIZE getSubsetIndex ∷ Alphabet String → Set String → Word32 → Word32 #-}
{-# SPECIALIZE getSubsetIndex ∷ Alphabet String → Set String → Word64 → Word64 #-}
{-# SPECIALIZE getSubsetIndex ∷ Alphabet String → Set String → Natural → Natural #-}
getSubsetIndex ∷ (Bits b, Ord a) ⇒ Alphabet a → Set a → b → b
getSubsetIndex a s zero
    | isSorted a = addGapVal . go zero low $ consumeSet s
    | otherwise = addGapVal . mo zero low $ consumeSet s
    where
        -- NOTE:
        -- sorted:   /O(log a + n)/, a >= n
        -- unsorted: /O(a)/

        vec = symbolVector a
        gap = gapSymbol a
        idx = fromEnum gapIndex
        low = idx + 1

        consumeSet = Set.toAscList . maybe id Set.delete gap
        inputHadGap = isJust $ gap >>= (`Set.lookupIndex` s)

        addGapVal
            | inputHadGap = (`setBit` idx)
            | otherwise = id

        -- Faster binary search for a sorted alphabet
        go !bits _ [] = bits
        go bits !lo (x : xs) = case withinVec vec x lo of
            Right i → go (bits .|. bit i) (i + 1) xs
            Left i → go bits i xs

        -- Slower version for an unsorted alphabet
        mo bits _ [] = bits
        mo bits i (x : xs)
            | i > length vec = bits
            | x == (vec ! i) = mo (bits .|. bit i) low xs
            | otherwise = mo bits (i + 1) (x : xs)


{- |
\( \mathcal{O} \left(\, n * \log_{2} n \,\right) \)

Constructs an 'Example.Alphabet.Internal.Alphabet' from a 'NonEmpty' sequence of symbols which are 'IsString' values.

/NOTE:/ Upstream this is generalised over @Foldable1 t => t a@. That class lives
in @Data.Foldable1@, which only entered @base@ in 4.18 (GHC 9.6), and the
examples are built with GHC 9.4 in CI. Since every call site passes a
'NonEmpty' anyway, the signature is specialised rather than taking on a
compatibility dependency. The same applies to 'fromSymbolsWithStateNames' and
'alphabetPreprocessing'.
-}
{-# INLINE [1] fromSymbols #-}
{-# SPECIALIZE fromSymbols ∷ NonEmpty String → Alphabet String #-}
-- {-# RULES "fromSymbols/Set" forall (s :: (IsString x, Ord x) => Set x). fromSymbols s = let g = fromString "-"; x = g : toList (Set.delete g s); v = V.fromList x; in  Alphabet True v mempty #-}
fromSymbols ∷ (Ord a, IsString a) ⇒ NonEmpty a → Alphabet a
fromSymbols inputSymbols =
    let (sorted, hasGap, uniqueSymbols) = processPre inputSymbols

        processPre ∷ (Ord a, IsString a) ⇒ NonEmpty a → (Bool, Bool, NonEmpty (AlphabetInputSingle a))
        processPre = alphabetPreprocessing . fmap fromSingle

        processPost ∷ NonEmpty (AlphabetInputSingle a) → Vector a
        processPost = V.fromList . fmap toSingle . toList

        symbols = processPost uniqueSymbols
    in  Alphabet sorted hasGap symbols []


{- |
\( \mathcal{O} \left(\, n * \log_{2} n \,\right) \)

Constructs an 'Example.Alphabet.Internal.Alphabet' from a 'NonEmpty' sequence of symbols and
corresponding state names, both of which are 'IsString' values.

The input ordering is preserved.
-}
{-# SPECIALIZE fromSymbolsWithStateNames ∷ NonEmpty (String, String) → Alphabet String #-}
fromSymbolsWithStateNames ∷ (Ord a, IsString a) ⇒ NonEmpty (a, a) → Alphabet a
fromSymbolsWithStateNames inputSymbols =
    let (sorted, hasGap, uniqueSymbols) = processPre inputSymbols

        processPre ∷ (Ord a, IsString a) ⇒ NonEmpty (a, a) → (Bool, Bool, NonEmpty (AlphabetInputTuple a))
        processPre = alphabetPreprocessing . fmap fromTuple

        processPost ∷ NonEmpty (AlphabetInputTuple a) → (Vector a, [a])
        processPost = bimap V.fromList toList . unzip . fmap toTuple . toList

        (symbols, names) = processPost uniqueSymbols
    in  Alphabet sorted hasGap symbols names


{-
    where
        (symbols, names) =
            bimap V.fromList toList
                . unzip
                . fmap toTuple
                . toList
                . alphabetPreprocessing
                . fmap fromTuple
                $ inputSymbols
-}

{- |
\( \mathcal{O} \left(\, \lvert\Sigma\rvert * \log_{2} \lvert\Sigma\rvert \,\right) \)
-}
alphabetPreprocessing ∷ (Ord a, InternalClass a) ⇒ NonEmpty a → (Bool, Bool, NonEmpty a)
alphabetPreprocessing inputSymbols =
    let s :| ss = inputSymbols

        initialSeenSet = Set.singleton s
        filteredSymbols = removeSpecialSymbolsAndDuplicates ss
        uniqueSymbols = s :| filteredSymbols
        prependGapSymbol = (gapSymbol' <|)

        removeSpecialSymbolsAndDuplicates =
            let p ∷ (InternalClass a, MonadState (Set a) f, Ord a) ⇒ a → f Bool
                p x
                    | isGapSymboled x = pure False
                    | isMissingSymboled x = pure False
                    | otherwise = do
                        seenSet ← get
                        put $ x `Set.insert` seenSet
                        pure $ x `notElem` seenSet
            in  (`evalState` initialSeenSet) . filterM p

        -- Zip each element with the next element,
        -- and assert that all pairs are less-then-equal
        sorted = all (uncurry (<=)) . zip (toList uniqueSymbols) $ NE.tail uniqueSymbols

        -- Check if Gap exists in input
        hasGap = any isGapSymboled inputSymbols

        -- Ensure Gap exists in output IFF 'hasGap'
        --
        -- In the case that the gap character was the first input element,
        -- it will not have been filtered out of the unique symbol set.
        -- Therefore, don't prepend the gap if the first element was gap.
        prefixing
            | hasGap && not (isGapSymboled s) = prependGapSymbol
            | otherwise = id
    in  (sorted, hasGap, prefixing uniqueSymbols)


fromSingle ∷ a → AlphabetInputSingle a
fromSingle = ASI


fromTuple ∷ (a, a) → AlphabetInputTuple a
fromTuple = ASNI


{-# INLINE withinVec #-}
{-# SPECIALIZE withinVec ∷ Vector String → String → Int → Either Int Int #-}
-- {-# SPECIALISE withinVec :: Vector ShortText -> ShortText -> Int -> Either Int Int #-}
withinVec ∷ (Ord a) ⇒ Vector a → a → Int → Either Int Int
withinVec v e m
    | e == gap = Right idx
    | otherwise = go m $ length v - 1
    where
        idx = fromEnum gapIndex
        gap = v ! idx
        -- Perform a binary search on the unboxed vector
        -- to determine if a symbol is present.
        --
        -- Equally fast, and uses less memory than a Set.
        {-# INLINE go #-}
        go !lo !hi
            | lo > hi = Left hi
            | otherwise =
                let !md = (hi + lo) `div` 2
                    !z = v ! md
                in  case z `compare` e of
                        EQ → Right md
                        LT → go (md + 1) hi
                        GT → go lo (md - 1)


thenBy ∷ (a → b → Ordering) → (a → b → Ordering) → a → b → Ordering
thenBy prev curr x y = case prev x y of
    EQ → curr x y
    cv → cv
