{-# LANGUAGE Trustworthy #-}

{- |
Units of measure for alignment, and the classes that expose them.

Merged from the former @Example.Measure.Transition@ and @Measure.Unit.*@ modules.

/NOTE:/ These are minimal stand-ins for a @Example.Measure.*@ package which is not
vendored in this repository; they exist so the API compiles and can be
exercised standalone.  The class signatures are reconstructed from their
use-sites.  When integrating with the real codebase, delete this module and
depend on the genuine package.
-}
module Example.Measure (
    -- * Units
    AlignmentCost (..),
    StateChangeCost (..),
    SymbolCount (..),
    SymbolDistance (..),
    SymbolIndex (..),

    -- ** Unit projections
    atSymbolIndex,
    fromSymbolDistance,

    -- * Example.Measure signatures
    TCM2Dλ,
    TCM3Dλ,

    -- * Classes
    HasSymbolCount (..),
    HasEditExtrema (..),
    HasSymbolDistances (..),
    HasStateTransitions (..),
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)


{--------------------------------------------------------------------------}
{- Units                                                                   -}
{--------------------------------------------------------------------------}

-- | The total cost of aligning two (or more) dynamic characters.
newtype AlignmentCost = AlignmentCost Word
    deriving stock (Eq, Generic, Ord, Show)
    deriving newtype (Enum, Integral, Num, Real)
    deriving anyclass (NFData)


-- | The cost of transitioning between two character /states/.
newtype StateChangeCost = StateChangeCost Word
    deriving stock (Eq, Generic, Ord, Show)
    deriving newtype (Enum, Integral, Num, Real)
    deriving anyclass (NFData)


-- | The number of symbols in a character alphabet.
newtype SymbolCount = SymbolCount Word
    deriving stock (Eq, Generic, Ord, Show)
    deriving newtype (Enum, Integral, Num, Real)
    deriving anyclass (NFData)


-- | The distance (cost) of transitioning between two /symbols/.
newtype SymbolDistance = SymbolDistance Word
    deriving stock (Eq, Generic, Ord, Show)
    deriving newtype (Enum, Integral, Num, Real)
    deriving anyclass (NFData)


-- | The index of a symbol within a character alphabet.
newtype SymbolIndex = SymbolIndex Word
    deriving stock (Eq, Generic, Ord, Show)
    deriving newtype (Enum, Integral, Num, Real)
    deriving anyclass (NFData)


-- | Project a 'SymbolIndex' to an 'Int' suitable for bit addressing.
atSymbolIndex :: SymbolIndex -> Int
atSymbolIndex (SymbolIndex w) = fromEnum w


-- | Project a 'SymbolDistance' to its underlying magnitude.
fromSymbolDistance :: SymbolDistance -> Word
fromSymbolDistance (SymbolDistance w) = w


{--------------------------------------------------------------------------}
{- Example.Measure signatures                                                      -}
{--------------------------------------------------------------------------}

{- |
A "2D transition cost matrix lambda": given two character states, yield the
cost of reconciling them together with the resulting median state.

This is the function threaded through the alignment code; a
'CostMatrix.TransitionMatrix' supplies one via
'stateTransitionPairwiseDispersion'.
-}
type TCM2Dλ e = e -> e -> (StateChangeCost, e)


-- | As 'TCM2Dλ', but reconciling /three/ states.
type TCM3Dλ e = e -> e -> e -> (StateChangeCost, e)


{--------------------------------------------------------------------------}
{- Classes                                                                 -}
{--------------------------------------------------------------------------}

{- |
Structures which know how many symbols are in their character alphabet.
-}
class HasSymbolCount a where
    symbolCount :: a -> SymbolCount


{- |
Structures which know the extremal costs of inserting and deleting a symbol.
-}
class HasEditExtrema a where
    maxDeletion :: a -> SymbolDistance
    maxInsertion :: a -> SymbolDistance
    minDeletion :: a -> SymbolDistance
    minInsertion :: a -> SymbolDistance


{- |
Structures which can report the transition distance between two symbols,
addressed by their alphabet index.
-}
class HasSymbolDistances a where
    symbolDistances :: a -> SymbolIndex -> SymbolIndex -> SymbolDistance


{- |
Structures which can measure transitions between /states/ encoded as @b@.
-}
class HasStateTransitions a b where
    stateTransitionPairwiseDispersion :: a -> TCM2Dλ b
    stateTransitionPairwiseDistance :: a -> b -> b -> Word
    stateTransitionPairwiseMedian :: a -> b -> b -> b
    stateTransitionThreewayDispersion :: a -> TCM3Dλ b
    stateTransitionThreewayDistance :: a -> b -> b -> b -> Word
    stateTransitionThreewayMedian :: a -> b -> b -> b -> b
