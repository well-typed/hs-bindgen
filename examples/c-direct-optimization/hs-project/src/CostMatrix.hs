{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
The compact, FFI-backed transition cost matrix.

A 'TransitionMatrix' owns two C-allocated structures -- a 2D matrix over pairs
of states and a 3D matrix over triples -- together with the symbol distances
they were built from.  Both structures are fully pre-computed, so every query
is /O(1)/ pointer arithmetic.

Merged from the former @Layout.Compact.Class@, @Layout.Compact.States@,
@Layout.Compact.States.Allocation@, @Layout.Compact.States.Indexing@ and
@Layout.Compact.States.Structure@ modules.

This module contains no C layout knowledge: the @struct@ definitions and the
two allocation calls all come from the generated "DirectOptimization" modules,
imported here as @DO@.
-}
module CostMatrix (
    -- * The matrix
    TransitionMatrix (..),

    -- * Construction
    initialize,
    maximumDimension,

    -- * The C structures
    CostMatrix2D,
    CostMatrix3D,
    SymbolElement,
    TransitionCost,
    withCostMatrix2D,
    withCostMatrix3D,

    -- * Alignment strategy
    AlignmentStrategy (..),
    alignmentStrategy,

    -- * Queries
    symbolDistance,
    distance2D,
    distance3D,
    median2D,
    median3D,
    dispersion2D,
    dispersion3D,

    -- * Rendering
    renderMatrix,
    renderSummary,
) where

import Control.DeepSeq (NFData)
import Data.Bits
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Vector.Storable (Vector, (!))
import Data.Vector.Storable qualified as V
import DirectOptimization qualified as DO
import Foreign
import Foreign.C.Types (CInt)
import Foreign.Concurrent qualified as Concurrent
import GHC.Generics (Generic)
import HsBindgen.Runtime.PtrConst qualified as PtrConst
import Example.Measure
import Prelude hiding (head, last)
import System.IO.Unsafe (unsafePerformIO)


{--------------------------------------------------------------------------}
{- Types                                                                   -}
{--------------------------------------------------------------------------}

{- |
One transition cost, as the C library encodes it (@cost_t@, currently
@uint64_t@).  The width is derived from the header, so it tracks the
@WIDE_COST@ switch in @costMatrix.h@ automatically.
-}
type TransitionCost = DO.TransitionCost


{- |
One character symbol, as the C library encodes it (@elem_t@, currently
@uint32_t@).
-}
type SymbolElement = DO.SymbolElement


{- |
The C @cost_matrices_2d_t@ structure: distances and medians for every /pair/
of states.
-}
type CostMatrix2D = DO.CostMatrix2D


{- |
The C @cost_matrices_3d_t@ structure: distances and medians for every /triple/
of states.
-}
type CostMatrix3D = DO.CostMatrix3D


-- | Which alignment algorithm the matrix was built for.
data AlignmentStrategy = Linear | Affine
    deriving stock (Eq, Generic, Show)
    deriving anyclass (NFData)


instance Enum AlignmentStrategy where
    fromEnum Linear = 0
    fromEnum Affine = 3

    toEnum 3 = Affine
    toEnum _ = Linear


{- |
A pre-computed transition cost matrix, owning its C-allocated structures.

Supports the following measures:

  * Dispersion -- 'dispersion2D', 'dispersion3D'
  * Distance   -- 'symbolDistance', 'distance2D', 'distance3D'
  * Median     -- 'median2D', 'median3D'
-}
data TransitionMatrix = TransitionMatrix
    { matrix2D :: {-# UNPACK #-} (ForeignPtr CostMatrix2D)
    , matrix3D :: {-# UNPACK #-} (ForeignPtr CostMatrix3D)
    , matrixDist :: {-# UNPACK #-} Vector TransitionCost
    , matrixSize :: {-# UNPACK #-} Word
    , gapPenalty :: {-# UNPACK #-} Word
    , maxDelCost :: {-# UNPACK #-} Word
    , maxInsCost :: {-# UNPACK #-} Word
    , minDelCost :: {-# UNPACK #-} Word
    , minInsCost :: {-# UNPACK #-} Word
    }
    deriving stock (Generic)


{- |
Borrow the structure required to align /two/ sequences across the FFI.

The pointer is valid only for the duration of the continuation. Do not return
it, or anything derived from it that aliases C memory -- the matrix may be
finalized once the continuation ends.
-}
withCostMatrix2D :: TransitionMatrix -> (Ptr CostMatrix2D -> IO a) -> IO a
withCostMatrix2D = withForeignPtr . matrix2D


{- |
Borrow the structure required to align /three/ sequences across the FFI.
See 'withCostMatrix2D' for the validity rule.
-}
withCostMatrix3D :: TransitionMatrix -> (Ptr CostMatrix3D -> IO a) -> IO a
withCostMatrix3D = withForeignPtr . matrix3D


{- |
The maximum alphabet size for which a compact matrix can be built: @8@, the
bit-width of a 'Word8'.
-}
maximumDimension :: Word
maximumDimension = toEnum $ finiteBitSize (0 :: Word8)


{- |
/ϴ(1)/ Determine the alignment strategy encoded in a 2D structure.
-}
alignmentStrategy :: CostMatrix2D -> AlignmentStrategy
alignmentStrategy = toEnum . fromEnum . DO.costMatrix2D_cost_model_type


{--------------------------------------------------------------------------}
{- Construction                                                            -}
{--------------------------------------------------------------------------}

{- |
/ϴ(a⁵)/ where /a ≤ 8/ is the number of symbols in the character alphabet.

Build the 2D and 3D compact matrices from a row-major vector of symbol
transition distances.  A gap-open penalty of zero selects 'Linear' alignment.
-}
initialize
    :: forall e
     . (Integral e, Storable e)
    => SymbolCount
    -- ^ Number of symbols to allocate for
    -> SymbolDistance
    -- ^ Penalty cost to begin a gap sequence
    -> Vector e
    -- ^ Row-major symbol distances
    -> TransitionMatrix
initialize (SymbolCount dim) penalty inputVector =
    let unsafeTruncateElements :: Vector e -> Vector TransitionCost
        unsafeTruncateElements = V.map fromIntegral

        clip :: Vector TransitionCost -> Vector TransitionCost
        (clip, size) = clampSize dim
        safeVector = clip $ unsafeTruncateElements inputVector
        openPenalty = fromIntegral penalty
        dimension = fromIntegral size
        rowLen = fromEnum size
        firstRow = V.slice 1 (rowLen - 1) safeVector
        firstCol = V.generate (rowLen - 1) $ \i -> safeVector V.! ((i + 1) * rowLen)
    in  unsafePerformIO . V.unsafeWith safeVector $ \arr -> do
            cm2D <- malloc :: IO (Ptr CostMatrix2D)
            cm3D <- malloc :: IO (Ptr CostMatrix3D)
            -- The `tcm` parameter is `cost_t const *` in C, which `hs-bindgen`
            -- faithfully reflects as `PtrConst`.
            let tcm = PtrConst.unsafeFromPtr arr
            DO.setUp2dCostMtx cm2D tcm openPenalty dimension
            DO.setUp3dCostMtx cm3D tcm openPenalty dimension
            -- `freeCostMtx` releases the internal lookup tables *and* the
            -- struct itself, so it exactly undoes the `malloc` above. It takes
            -- a flag argument, so it cannot be used as a bare `FinalizerPtr`;
            -- `Foreign.Concurrent` takes a Haskell action instead.
            fp2D <- Concurrent.newForeignPtr cm2D $
                DO.freeCostMtx (castPtr cm2D) isTwoDimensional
            fp3D <- Concurrent.newForeignPtr cm3D $
                DO.freeCostMtx (castPtr cm3D) isThreeDimensional
            pure
                TransitionMatrix
                    { matrix2D = fp2D
                    , matrix3D = fp3D
                    , matrixDist = safeVector
                    , matrixSize = size
                    , gapPenalty = fromSymbolDistance penalty
                    , maxDelCost = fromIntegral $ V.maximum firstCol
                    , maxInsCost = fromIntegral $ V.maximum firstRow
                    , minDelCost = fromIntegral $ V.minimum firstCol
                    , minInsCost = fromIntegral $ V.minimum firstRow
                    }


-- | Ensure that the size does not exceed 'maximumDimension'.
clampSize :: Word -> (Vector TransitionCost -> Vector TransitionCost, Word)
clampSize n =
    let dim = min maximumDimension n

        truncateVector :: Vector TransitionCost -> Vector TransitionCost
        truncateVector =
            let x = fromEnum dim
            in  V.slice 0 $ x * x
    in  (if n <= maximumDimension then id else truncateVector, dim)


-- | The @is_2d@ flag of the C @freeCostMtx@ function; a plain @int@, not an enum.
isTwoDimensional, isThreeDimensional :: CInt
isTwoDimensional = 1
isThreeDimensional = 0


{--------------------------------------------------------------------------}
{- Queries                                                                 -}
{--------------------------------------------------------------------------}

{- |
/O(1)/ The transition cost between two symbols, addressed by alphabet index.
-}
symbolDistance :: TransitionMatrix -> Word -> Word -> Word
symbolDistance tcm i j =
    let dim = matrixSize tcm
        idx = fromEnum $ (dim * i) + j
    in  fromIntegral $ matrixDist tcm ! idx


{- |
/O(1)/ The distance between a /pair/ of states, paired with their median.
-}
{-# INLINEABLE dispersion2D #-}
dispersion2D :: (Bits b, Num c) => TransitionMatrix -> b -> b -> (c, b)
dispersion2D tcm e1 e2 = unsafePerformIO . withForeignPtr (matrix2D tcm) $ \matrixPtr -> do
    cm <- peek matrixPtr
    let dim = fromEnum $ DO.costMatrix2D_alphSize cm
        idx = indexFromBits dim
    cost <- peek . offset2D dim idx e1 e2 $ DO.costMatrix2D_cost cm
    med <- peek . offset2D dim idx e1 e2 $ DO.costMatrix2D_median cm
    pure (fromIntegral (cost :: TransitionCost), symbolToBits e1 med)


{- |
/O(1)/ The distance between a /pair/ of states.
-}
{-# INLINEABLE distance2D #-}
distance2D :: (Bits b, Num c) => TransitionMatrix -> b -> b -> c
distance2D tcm e1 e2 = unsafePerformIO . withForeignPtr (matrix2D tcm) $ \matrixPtr -> do
    cm <- peek matrixPtr
    let dim = fromEnum $ DO.costMatrix2D_alphSize cm
        idx = indexFromBits dim
    cost <- peek . offset2D dim idx e1 e2 $ DO.costMatrix2D_cost cm
    pure . fromIntegral $ (cost :: TransitionCost)


{- |
/O(1)/ The median of a /pair/ of states.
-}
{-# INLINE median2D #-}
median2D :: (Bits b) => TransitionMatrix -> b -> b -> b
median2D tcm e1 e2 = unsafePerformIO . withForeignPtr (matrix2D tcm) $ \matrixPtr -> do
    cm <- peek matrixPtr
    let dim = fromEnum $ DO.costMatrix2D_alphSize cm
        idx = indexFromBits dim
    symbolToBits e1 <$> (peek . offset2D dim idx e1 e2 $ DO.costMatrix2D_median cm)


{- |
/O(1)/ The distance between a /triple/ of states, paired with their median.
-}
{-# INLINEABLE dispersion3D #-}
dispersion3D :: (Bits b, Num c) => TransitionMatrix -> b -> b -> b -> (c, b)
dispersion3D tcm e1 e2 e3 = unsafePerformIO . withForeignPtr (matrix3D tcm) $ \matrixPtr -> do
    cm <- peek matrixPtr
    let dim = fromEnum $ DO.costMatrix3D_alphSize cm
        idx = indexFromBits dim
    cost <- peek . offset3D dim idx e1 e2 e3 $ DO.costMatrix3D_cost cm
    med <- peek . offset3D dim idx e1 e2 e3 $ DO.costMatrix3D_median cm
    pure (fromIntegral (cost :: TransitionCost), symbolToBits e1 med)


{- |
/O(1)/ The distance between a /triple/ of states.
-}
{-# INLINEABLE distance3D #-}
distance3D :: (Bits b, Num c) => TransitionMatrix -> b -> b -> b -> c
distance3D tcm e1 e2 e3 = unsafePerformIO . withForeignPtr (matrix3D tcm) $ \matrixPtr -> do
    cm <- peek matrixPtr
    let dim = fromEnum $ DO.costMatrix3D_alphSize cm
        idx = indexFromBits dim
    cost <- peek . offset3D dim idx e1 e2 e3 $ DO.costMatrix3D_cost cm
    pure . fromIntegral $ (cost :: TransitionCost)


{- |
/O(1)/ The median of a /triple/ of states.
-}
{-# INLINE median3D #-}
median3D :: (Bits b) => TransitionMatrix -> b -> b -> b -> b
median3D tcm e1 e2 e3 = unsafePerformIO . withForeignPtr (matrix3D tcm) $ \matrixPtr -> do
    cm <- peek matrixPtr
    let dim = fromEnum $ DO.costMatrix3D_alphSize cm
        idx = indexFromBits dim
    symbolToBits e1 <$> (peek . offset3D dim idx e1 e2 e3 $ DO.costMatrix3D_median cm)


{- |
Mirrors the C function @cm_calc_cost_position_2d@:

> (size_t) (a << alphSize) + b
-}
offset2D :: (Storable b) => Int -> (a -> Int) -> a -> a -> Ptr b -> Ptr b
offset2D dim f e1 e2 = flip advancePtr $ (f e1 `shiftL` dim) + f e2


-- | Mirrors the C function @cm_calc_cost_position_3d@.
offset3D :: (Storable b) => Int -> (a -> Int) -> a -> a -> a -> Ptr b -> Ptr b
offset3D dim f e1 e2 e3 =
    flip advancePtr $ (((f e1 `shiftL` dim) + f e2) `shiftL` dim) + f e3


-- | Rebuild a state of type @b@ from the C symbol encoding.
symbolToBits :: (Bits b) => b -> SymbolElement -> b
symbolToBits b v =
    let z = b `xor` b
        f a i
            | v `testBit` i = a `setBit` i
            | otherwise = a
    in  foldl' f z bitsBuildRange


-- | Address a state of type @b@ as an index into the pre-computed matrix.
indexFromBits :: (Bits b) => Int -> b -> Int
indexFromBits n b =
    let f a (i, v)
            | b `testBit` i = a + v
            | otherwise = a
    in  foldl' f 0 $ take n bitsIndexRange


bitsBuildRange :: [Int]
bitsBuildRange = [0 .. fromEnum maximumDimension - 1]


bitsIndexRange :: [(Int, Int)]
bitsIndexRange = (\x -> (x, 2 ^ x)) <$> bitsBuildRange


{--------------------------------------------------------------------------}
{- Instances                                                               -}
{--------------------------------------------------------------------------}

instance Eq TransitionMatrix where
    (==) lhs rhs =
        let n = matrixSize lhs
        in  and
                [ n == matrixSize rhs
                , gapPenalty lhs == gapPenalty rhs
                , maxDelCost lhs == maxDelCost rhs
                , maxInsCost lhs == maxInsCost rhs
                , minDelCost lhs == minDelCost rhs
                , minInsCost lhs == minInsCost rhs
                ]
                && let symbolRange = [0 .. n - 1]
                       symbolλ i j = symbolDistance lhs i j == symbolDistance rhs i j
                   in  and $ symbolλ <$> symbolRange <*> symbolRange


instance Show TransitionMatrix where
    show = (<>) <$> renderSummary <*> renderMatrix


-- | /O(1)/
instance HasEditExtrema TransitionMatrix where
    {-# INLINE maxDeletion #-}
    maxDeletion = SymbolDistance . maxDelCost

    {-# INLINE maxInsertion #-}
    maxInsertion = SymbolDistance . maxInsCost

    {-# INLINE minDeletion #-}
    minDeletion = SymbolDistance . minDelCost

    {-# INLINE minInsertion #-}
    minInsertion = SymbolDistance . minInsCost


instance HasSymbolCount TransitionMatrix where
    symbolCount = SymbolCount . matrixSize


instance HasSymbolDistances TransitionMatrix where
    symbolDistances tcm i =
        let f (SymbolIndex w) = w
        in  SymbolDistance . symbolDistance tcm (f i) . f


-- | /O(1)/
instance (Bits b) => HasStateTransitions TransitionMatrix b where
    stateTransitionPairwiseDispersion = dispersion2D
    stateTransitionPairwiseDistance = distance2D
    stateTransitionPairwiseMedian = median2D
    stateTransitionThreewayDispersion = dispersion3D
    stateTransitionThreewayDistance = distance3D
    stateTransitionThreewayMedian = median3D


{--------------------------------------------------------------------------}
{- Rendering                                                               -}
{--------------------------------------------------------------------------}

-- | Bytes in the 2D lookup table portion of the C structure.
bytesSize2D :: Int -> Int
bytesSize2D n =
    sum
        [ sizeOf (undefined :: CostMatrix2D)
        , n * n * sizeOf (undefined :: TransitionCost) -- cost
        , n * n * sizeOf (undefined :: SymbolElement) -- median
        , n * n * sizeOf (undefined :: TransitionCost) -- worst
        , n * sizeOf (undefined :: TransitionCost) -- prepend
        , n * sizeOf (undefined :: TransitionCost) -- tail
        ]


-- | Bytes in the 3D lookup table portion of the C structure.
bytesSize3D :: Int -> Int
bytesSize3D n =
    sum
        [ sizeOf (undefined :: CostMatrix3D)
        , n * n * n * sizeOf (undefined :: TransitionCost)
        , n * n * n * sizeOf (undefined :: SymbolElement)
        ]


-- | Bytes allocated for the /entire/ matrix, C structures included.
bytesSizeTotal :: (Integral i) => i -> Int
bytesSizeTotal i =
    let dimension = fromIntegral i
    in  sum
            [ 6 * sizeOf (undefined :: Word)
            , sizeOf (undefined :: Ptr CostMatrix2D)
            , sizeOf (undefined :: Ptr CostMatrix3D)
            , bytesSize2D dimension
            , bytesSize3D dimension
            ]


-- | A concise, human-readable summary of a 'TransitionMatrix'.
renderSummary :: TransitionMatrix -> String
renderSummary tcm =
    let b = bytesSizeTotal n
        g = gapPenalty tcm
        n = matrixSize tcm
    in  unlines $
            fold
                [ ["General Metric (Compacted) with"]
                , [fold ["  allocated ", show b, " bytes"]]
                , [fold ["  dimension ", show n]]
                , if g > 0
                    then [fold ["  affine penalty ", show g, "Δ"]]
                    else []
                ]


-- | The full symbol distance matrix, in a human-readable grid.
renderMatrix :: TransitionMatrix -> String
renderMatrix tcm =
    let n = matrixSize tcm

        pad :: (Show p) => p -> String
        pad v =
            let shown = show v
            in  replicate (maxVal - length shown) ' ' <> shown

        range :: NonEmpty Word
        range = 0 :| [1 .. n - 1]

        values :: NonEmpty (NonEmpty Word)
        values = do
            i <- range
            pure $ do
                j <- range
                pure $ symbolDistance tcm i j

        uncons' :: NonEmpty a -> ([a], a)
        uncons' = (,) <$> NE.init <*> NE.last

        rowHead :| rowTail = values
        (rowBody, rowLast) = case rowTail of
            [] -> ([], rowHead)
            x : xs -> uncons' $ x :| xs

        maxVal = length . show . maximum $ maximum <$> values
        endcap = \b e str -> indent $ fold [b, " ", fold . NE.intersperse " " $ pad <$> str, " ", e]
        indent = ("  " <>)
    in  unlines $
            fold
                [ endcap "┌" "┐" <$> [rowHead]
                , endcap "│" "│" <$> rowBody
                , endcap "└" "┘" <$> [rowLast]
                ]
