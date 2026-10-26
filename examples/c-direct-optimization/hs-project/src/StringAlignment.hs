{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}

{- |
Pairwise direct-optimization alignment of dynamic characters.

This is the entry point of the library: 'smallAlphabetPairwiseDO' aligns two
characters by calling the C implementation across the FFI, and
'directOptimization' is the generic driver it is built from.

The former @StringAlignment@ @.hsc@ module opened with

@
#include "c_alignment_interface.h"
#include "alignmentMatrices.h"
@

and declared its own @foreign import ccall unsafe "c_alignment_interface.h
cAlign2D"@.  Both are gone: there is no @hsc2hs@ step and no hand-written
@foreign import@ anywhere in this library.  The generated
"DirectOptimization.Unsafe" module supplies the call, imported as @DO@ so that
every use of a generated binding is visible at its call site.

Merged from the former @StringAlignment@ and @StringAlignment.Internal@
modules.
-}
module StringAlignment (
    -- * Pairwise alignment
    smallAlphabetPairwiseDO,

    -- * Generic driver
    directOptimization,
    getCostλ,

    -- * Re-exports for callers
    TransitionMatrix,
    SlimDynamicCharacter,
    AlignmentCost (..),
) where

import Data.Bits
import Data.Coerce
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Vector qualified as V
import Data.Vector.Generic (basicUnsafeSlice, (!), (!?))
import Data.Vector.Generic qualified as GV
import Data.Vector.Storable qualified as SV
import Data.Vector.Unboxed qualified as UV
import DirectOptimization qualified as DO
import Foreign
import Foreign.C.Types
import Example.Measure
import Prelude hiding (sequence, tail)
import Example.CharacterSequence
import CostMatrix
import Example.CharacterElement (HugeState, SlimState, WideState)
import System.IO.Unsafe (unsafePerformIO)


{--------------------------------------------------------------------------}
{- FFI alignment                                                           -}
{--------------------------------------------------------------------------}

-- | Specify whether or not to compute median state values.
data MedianContext = ComputeMedians | DoNotComputeMedians


-- | Specify whether or not to compute union state values.
data UnionContext = ComputeUnions | DoNotComputeUnions


instance Enum MedianContext where
    fromEnum ComputeMedians = 1
    fromEnum DoNotComputeMedians = 0

    toEnum 0 = DoNotComputeMedians
    toEnum _ = ComputeMedians


instance Enum UnionContext where
    fromEnum ComputeUnions = 1
    fromEnum DoNotComputeUnions = 0

    toEnum 0 = DoNotComputeUnions
    toEnum _ = ComputeUnions


{- |
Align two dynamic characters using an FFI call for more efficient computation
on small alphabet sizes.

Requires a 'TransitionMatrix' from a call to
'CostMatrix.initialize' defining the alphabet and transition
costs.
-}
{-# SCC smallAlphabetPairwiseDO #-}
smallAlphabetPairwiseDO
    ∷ TransitionMatrix
    -- ^ Structure defining the transition costs between character states
    → SlimDynamicCharacter
    -- ^ First  dynamic character
    → SlimDynamicCharacter
    -- ^ Second dynamic character
    → (AlignmentCost, SlimDynamicCharacter)
    -- ^ The /ungapped/ character derived from the input characters' N-W-esque
    -- matrix traceback
smallAlphabetPairwiseDO = algn2d DoNotComputeUnions ComputeMedians


{- |
Performs a naive direct optimization, returning an assignment character and the
cost of that assignment.
-}
algn2d
    ∷ UnionContext
    → MedianContext
    → TransitionMatrix
    → SlimDynamicCharacter
    → SlimDynamicCharacter
    → (AlignmentCost, SlimDynamicCharacter)
algn2d computeUnion computeMedians denseTCMs =
    directOptimization useC $ stateTransitionPairwiseDispersion denseTCMs
    where
        neverComputeOnlyGapped ∷ CInt
        neverComputeOnlyGapped = 0

        useC ∷ SV.Vector SlimState → SV.Vector SlimState → (AlignmentCost, SlimDynamicCharacter)
        useC lesser longer = {-# SCC useC #-}
            unsafePerformIO . SV.unsafeWith lesser $ \lesserPtr →
                SV.unsafeWith longer $ \longerPtr →
                    -- The borrowed struct pointer must outlive the whole call,
                    -- so the bracket wraps the entire body.
                    withCostMatrix2D denseTCMs $ \costStruct → do
                    let lesserLength = SV.length lesser
                    let longerLength = SV.length longer
                    -- Add two because the C code needs a gap prepended to each
                    -- character. Forgetting to do this will eventually corrupt
                    -- the heap memory.
                    let bufferLength = lesserLength + longerLength + 2
                    lesserVector ← initializeCharacterBuffer bufferLength lesserLength lesserPtr
                    medianVector ← initializeCharacterBuffer bufferLength 0 nullPtr
                    longerVector ← initializeCharacterBuffer bufferLength longerLength longerPtr
                    -- `alloca` rather than `malloc`: the affine branch below
                    -- throws, and an async exception can arrive at any point,
                    -- either of which would skip a manual `free`.
                    alloca $ \(resultLength ∷ Ptr CSize) → do
                        strategy ← alignmentStrategy <$> peek costStruct
                        let medianOpt = coerceEnum computeMedians
                        cost ← case strategy of
                            Affine → {-# SCC affine_unsupported #-} affineUnsupported
                            _ → {-# SCC cAlign2D #-}
                                SV.unsafeWith lesserVector $ \lesserBuffer →
                                    SV.unsafeWith medianVector $ \medianBuffer →
                                        SV.unsafeWith longerVector $ \longerBuffer →
                                            {-
                                            The only FFI call in this library's
                                            alignment path.

                                            `castPtr` is required because the C
                                            parameter is `elem_t *`, which
                                            `hs-bindgen` faithfully renders as
                                            `Ptr DO.SymbolElement` (a newtype over
                                            `Word32`), whereas the Haskell side
                                            carries `Ptr SlimState` (a newtype over
                                            `CUInt`). The two agree in
                                            representation but are distinct types;
                                            the hand-written binding simply asserted
                                            the C parameter *was* `Ptr SlimState`.
                                            -}
                                            DO.cAlign2D
                                                (castPtr lesserBuffer)
                                                (castPtr longerBuffer)
                                                (castPtr medianBuffer)
                                                resultLength
                                                (ics bufferLength)
                                                (ics lesserLength)
                                                (ics longerLength)
                                                costStruct
                                                neverComputeOnlyGapped
                                                medianOpt
                                                (coerceEnum computeUnion)

                        alignedLength ← getAlignedLength resultLength
                        let g = finalizeCharacterBuffer bufferLength alignedLength
                        --
                        -- NOTE: Extremely important implementation detail!
                        --
                        -- The C FFI swaps the results somewhere, we swap back here:
                        let alignedLesser = {-# SCC alignedLesser #-} g longerVector
                        let alignedMedian = {-# SCC alignedMedian #-} g medianVector
                        let alignedLonger = {-# SCC alignedLonger #-} g lesserVector
                        let alignmentCost = fromIntegral cost
                        let alignmentContext = (alignedLesser, alignedMedian, alignedLonger)
                        pure $ {-# SCC ffi_result #-} (alignmentCost, alignmentContext)


{- |
The affine alignment path is not wired up.

The hand-written module had a bare @undefined@ here with the intended call
commented out. The C library does provide @cAlignAffine2D@; to reach it, add
that name to the selection predicate in @generate-bindings.sh@

> --select-by-decl-name '^(cAlign2D|cAlignAffine2D|setUp2dCostMtx|setUp3dCostMtx)$'

and call @DO.cAlignAffine2D@ in this branch.
-}
affineUnsupported ∷ IO CSize
affineUnsupported =
    error "StringAlignment: affine alignment is not implemented; see 'affineUnsupported'"


{--------------------------------------------------------------------------}
{- Generic direct-optimization driver                                      -}
{--------------------------------------------------------------------------}

{-# SCC directOptimization #-}
{-# INLINEABLE directOptimization #-}
{-# SPECIALIZE directOptimization ∷
    (SV.Vector SlimState → SV.Vector SlimState → (AlignmentCost, SlimDynamicCharacter))
    → TCM2Dλ SlimState
    → SlimDynamicCharacter
    → SlimDynamicCharacter
    → (AlignmentCost, SlimDynamicCharacter)
    #-}
{-# SPECIALIZE directOptimization ∷
    (UV.Vector WideState → UV.Vector WideState → (AlignmentCost, WideDynamicCharacter))
    → TCM2Dλ WideState
    → WideDynamicCharacter
    → WideDynamicCharacter
    → (AlignmentCost, WideDynamicCharacter)
    #-}
{-# SPECIALIZE directOptimization ∷
    (V.Vector HugeState → V.Vector HugeState → (AlignmentCost, HugeDynamicCharacter))
    → TCM2Dλ HugeState
    → HugeDynamicCharacter
    → HugeDynamicCharacter
    → (AlignmentCost, HugeDynamicCharacter)
    #-}
directOptimization
    ∷ ( FiniteBits e
      , Ord (v e)
      , GV.Vector v e
      )
    ⇒ (v e → v e → (AlignmentCost, OpenDynamicCharacter v e))
    -- ^ Alignment function
    → TCM2Dλ e
    -- ^ Metric for computing state distance and median state
    → OpenDynamicCharacter v e
    → OpenDynamicCharacter v e
    → (AlignmentCost, OpenDynamicCharacter v e)
directOptimization alignmentFunction overlapλ =
    let generateAlignmentResult lhs rhs =
            let -- Build a 'gap' state now that we know that we can access a non-empty sequence.
                gap = let tmp = extractMediansGapped rhs ! 0 in (tmp `xor` tmp) `setBit` 0
                -- Remove gaps from the inputs and measure the results to determine
                -- which ungapped character is longer and which is shorter.
                -- Always pass the shorter character into alignment functions first!
                ~(swapped, gapsLesser, gapsLonger, lesser, longer) = measureCharactersWithoutGaps lhs rhs
                lesserMeds = extractMediansGapped lesser
                longerMeds = extractMediansGapped longer
                ~(alignmentCost, ungappedAlignment) =
                    if GV.length lesserMeds == 0
                        then -- Neither character was Missing, but one or both are empty when gaps are removed
                            alignmentWithAllGaps overlapλ longerMeds
                        else -- Both have some non-gap elements, perform string alignment
                            alignmentFunction lesserMeds longerMeds
                regappedAlignment = insertGaps gap gapsLesser gapsLonger ungappedAlignment

                transformation ∷ OpenDynamicCharacter v e → OpenDynamicCharacter v e
                transformation
                    | swapped = transposeCharacter
                    | otherwise = id

                alignmentContext = transformation regappedAlignment
            in  (alignmentCost, forceDynamicCharacter alignmentContext)
    in  handleMissing generateAlignmentResult


{- |
Project the cost out of a 'TCM2Dλ', discarding the median.
-}
{-# INLINEABLE getCostλ #-}
getCostλ ∷ TCM2Dλ e → e → e → Word
getCostλ =
    let normalize ∷ (StateChangeCost, e) → Word
        normalize = fromIntegral . fst
    in  (.) . (.) $ normalize


{-# SCC alignmentWithAllGaps #-}
{-# INLINEABLE alignmentWithAllGaps #-}
alignmentWithAllGaps
    ∷ ( Bits e
      , GV.Vector v e
      )
    ⇒ TCM2Dλ e
    → v e
    → (AlignmentCost, OpenDynamicCharacter v e)
alignmentWithAllGaps overlapλ character =
    case character !? 0 of
        -- Neither character was Missing, but both are empty when gaps are removed
        Nothing → (0, (GV.empty, GV.empty, GV.empty))
        -- Neither character was Missing, but one of them is empty when gaps are removed
        Just e →
            let len = GV.length character
                nil = e `xor` e
                gap = nil `setBit` 0
                zed = GV.replicate len nil
                med = GV.generate len $ snd . overlapλ gap . (character !)
            in  (0, (zed, med, character))


{-# SCC handleMissing #-}
handleMissing
    ∷ ( Bits e
      , GV.Vector v e
      )
    ⇒ (OpenDynamicCharacter v e → OpenDynamicCharacter v e → (AlignmentCost, OpenDynamicCharacter v e))
    → OpenDynamicCharacter v e
    → OpenDynamicCharacter v e
    → (AlignmentCost, OpenDynamicCharacter v e)
handleMissing f lhs rhs =
    let implicitlyAlignWithMissing
            ∷ ∀ (v ∷ Type → Type) e a b
             . (Bits e, GV.Vector v e)
            ⇒ Bool
            → (a, v e, b)
            → (AlignmentCost, OpenDynamicCharacter v e)
        implicitlyAlignWithMissing strIsLeft (_, med, _) =
            let sym = med GV.! 0
                zed = sym `xor` sym
                nil = GV.replicate (GV.length med) zed
                str
                    | strIsLeft = (med, med, nil)
                    | otherwise = (nil, med, med)
            in  (0, str)
    in  case (isMissing lhs, isMissing rhs) of
            -- Case 1: *Both* children are missing
            (True, True) → (0, lhs)
            -- Case 2: The *left* child is missing
            (True, False) → implicitlyAlignWithMissing False rhs
            -- Case 3: The *right* child is missing
            (False, True) → implicitlyAlignWithMissing True lhs
            -- Case 4: *Neither* child is missing
            (False, False) → f lhs rhs


{--------------------------------------------------------------------------}
{- Buffer marshalling                                                      -}
{--------------------------------------------------------------------------}

-- | Efficiently convert from 'Int' to 'Foreign.C.Types.CSize'.
{-# INLINE ics #-}
ics ∷ Int → CSize
ics = coerce . (toEnum ∷ Int → Word64)


{- |
Use in conjunction with 'finalizeCharacterBuffer' to marshall data across FFI.

/Should/ minimize number of, and maximize speed of copying operations.
-}
{-# SCC initializeCharacterBuffer #-}
initializeCharacterBuffer ∷ Int → Int → Ptr SlimState → IO (SV.Vector SlimState)
initializeCharacterBuffer maxSize elemCount elements =
    let e = min maxSize elemCount
        off = maxSize - e
    in  {-
        Bind the vector creation within the monadic "do-block" scope to ensure
        that no sharing of "vec" occurs between calls.

        This operation is inherently unsafe, modifying the data of the underlying
        Ptr contained within the vector. However, this permits faster marshalling
        across the FFI.
        -}
        do
            let vec = SV.replicate maxSize 0
            SV.unsafeWith vec $ \ptr →
                moveArray (advancePtr ptr off) elements e $> vec


{- |
Use in conjunction with 'initializeCharacterBuffer' to marshall data across FFI.
-}
{-# SCC finalizeCharacterBuffer #-}
finalizeCharacterBuffer ∷ Int → Int → SV.Vector SlimState → SV.Vector SlimState
finalizeCharacterBuffer bufferLength alignedLength =
    let e = min bufferLength alignedLength
        off = bufferLength - e
    in  basicUnsafeSlice off e


{- |
Read the length of the resulting alignment.

The caller owns the pointer -- it comes from 'alloca' -- so this does not free
it.
-}
getAlignedLength ∷ Ptr CSize → IO Int
getAlignedLength lenRef =
    let f = coerce ∷ CSize → Word64
    in  fromEnum . f <$> peek lenRef


-- | Coercing one 'Enum' to another through their corresponding 'Int' values.
{-# INLINE coerceEnum #-}
{-# SPECIALIZE coerceEnum ∷ Int → CSize #-}
{-# SPECIALIZE coerceEnum ∷ Int → SlimState #-}
{-# SPECIALIZE coerceEnum ∷ SlimState → Int #-}
coerceEnum ∷ (Enum a, Enum b) ⇒ a → b
coerceEnum = toEnum . fromEnum
