{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE UnboxedTuples #-}
{- |
Dynamic characters: variable-length sequences of ambiguous states.

A dynamic character is a triple of equal-length vectors -- a "left" character,
a median, and a "right" character -- so that one value can carry both the
inputs and the result of an alignment.  See 'OpenDynamicCharacter'.

Merged from the former @Bio.DynamicCharacter@,
@Bio.DynamicCharacter.HandleGaps@, @Bio.DynamicCharacter.Example.Measure@ and
@Bio.DynamicCharacter.Rendering@ modules.
-}
module Example.CharacterSequence (
    -- * Generic constructions
    OpenDynamicCharacter,
    TempOpenDynamicCharacter,

    -- * Immutable varieties
    SlimDynamicCharacter,
    WideDynamicCharacter,
    HugeDynamicCharacter,

    -- * Mutable varieties
    TempSlimDynamicCharacter,
    TempWideDynamicCharacter,
    TempHugeDynamicCharacter,

    -- * Construction
    encodeDynamicCharacter,
    generateCharacter,
    newTempCharacter,
    freezeTempCharacter,
    unsafeCharacterBuiltByST,
    unsafeCharacterBuiltByBufferedST,

    -- * Accessors
    extractMedians,
    extractMediansSingle,
    extractMediansLeft,
    extractMediansRight,
    extractMediansGapped,
    extractMediansLeftGapped,
    extractMediansRightGapped,
    removeGapAndNil,

    -- * Mutators
    setAlign,
    setDelete,
    setInsert,
    setGapped,
    setFrom,
    transposeCharacter,

    -- * Queries
    AlignmentContext (..),
    getContext,
    isAlign,
    isDelete,
    isInsert,
    isGapped,
    isGap,
    isMissing,
    characterLength,

    -- * Gap handling
    GapSet (),
    nullGapSet,
    deleteGaps,
    insertGaps,

    -- * Length comparison
    measureCharacters,
    measureCharactersWithoutGaps,

    -- * Rendering
    renderDynamicCharacter,
    renderSlimCharacter,

    -- * Strictness
    forceDynamicCharacter,
) where

import Control.Monad (unless, when)
import Control.Monad.Loops (whileM_)
import Control.Monad.Primitive
import Control.Monad.ST
import Example.Alphabet
import Example.Alphabet.Codec
import Data.Bits
import Data.Foldable
import Data.IntMap.Strict (IntMap, fromDistinctAscList, lookupMax, toAscList)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Ord
import Data.STRef
import Data.Semigroup
import Data.Set (Set)
import Data.Vector qualified as RV
import Data.Vector.Generic (Mutable, Vector, basicLength, unsafeFreeze, (!))
import Data.Vector.Generic qualified as GV
import Data.Vector.Generic.Mutable (MVector, unsafeNew, unsafeRead, unsafeWrite)
import Data.Vector.Generic.Mutable qualified as GMV (length)
import Data.Vector.Generic.Mutable qualified as MGV
import Data.Vector.Storable qualified as SV
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as MUV
import Example.Measure
import Example.CharacterElement


{--------------------------------------------------------------------------}
{- Representation, construction, access and mutation                       -}
{--------------------------------------------------------------------------}

{- |
This triple of vectors is the general structure of all dynamic character
representations. Each vector is equal length.

A triple of empty vectors represents a missing character. This can be queried
by 'isMissing'.

All triples are arranged thusly:

  * 1: "Left" character. When normalized, this will be the shorter character.
       Delete events leave a void in this character.

  * 2: "Median" character. The alignment of both the Left and Right characters.
       Align, Delete, & Insert do *not* leave a void. This vector will always
       contain states with one or more bits set, i.e. never contain voids.

  * 3: "Right" character. When normalized, this will be the longer character.
       Insert events leave a void in this character.
-}
type OpenDynamicCharacter ∷ (k → Type) → k → Type
type OpenDynamicCharacter v e = (v e, v e, v e)


{- |
Encoding for dynamic characters with an alphabet size of /8/ or less.
-}
type SlimDynamicCharacter = OpenDynamicCharacter SV.Vector SlimState


{- |
Encoding for dynamic characters with an alphabet size in the range [9, 64].
-}
type WideDynamicCharacter = OpenDynamicCharacter UV.Vector WideState


{- |
Encoding for dynamic characters with an alphabet size of /65/ or greater.
-}
type HugeDynamicCharacter = OpenDynamicCharacter RV.Vector HugeState


{- |
Generic representation of a /mutable/ dynamic character.
-}
type TempOpenDynamicCharacter m v e = OpenDynamicCharacter (Mutable v (PrimState m)) e


{- |
Mutable encoding of 'SlimDynamicCharacter'.
-}
type TempSlimDynamicCharacter m = TempOpenDynamicCharacter m SV.Vector SlimState


{- |
Mutable encoding of 'WideDynamicCharacter'.
-}
type TempWideDynamicCharacter m = TempOpenDynamicCharacter m UV.Vector WideState


{- |
Mutable encoding of 'HugeDynamicCharacter'.
-}
type TempHugeDynamicCharacter m = TempOpenDynamicCharacter m RV.Vector HugeState


{- |
The "alignemnt context" of an 'OpenDynamicCharacter' element extracted from an index of the sequence.
-}
data AlignmentContext e
    = Gapped
    | Delete e e
    | Insert e e
    | Merged e e e
    deriving stock (Eq, Ord)


type role AlignmentContext representational


{- |
Get the 'AlignmentContext' for the given index of the 'OpenDynamicCharacter'.
-}
getContext
    ∷ ( FiniteBits e
      , Vector v e
      )
    ⇒ OpenDynamicCharacter v e
    → Int
    -- ^ Index to get
    → AlignmentContext e
getContext c@(lc, mc, rc) i
    | c `isAlign` i = Merged (lc ! i) (mc ! i) (rc ! i)
    | c `isDelete` i = Delete (mc ! i) (rc ! i)
    | c `isInsert` i = Insert (lc ! i) (mc ! i)
    | otherwise = Gapped


isAlign, isDelete, isInsert, isGapped ∷ (FiniteBits e, Vector v e) ⇒ OpenDynamicCharacter v e → Int → Bool
{-# INLINEABLE isAlign #-}
{-# SPECIALIZE isAlign ∷ SlimDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isAlign ∷ WideDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isAlign ∷ HugeDynamicCharacter → Int → Bool #-}
{-# INLINEABLE isDelete #-}
{-# SPECIALIZE isDelete ∷ SlimDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isDelete ∷ WideDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isDelete ∷ HugeDynamicCharacter → Int → Bool #-}
{-# INLINEABLE isInsert #-}
{-# SPECIALIZE isInsert ∷ SlimDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isInsert ∷ WideDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isInsert ∷ HugeDynamicCharacter → Int → Bool #-}
{-# INLINEABLE isGapped #-}
{-# SPECIALIZE isGapped ∷ SlimDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isGapped ∷ WideDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isGapped ∷ HugeDynamicCharacter → Int → Bool #-}
isAlign (lc, _, rc) i = i < GV.length lc && popCount (lc ! i) /= 0 && popCount (rc ! i) /= 0
isDelete (lc, _, rc) i = i < GV.length lc && popCount (lc ! i) == 0 && popCount (rc ! i) /= 0
isInsert (lc, _, rc) i = i < GV.length lc && popCount (lc ! i) /= 0 && popCount (rc ! i) == 0
isGapped (lc, _, rc) i = i < GV.length lc && popCount (lc ! i) == 0 && popCount (rc ! i) == 0


{-# INLINEABLE isGap #-}
{-# SPECIALIZE isGap ∷ SlimDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isGap ∷ WideDynamicCharacter → Int → Bool #-}
{-# SPECIALIZE isGap ∷ HugeDynamicCharacter → Int → Bool #-}
isGap ∷ (Bits e, Vector v e) ⇒ OpenDynamicCharacter v e → Int → Bool
isGap (_, mc, _) i =
    i < GV.length mc
        && let val = mc ! i
               gap = buildGap val
           in  gap == val


{-# INLINEABLE isMissing #-}
{-# SPECIALIZE isMissing ∷ SlimDynamicCharacter → Bool #-}
{-# SPECIALIZE isMissing ∷ WideDynamicCharacter → Bool #-}
{-# SPECIALIZE isMissing ∷ HugeDynamicCharacter → Bool #-}
isMissing ∷ (Vector v e) ⇒ OpenDynamicCharacter v e → Bool
isMissing (x, y, z) = GV.length x == 0 && GV.length y == 0 && GV.length z == 0


{-# INLINEABLE setFrom #-}
{-# SPECIALIZE setFrom ∷ SlimDynamicCharacter → TempSlimDynamicCharacter (ST s) → Int → Int → ST s () #-}
{-# SPECIALIZE setFrom ∷ WideDynamicCharacter → TempWideDynamicCharacter (ST s) → Int → Int → ST s () #-}
{-# SPECIALIZE setFrom ∷ HugeDynamicCharacter → TempHugeDynamicCharacter (ST s) → Int → Int → ST s () #-}
setFrom
    ∷ ( PrimMonad m
      , Vector v e
      )
    ⇒ OpenDynamicCharacter v e
    -- ^ source
    → TempOpenDynamicCharacter m v e
    -- ^ destination
    → Int
    -- ^ Index to read from source
    → Int
    -- ^ Index to write to destination
    → m ()
setFrom (slc, smc, src) (dlc, dmc, drc) i j =
    unsafeWrite dlc j (slc ! i) *> unsafeWrite dmc j (smc ! i) *> unsafeWrite drc j (src ! i)


{-# INLINEABLE setAlign #-}
{-# SPECIALIZE setAlign ∷ TempSlimDynamicCharacter (ST s) → Int → SlimState → SlimState → SlimState → ST s () #-}
{-# SPECIALIZE setAlign ∷ TempWideDynamicCharacter (ST s) → Int → WideState → WideState → WideState → ST s () #-}
{-# SPECIALIZE setAlign ∷ TempHugeDynamicCharacter (ST s) → Int → HugeState → HugeState → HugeState → ST s () #-}
setAlign
    ∷ ( PrimMonad m
      , Vector v e
      )
    ⇒ TempOpenDynamicCharacter m v e
    → Int
    -- ^ Index to set
    → e
    -- ^ Aligned "left" element
    → e
    -- ^ Median element
    → e
    -- ^ Aligned "right" element
    → m ()
setAlign (lc, mc, rc) i le me re =
    unsafeWrite lc i le *> unsafeWrite mc i me *> unsafeWrite rc i re


{-# INLINEABLE setDelete #-}
{-# SPECIALIZE setDelete ∷ TempSlimDynamicCharacter (ST s) → Int → SlimState → SlimState → ST s () #-}
{-# SPECIALIZE setDelete ∷ TempWideDynamicCharacter (ST s) → Int → WideState → WideState → ST s () #-}
{-# SPECIALIZE setDelete ∷ TempHugeDynamicCharacter (ST s) → Int → HugeState → HugeState → ST s () #-}
setDelete
    ∷ ( Bits e
      , PrimMonad m
      , Vector v e
      )
    ⇒ TempOpenDynamicCharacter m v e
    -- ^ Modifiable character
    → Int
    -- ^ Index to set
    → e
    -- ^ Deleted "right" element
    → e
    -- ^ Median element
    → m ()
setDelete (lc, mc, rc) i me re =
    unsafeWrite lc i (me `xor` me) *> unsafeWrite mc i me *> unsafeWrite rc i re


{-# INLINEABLE setInsert #-}
{-# SPECIALIZE setInsert ∷ TempSlimDynamicCharacter (ST s) → Int → SlimState → SlimState → ST s () #-}
{-# SPECIALIZE setInsert ∷ TempWideDynamicCharacter (ST s) → Int → WideState → WideState → ST s () #-}
{-# SPECIALIZE setInsert ∷ TempHugeDynamicCharacter (ST s) → Int → HugeState → HugeState → ST s () #-}
setInsert
    ∷ ( Bits e
      , PrimMonad m
      , Vector v e
      )
    ⇒ TempOpenDynamicCharacter m v e
    -- ^ Modifiable character
    → Int
    -- ^ Index to set
    → e
    -- ^ Median element
    → e
    -- ^ Inserted "left" element
    → m ()
setInsert (lc, mc, rc) i le me =
    unsafeWrite lc i le *> unsafeWrite mc i me *> unsafeWrite rc i (me `xor` me)


{-# INLINEABLE setGapped #-}
{-# SPECIALIZE setGapped ∷ TempSlimDynamicCharacter (ST s) → Int → ST s () #-}
{-# SPECIALIZE setGapped ∷ TempWideDynamicCharacter (ST s) → Int → ST s () #-}
{-# SPECIALIZE setGapped ∷ TempHugeDynamicCharacter (ST s) → Int → ST s () #-}
setGapped
    ∷ ( Bits e
      , PrimMonad m
      , Vector v e
      )
    ⇒ TempOpenDynamicCharacter m v e
    -- ^ Modifiable character
    → Int
    -- ^ Index to set
    → m ()
setGapped (lc, mc, rc) i = do
    tmp ← unsafeRead mc 0 -- Get cell dimension context from first cell in vector
    let (# gap, nil #) = buildGapAndNil tmp
    unsafeWrite lc i nil
    unsafeWrite mc i gap
    unsafeWrite rc i nil


{-# INLINEABLE transposeCharacter #-}
transposeCharacter ∷ OpenDynamicCharacter v e → OpenDynamicCharacter v e
transposeCharacter (lc, mc, rc) = (rc, mc, lc)


{-# INLINEABLE characterLength #-}
characterLength ∷ (Vector v e) ⇒ OpenDynamicCharacter v e → Word
characterLength = toEnum . GV.length . extractMediansGapped


{- |
Utility function to enforce strict evaluation of the dynmaic character.
-}
{-# INLINEABLE forceDynamicCharacter #-}
{-# SPECIALIZE forceDynamicCharacter ∷ SlimDynamicCharacter → SlimDynamicCharacter #-}
{-# SPECIALIZE forceDynamicCharacter ∷ WideDynamicCharacter → WideDynamicCharacter #-}
{-# SPECIALIZE forceDynamicCharacter ∷ HugeDynamicCharacter → HugeDynamicCharacter #-}
forceDynamicCharacter ∷ (Vector v e) ⇒ OpenDynamicCharacter v e → OpenDynamicCharacter v e
forceDynamicCharacter (lv, mv, rv) =
    let -- Force the individual vectors
        lv' = GV.force lv
        mv' = GV.force mv
        rv' = GV.force rv
    in  -- Then force the tuple containing the vectors
        lv' `seq` mv' `seq` rv' `seq` (lv', mv', rv')


{- |
Extract the /ungapped/ medians of a dynamic character.
-}
{-# INLINEABLE extractMedians #-}
{-# SPECIALIZE extractMedians ∷ SlimDynamicCharacter → SV.Vector SlimState #-}
{-# SPECIALIZE extractMedians ∷ WideDynamicCharacter → UV.Vector WideState #-}
{-# SPECIALIZE extractMedians ∷ HugeDynamicCharacter → RV.Vector HugeState #-}
extractMedians ∷ (FiniteBits e, Vector v e) ⇒ OpenDynamicCharacter v e → v e
extractMedians (_, me, _)
    | GV.null me = me
    | otherwise =
        -- let gap = buildGap $ me ! 0
        let (# gap, nil #) = buildGapAndNil $ me ! 0
        in  -- additing this due to some final state wierdness
            -- in  GV.filter (/= gap) me
            GV.filter (\e → e /= gap && e /= nil) me


{- |
Extract the /ungapped/ medians of a single field of a dynamic character.
-}
{-# INLINEABLE extractMediansSingle #-}
{-# SPECIALIZE extractMediansSingle ∷ SV.Vector SlimState → SV.Vector SlimState #-}
{-# SPECIALIZE extractMediansSingle ∷ UV.Vector WideState → UV.Vector WideState #-}
{-# SPECIALIZE extractMediansSingle ∷ RV.Vector HugeState → RV.Vector HugeState #-}
extractMediansSingle ∷ (FiniteBits e, Vector v e) ⇒ v e → v e
extractMediansSingle me
    | GV.null me = me
    | otherwise =
        let gap = buildGap $ me ! 0
        in  GV.filter (/= gap) me


{- |
Extract the left child's /ungapped/ medians used to construct the dynamic character.
-}
{-# INLINEABLE extractMediansLeft #-}
{-# SPECIALIZE extractMediansLeft ∷ SlimDynamicCharacter → SV.Vector SlimState #-}
{-# SPECIALIZE extractMediansLeft ∷ WideDynamicCharacter → UV.Vector WideState #-}
{-# SPECIALIZE extractMediansLeft ∷ HugeDynamicCharacter → RV.Vector HugeState #-}
extractMediansLeft ∷ (FiniteBits e, Vector v e) ⇒ OpenDynamicCharacter v e → v e
extractMediansLeft (lc, _, _)
    | GV.null lc = lc
    | otherwise =
        let nil = buildNil $ lc ! 0
        in  GV.filter (/= nil) lc


{- |
Extract the right child's /ungapped/ medians used to construct the dynamic character.
-}
{-# INLINEABLE extractMediansRight #-}
{-# SPECIALIZE extractMediansRight ∷ SlimDynamicCharacter → SV.Vector SlimState #-}
{-# SPECIALIZE extractMediansRight ∷ WideDynamicCharacter → UV.Vector WideState #-}
{-# SPECIALIZE extractMediansRight ∷ HugeDynamicCharacter → RV.Vector HugeState #-}
extractMediansRight ∷ (FiniteBits e, Vector v e) ⇒ OpenDynamicCharacter v e → v e
extractMediansRight (_, _, rc)
    | GV.null rc = rc
    | otherwise =
        let nil = buildNil $ rc ! 0
        in  GV.filter (/= nil) rc


{- |
Extract the /gapped/ medians of a dynamic character.
-}
{-# INLINEABLE extractMediansGapped #-}
extractMediansGapped ∷ OpenDynamicCharacter v e → v e
extractMediansGapped (_, me, _) = me


{- |
Extract the left child's /gapped/ medians used to construct the dynamic character.
-}
{-# INLINEABLE extractMediansLeftGapped #-}
extractMediansLeftGapped ∷ OpenDynamicCharacter v e → v e
extractMediansLeftGapped (lc, _, _) = lc


{- |
Extract the right child's /gapped/ medians used to construct the dynamic character.
-}
{-# INLINEABLE extractMediansRightGapped #-}
extractMediansRightGapped ∷ OpenDynamicCharacter v e → v e
extractMediansRightGapped (_, _, rc) = rc


{- |
Extract the  /ungapped/ medians used to construct the dynamic character.

Output medians will /not/ contain "gap" or "nil" states
-}
{-# INLINEABLE removeGapAndNil #-}
{-# SPECIALIZE removeGapAndNil ∷ SV.Vector SlimState → SV.Vector SlimState #-}
{-# SPECIALIZE removeGapAndNil ∷ UV.Vector WideState → UV.Vector WideState #-}
{-# SPECIALIZE removeGapAndNil ∷ RV.Vector HugeState → RV.Vector HugeState #-}
removeGapAndNil ∷ (FiniteBits e, Vector v e) ⇒ v e → v e
removeGapAndNil rc
    | GV.null rc = rc
    | otherwise =
        let (# gap, nil #) = buildGapAndNil $ rc ! 0
        in  GV.filter (\e → e /= gap && e /= nil) rc


{- |
Construct a dynamic character from a sequence of "symbol sets".
The provided 'Alphabet' is used to encode the present symbols of each set into a dynamic character element.
-}
{-# INLINEABLE encodeDynamicCharacter #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Foldable g, Ord s) ⇒ Alphabet s → (Word → SlimState) → f (g s) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Foldable g, Ord s) ⇒ Alphabet s → (Word → WideState) → f (g s) → WideDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Foldable g, Ord s) ⇒ Alphabet s → (Word → HugeState) → f (g s) → HugeDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Ord s) ⇒ Alphabet s → (Word → SlimState) → f (Set s) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Ord s) ⇒ Alphabet s → (Word → WideState) → f (Set s) → WideDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f, Ord s) ⇒ Alphabet s → (Word → HugeState) → f (Set s) → HugeDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f) ⇒ Alphabet String → (Word → SlimState) → f (Set String) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f) ⇒ Alphabet String → (Word → WideState) → f (Set String) → WideDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Foldable f) ⇒ Alphabet String → (Word → HugeState) → f (Set String) → HugeDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Ord s) ⇒ Alphabet s → (Word → SlimState) → RV.Vector (Set s) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Ord s) ⇒ Alphabet s → (Word → WideState) → RV.Vector (Set s) → WideDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    (Ord s) ⇒ Alphabet s → (Word → HugeState) → RV.Vector (Set s) → HugeDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    Alphabet String → (Word → SlimState) → RV.Vector (Set String) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    Alphabet String → (Word → WideState) → RV.Vector (Set String) → WideDynamicCharacter
    #-}
{-# SPECIALIZE encodeDynamicCharacter ∷
    Alphabet String → (Word → HugeState) → RV.Vector (Set String) → HugeDynamicCharacter
    #-}
encodeDynamicCharacter
    ∷ ( Bits e
      , Foldable f
      , Foldable g
      , Ord s
      , Vector v e
      )
    ⇒ Alphabet s
    -- ^ Alphabet of symbols
    → (Word → e)
    -- ^ Constructor for an empty element, taking the alphabet size
    → f (g s)
    -- ^ Sequence of ambiguity groups of symbols
    → OpenDynamicCharacter v e
    -- ^ Encoded dynamic character
encodeDynamicCharacter alphabet f sequenceOfSymbols = dynamicCharater
    where
        len = length sequenceOfSymbols

        dynamicCharater = unsafeCharacterBuiltByST (toEnum len) $ \char → do
            iRef ← newSTRef 0

            let writeElement symbols =
                    let encodedVal = encodeState alphabet f symbols
                    in  do
                            i ← readSTRef iRef
                            setAlign char i encodedVal encodedVal encodedVal
                            modifySTRef iRef succ

            traverse_ writeElement sequenceOfSymbols


{-# INLINEABLE newTempCharacter #-}
{-# SPECIALIZE newTempCharacter ∷ Word → ST s (TempSlimDynamicCharacter (ST s)) #-}
{-# SPECIALIZE newTempCharacter ∷ Word → ST s (TempWideDynamicCharacter (ST s)) #-}
{-# SPECIALIZE newTempCharacter ∷ Word → ST s (TempHugeDynamicCharacter (ST s)) #-}
newTempCharacter
    ∷ ( Vector v e
      , PrimMonad m
      )
    ⇒ Word
    → m (TempOpenDynamicCharacter m v e)
newTempCharacter n =
    let i = fromEnum n
    in  (,,) <$> unsafeNew i <*> unsafeNew i <*> unsafeNew i


{-# INLINEABLE freezeTempCharacter #-}
{-# SPECIALIZE freezeTempCharacter ∷ TempSlimDynamicCharacter (ST s) → ST s SlimDynamicCharacter #-}
{-# SPECIALIZE freezeTempCharacter ∷ TempWideDynamicCharacter (ST s) → ST s WideDynamicCharacter #-}
{-# SPECIALIZE freezeTempCharacter ∷ TempHugeDynamicCharacter (ST s) → ST s HugeDynamicCharacter #-}
freezeTempCharacter
    ∷ ( PrimMonad m
      , Vector v e
      )
    ⇒ TempOpenDynamicCharacter m v e
    → m (OpenDynamicCharacter v e)
freezeTempCharacter (lc, mc, rc) =
    (,,) <$> unsafeFreeze lc <*> unsafeFreeze mc <*> unsafeFreeze rc


{-# INLINEABLE generateCharacter #-}
{-# SPECIALIZE generateCharacter ∷ Word → (Word → (# SlimState, SlimState, SlimState #)) → SlimDynamicCharacter #-}
{-# SPECIALIZE generateCharacter ∷ Word → (Word → (# WideState, WideState, WideState #)) → WideDynamicCharacter #-}
{-# SPECIALIZE generateCharacter ∷ Word → (Word → (# HugeState, HugeState, HugeState #)) → HugeDynamicCharacter #-}
generateCharacter
    ∷ (Vector v e)
    ⇒ Word
    → (Word → (# e, e, e #))
    → OpenDynamicCharacter v e
generateCharacter n f = runST $ do
    char ← newTempCharacter n
    forM_ [0 .. n - 1] $ \i →
        let (# x, y, z #) = f i
        in  setAlign char (fromEnum i) x y z
    freezeTempCharacter char


{-# INLINEABLE unsafeCharacterBuiltByST #-}
{-# SPECIALIZE unsafeCharacterBuiltByST ∷
    Word → (∀ s. TempSlimDynamicCharacter (ST s) → ST s ()) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE unsafeCharacterBuiltByST ∷
    Word → (∀ s. TempWideDynamicCharacter (ST s) → ST s ()) → WideDynamicCharacter
    #-}
{-# SPECIALIZE unsafeCharacterBuiltByST ∷
    Word → (∀ s. TempHugeDynamicCharacter (ST s) → ST s ()) → HugeDynamicCharacter
    #-}
unsafeCharacterBuiltByST
    ∷ (Vector v e)
    ⇒ Word
    → (∀ s. TempOpenDynamicCharacter (ST s) v e → ST s ())
    → OpenDynamicCharacter v e
unsafeCharacterBuiltByST n f = runST $ do
    char ← newTempCharacter n
    f char
    freezeTempCharacter char


{- |
Allocated a buffer of specified size.
Uses function to generate character and return the final size.
Copies character from buffer to character of final size.
-}
{-# INLINEABLE unsafeCharacterBuiltByBufferedST #-}
{-# SPECIALIZE unsafeCharacterBuiltByBufferedST ∷
    Word → (∀ s. TempSlimDynamicCharacter (ST s) → ST s Word) → SlimDynamicCharacter
    #-}
{-# SPECIALIZE unsafeCharacterBuiltByBufferedST ∷
    Word → (∀ s. TempWideDynamicCharacter (ST s) → ST s Word) → WideDynamicCharacter
    #-}
{-# SPECIALIZE unsafeCharacterBuiltByBufferedST ∷
    Word → (∀ s. TempHugeDynamicCharacter (ST s) → ST s Word) → HugeDynamicCharacter
    #-}
unsafeCharacterBuiltByBufferedST
    ∷ (Vector v e)
    ⇒ Word
    -- ^ Buffer length
    → (∀ s. TempOpenDynamicCharacter (ST s) v e → ST s Word)
    → OpenDynamicCharacter v e
unsafeCharacterBuiltByBufferedST b f = runST $ do
    buff ← newTempCharacter b
    char ← f buff >>= newTempCharacter
    copyCharacter buff char
    freezeTempCharacter char
    where
        copyCharacter
            ∷ ( PrimMonad m
              , MVector v1 a1
              , MVector v2 a1
              , MVector v3 a2
              , MVector v4 a2
              , MVector v5 a3
              , MVector v6 a3
              )
            ⇒ (v2 (PrimState m) a1, v3 (PrimState m) a2, v5 (PrimState m) a3)
            → (v1 (PrimState m) a1, v4 (PrimState m) a2, v6 (PrimState m) a3)
            → m ()
        copyCharacter src@(x, _, _) des@(y, _, _) =
            let m = GMV.length x
                n = GMV.length y
                o = m - n
            in  forM_ [0 .. n - 1] $ copyAt src des o

        copyAt
            ∷ ( PrimMonad m
              , MVector v1 a1
              , MVector v2 a1
              , MVector v3 a2
              , MVector v4 a2
              , MVector v5 a3
              , MVector v6 a3
              )
            ⇒ (v2 (PrimState m) a1, v3 (PrimState m) a2, v5 (PrimState m) a3)
            → (v1 (PrimState m) a1, v4 (PrimState m) a2, v6 (PrimState m) a3)
            → Int
            → Int
            → m ()
        copyAt (slc, smc, src) (dlc, dmc, drc) o i =
            let i' = o + i
            in  do
                    unsafeRead slc i' >>= unsafeWrite dlc i
                    unsafeRead smc i' >>= unsafeWrite dmc i
                    unsafeRead src i' >>= unsafeWrite drc i


buildGap ∷ (Bits e) ⇒ e → e
buildGap e = buildNil e `setBit` atSymbolIndex gapIndex


buildNil ∷ (Bits e) ⇒ e → e
buildNil e = e `xor` e


buildGapAndNil ∷ (Bits e) ⇒ e → (# e, e #)
buildGapAndNil e =
    let nil = buildNil e
        gap = nil `setBit` atSymbolIndex gapIndex
    in  (# gap, nil #)

{--------------------------------------------------------------------------}
{- Gap extraction and re-insertion                                         -}
{--------------------------------------------------------------------------}

{- |
A set of gaps extracted froma sequence to be re-inserted later.

/NOTE:/ The 'Bio.DynamicCharacter.HandleGaps.GapSet' type is closed to ensure invaiants hold.
-}
newtype GapSet = GapSet (IntMap Word)
    deriving newtype (Eq, Show)


{- |
The 'Bio.DynamicCharacter.HandleGaps.GapSet' which contains no gaps.

/NOTE:/ Useful for making comparisons.
-}
nullGapSet ∷ GapSet
nullGapSet = GapSet mempty


{- |
Strips the gap elements from the supplied character.

Remembers the locations of the gap characters that were deleted

If the character contains /only/ gaps, a missing character is returned.
-}
{-# INLINEABLE deleteGaps #-}
{-# SPECIALIZE deleteGaps ∷ SlimDynamicCharacter → (GapSet, SlimDynamicCharacter) #-}
{-# SPECIALIZE deleteGaps ∷ WideDynamicCharacter → (GapSet, WideDynamicCharacter) #-}
{-# SPECIALIZE deleteGaps ∷ HugeDynamicCharacter → (GapSet, HugeDynamicCharacter) #-}
deleteGaps
    ∷ ( FiniteBits e
      , Vector v e
      )
    ⇒ OpenDynamicCharacter v e
    -- ^ Dynamic character
    → (GapSet, OpenDynamicCharacter v e)
deleteGaps c@(x, y, z)
    | GV.null x = (noGaps, c)
    | null gaps = (noGaps, c)
    | newLen == 0 = (gapSet, missing)
    | otherwise = (gapSet, newVector)
    where
        noGaps = GapSet mempty
        missing = (GV.empty, GV.empty, GV.empty)
        newVector = runST $ do
            j ← newSTRef 0
            let isGapAtJ = isGap c <$> readSTRef j

            let g v = do
                    whileM_ isGapAtJ $ modifySTRef j succ
                    j' ← readSTRef j
                    modifySTRef j succ
                    pure $ v ! j'

            let genVec v = do
                    vec ← GV.generateM newLen (const (g v))
                    writeSTRef j 0
                    pure vec

            (,,) <$> genVec x <*> genVec y <*> genVec z

        gapCount = fromEnum . getSum $ foldMap Sum gaps
        charLen = GV.length x
        newLen = charLen - gapCount

        gapSet = GapSet gaps
        gaps = fromDistinctAscList $ reverse refs
        refs = runST $ do
            nonGaps ← newSTRef 0
            prevGap ← newSTRef False
            gapLen ← newSTRef 0
            gapRefs ← newSTRef []

            let handleGapBefore op = do
                    gapBefore ← readSTRef prevGap
                    when gapBefore $ do
                        j ← readSTRef nonGaps
                        g ← readSTRef gapLen
                        modifySTRef gapRefs ((j, g) :)
                        op

            for_ [0 .. charLen - 1] $ \i →
                if isGap c i
                    then modifySTRef gapLen succ *> writeSTRef prevGap True
                    else do
                        handleGapBefore $ do
                            writeSTRef gapLen 0
                            writeSTRef prevGap False
                        modifySTRef nonGaps succ

            handleGapBefore $ pure ()
            readSTRef gapRefs


{- |
Adds gaps elements to the supplied character.

/NOTE:/ It is important to have the 'Example.Alphabet.gapSymbol' state passed in as a parameter!
There is the possibility that the alignment context is empty, but one or more
gaps need to be added. We cannot construct a gap state from an empty input so
the gap state must be externally supplied.
-}
{-# INLINEABLE insertGaps #-}
{-# SPECIALIZE insertGaps ∷ SlimState → GapSet → GapSet → SlimDynamicCharacter → SlimDynamicCharacter #-}
{-# SPECIALIZE insertGaps ∷ WideState → GapSet → GapSet → WideDynamicCharacter → WideDynamicCharacter #-}
{-# SPECIALIZE insertGaps ∷ HugeState → GapSet → GapSet → HugeDynamicCharacter → HugeDynamicCharacter #-}
insertGaps
    ∷ ( FiniteBits e
      , Vector v e
      )
    ⇒ e
    -- ^ Gap State
    → GapSet
    -- ^ Removed gap references from "left"  (shorter) dynamic character
    → GapSet
    -- ^ Removed gap references from "right" (larger)  dynamic character
    → OpenDynamicCharacter v e
    -- ^ Alignment context to have gap references inserted
    → OpenDynamicCharacter v e
    -- ^ Fully gapped alignment context
insertGaps gap (GapSet lGaps) (GapSet rGaps) input@(x, y, z)
    | null lGaps && null rGaps = input -- No work needed
    | otherwise =
        -- Use a Let/In binding here since the module is STRICT!
        -- Prevents indexing errors on missing character input.
        let gapVecLen ∷ IntMap b → Int
            gapVecLen = maybe 0 (succ . fst) . lookupMax

            nil = gap `xor` gap
            totalGaps = fromEnum . getSum . foldMap Sum
            lGapCount = totalGaps lGaps
            rGapCount = totalGaps rGaps
            medLength = GV.length x
            newLength = lGapCount + rGapCount + medLength

            ins = (gap, gap, nil)
            del = (nil, gap, gap)
        in  runST $ do
                --        xVec <- MGV.unsafeNew newLength
                --        yVec <- MGV.unsafeNew newLength
                --        zVec <- MGV.unsafeNew newLength
                xVec ← MGV.replicate newLength zeroBits
                yVec ← MGV.replicate newLength zeroBits
                zVec ← MGV.replicate newLength zeroBits
                lVec ← MUV.replicate (gapVecLen lGaps) 0
                rVec ← MUV.replicate (gapVecLen rGaps) 0
                lGap ← newSTRef 0
                mPtr ← newSTRef 0
                rGap ← newSTRef 0
                -- Write out to the mutable vectors
                for_ (toAscList lGaps) $ uncurry (MUV.unsafeWrite lVec)
                for_ (toAscList rGaps) $ uncurry (MUV.unsafeWrite rVec)

                let inc ∷ ∀ s. STRef s Int → ST s ()
                    inc r = modifySTRef r succ

                let align i = do
                        m ← readSTRef mPtr
                        MGV.unsafeWrite xVec i $ x ! m
                        MGV.unsafeWrite yVec i $ y ! m
                        MGV.unsafeWrite zVec i $ z ! m
                        inc mPtr
                        -- Deletions leave a void in the left (shorter) character.
                        --
                        -- This means we are "consuming" an element from the right (longer) character
                        -- Hence we increment the right gap reference, as we "progress" through the
                        -- right character sequence's elements in the alignment context.
                        when (isAlign input m || isDelete input m) $
                            inc rGap
                        -- Similar logic as above, however,
                        -- Insertions leave a void in the right (longer) character.
                        when (isAlign input m || isInsert input m) $
                            inc lGap

                let insertGapWith i (xe, ye, ze) gapRef gapVec = do
                        let len = MGV.length gapVec
                        rg ← readSTRef gapRef
                        v ← if rg >= len then pure 0 else MGV.unsafeRead gapVec rg
                        if v == 0
                            then pure False
                            else do
                                MGV.unsafeWrite xVec i xe
                                MGV.unsafeWrite yVec i ye
                                MGV.unsafeWrite zVec i ze
                                MGV.unsafeWrite gapVec rg $ v - 1
                                pure True

                for_ [0 .. newLength - 1] $ \i → do
                    written ← insertGapWith i ins lGap lVec
                    unless written $ do
                        written' ← insertGapWith i del rGap rVec
                        unless written' $ align i

                x' ← GV.unsafeFreeze xVec
                y' ← GV.unsafeFreeze yVec
                z' ← GV.unsafeFreeze zVec
                pure (x', y', z')

{--------------------------------------------------------------------------}
{- Length comparison                                                       -}
{--------------------------------------------------------------------------}

{- |
__Time:__ \( \Theta \left(\, 1 \,\right) \) for input characters of differing lengths.

__Time:__ \( \mathcal{O}\left(\, k \,\right) \) for input characters of equal length,
where \( k \) is the shared prefix of both characters.

Returns the dynamic character that is shorter first, longer second, and notes
whether or not the inputs were swapped to place the characters in this ordering.

Handles equal length characters by considering the lexicographically larger
character as longer.

Handles equality of inputs by /not/ swapping.
-}
{-# INLINEABLE measureCharacters #-}
{-# SPECIALIZE measureCharacters ∷
    SlimDynamicCharacter → SlimDynamicCharacter → (Ordering, SlimDynamicCharacter, SlimDynamicCharacter)
    #-}
{-# SPECIALIZE measureCharacters ∷
    WideDynamicCharacter → WideDynamicCharacter → (Ordering, WideDynamicCharacter, WideDynamicCharacter)
    #-}
{-# SPECIALIZE measureCharacters ∷
    HugeDynamicCharacter → HugeDynamicCharacter → (Ordering, HugeDynamicCharacter, HugeDynamicCharacter)
    #-}
measureCharacters
    ∷ ( Ord (v e)
      , Vector v e
      )
    ⇒ OpenDynamicCharacter v e
    → OpenDynamicCharacter v e
    → (Ordering, OpenDynamicCharacter v e, OpenDynamicCharacter v e)
measureCharacters lhs rhs
    | lhsOrdering == GT = (lhsOrdering, rhs, lhs)
    | otherwise = (lhsOrdering, lhs, rhs)
    where
        lhsMedians = extractMediansGapped lhs
        rhsMedians = extractMediansGapped rhs
        lhsOrdering =
            -- First, compare inputs by length.
            case comparing basicLength lhsMedians rhsMedians of
                -- If the inputs are equal length,
                -- Then compare by the (arbitrary) lexicographical ordering of the median states.
                EQ → case lhsMedians `compare` rhsMedians of
                    -- If the input median states have the same ordering,
                    -- Lastly, we compare by the lexicographic ordering of the "tagged triples."
                    --
                    -- If they are equal after this step,
                    -- Then the inputs are representationally equal.
                    -- Actually, honest to goodness 100% equal!
                    EQ → lhs `compare` rhs
                    v → v
                v → v


{- |
__Time:__ \( \mathcal{O}\left(\, k \,\right) \)
where \( k \) is the length of the longest character.

Considers the median values of the characters, ignores the left/right tagging.

First remove the gaps from the input characters.

If both "ungapped" inputs are empty, we measure the original "gapped" inputs to
determine if the inputs need to be swapped. This is required to ensure comutativity
of subsequent operations which use this method.

Returns the "ungapped" dynamic character that is "shorter" first, "longer" second,
the removed gap mappings (in the same order), and notes whether or not the inputs
were swapped to place the characters in this ordering.

Handles equal length characters by considering the lexicographically larger
character as longer.

Handles equality of inputs by /not/ swapping.
-}
{-# INLINEABLE measureCharactersWithoutGaps #-}
{-# SPECIALIZE measureCharactersWithoutGaps ∷
    SlimDynamicCharacter → SlimDynamicCharacter → (Bool, GapSet, GapSet, SlimDynamicCharacter, SlimDynamicCharacter)
    #-}
{-# SPECIALIZE measureCharactersWithoutGaps ∷
    WideDynamicCharacter → WideDynamicCharacter → (Bool, GapSet, GapSet, WideDynamicCharacter, WideDynamicCharacter)
    #-}
{-# SPECIALIZE measureCharactersWithoutGaps ∷
    HugeDynamicCharacter → HugeDynamicCharacter → (Bool, GapSet, GapSet, HugeDynamicCharacter, HugeDynamicCharacter)
    #-}
measureCharactersWithoutGaps
    ∷ ( FiniteBits e
      , Ord (v e)
      , Vector v e
      )
    ⇒ OpenDynamicCharacter v e
    -- ^ First  dynamic character
    → OpenDynamicCharacter v e
    -- ^ Second dynamic character
    → (Bool, GapSet, GapSet, OpenDynamicCharacter v e, OpenDynamicCharacter v e)
measureCharactersWithoutGaps char1 char2
    | swapInputs = (True, gapsChar2, gapsChar1, ungappedChar2, ungappedChar1)
    | otherwise = (False, gapsChar1, gapsChar2, ungappedChar1, ungappedChar2)
    where
        swapInputs = measure == GT
        (gapsChar1, ungappedChar1) = deleteGaps char1
        (gapsChar2, ungappedChar2) = deleteGaps char2
        (measure, _, _) =
            case measureCharacters ungappedChar1 ungappedChar2 of
                (EQ, _, _) → measureCharacters char1 char2
                x → x

{--------------------------------------------------------------------------}
{- Rendering                                                               -}
{--------------------------------------------------------------------------}

renderDynamicCharacter
    ∷ ( FiniteBits e
      , Show e
      , Vector v e
      )
    ⇒ OpenDynamicCharacter v e
    → String
renderDynamicCharacter (lc, mc, rc) =
    unlines
        [ "Character Length: " <> show (GV.length mc)
        , printVector lcStr
        , printVector mcStr
        , printVector rcStr
        ]
    where
        show' ∷ (Bits a, Show a) ⇒ a → String
        show' x
            | popCount x > 0 = show x
            | otherwise = [voidC]

        voidC = '█'
        lcStr = show' <$> GV.toList lc
        mcStr = show' <$> GV.toList mc
        rcStr = show' <$> GV.toList rc
        eSize = length . maximumBy (comparing length) $ lcStr <> mcStr <> rcStr <> [[voidC]]
        pad s =
            let c
                    | s == [voidC] = voidC
                    | otherwise = ' '
            in  replicate (eSize - length s) c <> s

        intercalate' [] = []
        intercalate' [x] = x
        intercalate' (x : xs) =
            let sep = case x of
                    e : _ | e == voidC → [voidC]
                    _ → " "
            in  x <> sep <> intercalate' xs

        printVector vec = "[ " <> intercalate' (pad <$> vec) <> " ]"


renderSlimCharacter ∷ SlimDynamicCharacter → String
renderSlimCharacter char =
    let len = characterLength char

        slimChar = renderSlimStateChar

        renderIndex ∷ Int → (Char, Char, Char)
        renderIndex i = case char `getContext` i of
            Gapped → ('█', '█', '█')
            Delete med rhs → ('█', slimChar med, slimChar rhs)
            Insert lhs med → (slimChar lhs, slimChar med, '█')
            Merged lhs med rhs → (slimChar lhs, slimChar med, slimChar rhs)

        (lhsStr, medStr, rhsStr) = unzip3 $ renderIndex <$> [0 .. fromEnum len - 1]
    in  List.intercalate
            "\n"
            [ "Length: " <> show len
            , "Lesser: " <> lhsStr
            , "Median: " <> medStr
            , "Longer: " <> rhsStr
            ]
