{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Concrete encodings for a single element of a dynamic character.

A dynamic character element is an /ambiguity group/: a set of alphabet symbols
the character might take at that position, held as a bit set.  Three widths are
provided, selected by alphabet size:

  * 'SlimState' -- alphabet of  1 to  8 symbols; 32 bits wide, and the only
    encoding the C FFI accepts.
  * 'WideState' -- alphabet of  9 to 64 symbols; 64 bits wide.
  * 'HugeState' -- alphabet of 65 or more symbols; an unboxed bit vector.

Merged from the former @Bio.DynamicCharacter.Element@,
@Bio.DynamicCharacter.Element.Class@ and the three
@Bio.DynamicCharacter.Element.*State@ modules.
-}
module Example.CharacterElement (
    -- * Type-class
    StateOfAmbiguity (..),

    -- * Encodings
    SlimState (..),
    WideState (..),
    HugeState (..),

    -- * Rendering
    renderSlimStateChar,
) where

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData)
import Example.Alphabet (Alphabet, fromSymbols)
import Example.Alphabet.Codec (decodeState)
import Example.Alphabet.Gap (gapIndex)
import Data.Bifunctor (bimap, second)
import Data.Bimap (Bimap, (!>))
import Data.Bimap qualified as BM
import Data.Bit
import Data.Bits (Bits (..), FiniteBits (..))
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.Data ()
import Data.Foldable (fold)
import Data.Foldable (toList)
import Data.Hashable (Hashable (..))
import Data.Ix (Ix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Generic qualified as GV
import Data.Vector.Generic.Mutable qualified as MGV
import Data.Vector.Primitive qualified as PV
import Data.Vector.Unboxed qualified as UV
import Data.Word (Word64)
import Foreign.C.Types (CUInt)
import Foreign.Storable (Storable)
import GHC.IsList (IsList (Item))
import GHC.IsList qualified as List (IsList (..))
import Example.Measure (atSymbolIndex)
import Unsafe.Coerce (unsafeCoerce)


{--------------------------------------------------------------------------}
{- Ambiguity type-class                                                    -}
{--------------------------------------------------------------------------}

{- |
Conversion of a character which ambiguously exists in a super position of possible states
to/from a linear ordering of bits.
-}
class StateOfAmbiguity e where
    toBits ∷ (IsList f, Item f ~ Bool) ⇒ e → f


    fromBits ∷ (IsList f, Item f ~ Bool) ⇒ f → e


    fromNumber ∷ (Integral v) ⇒ Word → v → e


    toUnsignedNumber ∷ (Integral v) ⇒ e → v

{--------------------------------------------------------------------------}
{- SlimState: alphabets of 1-8 symbols                                     -}
{--------------------------------------------------------------------------}

{- |
Encoding for a dynamic character element with an alphabet size \( \lvert\, \Sigma \,\rvert \in \left[\;1,\, 8\;\right] \).

/NOTE:/ This encoding uses more bits than required! This is due to the C FFI
implementation details. It would be possible to reduce this to a @CUChar@ if
and only iff the C interface and implementation is updated.
-}
newtype SlimState = SlimState {fromSlimState ∷ CUInt}


newtype instance UV.MVector s SlimState = MV_SlimState (PV.MVector s CUInt)


newtype instance UV.Vector SlimState = V_SlimState (PV.Vector CUInt)


instance UV.Unbox SlimState


deriving newtype instance Bits SlimState


deriving newtype instance Bounded SlimState


deriving newtype instance Enum SlimState


deriving newtype instance Eq SlimState


deriving newtype instance FiniteBits SlimState


deriving newtype instance Integral SlimState


deriving newtype instance Ix SlimState


deriving newtype instance NFData SlimState


deriving newtype instance Num SlimState


deriving newtype instance Ord SlimState


deriving newtype instance Real SlimState


deriving newtype instance Storable SlimState


instance Hashable SlimState where
    hashWithSalt salt = (salt `xor`) . fromEnum . fromSlimState


instance Show SlimState where
    show = pure . renderSlimStateChar


instance StateOfAmbiguity SlimState where
    toBits state =
        let n = finiteBitSize state
        in  List.fromList $ foldMap (pure . (state `testBit`)) [0 .. n - 1]


    fromBits =
        let f ∷ Bool → (CUInt, Int) → (CUInt, Int)
            f b
                | b = \(a, i) → (a `setBit` i, i + 1)
                | otherwise = fmap succ
        in  coerce . fst . foldr f (0, 0) . List.toList


    fromNumber !dimValue !intValue =
        let size = min 8 $ fromEnum dimValue
            mask = 1 `shiftL` size
            bits ∷ CUInt
            bits = fromIntegral $ toInteger intValue `mod` mask
        in  coerce bits


    toUnsignedNumber = fromIntegral . (coerce ∷ SlimState → CUInt)


instance MGV.MVector UV.MVector SlimState where
    {-# INLINE basicLength #-}
    basicLength (MV_SlimState v) = MGV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (MV_SlimState v) = MV_SlimState $ MGV.basicUnsafeSlice i n v


    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_SlimState v1) (MV_SlimState v2) = MGV.basicOverlaps v1 v2


    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MV_SlimState <$> MGV.basicUnsafeNew n


    {-# INLINE basicInitialize #-}
    basicInitialize (MV_SlimState v) = MGV.basicInitialize v


    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = MV_SlimState <$> MGV.basicUnsafeReplicate n (fromSlimState x)


    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_SlimState v) i = SlimState <$> MGV.basicUnsafeRead v i


    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_SlimState v) i x = MGV.basicUnsafeWrite v i (fromSlimState x)


    {-# INLINE basicClear #-}
    basicClear (MV_SlimState v) = MGV.basicClear v


    {-# INLINE basicSet #-}
    basicSet (MV_SlimState v) x = MGV.basicSet v (fromSlimState x)


    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_SlimState v1) (MV_SlimState v2) = MGV.basicUnsafeCopy v1 v2


    basicUnsafeMove (MV_SlimState v1) (MV_SlimState v2) = MGV.basicUnsafeMove v1 v2


    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_SlimState v) n = MV_SlimState <$> MGV.basicUnsafeGrow v n


instance GV.Vector UV.Vector SlimState where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_SlimState v) = V_SlimState <$> GV.basicUnsafeFreeze v


    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_SlimState v) = MV_SlimState <$> GV.basicUnsafeThaw v


    {-# INLINE basicLength #-}
    basicLength (V_SlimState v) = GV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (V_SlimState v) = V_SlimState $ GV.basicUnsafeSlice i n v


    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_SlimState v) i = SlimState <$> GV.basicUnsafeIndexM v i


    basicUnsafeCopy (MV_SlimState mv) (V_SlimState v) = GV.basicUnsafeCopy mv v


    {-# INLINE elemseq #-}
    elemseq = const seq


type CharChange = Maybe (Char, Char → Char)


{- |
Used as the 'Show' instance for 'Bio.DynamicCharacter.Element.SlimState.SlimState'.
Correctly renderes all 255 possible values.
For alphabet size less than or equal to 5, the 'Bio.DynamicCharacter.Element.SlimState.SlimState' is rendered as the IUPAC code for DNA.
If the alphabet size is 6, the 6th state's presence is rendered as an italic font style.
If the alphabet size is 7, the 7th state's presence is rendered as an bold font style.
If the alpahbet size is 8, another state @\'Ω\'@ is added to the alphabet.

Below is a complete rendering table for 'Bio.DynamicCharacter.Element.SlimState.SlimState':

=== __Rendering table:__
@
                           Bit Indices     |     Set Modulus
                       --------------------+--------------------
                         0 1 2 3 4 5 6 7   |   A C G T Ω - Π Σ
    █  <--->  ∅        < ∙ ∙ ∙ ∙ ∙ ∙ ∙ ∙ >   [ ∙ ∙ ∙ ∙ ∙ ∙ ∙ ∙ ]
    A  <--->  A        < ∙ A ∙ ∙ ∙ ∙ ∙ ∙ >   [ A ∙ ∙ ∙ ∙ ∙ ∙ ∙ ]
    C  <--->  C        < ∙ ∙ C ∙ ∙ ∙ ∙ ∙ >   [ ∙ C ∙ ∙ ∙ ∙ ∙ ∙ ]
    G  <--->  G        < ∙ ∙ ∙ G ∙ ∙ ∙ ∙ >   [ ∙ ∙ G ∙ ∙ ∙ ∙ ∙ ]
    T  <--->  T        < ∙ ∙ ∙ ∙ T ∙ ∙ ∙ >   [ ∙ ∙ ∙ T ∙ ∙ ∙ ∙ ]
    Ω  <--->  Ω        < ∙ ∙ ∙ ∙ ∙ ∙ ∙ Ω >   [ ∙ ∙ ∙ ∙ Ω ∙ ∙ ∙ ]
    M  <--->  AC       < ∙ A C ∙ ∙ ∙ ∙ ∙ >   [ A C ∙ ∙ ∙ ∙ ∙ ∙ ]
    R  <--->  AG       < ∙ A ∙ G ∙ ∙ ∙ ∙ >   [ A ∙ G ∙ ∙ ∙ ∙ ∙ ]
    W  <--->  AT       < ∙ A ∙ ∙ T ∙ ∙ ∙ >   [ A ∙ ∙ T ∙ ∙ ∙ ∙ ]
    Λ  <--->  AΩ       < ∙ A ∙ ∙ ∙ ∙ ∙ Ω >   [ A ∙ ∙ ∙ Ω ∙ ∙ ∙ ]
    S  <--->  CG       < ∙ ∙ C G ∙ ∙ ∙ ∙ >   [ ∙ C G ∙ ∙ ∙ ∙ ∙ ]
    Y  <--->  CT       < ∙ ∙ C ∙ T ∙ ∙ ∙ >   [ ∙ C ∙ T ∙ ∙ ∙ ∙ ]
    Δ  <--->  CΩ       < ∙ ∙ C ∙ ∙ ∙ ∙ Ω >   [ ∙ C ∙ ∙ Ω ∙ ∙ ∙ ]
    K  <--->  GT       < ∙ ∙ ∙ G T ∙ ∙ ∙ >   [ ∙ ∙ G T ∙ ∙ ∙ ∙ ]
    Γ  <--->  GΩ       < ∙ ∙ ∙ G ∙ ∙ ∙ Ω >   [ ∙ ∙ G ∙ Ω ∙ ∙ ∙ ]
    Ξ  <--->  TΩ       < ∙ ∙ ∙ ∙ T ∙ ∙ Ω >   [ ∙ ∙ ∙ T Ω ∙ ∙ ∙ ]
    V  <--->  ACG      < ∙ A C G ∙ ∙ ∙ ∙ >   [ A C G ∙ ∙ ∙ ∙ ∙ ]
    H  <--->  ACT      < ∙ A C ∙ T ∙ ∙ ∙ >   [ A C ∙ T ∙ ∙ ∙ ∙ ]
    Z  <--->  ACΩ      < ∙ A C ∙ ∙ ∙ ∙ Ω >   [ A C ∙ ∙ Ω ∙ ∙ ∙ ]
    D  <--->  AGT      < ∙ A ∙ G T ∙ ∙ ∙ >   [ A ∙ G T ∙ ∙ ∙ ∙ ]
    X  <--->  AGΩ      < ∙ A ∙ G ∙ ∙ ∙ Ω >   [ A ∙ G ∙ Ω ∙ ∙ ∙ ]
    Q  <--->  ATΩ      < ∙ A ∙ ∙ T ∙ ∙ Ω >   [ A ∙ ∙ T Ω ∙ ∙ ∙ ]
    B  <--->  CGT      < ∙ ∙ C G T ∙ ∙ ∙ >   [ ∙ C G T ∙ ∙ ∙ ∙ ]
    P  <--->  CGΩ      < ∙ ∙ C G ∙ ∙ ∙ Ω >   [ ∙ C G ∙ Ω ∙ ∙ ∙ ]
    O  <--->  CTΩ      < ∙ ∙ C ∙ T ∙ ∙ Ω >   [ ∙ C ∙ T Ω ∙ ∙ ∙ ]
    J  <--->  GTΩ      < ∙ ∙ ∙ G T ∙ ∙ Ω >   [ ∙ ∙ G T Ω ∙ ∙ ∙ ]
    N  <--->  ACGT     < ∙ A C G T ∙ ∙ ∙ >   [ A C G T ∙ ∙ ∙ ∙ ]
    U  <--->  ACGΩ     < ∙ A C G ∙ ∙ ∙ Ω >   [ A C G ∙ Ω ∙ ∙ ∙ ]
    I  <--->  ACTΩ     < ∙ A C ∙ T ∙ ∙ Ω >   [ A C ∙ T Ω ∙ ∙ ∙ ]
    F  <--->  AGTΩ     < ∙ A ∙ G T ∙ ∙ Ω >   [ A ∙ G T Ω ∙ ∙ ∙ ]
    L  <--->  CGTΩ     < ∙ ∙ C G T ∙ ∙ Ω >   [ ∙ C G T Ω ∙ ∙ ∙ ]
    E  <--->  ACGTΩ    < ∙ A C G T ∙ ∙ Ω >   [ A C G T Ω ∙ ∙ ∙ ]
    -  <--->  -        < - ∙ ∙ ∙ ∙ ∙ ∙ ∙ >   [ ∙ ∙ ∙ ∙ ∙ - ∙ ∙ ]
    a  <--->  -A       < - A ∙ ∙ ∙ ∙ ∙ ∙ >   [ A ∙ ∙ ∙ ∙ - ∙ ∙ ]
    c  <--->  -C       < - ∙ C ∙ ∙ ∙ ∙ ∙ >   [ ∙ C ∙ ∙ ∙ - ∙ ∙ ]
    g  <--->  -G       < - ∙ ∙ G ∙ ∙ ∙ ∙ >   [ ∙ ∙ G ∙ ∙ - ∙ ∙ ]
    t  <--->  -T       < - ∙ ∙ ∙ T ∙ ∙ ∙ >   [ ∙ ∙ ∙ T ∙ - ∙ ∙ ]
    ω  <--->  -Ω       < - ∙ ∙ ∙ ∙ ∙ ∙ Ω >   [ ∙ ∙ ∙ ∙ Ω - ∙ ∙ ]
    m  <--->  -AC      < - A C ∙ ∙ ∙ ∙ ∙ >   [ A C ∙ ∙ ∙ - ∙ ∙ ]
    r  <--->  -AG      < - A ∙ G ∙ ∙ ∙ ∙ >   [ A ∙ G ∙ ∙ - ∙ ∙ ]
    w  <--->  -AT      < - A ∙ ∙ T ∙ ∙ ∙ >   [ A ∙ ∙ T ∙ - ∙ ∙ ]
    λ  <--->  -AΩ      < - A ∙ ∙ ∙ ∙ ∙ Ω >   [ A ∙ ∙ ∙ Ω - ∙ ∙ ]
    s  <--->  -CG      < - ∙ C G ∙ ∙ ∙ ∙ >   [ ∙ C G ∙ ∙ - ∙ ∙ ]
    y  <--->  -CT      < - ∙ C ∙ T ∙ ∙ ∙ >   [ ∙ C ∙ T ∙ - ∙ ∙ ]
    δ  <--->  -CΩ      < - ∙ C ∙ ∙ ∙ ∙ Ω >   [ ∙ C ∙ ∙ Ω - ∙ ∙ ]
    k  <--->  -GT      < - ∙ ∙ G T ∙ ∙ ∙ >   [ ∙ ∙ G T ∙ - ∙ ∙ ]
    γ  <--->  -GΩ      < - ∙ ∙ G ∙ ∙ ∙ Ω >   [ ∙ ∙ G ∙ Ω - ∙ ∙ ]
    ξ  <--->  -TΩ      < - ∙ ∙ ∙ T ∙ ∙ Ω >   [ ∙ ∙ ∙ T Ω - ∙ ∙ ]
    v  <--->  -ACG     < - A C G ∙ ∙ ∙ ∙ >   [ A C G ∙ ∙ - ∙ ∙ ]
    h  <--->  -ACT     < - A C ∙ T ∙ ∙ ∙ >   [ A C ∙ T ∙ - ∙ ∙ ]
    z  <--->  -ACΩ     < - A C ∙ ∙ ∙ ∙ Ω >   [ A C ∙ ∙ Ω - ∙ ∙ ]
    d  <--->  -AGT     < - A ∙ G T ∙ ∙ ∙ >   [ A ∙ G T ∙ - ∙ ∙ ]
    x  <--->  -AGΩ     < - A ∙ G ∙ ∙ ∙ Ω >   [ A ∙ G ∙ Ω - ∙ ∙ ]
    q  <--->  -ATΩ     < - A ∙ ∙ T ∙ ∙ Ω >   [ A ∙ ∙ T Ω - ∙ ∙ ]
    b  <--->  -CGT     < - ∙ C G T ∙ ∙ ∙ >   [ ∙ C G T ∙ - ∙ ∙ ]
    p  <--->  -CGΩ     < - ∙ C G ∙ ∙ ∙ Ω >   [ ∙ C G ∙ Ω - ∙ ∙ ]
    o  <--->  -CTΩ     < - ∙ C ∙ T ∙ ∙ Ω >   [ ∙ C ∙ T Ω - ∙ ∙ ]
    j  <--->  -GTΩ     < - ∙ ∙ G T ∙ ∙ Ω >   [ ∙ ∙ G T Ω - ∙ ∙ ]
    n  <--->  -ACGT    < - A C G T ∙ ∙ ∙ >   [ A C G T ∙ - ∙ ∙ ]
    u  <--->  -ACGΩ    < - A C G ∙ ∙ ∙ Ω >   [ A C G ∙ Ω - ∙ ∙ ]
    i  <--->  -ACTΩ    < - A C ∙ T ∙ ∙ Ω >   [ A C ∙ T Ω - ∙ ∙ ]
    f  <--->  -AGTΩ    < - A ∙ G T ∙ ∙ Ω >   [ A ∙ G T Ω - ∙ ∙ ]
    l  <--->  -CGTΩ    < - ∙ C G T ∙ ∙ Ω >   [ ∙ C G T Ω - ∙ ∙ ]
    e  <--->  -ACGTΩ   < - A C G T ∙ ∙ Ω >   [ A C G T Ω - ∙ ∙ ]
    𝛱  <--->  Π        < ∙ ∙ ∙ ∙ ∙ Π ∙ ∙ >   [ ∙ ∙ ∙ ∙ ∙ ∙ Π ∙ ]
    𝘈  <--->  AΠ       < ∙ A ∙ ∙ ∙ Π ∙ ∙ >   [ A ∙ ∙ ∙ ∙ ∙ Π ∙ ]
    𝘊  <--->  CΠ       < ∙ ∙ C ∙ ∙ Π ∙ ∙ >   [ ∙ C ∙ ∙ ∙ ∙ Π ∙ ]
    𝘎  <--->  GΠ       < ∙ ∙ ∙ G ∙ Π ∙ ∙ >   [ ∙ ∙ G ∙ ∙ ∙ Π ∙ ]
    𝘛  <--->  TΠ       < ∙ ∙ ∙ ∙ T Π ∙ ∙ >   [ ∙ ∙ ∙ T ∙ ∙ Π ∙ ]
    𝛺  <--->  ΠΩ       < ∙ ∙ ∙ ∙ ∙ Π ∙ Ω >   [ ∙ ∙ ∙ ∙ Ω ∙ Π ∙ ]
    𝘔  <--->  ACΠ      < ∙ A C ∙ ∙ Π ∙ ∙ >   [ A C ∙ ∙ ∙ ∙ Π ∙ ]
    𝘙  <--->  AGΠ      < ∙ A ∙ G ∙ Π ∙ ∙ >   [ A ∙ G ∙ ∙ ∙ Π ∙ ]
    𝘞  <--->  ATΠ      < ∙ A ∙ ∙ T Π ∙ ∙ >   [ A ∙ ∙ T ∙ ∙ Π ∙ ]
    𝛬  <--->  AΠΩ      < ∙ A ∙ ∙ ∙ Π ∙ Ω >   [ A ∙ ∙ ∙ Ω ∙ Π ∙ ]
    𝘚  <--->  CGΠ      < ∙ ∙ C G ∙ Π ∙ ∙ >   [ ∙ C G ∙ ∙ ∙ Π ∙ ]
    𝘠  <--->  CTΠ      < ∙ ∙ C ∙ T Π ∙ ∙ >   [ ∙ C ∙ T ∙ ∙ Π ∙ ]
    𝛥  <--->  CΠΩ      < ∙ ∙ C ∙ ∙ Π ∙ Ω >   [ ∙ C ∙ ∙ Ω ∙ Π ∙ ]
    𝘒  <--->  GTΠ      < ∙ ∙ ∙ G T Π ∙ ∙ >   [ ∙ ∙ G T ∙ ∙ Π ∙ ]
    𝛤  <--->  GΠΩ      < ∙ ∙ ∙ G ∙ Π ∙ Ω >   [ ∙ ∙ G ∙ Ω ∙ Π ∙ ]
    𝛯  <--->  TΠΩ      < ∙ ∙ ∙ ∙ T Π ∙ Ω >   [ ∙ ∙ ∙ T Ω ∙ Π ∙ ]
    𝘝  <--->  ACGΠ     < ∙ A C G ∙ Π ∙ ∙ >   [ A C G ∙ ∙ ∙ Π ∙ ]
    𝘏  <--->  ACTΠ     < ∙ A C ∙ T Π ∙ ∙ >   [ A C ∙ T ∙ ∙ Π ∙ ]
    𝘡  <--->  ACΠΩ     < ∙ A C ∙ ∙ Π ∙ Ω >   [ A C ∙ ∙ Ω ∙ Π ∙ ]
    𝘋  <--->  AGTΠ     < ∙ A ∙ G T Π ∙ ∙ >   [ A ∙ G T ∙ ∙ Π ∙ ]
    𝘟  <--->  AGΠΩ     < ∙ A ∙ G ∙ Π ∙ Ω >   [ A ∙ G ∙ Ω ∙ Π ∙ ]
    𝘘  <--->  ATΠΩ     < ∙ A ∙ ∙ T Π ∙ Ω >   [ A ∙ ∙ T Ω ∙ Π ∙ ]
    𝘉  <--->  CGTΠ     < ∙ ∙ C G T Π ∙ ∙ >   [ ∙ C G T ∙ ∙ Π ∙ ]
    𝘗  <--->  CGΠΩ     < ∙ ∙ C G ∙ Π ∙ Ω >   [ ∙ C G ∙ Ω ∙ Π ∙ ]
    𝘖  <--->  CTΠΩ     < ∙ ∙ C ∙ T Π ∙ Ω >   [ ∙ C ∙ T Ω ∙ Π ∙ ]
    𝘑  <--->  GTΠΩ     < ∙ ∙ ∙ G T Π ∙ Ω >   [ ∙ ∙ G T Ω ∙ Π ∙ ]
    𝘕  <--->  ACGTΠ    < ∙ A C G T Π ∙ ∙ >   [ A C G T ∙ ∙ Π ∙ ]
    𝘜  <--->  ACGΠΩ    < ∙ A C G ∙ Π ∙ Ω >   [ A C G ∙ Ω ∙ Π ∙ ]
    𝘐  <--->  ACTΠΩ    < ∙ A C ∙ T Π ∙ Ω >   [ A C ∙ T Ω ∙ Π ∙ ]
    𝘍  <--->  AGTΠΩ    < ∙ A ∙ G T Π ∙ Ω >   [ A ∙ G T Ω ∙ Π ∙ ]
    𝘓  <--->  CGTΠΩ    < ∙ ∙ C G T Π ∙ Ω >   [ ∙ C G T Ω ∙ Π ∙ ]
    𝘌  <--->  ACGTΠΩ   < ∙ A C G T Π ∙ Ω >   [ A C G T Ω ∙ Π ∙ ]
    𝜋  <--->  -Π       < - ∙ ∙ ∙ ∙ Π ∙ ∙ >   [ ∙ ∙ ∙ ∙ ∙ - Π ∙ ]
    𝘢  <--->  -AΠ      < - A ∙ ∙ ∙ Π ∙ ∙ >   [ A ∙ ∙ ∙ ∙ - Π ∙ ]
    𝘤  <--->  -CΠ      < - ∙ C ∙ ∙ Π ∙ ∙ >   [ ∙ C ∙ ∙ ∙ - Π ∙ ]
    𝘨  <--->  -GΠ      < - ∙ ∙ G ∙ Π ∙ ∙ >   [ ∙ ∙ G ∙ ∙ - Π ∙ ]
    𝘵  <--->  -TΠ      < - ∙ ∙ ∙ T Π ∙ ∙ >   [ ∙ ∙ ∙ T ∙ - Π ∙ ]
    𝜔  <--->  -ΠΩ      < - ∙ ∙ ∙ ∙ Π ∙ Ω >   [ ∙ ∙ ∙ ∙ Ω - Π ∙ ]
    𝘮  <--->  -ACΠ     < - A C ∙ ∙ Π ∙ ∙ >   [ A C ∙ ∙ ∙ - Π ∙ ]
    𝘳  <--->  -AGΠ     < - A ∙ G ∙ Π ∙ ∙ >   [ A ∙ G ∙ ∙ - Π ∙ ]
    𝘸  <--->  -ATΠ     < - A ∙ ∙ T Π ∙ ∙ >   [ A ∙ ∙ T ∙ - Π ∙ ]
    𝜆  <--->  -AΠΩ     < - A ∙ ∙ ∙ Π ∙ Ω >   [ A ∙ ∙ ∙ Ω - Π ∙ ]
    𝘴  <--->  -CGΠ     < - ∙ C G ∙ Π ∙ ∙ >   [ ∙ C G ∙ ∙ - Π ∙ ]
    𝘺  <--->  -CTΠ     < - ∙ C ∙ T Π ∙ ∙ >   [ ∙ C ∙ T ∙ - Π ∙ ]
    𝛿  <--->  -CΠΩ     < - ∙ C ∙ ∙ Π ∙ Ω >   [ ∙ C ∙ ∙ Ω - Π ∙ ]
    𝘬  <--->  -GTΠ     < - ∙ ∙ G T Π ∙ ∙ >   [ ∙ ∙ G T ∙ - Π ∙ ]
    𝛾  <--->  -GΠΩ     < - ∙ ∙ G ∙ Π ∙ Ω >   [ ∙ ∙ G ∙ Ω - Π ∙ ]
    𝜉  <--->  -TΠΩ     < - ∙ ∙ ∙ T Π ∙ Ω >   [ ∙ ∙ ∙ T Ω - Π ∙ ]
    𝘷  <--->  -ACGΠ    < - A C G ∙ Π ∙ ∙ >   [ A C G ∙ ∙ - Π ∙ ]
    𝘩  <--->  -ACTΠ    < - A C ∙ T Π ∙ ∙ >   [ A C ∙ T ∙ - Π ∙ ]
    𝘻  <--->  -ACΠΩ    < - A C ∙ ∙ Π ∙ Ω >   [ A C ∙ ∙ Ω - Π ∙ ]
    𝘥  <--->  -AGTΠ    < - A ∙ G T Π ∙ ∙ >   [ A ∙ G T ∙ - Π ∙ ]
    𝘹  <--->  -AGΠΩ    < - A ∙ G ∙ Π ∙ Ω >   [ A ∙ G ∙ Ω - Π ∙ ]
    𝘲  <--->  -ATΠΩ    < - A ∙ ∙ T Π ∙ Ω >   [ A ∙ ∙ T Ω - Π ∙ ]
    𝘣  <--->  -CGTΠ    < - ∙ C G T Π ∙ ∙ >   [ ∙ C G T ∙ - Π ∙ ]
    𝘱  <--->  -CGΠΩ    < - ∙ C G ∙ Π ∙ Ω >   [ ∙ C G ∙ Ω - Π ∙ ]
    𝘰  <--->  -CTΠΩ    < - ∙ C ∙ T Π ∙ Ω >   [ ∙ C ∙ T Ω - Π ∙ ]
    𝘫  <--->  -GTΠΩ    < - ∙ ∙ G T Π ∙ Ω >   [ ∙ ∙ G T Ω - Π ∙ ]
    𝘯  <--->  -ACGTΠ   < - A C G T Π ∙ ∙ >   [ A C G T ∙ - Π ∙ ]
    𝘶  <--->  -ACGΠΩ   < - A C G ∙ Π ∙ Ω >   [ A C G ∙ Ω - Π ∙ ]
    𝘪  <--->  -ACTΠΩ   < - A C ∙ T Π ∙ Ω >   [ A C ∙ T Ω - Π ∙ ]
    𝘧  <--->  -AGTΠΩ   < - A ∙ G T Π ∙ Ω >   [ A ∙ G T Ω - Π ∙ ]
    𝘭  <--->  -CGTΠΩ   < - ∙ C G T Π ∙ Ω >   [ ∙ C G T Ω - Π ∙ ]
    𝘦  <--->  -ACGTΠΩ  < - A C G T Π ∙ Ω >   [ A C G T Ω - Π ∙ ]
    𝚺  <--->  Σ        < ∙ ∙ ∙ ∙ ∙ ∙ Σ ∙ >   [ ∙ ∙ ∙ ∙ ∙ ∙ ∙ Σ ]
    𝐀  <--->  AΣ       < ∙ A ∙ ∙ ∙ ∙ Σ ∙ >   [ A ∙ ∙ ∙ ∙ ∙ ∙ Σ ]
    𝐂  <--->  CΣ       < ∙ ∙ C ∙ ∙ ∙ Σ ∙ >   [ ∙ C ∙ ∙ ∙ ∙ ∙ Σ ]
    𝐆  <--->  GΣ       < ∙ ∙ ∙ G ∙ ∙ Σ ∙ >   [ ∙ ∙ G ∙ ∙ ∙ ∙ Σ ]
    𝐓  <--->  TΣ       < ∙ ∙ ∙ ∙ T ∙ Σ ∙ >   [ ∙ ∙ ∙ T ∙ ∙ ∙ Σ ]
    𝛀  <--->  ΣΩ       < ∙ ∙ ∙ ∙ ∙ ∙ Σ Ω >   [ ∙ ∙ ∙ ∙ Ω ∙ ∙ Σ ]
    𝐌  <--->  ACΣ      < ∙ A C ∙ ∙ ∙ Σ ∙ >   [ A C ∙ ∙ ∙ ∙ ∙ Σ ]
    𝐑  <--->  AGΣ      < ∙ A ∙ G ∙ ∙ Σ ∙ >   [ A ∙ G ∙ ∙ ∙ ∙ Σ ]
    𝐖  <--->  ATΣ      < ∙ A ∙ ∙ T ∙ Σ ∙ >   [ A ∙ ∙ T ∙ ∙ ∙ Σ ]
    𝚲  <--->  AΣΩ      < ∙ A ∙ ∙ ∙ ∙ Σ Ω >   [ A ∙ ∙ ∙ Ω ∙ ∙ Σ ]
    𝐒  <--->  CGΣ      < ∙ ∙ C G ∙ ∙ Σ ∙ >   [ ∙ C G ∙ ∙ ∙ ∙ Σ ]
    𝐘  <--->  CTΣ      < ∙ ∙ C ∙ T ∙ Σ ∙ >   [ ∙ C ∙ T ∙ ∙ ∙ Σ ]
    𝚫  <--->  CΣΩ      < ∙ ∙ C ∙ ∙ ∙ Σ Ω >   [ ∙ C ∙ ∙ Ω ∙ ∙ Σ ]
    𝐊  <--->  GTΣ      < ∙ ∙ ∙ G T ∙ Σ ∙ >   [ ∙ ∙ G T ∙ ∙ ∙ Σ ]
    𝚪  <--->  GΣΩ      < ∙ ∙ ∙ G ∙ ∙ Σ Ω >   [ ∙ ∙ G ∙ Ω ∙ ∙ Σ ]
    𝚵  <--->  TΣΩ      < ∙ ∙ ∙ T Ω ∙ ∙ Σ >   [ ∙ ∙ ∙ ∙ T ∙ Σ Ω ]
    𝐕  <--->  ACGΣ     < A C G ∙ ∙ ∙ ∙ Σ >   [ ∙ A C G ∙ ∙ Σ ∙ ]
    𝐇  <--->  ACTΣ     < A C ∙ T ∙ ∙ ∙ Σ >   [ ∙ A C ∙ T ∙ Σ ∙ ]
    𝐙  <--->  ACΣΩ     < A C ∙ ∙ Ω ∙ ∙ Σ >   [ ∙ A C ∙ ∙ ∙ Σ Ω ]
    𝐃  <--->  AGTΣ     < A ∙ G T ∙ ∙ ∙ Σ >   [ ∙ A ∙ G T ∙ Σ ∙ ]
    𝐗  <--->  AGΣΩ     < A ∙ G ∙ Ω ∙ ∙ Σ >   [ ∙ A ∙ G ∙ ∙ Σ Ω ]
    𝐐  <--->  ATΣΩ     < A ∙ ∙ T Ω ∙ ∙ Σ >   [ ∙ A ∙ ∙ T ∙ Σ Ω ]
    𝐁  <--->  CGTΣ     < ∙ C G T ∙ ∙ ∙ Σ >   [ ∙ ∙ C G T ∙ Σ ∙ ]
    𝐏  <--->  CGΣΩ     < ∙ C G ∙ Ω ∙ ∙ Σ >   [ ∙ ∙ C G ∙ ∙ Σ Ω ]
    𝐎  <--->  CTΣΩ     < ∙ C ∙ T Ω ∙ ∙ Σ >   [ ∙ ∙ C ∙ T ∙ Σ Ω ]
    𝐉  <--->  GTΣΩ     < ∙ ∙ G T Ω ∙ ∙ Σ >   [ ∙ ∙ ∙ G T ∙ Σ Ω ]
    𝐍  <--->  ACGTΣ    < A C G T ∙ ∙ ∙ Σ >   [ ∙ A C G T ∙ Σ ∙ ]
    𝐔  <--->  ACGΣΩ    < A C G ∙ Ω ∙ ∙ Σ >   [ ∙ A C G ∙ ∙ Σ Ω ]
    𝐈  <--->  ACTΣΩ    < A C ∙ T Ω ∙ ∙ Σ >   [ ∙ A C ∙ T ∙ Σ Ω ]
    𝐅  <--->  AGTΣΩ    < A ∙ G T Ω ∙ ∙ Σ >   [ ∙ A ∙ G T ∙ Σ Ω ]
    𝐋  <--->  CGTΣΩ    < ∙ C G T Ω ∙ ∙ Σ >   [ ∙ ∙ C G T ∙ Σ Ω ]
    𝐄  <--->  ACGTΣΩ   < A C G T Ω ∙ ∙ Σ >   [ ∙ A C G T ∙ Σ Ω ]
    𝞂  <--->  -Σ       < ∙ ∙ ∙ ∙ ∙ - ∙ Σ >   [ - ∙ ∙ ∙ ∙ ∙ Σ ∙ ]
    𝐚  <--->  -AΣ      < A ∙ ∙ ∙ ∙ - ∙ Σ >   [ - A ∙ ∙ ∙ ∙ Σ ∙ ]
    𝐜  <--->  -CΣ      < ∙ C ∙ ∙ ∙ - ∙ Σ >   [ - ∙ C ∙ ∙ ∙ Σ ∙ ]
    𝐠  <--->  -GΣ      < ∙ ∙ G ∙ ∙ - ∙ Σ >   [ - ∙ ∙ G ∙ ∙ Σ ∙ ]
    𝐭  <--->  -TΣ      < ∙ ∙ ∙ T ∙ - ∙ Σ >   [ - ∙ ∙ ∙ T ∙ Σ ∙ ]
    𝞈  <--->  -ΣΩ      < ∙ ∙ ∙ ∙ Ω - ∙ Σ >   [ - ∙ ∙ ∙ ∙ ∙ Σ Ω ]
    𝐦  <--->  -ACΣ     < A C ∙ ∙ ∙ - ∙ Σ >   [ - A C ∙ ∙ ∙ Σ ∙ ]
    𝐫  <--->  -AGΣ     < A ∙ G ∙ ∙ - ∙ Σ >   [ - A ∙ G ∙ ∙ Σ ∙ ]
    𝐰  <--->  -ATΣ     < A ∙ ∙ T ∙ - ∙ Σ >   [ - A ∙ ∙ T ∙ Σ ∙ ]
    𝝺  <--->  -AΣΩ     < A ∙ ∙ ∙ Ω - ∙ Σ >   [ - A ∙ ∙ ∙ ∙ Σ Ω ]
    𝐬  <--->  -CGΣ     < ∙ C G ∙ ∙ - ∙ Σ >   [ - ∙ C G ∙ ∙ Σ ∙ ]
    𝐲  <--->  -CTΣ     < ∙ C ∙ T ∙ - ∙ Σ >   [ - ∙ C ∙ T ∙ Σ ∙ ]
    𝝳  <--->  -CΣΩ     < ∙ C ∙ ∙ Ω - ∙ Σ >   [ - ∙ C ∙ ∙ ∙ Σ Ω ]
    𝐤  <--->  -GTΣ     < ∙ ∙ G T ∙ - ∙ Σ >   [ - ∙ ∙ G T ∙ Σ ∙ ]
    𝝲  <--->  -GΣΩ     < ∙ ∙ G ∙ Ω - ∙ Σ >   [ - ∙ ∙ G ∙ ∙ Σ Ω ]
    𝝽  <--->  -TΣΩ     < ∙ ∙ ∙ T Ω - ∙ Σ >   [ - ∙ ∙ ∙ T ∙ Σ Ω ]
    𝐯  <--->  -ACGΣ    < A C G ∙ ∙ - ∙ Σ >   [ - A C G ∙ ∙ Σ ∙ ]
    𝐡  <--->  -ACTΣ    < A C ∙ T ∙ - ∙ Σ >   [ - A C ∙ T ∙ Σ ∙ ]
    𝐳  <--->  -ACΣΩ    < A C ∙ ∙ Ω - ∙ Σ >   [ - A C ∙ ∙ ∙ Σ Ω ]
    𝐝  <--->  -AGTΣ    < A ∙ G T ∙ - ∙ Σ >   [ - A ∙ G T ∙ Σ ∙ ]
    𝐱  <--->  -AGΣΩ    < A ∙ G ∙ Ω - ∙ Σ >   [ - A ∙ G ∙ ∙ Σ Ω ]
    𝐪  <--->  -ATΣΩ    < A ∙ ∙ T Ω - ∙ Σ >   [ - A ∙ ∙ T ∙ Σ Ω ]
    𝐛  <--->  -CGTΣ    < ∙ C G T ∙ - ∙ Σ >   [ - ∙ C G T ∙ Σ ∙ ]
    𝐩  <--->  -CGΣΩ    < ∙ C G ∙ Ω - ∙ Σ >   [ - ∙ C G ∙ ∙ Σ Ω ]
    𝐨  <--->  -CTΣΩ    < ∙ C ∙ T Ω - ∙ Σ >   [ - ∙ C ∙ T ∙ Σ Ω ]
    𝐣  <--->  -GTΣΩ    < ∙ ∙ G T Ω - ∙ Σ >   [ - ∙ ∙ G T ∙ Σ Ω ]
    𝐧  <--->  -ACGTΣ   < A C G T ∙ - ∙ Σ >   [ - A C G T ∙ Σ ∙ ]
    𝐮  <--->  -ACGΣΩ   < A C G ∙ Ω - ∙ Σ >   [ - A C G ∙ ∙ Σ Ω ]
    𝐢  <--->  -ACTΣΩ   < A C ∙ T Ω - ∙ Σ >   [ - A C ∙ T ∙ Σ Ω ]
    𝐟  <--->  -AGTΣΩ   < A ∙ G T Ω - ∙ Σ >   [ - A ∙ G T ∙ Σ Ω ]
    𝐥  <--->  -CGTΣΩ   < - ∙ C G T ∙ Σ Ω >   [ ∙ C G T Ω - ∙ Σ ]
    𝐞  <--->  -ACGTΣΩ  < - A C G T ∙ Σ Ω >   [ A C G T Ω - ∙ Σ ]
    𝜫  <--->  ΠΣ       < ∙ ∙ ∙ ∙ ∙ Π Σ ∙ >   [ ∙ ∙ ∙ ∙ ∙ ∙ Π Σ ]
    𝑨  <--->  AΠΣ      < ∙ A ∙ ∙ ∙ Π Σ ∙ >   [ A ∙ ∙ ∙ ∙ ∙ Π Σ ]
    𝑪  <--->  CΠΣ      < ∙ ∙ C ∙ ∙ Π Σ ∙ >   [ ∙ C ∙ ∙ ∙ ∙ Π Σ ]
    𝑮  <--->  GΠΣ      < ∙ ∙ ∙ G ∙ Π Σ ∙ >   [ ∙ ∙ G ∙ ∙ ∙ Π Σ ]
    𝑻  <--->  TΠΣ      < ∙ ∙ ∙ ∙ T Π Σ ∙ >   [ ∙ ∙ ∙ T ∙ ∙ Π Σ ]
    𝜴  <--->  ΠΣΩ      < ∙ ∙ ∙ ∙ ∙ Π Σ Ω >   [ ∙ ∙ ∙ ∙ Ω ∙ Π Σ ]
    𝑴  <--->  ACΠΣ     < ∙ A C ∙ ∙ Π Σ ∙ >   [ A C ∙ ∙ ∙ ∙ Π Σ ]
    𝑹  <--->  AGΠΣ     < ∙ A ∙ G ∙ Π Σ ∙ >   [ A ∙ G ∙ ∙ ∙ Π Σ ]
    𝑾  <--->  ATΠΣ     < ∙ A ∙ ∙ T Π Σ ∙ >   [ A ∙ ∙ T ∙ ∙ Π Σ ]
    𝜦  <--->  AΠΣΩ     < ∙ A ∙ ∙ ∙ Π Σ Ω >   [ A ∙ ∙ ∙ Ω ∙ Π Σ ]
    𝑺  <--->  CGΠΣ     < ∙ ∙ C G ∙ Π Σ ∙ >   [ ∙ C G ∙ ∙ ∙ Π Σ ]
    𝒀  <--->  CTΠΣ     < ∙ ∙ C ∙ T Π Σ ∙ >   [ ∙ C ∙ T ∙ ∙ Π Σ ]
    𝜟  <--->  CΠΣΩ     < ∙ ∙ C ∙ ∙ Π Σ Ω >   [ ∙ C ∙ ∙ Ω ∙ Π Σ ]
    𝑲  <--->  GTΠΣ     < ∙ ∙ ∙ G T Π Σ ∙ >   [ ∙ ∙ G T ∙ ∙ Π Σ ]
    𝜞  <--->  GΠΣΩ     < ∙ ∙ ∙ G ∙ Π Σ Ω >   [ ∙ ∙ G ∙ Ω ∙ Π Σ ]
    𝜩  <--->  TΠΣΩ     < ∙ ∙ ∙ ∙ T Π Σ Ω >   [ ∙ ∙ ∙ T Ω ∙ Π Σ ]
    𝑽  <--->  ACGΠΣ    < ∙ A C G ∙ Π Σ ∙ >   [ A C G ∙ ∙ ∙ Π Σ ]
    𝑯  <--->  ACTΠΣ    < ∙ A C ∙ T Π Σ ∙ >   [ A C ∙ T ∙ ∙ Π Σ ]
    𝒁  <--->  ACΠΣΩ    < ∙ A C ∙ ∙ Π Σ Ω >   [ A C ∙ ∙ Ω ∙ Π Σ ]
    𝑫  <--->  AGTΠΣ    < ∙ A ∙ G T Π Σ ∙ >   [ A ∙ G T ∙ ∙ Π Σ ]
    𝑿  <--->  AGΠΣΩ    < ∙ A ∙ G ∙ Π Σ Ω >   [ A ∙ G ∙ Ω ∙ Π Σ ]
    𝑸  <--->  ATΠΣΩ    < ∙ A ∙ ∙ T Π Σ Ω >   [ A ∙ ∙ T Ω ∙ Π Σ ]
    𝑩  <--->  CGTΠΣ    < ∙ ∙ C G T Π Σ ∙ >   [ ∙ C G T ∙ ∙ Π Σ ]
    𝑷  <--->  CGΠΣΩ    < ∙ ∙ C G ∙ Π Σ Ω >   [ ∙ C G ∙ Ω ∙ Π Σ ]
    𝑶  <--->  CTΠΣΩ    < ∙ ∙ C ∙ T Π Σ Ω >   [ ∙ C ∙ T Ω ∙ Π Σ ]
    𝑱  <--->  GTΠΣΩ    < ∙ ∙ ∙ G T Π Σ Ω >   [ ∙ ∙ G T Ω ∙ Π Σ ]
    𝑵  <--->  ACGTΠΣ   < ∙ A C G T Π Σ ∙ >   [ A C G T ∙ ∙ Π Σ ]
    𝑼  <--->  ACGΠΣΩ   < ∙ A C G ∙ Π Σ Ω >   [ A C G ∙ Ω ∙ Π Σ ]
    𝑰  <--->  ACTΠΣΩ   < ∙ A C ∙ T Π Σ Ω >   [ A C ∙ T Ω ∙ Π Σ ]
    𝑭  <--->  AGTΠΣΩ   < ∙ A ∙ G T Π Σ Ω >   [ A ∙ G T Ω ∙ Π Σ ]
    𝑳  <--->  CGTΠΣΩ   < ∙ ∙ C G T Π Σ Ω >   [ ∙ C G T Ω ∙ Π Σ ]
    𝑬  <--->  ACGTΠΣΩ  < ∙ A C G T Π Σ Ω >   [ A C G T Ω ∙ Π Σ ]
    𝞹  <--->  -ΠΣ      < - ∙ ∙ ∙ ∙ Π Σ ∙ >   [ ∙ ∙ ∙ ∙ ∙ - Π Σ ]
    𝐚  <--->  -AΠΣ     < - A ∙ ∙ ∙ Π Σ ∙ >   [ A ∙ ∙ ∙ ∙ - Π Σ ]
    𝒄  <--->  -CΠΣ     < - ∙ C ∙ ∙ Π Σ ∙ >   [ ∙ C ∙ ∙ ∙ - Π Σ ]
    𝒈  <--->  -GΠΣ     < - ∙ ∙ G ∙ Π Σ ∙ >   [ ∙ ∙ G ∙ ∙ - Π Σ ]
    𝒕  <--->  -TΠΣ     < - ∙ ∙ ∙ T Π Σ ∙ >   [ ∙ ∙ ∙ T ∙ - Π Σ ]
    𝟂  <--->  -ΠΣΩ     < - ∙ ∙ ∙ ∙ Π Σ Ω >   [ ∙ ∙ ∙ ∙ Ω - Π Σ ]
    𝒎  <--->  -ACΠΣ    < - A C ∙ ∙ Π Σ ∙ >   [ A C ∙ ∙ ∙ - Π Σ ]
    𝒓  <--->  -AGΠΣ    < - A ∙ G ∙ Π Σ ∙ >   [ A ∙ G ∙ ∙ - Π Σ ]
    𝒘  <--->  -ATΠΣ    < - A ∙ ∙ T Π Σ ∙ >   [ A ∙ ∙ T ∙ - Π Σ ]
    𝞴  <--->  -AΠΣΩ    < - A ∙ ∙ ∙ Π Σ Ω >   [ A ∙ ∙ ∙ Ω - Π Σ ]
    𝒔  <--->  -CGΠΣ    < - ∙ C G ∙ Π Σ ∙ >   [ ∙ C G ∙ ∙ - Π Σ ]
    𝒚  <--->  -CTΠΣ    < - ∙ C ∙ T Π Σ ∙ >   [ ∙ C ∙ T ∙ - Π Σ ]
    𝞭  <--->  -CΠΣΩ    < - ∙ C ∙ ∙ Π Σ Ω >   [ ∙ C ∙ ∙ Ω - Π Σ ]
    𝒌  <--->  -GTΠΣ    < - ∙ ∙ G T Π Σ ∙ >   [ ∙ ∙ G T ∙ - Π Σ ]
    𝞬  <--->  -GΠΣΩ    < - ∙ ∙ G ∙ Π Σ Ω >   [ ∙ ∙ G ∙ Ω - Π Σ ]
    𝞷  <--->  -TΠΣΩ    < - ∙ ∙ ∙ T Π Σ Ω >   [ ∙ ∙ ∙ T Ω - Π Σ ]
    𝒗  <--->  -ACGΠΣ   < - A C G ∙ Π Σ ∙ >   [ A C G ∙ ∙ - Π Σ ]
    𝒉  <--->  -ACTΠΣ   < - A C ∙ T Π Σ ∙ >   [ A C ∙ T ∙ - Π Σ ]
    𝒛  <--->  -ACΠΣΩ   < - A C ∙ ∙ Π Σ Ω >   [ A C ∙ ∙ Ω - Π Σ ]
    𝒅  <--->  -AGTΠΣ   < - A ∙ G T Π Σ ∙ >   [ A ∙ G T ∙ - Π Σ ]
    𝒙  <--->  -AGΠΣΩ   < - A ∙ G ∙ Π Σ Ω >   [ A ∙ G ∙ Ω - Π Σ ]
    𝒒  <--->  -ATΠΣΩ   < - A ∙ ∙ T Π Σ Ω >   [ A ∙ ∙ T Ω - Π Σ ]
    𝒃  <--->  -CGTΠΣ   < - ∙ C G T Π Σ ∙ >   [ ∙ C G T ∙ - Π Σ ]
    𝒑  <--->  -CGΠΣΩ   < - ∙ C G ∙ Π Σ Ω >   [ ∙ C G ∙ Ω - Π Σ ]
    𝒐  <--->  -CTΠΣΩ   < - ∙ C ∙ T Π Σ Ω >   [ ∙ C ∙ T Ω - Π Σ ]
    𝒋  <--->  -GTΠΣΩ   < - ∙ ∙ G T Π Σ Ω >   [ ∙ ∙ G T Ω - Π Σ ]
    𝒏  <--->  -ACGTΠΣ  < - A C G T Π Σ ∙ >   [ A C G T ∙ - Π Σ ]
    𝒖  <--->  -ACGΠΣΩ  < - A C G ∙ Π Σ Ω >   [ A C G ∙ Ω - Π Σ ]
    𝒊  <--->  -ACTΠΣΩ  < - A C ∙ T Π Σ Ω >   [ A C ∙ T Ω - Π Σ ]
    𝒇  <--->  -AGTΠΣΩ  < - A ∙ G T Π Σ Ω >   [ A ∙ G T Ω - Π Σ ]
    𝒍  <--->  -CGTΠΣΩ  < - ∙ C G T Π Σ Ω >   [ ∙ C G T Ω - Π Σ ]
    𝒆  <--->  -ACGTΠΣΩ < - A C G T Π Σ Ω >   [ A C G T Ω - Π Σ ]
@
-}
renderSlimStateChar ∷ SlimState → Char
renderSlimStateChar =
    let (pref, suff) = splitAt (atSymbolIndex gapIndex) $ pure <$> "ACGTΠΣΩ"

        alpha ∷ Alphabet String
        alpha = fromSymbols . NE.fromList $ pref <> ["-"] <> suff

        mapBase32 ∷ [(Char, String)]
        mapBase32 =
            [ ('A', "A")
            , ('B', "CGT")
            , ('C', "C")
            , ('D', "AGT")
            , ('E', "ACGTΩ")
            , ('F', "AGTΩ")
            , ('G', "G")
            , ('H', "ACT")
            , ('I', "ACTΩ")
            , ('J', "GTΩ")
            , ('K', "GT")
            , ('L', "CGTΩ")
            , ('M', "AC")
            , ('N', "ACGT")
            , ('O', "CTΩ")
            , ('P', "CGΩ")
            , ('Q', "ATΩ")
            , ('R', "AG")
            , ('S', "CG")
            , ('T', "T")
            , ('U', "ACGΩ")
            , ('V', "ACG")
            , ('W', "AT")
            , ('X', "AGΩ")
            , ('Y', "CT")
            , ('Z', "ACΩ")
            , ('Γ', "GΩ")
            , ('Δ', "CΩ")
            , ('Λ', "AΩ")
            , ('Ξ', "TΩ")
            , ('Ω', "Ω")
            ]

        combos ∷ [(CharChange, CharChange, CharChange)]
        combos = do
            casing ← [Nothing, Just ('-', toLower')]
            italic ← [Nothing, Just ('Π', toItalic)]
            strong ← [Nothing, Just ('Σ', toStrong)]
            pure (casing, italic, strong)

        applyCombo
            ∷ (CharChange, CharChange, CharChange)
            → [(Char, String)]
            → [(Char, String)]
        applyCombo (casing, italic, strong) =
            let compose ∷ (Foldable f) ⇒ f (a → a) → a → a
                compose = foldr (flip (.)) id

                combine ∷ CharChange → CharChange → CharChange → [(Char, Char → Char)]
                combine x y z = toList x <> toList y <> toList z

                options ∷ [(Char, Char → Char)]
                options = combine casing italic strong

                prefix ∷ (Char, String)
                prefix =
                    let run ∷ ((Char, String), Char → Char) → (Char, String)
                        run ((c, str), f) = (f c, str)

                        get ∷ [(Char, Char → Char)] → ((Char, String), Char → Char)
                        get = (select `bimap` compose) . unzip
                    in  run . get $ options

                select ∷ String → (Char, String)
                select =
                    let f ∷ (Foldable t) ⇒ t Char → Char
                        f x
                            | 'Π' `elem` x = 'Π'
                            | 'Σ' `elem` x = 'Σ'
                            | '-' `elem` x = '-'
                            | otherwise = '█'
                    in  f &&& id

                keyChange ∷ Char → Char
                keyChange = compose $ maybe id snd <$> [casing, italic, strong]

                valChange ∷ String → String
                valChange =
                    let extra = foldMap (fmap fst . toList) [casing, italic, strong]
                    in  (<> extra)
            in  (prefix :) . fmap (keyChange `bimap` valChange)

        mapParts ∷ [(Char, String)]
        mapParts = foldMap (`applyCombo` mapBase32) combos

        mapFull ∷ Bimap Char (Set String)
        mapFull =
            let vSet ∷ String → Set String
                vSet = Set.fromList . fmap pure
            in  BM.fromList $ second vSet <$> mapParts
    in  (mapFull !>) . decodeState alpha


toLower' ∷ Char → Char
toLower' = \case
    'Γ' → 'γ'
    'Δ' → 'δ'
    'Λ' → 'λ'
    'Ξ' → 'ξ'
    'Π' → 'π'
    'Σ' → 'σ'
    'Ω' → 'ω'
    c → toLower c


toItalic ∷ Char → Char
toItalic = \case
    -- Upper
    'A' → '𝘈'
    'B' → '𝘉'
    'C' → '𝘊'
    'D' → '𝘋'
    'E' → '𝘌'
    'F' → '𝘍'
    'G' → '𝘎'
    'H' → '𝘏'
    'I' → '𝘐'
    'J' → '𝘑'
    'K' → '𝘒'
    'L' → '𝘓'
    'M' → '𝘔'
    'N' → '𝘕'
    'O' → '𝘖'
    'P' → '𝘗'
    'Q' → '𝘘'
    'R' → '𝘙'
    'S' → '𝘚'
    'T' → '𝘛'
    'U' → '𝘜'
    'V' → '𝘝'
    'W' → '𝘞'
    'X' → '𝘟'
    'Y' → '𝘠'
    'Z' → '𝘡'
    'Γ' → '𝛤'
    'Δ' → '𝛥'
    'Λ' → '𝛬'
    'Ξ' → '𝛯'
    'Π' → '𝛱'
    'Σ' → '𝛴'
    'Ω' → '𝛺'
    -- Lower
    'a' → '𝘢'
    'b' → '𝘣'
    'c' → '𝘤'
    'd' → '𝘥'
    'e' → '𝘦'
    'f' → '𝘧'
    'g' → '𝘨'
    'h' → '𝘩'
    'i' → '𝘪'
    'j' → '𝘫'
    'k' → '𝘬'
    'l' → '𝘭'
    'm' → '𝘮'
    'n' → '𝘯'
    'o' → '𝘰'
    'p' → '𝘱'
    'q' → '𝘲'
    'r' → '𝘳'
    's' → '𝘴'
    't' → '𝘵'
    'u' → '𝘶'
    'v' → '𝘷'
    'w' → '𝘸'
    'x' → '𝘹'
    'y' → '𝘺'
    'z' → '𝘻'
    'γ' → '𝛾'
    'δ' → '𝛿'
    'λ' → '𝜆'
    'ξ' → '𝜉'
    'π' → '𝜋'
    'ς' → '𝜍'
    'σ' → '𝜎'
    'ω' → '𝜔'
    c → c


toStrong ∷ Char → Char
toStrong = \case
    -- Upper
    'A' → '𝐀'
    'B' → '𝐁'
    'C' → '𝐂'
    'D' → '𝐃'
    'E' → '𝐄'
    'F' → '𝐅'
    'G' → '𝐆'
    'H' → '𝐇'
    'I' → '𝐈'
    'J' → '𝐉'
    'K' → '𝐊'
    'L' → '𝐋'
    'M' → '𝐌'
    'N' → '𝐍'
    'O' → '𝐎'
    'P' → '𝐏'
    'Q' → '𝐐'
    'R' → '𝐑'
    'S' → '𝐒'
    'T' → '𝐓'
    'U' → '𝐔'
    'V' → '𝐕'
    'W' → '𝐖'
    'X' → '𝐗'
    'Y' → '𝐘'
    'Z' → '𝐙'
    'Γ' → '𝚪'
    'Δ' → '𝚫'
    'Λ' → '𝚲'
    'Ξ' → '𝚵'
    'Π' → '𝚷'
    'Σ' → '𝚺'
    'Ω' → '𝛀'
    -- Lower
    'a' → '𝐚'
    'b' → '𝐛'
    'c' → '𝐜'
    'd' → '𝐝'
    'e' → '𝐞'
    'f' → '𝐟'
    'g' → '𝐠'
    'h' → '𝐡'
    'i' → '𝐢'
    'j' → '𝐣'
    'k' → '𝐤'
    'l' → '𝐥'
    'm' → '𝐦'
    'n' → '𝐧'
    'o' → '𝐨'
    'p' → '𝐩'
    'q' → '𝐪'
    'r' → '𝐫'
    's' → '𝐬'
    't' → '𝐭'
    'u' → '𝐮'
    'v' → '𝐯'
    'w' → '𝐰'
    'x' → '𝐱'
    'y' → '𝐲'
    'z' → '𝐳'
    'γ' → '𝝲'
    'δ' → '𝝳'
    'λ' → '𝝺'
    'ξ' → '𝝽'
    'π' → '𝝿'
    'ς' → '𝞁'
    'σ' → '𝞂'
    'ω' → '𝞈'
    -- Upper Italic
    '𝘈' → '𝑨'
    '𝘉' → '𝑩'
    '𝘊' → '𝑪'
    '𝘋' → '𝑫'
    '𝘌' → '𝑬'
    '𝘍' → '𝑭'
    '𝘎' → '𝑮'
    '𝘏' → '𝑯'
    '𝘐' → '𝑰'
    '𝘑' → '𝑱'
    '𝘒' → '𝑲'
    '𝘓' → '𝑳'
    '𝘔' → '𝑴'
    '𝘕' → '𝑵'
    '𝘖' → '𝑶'
    '𝘗' → '𝑷'
    '𝘘' → '𝑸'
    '𝘙' → '𝑹'
    '𝘚' → '𝑺'
    '𝘛' → '𝑻'
    '𝘜' → '𝑼'
    '𝘝' → '𝑽'
    '𝘞' → '𝑾'
    '𝘟' → '𝑿'
    '𝘠' → '𝒀'
    '𝘡' → '𝒁'
    '𝛤' → '𝜞'
    '𝛥' → '𝜟'
    '𝛬' → '𝜦'
    '𝛯' → '𝜩'
    '𝛱' → '𝜫'
    '𝛴' → '𝜮'
    '𝛺' → '𝜴'
    -- Lower Italic
    '𝘢' → '𝙖'
    '𝘣' → '𝒃'
    '𝘤' → '𝒄'
    '𝘥' → '𝒅'
    '𝘦' → '𝒆'
    '𝘧' → '𝒇'
    '𝘨' → '𝒈'
    '𝘩' → '𝒉'
    '𝘪' → '𝒊'
    '𝘫' → '𝒋'
    '𝘬' → '𝒌'
    '𝘭' → '𝒍'
    '𝘮' → '𝒎'
    '𝘯' → '𝒏'
    '𝘰' → '𝒐'
    '𝘱' → '𝒑'
    '𝘲' → '𝒒'
    '𝘳' → '𝒓'
    '𝘴' → '𝒔'
    '𝘵' → '𝒕'
    '𝘶' → '𝒖'
    '𝘷' → '𝒗'
    '𝘸' → '𝒘'
    '𝘹' → '𝒙'
    '𝘺' → '𝒚'
    '𝘻' → '𝒛'
    '𝛾' → '𝞬'
    '𝛿' → '𝞭'
    '𝜆' → '𝞴'
    '𝜉' → '𝞷'
    '𝜋' → '𝞹'
    '𝜍' → '𝞻'
    '𝜎' → '𝞼'
    '𝜔' → '𝟂'
    c → c

{--------------------------------------------------------------------------}
{- WideState: alphabets of 9-64 symbols                                    -}
{--------------------------------------------------------------------------}

{- |
Encoding for a dynamic character element with an alphabet size \( \lvert\, \Sigma \,\rvert \in \left[\;9,\, 64\;\right] \).
-}
newtype WideState = WideState {fromWideState ∷ Word64}


newtype instance UV.MVector s WideState = MV_WideState (PV.MVector s Word64)


newtype instance UV.Vector WideState = V_WideState (PV.Vector Word64)


instance UV.Unbox WideState


deriving newtype instance Bits WideState


deriving newtype instance Bounded WideState


deriving newtype instance Enum WideState


deriving newtype instance Eq WideState


deriving newtype instance FiniteBits WideState


deriving newtype instance Hashable WideState


deriving newtype instance Integral WideState


deriving newtype instance Ix WideState


deriving newtype instance NFData WideState


deriving newtype instance Num WideState


deriving newtype instance Ord WideState


deriving newtype instance Real WideState


deriving newtype instance Storable WideState


instance Show WideState where
    show (WideState 0) = "∅"
    show (WideState v) =
        let symbolsBase64 ∷ Alphabet String
            symbolsBase64 = fromSymbols $ pure <$> symbolList

            -- Use Unicode sans-serif font for all wide individual states to
            -- differentiate them from other forms of rendering.
            symbolList ∷ NonEmpty Char
            symbolList = '-' :| ['𝟢' .. '𝟫'] <> ['𝖠' .. '𝖹'] <> ['𝖺' .. '𝗓'] <> ['Ω']

            render ∷ NonEmpty String → String
            render input@(x :| xs) = case xs of
                [] → x
                _ → '[' : fold input <> "]"
        in  render $ decodeState symbolsBase64 v


instance StateOfAmbiguity WideState where
    toBits state =
        let n = finiteBitSize state
        in  List.fromList $ foldMap (pure . (state `testBit`)) [0 .. n - 1]


    fromBits =
        let f ∷ Bool → (Word64, Int) → (Word64, Int)
            f b
                | b = \(a, i) → (a `setBit` i, i + 1)
                | otherwise = fmap succ
        in  coerce . fst . foldr f (0, 0) . List.toList


    fromNumber !dimValue !intValue =
        let size = min (fromEnum dimValue) $ finiteBitSize (undefined ∷ WideState)
            mask = 1 `shiftL` size
            bits ∷ Word64
            bits = fromIntegral $ toInteger intValue `mod` mask
        in  coerce bits


    toUnsignedNumber = fromIntegral . (coerce ∷ WideState → Word64)


instance MGV.MVector UV.MVector WideState where
    {-# INLINE basicLength #-}
    basicLength (MV_WideState v) = MGV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (MV_WideState v) = MV_WideState $ MGV.basicUnsafeSlice i n v


    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_WideState v1) (MV_WideState v2) = MGV.basicOverlaps v1 v2


    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MV_WideState <$> MGV.basicUnsafeNew n


    {-# INLINE basicInitialize #-}
    basicInitialize (MV_WideState v) = MGV.basicInitialize v


    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = MV_WideState <$> MGV.basicUnsafeReplicate n (fromWideState x)


    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_WideState v) i = WideState <$> MGV.basicUnsafeRead v i


    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_WideState v) i x = MGV.basicUnsafeWrite v i (fromWideState x)


    {-# INLINE basicClear #-}
    basicClear (MV_WideState v) = MGV.basicClear v


    {-# INLINE basicSet #-}
    basicSet (MV_WideState v) x = MGV.basicSet v (fromWideState x)


    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_WideState v1) (MV_WideState v2) = MGV.basicUnsafeCopy v1 v2


    basicUnsafeMove (MV_WideState v1) (MV_WideState v2) = MGV.basicUnsafeMove v1 v2


    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_WideState v) n = MV_WideState <$> MGV.basicUnsafeGrow v n


instance GV.Vector UV.Vector WideState where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_WideState v) = V_WideState <$> GV.basicUnsafeFreeze v


    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_WideState v) = MV_WideState <$> GV.basicUnsafeThaw v


    {-# INLINE basicLength #-}
    basicLength (V_WideState v) = GV.basicLength v


    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (V_WideState v) = V_WideState $ GV.basicUnsafeSlice i n v


    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_WideState v) i = WideState <$> GV.basicUnsafeIndexM v i


    basicUnsafeCopy (MV_WideState mv) (V_WideState v) = GV.basicUnsafeCopy mv v


    {-# INLINE elemseq #-}
    elemseq = const seq

{--------------------------------------------------------------------------}
{- HugeState: alphabets of 65+ symbols                                     -}
{--------------------------------------------------------------------------}

{- |
Encoding for a dynamic character element with an alphabet size \( \lvert\, \Sigma \,\rvert \in \left[\;65,\, \infty\;\right) \).
-}
newtype HugeState = HugeState {fromHugeState ∷ UV.Vector Bit}


deriving newtype instance Bits HugeState


deriving newtype instance Eq HugeState


instance FiniteBits HugeState where
    finiteBitSize = UV.length . fromHugeState


instance Hashable HugeState where
    hashWithSalt salt = unsafeCoerce . UV.foldr xor (unsafeCoerce salt) . cloneToWords . fromHugeState


deriving newtype instance NFData HugeState


deriving newtype instance Ord HugeState


instance Show HugeState where
    show =
        let showBit (Bit True) = "1"
            showBit _ = "0"

            enclose ∷ String → String
            enclose x = '[' : x <> "]"
        in  enclose . UV.foldMap showBit . fromHugeState


instance StateOfAmbiguity HugeState where
    toBits = List.fromList . fmap coerce . UV.toList . coerce


    fromBits = coerce . UV.fromList . fmap Bit . List.toList


    fromNumber !dimValue !intValue =
        let n = fromEnum dimValue
            v = toInteger intValue
        in  coerce . UV.generate n $ \i → Bit $ v `testBit` i


    toUnsignedNumber =
        let gen ∷ Int → Word → Integer → Integer
            gen i e =
                let off = i * 64
                    val = toInteger e
                in  (+) (val `shiftL` off)
        in  fromInteger . UV.ifoldr' gen 0 . cloneToWords . coerce
