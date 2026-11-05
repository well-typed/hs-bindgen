{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Encoding and decoding of character states relative to an 'Alphabet'.

/NOTE:/ Like "Example.Alphabet", this module was not among the sources placed in
@lib\/Data\/Alphabet@.  It is reconstructed from its use-sites:

  * @Bio.DynamicCharacter.encodeDynamicCharacter@ calls
    @encodeState alphabet f symbols@ where @f :: Word -> e@ builds an empty
    element of the right width.

  * @Bio.DynamicCharacter.Element.SlimState.renderSlimStateChar@ calls
    @decodeState alpha@ at result type @Set String@.

  * @Bio.DynamicCharacter.Element.WideState@'s 'Show' instance calls
    @decodeState symbolsBase64@ at result type @NonEmpty String@.

The differing result types are why 'decodeState' is polymorphic via 'IsList'.
-}
module Example.Alphabet.Codec (
    encodeState,
    decodeState,
) where

import Example.Alphabet.Internal
import Data.Bits (Bits (..))
import Data.Foldable (toList)
import Data.Set qualified as Set
import GHC.Exts (IsList (Item, fromList))
import Example.Measure (HasSymbolCount (..), SymbolCount (..))


{- |
Encode a collection of symbols as a single, ambiguous character state.

The supplied function builds the "empty" state of the correct width from the
alphabet's symbol count; the bit for each present symbol is then set.
-}
{-# INLINEABLE encodeState #-}
encodeState
    :: ( Bits b
       , Foldable t
       , Ord a
       )
    => Alphabet a
    -- ^ Alphabet of symbols
    -> (Word -> b)
    -- ^ Constructor for an empty state, taking the alphabet size
    -> t a
    -- ^ Ambiguity group of symbols
    -> b
encodeState alphabet f symbols =
    let SymbolCount n = symbolCount alphabet
    in  getSubsetIndex alphabet (Set.fromList (toList symbols)) (f n)


{- |
Decode an ambiguous character state into the collection of symbols it denotes.

/NOTE:/ Decoding the empty state at a non-empty result type (such as
'Data.List.NonEmpty.NonEmpty') will throw, exactly as 'fromList' does; callers
are expected to special-case the empty state, as @WideState@'s 'Show' instance
does.
-}
{-# INLINEABLE decodeState #-}
decodeState
    :: ( Bits b
       , IsList (f a)
       , Item (f a) ~ a
       )
    => Alphabet a
    -- ^ Alphabet of symbols
    -> b
    -- ^ State to decode
    -> f a
decodeState alphabet state =
    fromList
        [ symbol
        | (i, symbol) <- zip [0 ..] (toList alphabet)
        , state `testBit` i
        ]
