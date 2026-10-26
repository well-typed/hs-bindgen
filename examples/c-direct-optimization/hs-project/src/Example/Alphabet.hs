{- |
The public interface of the @alphabet@ library.

/NOTE:/ This module was not among the sources placed in @lib\/Data\/Alphabet@;
only @Example.Alphabet.Internal@ and @Example.Alphabet.Gap@ were.  It is
reconstructed here as the re-export surface that the consuming modules
(@Bio.DynamicCharacter@, @Bio.DynamicCharacter.Element.SlimState@,
@Bio.DynamicCharacter.Element.WideState@) import.  If the genuine module
exports more, prefer the genuine one.
-}
module Example.Alphabet (
    -- * Data-type
    Alphabet (),

    -- * Constructors
    fromSymbols,
    fromSymbolsWithStateNames,

    -- * Accessors
    alphabetStateNames,
    alphabetSymbols,
    gapIndex,
    gapSymbol,
    symbolSet,

    -- * Subsetting
    getSubsetIndex,
    getSubsetIndices,
) where

import Example.Alphabet.Internal
