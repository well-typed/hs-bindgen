{- |
A command line harness for the replicated string-alignment API.

Takes two IUPAC DNA sequences, encodes them as dynamic characters, aligns them
with 'smallAlphabetPairwiseDO' -- the API entry point which calls
@DO.cAlign2D@ across the FFI -- and prints the three rows of the resulting
alignment context followed by its cost.
-}
module Main (main) where

import Example.CharacterSequence (
    AlignmentContext (..),
    SlimDynamicCharacter,
    characterLength,
    encodeDynamicCharacter,
    getContext,
 )
import Example.CharacterElement (SlimState, renderSlimStateChar)
import Example.Alphabet (Alphabet, fromSymbols)
import Example.Alphabet.IUPAC (iupacToDna)
import Data.Bimap qualified as BM
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Storable qualified as SV
import CostMatrix (TransitionMatrix, initialize)
import Example.Measure
import StringAlignment (smallAlphabetPairwiseDO)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


{--------------------------------------------------------------------------}
{- The alphabet                                                            -}
{--------------------------------------------------------------------------}

{- |
The DNA alphabet, with the gap symbol at index 0.

The position of the gap is not arbitrary: it must agree with
'Example.Alphabet.Gap.gapIndex' and with the C library, which sets
@gap_char = 1@ (i.e. bit 0) in @cm_alloc_2d@.
-}
dnaAlphabet :: Alphabet String
dnaAlphabet = fromSymbols $ gapSymbol :| baseSymbols


gapSymbol :: String
gapSymbol = "-"


baseSymbols :: [String]
baseSymbols = ["A", "C", "G", "T"]


alphabetSize :: Word
alphabetSize = toEnum $ 1 + length baseSymbols


{--------------------------------------------------------------------------}
{- Transition costs                                                        -}
{--------------------------------------------------------------------------}

{- |
A unit-cost metric: zero along the diagonal, one everywhere else. Row-major,
indexed by cardinal symbol index (/not/ by bit encoding).

Change this vector to explore other cost regimes; the gap row and column are
index 0.
-}
tcmVector :: SV.Vector Word
tcmVector =
    SV.fromList
        [ if i == j then 0 else 1
        | i <- [0 .. alphabetSize - 1]
        , j <- [0 .. alphabetSize - 1]
        ]


{- |
Build the compact transition cost matrix across the FFI.

A gap-open penalty of zero selects the linear (non-affine) alignment strategy;
the affine path is not wired up. See 'StringAlignment.affineUnsupported'.
-}
buildTCM :: TransitionMatrix
buildTCM = initialize (SymbolCount alphabetSize) (SymbolDistance 0) tcmVector


{--------------------------------------------------------------------------}
{- IUPAC parsing                                                           -}
{--------------------------------------------------------------------------}

{- |
Decode one IUPAC character into its ambiguity group.

The table itself lives in "Example.Alphabet.IUPAC"; this only adapts its
@Bimap (NonEmpty String) (NonEmpty String)@ to the 'Set' of symbols that
'Example.Alphabet.Internal.fromSymbols' expects. An /uppercase/ code denotes the
bases alone, the corresponding /lowercase/ code those bases together with the
gap symbol, @\'-\'@ is the gap alone and @\'?\'@ is fully ambiguous.
-}
iupacSymbols :: Char -> Maybe (Set String)
iupacSymbols c =
    let code :: NonEmpty String
        code = pure [c]
    in  Set.fromList . NE.toList <$> BM.lookup code iupacToDna


{- |
Decode a whole sequence, reporting the offending position on failure.
-}
parseSequence :: String -> String -> Either String [Set String]
parseSequence label = traverse step . zip [1 :: Int ..]
    where
        step (i, c) = case iupacSymbols c of
            Just symbols -> Right symbols
            Nothing ->
                Left $
                    concat
                        [ label
                        , ": unrecognized IUPAC character "
                        , show c
                        , " at position "
                        , show i
                        ]


encode :: [Set String] -> SlimDynamicCharacter
encode = encodeDynamicCharacter dnaAlphabet (const 0 :: Word -> SlimState)


{--------------------------------------------------------------------------}
{- Rendering                                                               -}
{--------------------------------------------------------------------------}

{- |
The void character, marking a position where a character has no state because
the alignment inserted or deleted there.

/NOTE:/ This is deliberately distinct from @\'-\'@, which renders an actual
gap /state/ present in the data.
-}
voidChar :: Char
voidChar = '\9608' -- '█'


{- |
Project the alignment context into its three rows.

The triple's components are structurally named "lesser" and "longer" because
the alignment orders its inputs by length internally. By the time
'directOptimization' returns, however, it has transposed the result back, so
the first row always corresponds to the /first/ argument and the third row to
the /second/ -- regardless of which was longer.
-}
alignmentRows :: SlimDynamicCharacter -> (String, String, String)
alignmentRows char =
    let renderIndex i = case char `getContext` i of
            Gapped -> (voidChar, voidChar, voidChar)
            Delete med rhs -> (voidChar, renderSlimStateChar med, renderSlimStateChar rhs)
            Insert lhs med -> (renderSlimStateChar lhs, renderSlimStateChar med, voidChar)
            Merged lhs med rhs ->
                (renderSlimStateChar lhs, renderSlimStateChar med, renderSlimStateChar rhs)

        indices = [0 .. fromEnum (characterLength char) - 1]
    in  unzip3 $ renderIndex <$> indices


{--------------------------------------------------------------------------}
{- Entry point                                                             -}
{--------------------------------------------------------------------------}

usage :: String -> String
usage progName =
    unlines
        [ "Usage: " <> progName <> " <SEQUENCE-1> <SEQUENCE-2>"
        , ""
        , "  Aligns two IUPAC DNA sequences via the C FFI and prints the result."
        , ""
        , "  Each character is an ambiguity group:"
        , "    A C G T        unambiguous bases"
        , "    R Y S W K M    two-base ambiguities"
        , "    B D H V        three-base ambiguities"
        , "    N              any base"
        , "    -              gap"
        , "    ?              any base or gap"
        , ""
        , "  The lowercase form of any code above adds the gap state."
        , ""
        , "Example:"
        , "  " <> progName <> " ACGTRYKM ACTTRYK"
        ]


main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [lhsRaw, rhsRaw] -> run progName lhsRaw rhsRaw
        _ -> die progName "expected exactly two sequence arguments"


die :: String -> String -> IO a
die progName message = do
    hPutStrLn stderr $ progName <> ": " <> message
    hPutStrLn stderr ""
    hPutStrLn stderr $ usage progName
    exitFailure


run :: String -> String -> String -> IO ()
run progName lhsRaw rhsRaw = do
    lhsSymbols <- either (die progName) pure $ parseSequence "SEQUENCE-1" lhsRaw
    rhsSymbols <- either (die progName) pure $ parseSequence "SEQUENCE-2" rhsRaw

    case (lhsSymbols, rhsSymbols) of
        ([], _) -> die progName "SEQUENCE-1 is empty"
        (_, []) -> die progName "SEQUENCE-2 is empty"
        _ -> pure ()

    let tcm = buildTCM
        lhs = encode lhsSymbols
        rhs = encode rhsSymbols

        -- The FFI call lives inside `smallAlphabetPairwiseDO`.
        (AlignmentCost cost, aligned) = smallAlphabetPairwiseDO tcm lhs rhs

        (fstRow, medianRow, sndRow) = alignmentRows aligned

        label :: String -> String -> String
        label name row = "  " <> name <> "  " <> row

    putStrLn $ label "seq-1 " fstRow
    putStrLn $ label "median" medianRow
    putStrLn $ label "seq-2 " sndRow
    putStrLn ""
    putStrLn $ "Alignment cost: " <> show cost

    -- The void marker is not self-explanatory; describe it only when present.
    if any (voidChar `elem`) [fstRow, medianRow, sndRow]
        then
            putStrLn $
                "\n  (" <> [voidChar] <> " marks a position where that sequence has no state;"
                    <> " '-' is a gap state present in the data.)"
        else pure ()
