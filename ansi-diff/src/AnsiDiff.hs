module AnsiDiff (
    ansidiff,
    -- * Implementation bits
    metric,
) where

import Control.Monad (forM_)
import Data.Align (alignWith)
import Data.Foldable (foldl')
import Data.Primitive.PrimArray ()
import Data.These (These (..))
import Control.Monad.Trans.State.Strict qualified as S
import System.Console.ANSI qualified as ANSI

import Data.Algorithm.Diff qualified as Diff

ansidiff :: String -> String -> String
ansidiff old new = runStringWriter $ do
    writeStr ansiReset
    let linesDiff = Diff.getDiff (lines old) (lines new)
    forM_ (chunks linesDiff) $ \case
        Same xs _  -> forM_ xs $ \x -> writeStrLn $ ' ' : x
        Diff xs ys -> ansiDiff' xs ys

ansiDiff' :: [String] -> [String] -> StringWriter ()
ansiDiff' old new = do
    -- removed
    forM_ diffs $ \case
        Ins x      -> writeStrLn $ ansiRed ++ '-' : x ++ ansiReset
        Del _      -> return ()
        Swp n d ds
            -- empasise line diffs only if there are similar enough.
            | 2 * n < d -> removedLine ds
            | otherwise -> removedLine' ds

    -- added
    forM_ diffs $ \case
        Ins _ -> return ()
        Del y -> writeStrLn $ ansiGreen ++ '+' : y ++ ansiReset
        Swp n d ds
            | 2 * n < d -> addedLine ds
            | otherwise -> addedLine' ds

  where
    -- TODO this is a very poor alignment
    diffs :: [D Char]
    diffs = alignWith f old new where
        f (This x)    = Ins x
        f (That y)    = Del y
        f (These x y) =
            let d :: [Diff.Diff Char]
                d = Diff.getDiff x y

                (m, n) = metric' d
            in Swp m n (chunks d)

data D a
    = Del [a]
    | Ins [a]
    | Swp !Int !Int [Chunk a a]

removedLine :: [Chunk Char Char] -> StringWriter ()
removedLine ds = do
    writeStr ansiRed
    writeChar '-'

    forM_ ds $ \case
        Diff s _ -> do
            writeStr ansiRED
            writeStr s
            writeStr ansiReset
        Same s _ -> do
            writeStr ansiRed
            writeStr s
            writeStr ansiReset

    writeChar '\n'

removedLine' :: [Chunk Char Char] -> StringWriter ()
removedLine' ds = do
    writeStr ansiRed
    writeChar '-'

    forM_ ds $ \case
        Diff s _ -> writeStr s
        Same s _ -> writeStr s

    writeStr ansiReset
    writeChar '\n'

addedLine :: [Chunk Char Char] -> StringWriter ()
addedLine ds = do
    writeStr ansiGreen
    writeChar '+'

    forM_ ds $ \case
        Diff _ s -> do
            writeStr ansiGREEN
            writeStr s
            writeStr ansiReset
        Same _ s -> do
            writeStr ansiGreen
            writeStr s
            writeStr ansiReset

    writeChar '\n'

addedLine' :: [Chunk Char Char] -> StringWriter ()
addedLine' ds = do
    writeStr ansiGreen
    writeChar '+'

    forM_ ds $ \case
        Diff _ s -> writeStr s
        Same _ s -> writeStr s

    writeStr ansiReset
    writeChar '\n'

-------------------------------------------------------------------------------
-- String Writer
-------------------------------------------------------------------------------

type StringWriter = S.State ShowS

runStringWriter :: StringWriter () -> String
runStringWriter m = S.execState m id ""

writeChar :: Char -> StringWriter ()
writeChar c = S.modify' $ \s -> s . (c :)

writeStr :: String -> StringWriter ()
writeStr str = S.modify' $ \s -> s . (str ++)

writeStrLn :: String -> StringWriter ()
writeStrLn str = S.modify' $ \s -> s . (str ++) . ('\n' :)

-------------------------------------------------------------------------------
-- Chunks
-------------------------------------------------------------------------------

-- | Diff chunks
data Chunk a b
    = Same [a] [b]
    | Diff [a] [b]

chunks :: [Diff.PolyDiff a b] -> [Chunk a b]
chunks [] = []
chunks (Diff.Both x y : ds) = consS (chunks ds) where
    consS (Same xs ys : ds') = Same (x : xs) (y : ys) : ds'
    consS ds'                = Same [x] [y] : ds'
chunks (Diff.First x  : ds) = consL (chunks ds) where
    consL (Diff xs ys : ds') = Diff (x : xs) ys : ds'
    consL ds'                = Diff [x] [] : ds'
chunks (Diff.Second y : ds) = consR (chunks ds) where
    consR (Diff xs ys : ds') = Diff xs (y : ys) : ds'
    consR ds'                = Diff [] [y] : ds'

-------------------------------------------------------------------------------
-- metric
-------------------------------------------------------------------------------

-- | A edit-distance like metric.
--
-- This is not Levenshtein, but related metric.
-- The result is less or equal to an ordinary Levenshtein metric,
-- but is zero iff Levenshtein distance is zero.
--
-- This is a lot faster than @edit-distance@ or @text-metrics@  metric.
--
metric :: Eq a => [a] -> [a] -> (Int, Int)
metric xs ys = metric' (Diff.getDiff xs ys)

metric' :: [Diff.PolyDiff a b] -> (Int, Int)
metric' ds = case Data.Foldable.foldl' f (Int3 0 0 0) ds of
    Int3 k i d -> let !l = max i d in (l - k, l)
  where
    f :: Int3 -> Diff.PolyDiff a b -> Int3
    f (Int3 k i d) (Diff.Both _ _) = (Int3 (k + 1) (i + 1) (d + 1))
    f (Int3 k i d) (Diff.First _)  = (Int3 k (i + 1) d)
    f (Int3 k i d) (Diff.Second _) = (Int3 k i (d + 1))

data Int3 = Int3 !Int !Int !Int

-------------------------------------------------------------------------------
-- ANSI helpers
-------------------------------------------------------------------------------

ansiRed :: String
ansiRed = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]

ansiRED :: String
ansiRED = ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Red]

ansiGreen :: String
ansiGreen = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]

ansiGREEN :: String
ansiGREEN = ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Green]

ansiReset :: String
ansiReset = ANSI.setSGRCode [ANSI.Reset]
