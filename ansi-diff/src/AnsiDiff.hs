module AnsiDiff (
    ansidiff,
    -- * Implementation bits
    metric,
) where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Foldable (foldl')
import Data.Primitive.Types (Prim)
import Data.Primitive.Array
import Data.Primitive.PrimArray
import Control.Monad.Trans.State.Strict qualified as S
import System.Console.ANSI qualified as ANSI

import Data.Algorithm.Diff qualified as Diff
import Text.EditDistance qualified as ED

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
        Del x  -> writeStrLn $ ansiRed ++ '-' : x ++ ansiReset
        Ins _  -> return ()
        Swp ds -> removedLine ds

    -- added
    forM_ diffs $ \case
        Del _  -> return ()
        Ins y  -> writeStrLn $ ansiGreen ++ '+' : y ++ ansiReset
        Swp ds -> addedLine ds

  where
    diffs :: [D Char Char]
    diffs = alignment old new

-- empasise line diffs only if there are similar enough.
-- this is different from 'metric'
--   we emphasise a line when there are over half of shorter line is preserved.
predicate :: Double -> Bool
predicate d = d < 0.2

-------------------------------------------------------------------------------
-- Alignment
-------------------------------------------------------------------------------

alignment :: forall a b. (a ~ Char, b ~ Char) => [String] -> [String] -> [D Char Char]
alignment xs' ys' = do
    reverse $ walk xn yn
  where
    xs :: Array [a]
    xs = arrayFromList xs'

    ys :: Array [b]
    ys = arrayFromList ys'

    xn :: Int
    xn = length xs

    yn :: Int
    yn = length ys

    diffs :: PrimArray Double
    diffs = createPrimArray (xn * yn) $ \arr -> do
        forM_ [0..xn-1] $ \i -> forM_ [0..yn-1] $ \j -> do
            let xc = indexArray xs i
            let yc = indexArray ys j
            -- here we use edit-distance, because all the strings we are comparing are different,
            -- potentially a lot. Thus we care about worse case performance.
            writePrimArray arr (i + j * xn) $ fromIntegral (ED.levenshteinDistance ED.defaultEditCosts xc yc) / fromIntegral (max (length xc) (length yc))

    indexDiff :: Int -> Int -> Double
    indexDiff i j = indexPrimArray diffs (i + j * xn)
    {-# INLINE indexDiff #-}

    distances :: PrimArray Double
    distances = createPrimArray ((xn + 1) * (yn + 1)) $ \arr -> do
        forM_ [0..xn] $ \i -> do
            writePrimArray arr i (fromIntegral i)

        forM_ [1..yn] $ \j -> do
            writePrimArray arr (j * (xn + 1)) (fromIntegral j)

            forM_ [1..xn] $ \i -> do
                let r = indexDiff (i - 1) (j - 1)

                if r == 0
                then do
                    !z <- readPrimArray arr ((i - 1) + (j - 1) * (xn + 1))
                    writePrimArray arr (i + j * (xn + 1)) z
                else do
                    !x <- readPrimArray arr ((i    ) + (j - 1) * (xn + 1))
                    !y <- readPrimArray arr ((i - 1) + (j    ) * (xn + 1))
                    !z <- readPrimArray arr ((i - 1) + (j - 1) * (xn + 1))

                    writePrimArray arr (i + j * (xn + 1)) $!
                        min (r + z) $ min (x + 1) (y + 1)

    indexDistance :: Int -> Int -> Double
    indexDistance i j = indexPrimArray distances (i + j * (xn + 1))
    {-# INLINE indexDistance #-}

    -- trace back walk
    walk :: Int -> Int -> [D a b]
    walk i j
        | i == 0, j == 0
        = []

        | i == 0
        = Ins (indexArray ys (j - 1)) : walk i (j - 1)

        | j == 0
        = Del (indexArray xs (i - 1)) : walk (i - 1) j

        | otherwise
        = do
            let !x = indexDistance (i    ) (j - 1)
            let !y = indexDistance (i - 1) (j    )
            let !z = indexDistance (i - 1) (j - 1)

            if z <= x
                then
                    if z <= y
                    -- z is smallest
                    then do
                        let xc = indexArray xs (i - 1)
                        let yc = indexArray ys (j - 1)
                        let !r = indexDiff (i - 1) (j - 1)
                        if predicate r
                        then do
                            let !d = Diff.getDiff xc yc
                            Swp (chunks d) : walk (i - 1) (j - 1)
                        else
                            Del xc : Ins yc : walk (i - 1) (j - 1)

                    -- y is smallest
                    else Del (indexArray xs (i - 1)) : walk (i - 1) j

                else
                    if y <= x

                    -- y is smallest
                    then Del (indexArray xs (i - 1)) : walk (i - 1) j

                    -- x is smallest
                    else Ins (indexArray ys (j - 1)) : walk i (j - 1)

data D a b
    = Del [a]
    | Ins [b]
    | Swp [Chunk a b]
  deriving Show

-------------------------------------------------------------------------------
-- primitive extras
-------------------------------------------------------------------------------

createPrimArray :: Prim a => Int -> (forall s. MutablePrimArray s a -> ST s ()) -> PrimArray a
createPrimArray n action = runPrimArray $ do
    arr <- newPrimArray n
    action arr
    return arr

-------------------------------------------------------------------------------
-- Printing
-------------------------------------------------------------------------------

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
  deriving Show

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
metric :: Eq a => [a] -> [a] -> Int
metric xs ys = case metric' (Diff.getDiff xs ys) of
    Int3 k i d -> max i d - k

metric' :: [Diff.PolyDiff a b] -> Int3
metric' ds = Data.Foldable.foldl' f (Int3 0 0 0) ds
  where
    f :: Int3 -> Diff.PolyDiff a b -> Int3
    f (Int3 k i d) (Diff.Both _ _) = (Int3 (k + 1) (i + 1) (d + 1))
    f (Int3 k i d) (Diff.First _)  = (Int3 k (i + 1) d)
    f (Int3 k i d) (Diff.Second _) = (Int3 k i (d + 1))

data Int3 = Int3 !Int !Int !Int
  deriving Show

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
