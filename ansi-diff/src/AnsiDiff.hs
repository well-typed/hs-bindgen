module AnsiDiff (
    ansidiff,
) where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Primitive.Types (Prim)
import Data.Primitive.Array qualified as P
import Data.Primitive.PrimArray qualified as P
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
    xs :: P.Array [a]
    xs = P.arrayFromList xs'

    ys :: P.Array [b]
    ys = P.arrayFromList ys'

    xn :: Int
    xn = length xs

    yn :: Int
    yn = length ys

    diffs :: P.PrimArray Double
    diffs = createPrimArray (xn * yn) $ \arr -> do
        forM_ [0..xn-1] $ \i -> forM_ [0..yn-1] $ \j -> do
            let xc = P.indexArray xs i
            let yc = P.indexArray ys j
            let xm = length xc
            let ym = length yc
            let mi = min xm ym
            let ma = max xm ym

            let r :: Double
                !r | ma == 0 = 0
                   | mi == 0 = 1

                    -- a lower bound of edit-distance,
                    -- if it's not satisfying a predicate for inter-line diff,
                    -- just use it instead of calculating proper edit-distance
                    -- (see tests)
                   | let lb = fromIntegral (ma - mi) / fromIntegral ma
                   , not (predicate lb) = lb

                    -- here we use edit-distance, because all the strings we are comparing are different,
                    -- potentially a lot. Thus we care about worse case performance i.e. O (nm) is the same as O(nd).
                   | otherwise = fromIntegral (ED.levenshteinDistance ED.defaultEditCosts xc yc) / fromIntegral ma

            P.writePrimArray arr (i + j * xn) r

    indexDiff :: Int -> Int -> Double
    indexDiff i j = P.indexPrimArray diffs (i + j * xn)
    {-# INLINE indexDiff #-}

    distances :: P.PrimArray Double
    distances = createPrimArray ((xn + 1) * (yn + 1)) $ \arr -> do
        forM_ [0..xn] $ \i -> do
            P.writePrimArray arr i (fromIntegral i)

        forM_ [1..yn] $ \j -> do
            P.writePrimArray arr (j * (xn + 1)) (fromIntegral j)

            forM_ [1..xn] $ \i -> do
                let r = indexDiff (i - 1) (j - 1)

                if r == 0
                then do
                    !z <- P.readPrimArray arr ((i - 1) + (j - 1) * (xn + 1))
                    P.writePrimArray arr (i + j * (xn + 1)) z
                else do
                    !x <- P.readPrimArray arr ((i    ) + (j - 1) * (xn + 1))
                    !y <- P.readPrimArray arr ((i - 1) + (j    ) * (xn + 1))
                    !z <- P.readPrimArray arr ((i - 1) + (j - 1) * (xn + 1))

                    P.writePrimArray arr (i + j * (xn + 1)) $!
                        min (r + z) $ min (x + 1) (y + 1)

    indexDistance :: Int -> Int -> Double
    indexDistance i j = P.indexPrimArray distances (i + j * (xn + 1))
    {-# INLINE indexDistance #-}

    -- trace back walk
    walk :: Int -> Int -> [D a b]
    walk i j
        | i == 0, j == 0
        = []

        | i == 0
        = Ins (P.indexArray ys (j - 1)) : walk i (j - 1)

        | j == 0
        = Del (P.indexArray xs (i - 1)) : walk (i - 1) j

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
                        let xc = P.indexArray xs (i - 1)
                        let yc = P.indexArray ys (j - 1)
                        let !r = indexDiff (i - 1) (j - 1)
                        if predicate r
                        then do
                            let !d = Diff.getDiff xc yc
                            Swp (chunks d) : walk (i - 1) (j - 1)
                        else
                            Del xc : Ins yc : walk (i - 1) (j - 1)

                    -- y is smallest
                    else Del (P.indexArray xs (i - 1)) : walk (i - 1) j

                else
                    if y <= x

                    -- y is smallest
                    then Del (P.indexArray xs (i - 1)) : walk (i - 1) j

                    -- x is smallest
                    else Ins (P.indexArray ys (j - 1)) : walk i (j - 1)

data D a b
    = Del [a]
    | Ins [b]
    | Swp [Chunk a b]
  deriving Show

-------------------------------------------------------------------------------
-- primitive extras
-------------------------------------------------------------------------------

createPrimArray :: Prim a => Int -> (forall s. P.MutablePrimArray s a -> ST s ()) -> P.PrimArray a
createPrimArray n action = P.runPrimArray $ do
    arr <- P.newPrimArray n
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
