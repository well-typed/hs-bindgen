-- | Colorful diff. Reasonably efficient, pretty colors.
module Diff (ansiLinesDiff) where

import Data.Maybe (mapMaybe)
import Data.Vector qualified as V
import System.Console.ANSI qualified as ANSI

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

min3 :: Ord k => k -> k -> k -> k
min3 x y z = min x (min y z)

pick3 :: Ord k => k -> a -> k -> a -> k -> a -> a
pick3 a x b y c z =
    if a < b
    then (if a < c then x else z)
    else (if b < c then y else z)

-------------------------------------------------------------------------------
-- Generic diff
-------------------------------------------------------------------------------

data Diff a b d
    = End
    | Same [a] [b] (Diff a b d)
    | Diff [a] [b] [d] (Diff a b d)
  deriving Show

consS :: a -> b -> Diff a b d -> Diff a b d
consS x y (Same xs ys df) = Same (x : xs) (y : ys) df
consS x y df              = Same [x] [y] df

consR :: b -> d -> Diff a b d -> Diff a b d
consR y d (Diff xs ys ds df) = Diff xs (y : ys) (d : ds) df
consR y d df                 = Diff [] [y] [d] df

consL :: a -> d -> Diff a b d -> Diff a b d
consL x d (Diff xs ys ds df) = Diff (x : xs) ys (d : ds) df
consL x d df                 = Diff [x] [] [d] df

consD :: a -> b -> d -> Diff a b d -> Diff a b d
consD x y d (Diff xs ys ds df) = Diff (x : xs) (y : ys) (d : ds) df
consD x y d df                 = Diff [x] [y] [d] df

genericDiff :: forall a b d. (a -> b -> (Double, d))
     -> (a -> d)
     -> (b -> d)
     -> [a]
     -> [b]
     -> (Double, Diff a b d)
genericDiff cmp_ inl inr xs_ ys_ = (distance, walk 0 0)
  where
    distance = if dist 0 0 == 0 then 0 else dist 0 0 / fromIntegral (max xn yn)

    xs :: V.Vector a
    xs = V.fromList xs_

    ys :: V.Vector b
    ys = V.fromList ys_

    xn = V.length xs
    yn = V.length ys

    cmp :: Int -> Int -> (Double, d)
    cmp !i !j = comparisons V.! (j + i * yn)

    comparisons :: V.Vector (Double, d)
    comparisons = V.generate (xn * yn) aux
      where
        aux ij = cmp_ xc yc
          where
            (i, j) = ij `divMod` yn
            xc = xs V.! i
            yc = ys V.! j

    dist :: Int -> Int -> Double
    dist !i !j = distances V.! (j + i * (yn + 1))

    distances :: V.Vector Double
    distances = V.generate ((xn + 1) * (yn + 1)) $ \ij -> case ij `divMod` (yn + 1) of
        (i, j)
            | i == xn, j == yn
            -> 0

            | i == xn
            -> 1 + dist i (j + 1)

            | j == yn
            -> 1 + dist (i + 1) j

            | otherwise
            , let (r, _) = cmp i j
            -> if r == 0
               then dist (i + 1) (j + 1)
               else min3 (r + dist i       (j + 1))
                         (r + dist (i + 1) j)
                         (r + dist (i + 1) (j + 1))

    walk :: Int -> Int -> Diff a b d
    walk !i !j
        | i == xn, j == yn
        = End

        | i == xn
        = consR yc (inr yc) (walk i (j + 1))

        | j == yn
        = consL xc (inl xc) (walk (i + 1) j)

        | otherwise
        , let (r, d) = cmp i j
        = if r == 0
          then consS xc yc (walk (i + 1) (j + 1))
          else pick3 (dist i       (j + 1)) (consR yc    (inr yc) (walk i       (j + 1)))
                     (dist (i + 1) j)       (consL xc    (inl xc) (walk (i + 1) j))
                     (dist (i + 1) (j + 1)) (consD xc yc d        (walk (i + 1) (j + 1)))
      where
        xc = xs V.! i
        yc = ys V.! j

-------------------------------------------------------------------------------
-- ANSI helpers
-------------------------------------------------------------------------------

ansiRed :: String
ansiRed = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]

ansiGreen :: String
ansiGreen = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]

ansiReset :: String
ansiReset = ANSI.setSGRCode [ANSI.Reset]

-------------------------------------------------------------------------------
-- Lines diff
-------------------------------------------------------------------------------

-- | Lines diff, where each line is also diffed to show in line additions/removals
linesDiff :: [String] -> [String] -> (Double, Diff String String (Either (Either String String) (Diff Char Char ())))
linesDiff xss yss = fullDiff
  where
    fullDiff = genericDiff
        (\xs ys -> let (r, d) = lineDiff xs ys in (r, Right d))
        (Left . Left)
        (Left . Right)
        xss
        yss

    lineDiff :: String -> String -> (Double, Diff Char Char ())
    lineDiff = genericDiff
        (\x y -> (if x == y then 0 else 1, ()))
        (\_ -> ())
        (\_ -> ())

ansiLinesDiff :: [String] -> [String] -> [String]
ansiLinesDiff xss yss = ansify (snd (linesDiff xss yss))

ansify :: Diff String String (Either (Either String String) (Diff Char Char ())) -> [String]
ansify End = []
ansify (Same xs _ df) =
    map noChange xs ++
    ansify df
  where
    noChange s = ansiReset ++ " " ++ s
ansify (Diff _xs _ys ds df) =
    mapMaybe removed_ ds ++
    mapMaybe added_ ds ++
    ansify df
  where
    removed s = ansiRed   ++ "-" ++ s ++ ansiReset
    added   s = ansiGreen ++ "+" ++ s ++ ansiReset

    removed_ :: Either (Either String String) (Diff Char Char ()) -> Maybe String
    removed_ (Left (Left s))  = Just $ removed s
    removed_ (Left (Right _)) = Nothing
    removed_ (Right d)        = Just $ ansiRed ++ "-" ++ ansiReset ++ removedLine d

    added_ :: Either (Either String String) (Diff Char Char ()) -> Maybe String
    added_ (Left (Right s)) = Just $ added s
    added_ (Left (Left _))  = Nothing
    added_ (Right d)        = Just $ ansiGreen ++ "+" ++ ansiReset ++ addedLine d

removedLine :: Diff Char Char () -> String
removedLine End = ansiReset
removedLine (Same xs _ df)   = ansiRed ++ xs ++ ansiReset ++ removedLine df
removedLine (Diff xs _ _ df) = ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Red] ++ xs ++ ansiReset ++ removedLine df

addedLine :: Diff Char Char () -> String
addedLine End = ansiReset
addedLine (Same _ ys df)   = ansiGreen ++ ys ++ ansiReset ++ addedLine df
addedLine (Diff _ ys _ df) = ANSI.setSGRCode [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Green] ++ ys ++ ansiReset ++ addedLine df
