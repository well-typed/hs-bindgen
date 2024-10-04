{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.DeepSeq (NFData (..))
import Data.Algorithm.Diff qualified as Diff
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Metrics as Metrics
import System.Directory (doesFileExist, setCurrentDirectory)
import System.FilePath ((</>), (-<.>))
import Test.Tasty.Bench (Benchmark, defaultMain, env, nf, whnf, bench, bgroup)
import Text.EditDistance qualified as ED

import AnsiDiff qualified

main :: IO ()
main = do
    findPackageDirectory "ansi-diff"
    defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "th"
        [ env (readString "fixtures/primitive_types.th.old.txt") $ \old ->
          env (readString "fixtures/primitive_types.th.new.txt") $ \new ->
          bench "edit-distance" $
            whnf (uncurry (ED.levenshteinDistance ED.defaultEditCosts { ED.transpositionCosts = ED.ConstantCost 100 }))
            (old, new)

        , env (readText "fixtures/primitive_types.th.old.txt") $ \old ->
          env (readText "fixtures/primitive_types.th.new.txt") $ \new ->
          bench "text-metrics" $
            whnf (uncurry Metrics.levenshtein)
            (old, new)

        , env (readString "fixtures/primitive_types.th.old.txt") $ \old ->
          env (readString "fixtures/primitive_types.th.new.txt") $ \new ->
          bench "Diff" $
            nf (uncurry Diff.getDiff)
            (old, new)

        , env (readString "fixtures/primitive_types.th.old.txt") $ \old ->
          env (readString "fixtures/primitive_types.th.new.txt") $ \new ->
          bench "ansidiff" $
            nf (uncurry AnsiDiff.ansidiff)
            (old, new)
        ]

    , bgroup "hs"
        [ env (readString "fixtures/primitive_types.hs.old.txt") $ \old ->
          env (readString "fixtures/primitive_types.hs.new.txt") $ \new ->
          bench "edit-distance" $
            whnf (uncurry (ED.levenshteinDistance ED.defaultEditCosts))
            (old, new)

        , env (readText "fixtures/primitive_types.hs.old.txt") $ \old ->
          env (readText "fixtures/primitive_types.hs.new.txt") $ \new ->
          bench "text-metrics" $
            whnf (uncurry Metrics.levenshtein)
            (old, new)

        , env (readString "fixtures/primitive_types.hs.old.txt") $ \old ->
          env (readString "fixtures/primitive_types.hs.new.txt") $ \new ->
          bench "Diff" $
            nf (uncurry Diff.getDiff)
            (old, new)

        , env (readString "fixtures/primitive_types.hs.old.txt") $ \old ->
          env (readString "fixtures/primitive_types.hs.new.txt") $ \new ->
          bench "ansidiff" $
            nf (uncurry AnsiDiff.ansidiff)
            (old, new)
        ]
    ]

readString :: FilePath -> IO String
readString fp = do
    contents <- BS8.readFile fp
    return (BS8.unpack contents)

readText :: FilePath -> IO Text
readText fp = do
    contents <- BS8.readFile fp
    return (TE.decodeUtf8 contents)

-- | In multi-package projects @cabal run test-suite@ will run the test-suite
-- from your current working directory (e.g. project root), which is often
-- not the package directory.
--
-- However, many tests are written so they assume that are run from
-- *package* directory.
findPackageDirectory :: String -> IO ()
findPackageDirectory pkgname = do
    here <- doesFileExist (pkgname -<.> ".cabal")
    if here
    then return ()
    else do
        there <- doesFileExist (pkgname </> pkgname -<.> ".cabal")
        if there
        then setCurrentDirectory pkgname
        -- do not try too hard, if not in the package directory, nor project root: abort
        else fail $ "Cannot find package directory for " ++ pkgname

instance (NFData a, NFData b) => NFData (Diff.PolyDiff a b) where
    rnf (Diff.First x)  = rnf x
    rnf (Diff.Second y) = rnf y
    rnf (Diff.Both x y) = rnf x `seq` rnf y
