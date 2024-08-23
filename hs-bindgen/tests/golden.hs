module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Tree (Tree (..))
import System.Directory (doesFileExist, setCurrentDirectory)
import System.FilePath ((</>), (-<.>))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

import HsBindgen.Lib

main :: IO ()
main = do
    findPackageDirectory "hs-bindgen"
    defaultMain $ testGroup "golden"
        [ goldenVsStringDiff "simple_structs" diff "fixtures/simple_structs.dump.txt" $ do
            let fp = "examples/simple_structs.h"
                args = []
            res <- showClangAST args fp

            return $ LBS8.pack $ unlines $ concatMap treeToLines res
        ]
  where
    diff ref new = ["diff", "-u", ref, new]

treeToLines :: Tree String -> [String]
treeToLines tree = go 0 tree [] where
    go :: Int -> Tree String -> [String] -> [String]
    go !n (Node l xs) next = (replicate (n * 2) ' ' ++ l) : foldr (go (n + 1)) next xs

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
