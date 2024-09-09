{-# LANGUAGE CPP #-}
module Main (main) where

import Data.ByteString.Char8 qualified as BS8
import Data.Tree (Tree (..))
import Data.TreeDiff.Golden (ediffGolden)
import System.Directory (doesFileExist, setCurrentDirectory)
import System.FilePath ((</>), (-<.>))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit (testCase, (@?=))

import Orphans ()
import TH

import HsBindgen.Clang.Util.Classification
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Lib
import HsBindgen.Util.PHOAS

main :: IO ()
main = do
    findPackageDirectory "hs-bindgen"
    defaultMain $ testGroup "golden"
        [ testCase "target-triple" $ do
            let fp = "examples/simple_structs.h"
                args = ["-target", "x86_64-pc-linux-gnu"]
            triple <- getTargetTriple args fp

            -- macos-latest (macos-14) returns "arm64-apple-macosx14.0.0"
            -- windows-latest (???) returns "x86_64-pc-windows-msvc19.41.34120"
            triple @?= BS8.pack "x86_64-pc-linux-gnu"

        , golden "simple_structs"
        , golden "nested_types"
        , golden "enums"
        , golden "primitive_types"
        , golden "macros"
        , golden "macro_functions"
        ]
  where
    golden name = testGroup name
        [ goldenDump name
        , goldenTreeDiff name
        , goldenHs name
-- Since GHC-9.4 the Template Haskell ppr function has changed slightly
#if __GLASGOW_HASKELL__ >=904
        , goldenTh name
#endif
        ]

    goldenDump name = goldenVsStringDiff_ "ast" ("fixtures" </> (name ++ ".dump.txt")) $ do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let fp = "examples" </> (name ++ ".h")
            args = ["-target", "x86_64-pc-linux-gnu"]

        res <- getClangAST SelectFromMainFile args fp

        return $ unlines $ concatMap treeToLines res

    goldenTreeDiff name = ediffGolden goldenTest "treediff" ("fixtures" </> (name ++ ".tree-diff.txt")) $ do
        let fp = "examples" </> (name ++ ".h")
            args = ["-target", "x86_64-pc-linux-gnu"]

        header <- parseCHeader nullTracer SelectFromMainFile args fp
        return header

    goldenHs name = goldenVsStringDiff_ "hs" ("fixtures" </> (name ++ ".hs")) $ do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let fp = "examples" </> (name ++ ".h")
            args = ["-target", "x86_64-pc-linux-gnu"]

        header <- parseCHeader nullTracer SelectFromMainFile args fp
        let decls :: forall f. List Hs.Decl f
            decls = List $ genHaskell header

        return $ showClosed decls

treeToLines :: Tree Element -> [String]
treeToLines tree = go 0 tree [] where
    go :: Int -> Tree Element -> [String] -> [String]
    go !n (Node l xs) next = (replicate (n * 2) ' ' ++ showElem l) : foldr (go (n + 1)) next xs

showElem :: Element -> [Char]
showElem Element{elementName, elementKind, elementTypeKind} = mconcat [
      show elementKind
    , " "
    , case elementName of
        UserProvided name  -> show name
        LibclangProvided _ -> "_" -- varies between llvm versions, so ignore
    , " :: "
    , show elementTypeKind
    ]

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
