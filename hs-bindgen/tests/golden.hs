module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Tree (Tree (..))
import System.Directory (doesFileExist, setCurrentDirectory)
import System.FilePath ((</>), (-<.>))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.Golden.Advanced (goldenTest)
import Language.Haskell.TH.Ppr (ppr)
import Data.TreeDiff.Golden (ediffGolden)

import Orphans ()

import HsBindgen.Clang.Util.Classification
import HsBindgen.Lib

main :: IO ()
main = do
    findPackageDirectory "hs-bindgen"
    defaultMain $ testGroup "golden"
        [ golden "simple_structs"
        , golden "nested_types"
        , golden "enums"
        , golden "primitive_types"
        , golden "macros"
        ]
  where
    diff ref new = ["diff", "-u", ref, new]

    golden name = testGroup name
        [ goldenDump name
        , goldenTreeDiff name
        , goldenTH name
        ]

    goldenDump name = goldenVsStringDiff "ast" diff ("fixtures" </> (name ++ ".dump.txt")) $ do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let fp = "examples" </> (name ++ ".h")
            args = ["-target", "x86_64-pc-linux-gnu"]

        res <- getClangAST args fp

        return $ LBS8.pack $ unlines $ concatMap treeToLines res

    goldenTreeDiff name = ediffGolden goldenTest "treediff" ("fixtures" </> (name ++ ".tree-diff.txt")) $ do
        let fp = "examples" </> (name ++ ".h")
            args = ["-target", "x86_64-pc-linux-gnu"]

        header <- parseCHeader nullTracer args fp
        return header

    goldenTH name = goldenVsStringDiff "th" diff ("fixtures" </> (name ++ ".th.txt")) $ do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let fp = "examples" </> (name ++ ".h")
            args = ["-target", "x86_64-pc-linux-gnu"]

        header <- parseCHeader nullTracer args fp
        let decls = genDecls header

        return $ LBS8.pack $ unlines $ map (show . ppr) decls

treeToLines :: Tree Element -> [String]
treeToLines tree = go 0 tree [] where
    go :: Int -> Tree Element -> [String] -> [String]
    go !n (Node l xs) next = (replicate (n * 2) ' ' ++ showElem l) : foldr (go (n + 1)) next xs

showElem :: Element -> [Char]
showElem Element{elementName, elementTypeKind} = mconcat [
      case elementName of
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
