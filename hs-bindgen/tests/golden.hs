{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Main (main) where

import Data.TreeDiff.Golden (ediffGolden)
import System.FilePath ((</>))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit (testCase, (@?=))

import Orphans ()
import Misc

#if __GLASGOW_HASKELL__ >=904
import TH
#endif

import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Lib
import HsBindgen.Util.PHOAS

main :: IO ()
main = findPackageDirectory "hs-bindgen" >>= main'

main' :: FilePath -> IO ()
main' packageRoot = defaultMain $ testGroup "golden"
    [ testCase "target-triple" $ do
        let fp = "examples/simple_structs.h"
            args = clangArgs packageRoot
        triple <- withC nullTracer args fp $ getTargetTriple

        -- macos-latest (macos-14) returns "arm64-apple-macosx14.0.0"
        -- windows-latest (???) returns "x86_64-pc-windows-msvc19.41.34120"
        triple @?= "x86_64-pc-linux-gnu"

    , golden "simple_structs"
    , golden "nested_types"
    , golden "enums"
    , golden "primitive_types"
    , golden "macros"
    , golden "macro_functions"
    , golden "uses_utf8"
    , golden "typedef_vs_macro"
    , golden "headers"
    , golden "fixedwidth"
    ]
  where
    golden name = testGroup name
        [ goldenTreeDiff name
        , goldenHs name
-- Since GHC-9.4 the Template Haskell ppr function has changed slightly
#if __GLASGOW_HASKELL__ >=904
        , goldenTh packageRoot name
#endif
        ]

    goldenTreeDiff name = ediffGolden goldenTest "treediff" ("fixtures" </> (name ++ ".tree-diff.txt")) $ do
        -- TODO: there aren't ediffGolden variant for goldenTestSteps like signature... yet

        let fp = "examples" </> (name ++ ".h")
            args = clangArgs packageRoot

        header <- parseC nullTracer args fp
        return header

    goldenHs name = goldenVsStringDiff_ "hs" ("fixtures" </> (name ++ ".hs")) $ \report -> do
        -- -<.> does weird stuff for filenames with multiple dots;
        -- I usually simply avoid using it.
        let fp = "examples" </> (name ++ ".h")
            args = clangArgs packageRoot

        let tracer = mkTracer report report report False

        header <- parseC tracer args fp
        let decls :: forall f. List Hs.Decl f
            decls = genHsDecls header

        return $ showClosed decls

withC ::
     Tracer IO String
  -> ClangArgs
  -> FilePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withC tracer args fp =
    withTranslationUnit tracerD args fp
  where
    tracerD = contramap show tracer

parseC ::
     Tracer IO String
  -> ClangArgs
  -> FilePath
  -> IO CHeader
parseC tracer args fp =
    withC tracer args fp $ parseCHeader tracerP SelectFromMainFile
  where
    tracerP = contramap prettyLogMsg tracer
