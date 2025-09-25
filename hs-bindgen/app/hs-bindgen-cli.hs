{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Control.Exception (Exception (..), SomeException (..), handle)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Version (showVersion)
import Options.Applicative
import Options.Applicative.Help qualified as Help
import Prettyprinter.Util qualified as PP
import System.Exit (ExitCode, exitFailure)

import Clang.Version (clang_getClangVersion)

import HsBindgen.App
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Cli qualified as Cli
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Lib

import Paths_hs_bindgen qualified as Package

{-------------------------------------------------------------------------------
  CLI parser
-------------------------------------------------------------------------------}

data Cli = Cli {
      cliGlobalOpts :: GlobalOpts
    , cliCmd        :: Cli.Cmd
    }

parseCli :: Parser Cli
parseCli =
    Cli
      <$> parseGlobalOpts
      <*> Cli.parseCmd

execCliParser :: IO Cli
execCliParser = do
    clangVersion <- Text.unpack <$> clang_getClangVersion
    let vers = List.intercalate "\n" [
            "hs-bindgen " ++ showVersion Package.version
          , "binding specification " ++ show BindingSpec.version
          , clangVersion
          ]
    customExecParser prefs' (opts vers)
  where
    prefs' :: ParserPrefs
    prefs' = prefs $ helpShowGlobals <> subparserInline

    opts :: String -> ParserInfo Cli
    opts vers = info (parseCli <**> simpleVersioner vers <**> helper) $
      mconcat [
          header "hs-bindgen - generate Haskell bindings from C headers"
        , footerDoc . Just . Help.vcat $ List.intersperse "" [
              envVarsFooter
            , clangArgsFooter
            , selectSliceFooter
            ]
        ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

main :: IO ()
main = handle exceptionHandler $ do
    Cli{..} <- execCliParser
    Cli.exec cliGlobalOpts cliCmd

{-------------------------------------------------------------------------------
  Auxiliary functions: exception handling
-------------------------------------------------------------------------------}

exceptionHandler :: SomeException -> IO ()
exceptionHandler e@(SomeException e')
    | Just _ <- fromException e :: Maybe ExitCode
    = throwIO e'

    | Just (HsBindgenException e'') <- fromException e = do
      putStrLn $ displayException e''
      exitFailure

    -- truly unexpected exceptions
    | otherwise = do
      -- Note: displayException of internal exception; this will ensure uniform
      -- behavior while `base`/GHC figures out the ending of exceptions and
      -- backtrace story
      putStrLn $ "Uncaught exception: " ++ displayException e'
      putStrLn
        "Please report this at https://github.com/well-typed/hs-bindgen/issues"
      -- TODO: we could print exception context here, but it seems to be empty
      -- for IOExceptions anyway.
      exitFailure

{-------------------------------------------------------------------------------
  Auxiliary functions: footers
-------------------------------------------------------------------------------}

envVarsFooter :: Help.Doc
envVarsFooter = Help.vcat [
      "Environment variables:"
    , li $ "BINDGEN_EXTRA_CLANG_ARGS: Arguments passed to Clang"
    , li $ mconcat [
          "BINDGEN_EXTRA_CLANG_ARGS_<TARGET>:"
        , " Target-specific arguments passed to Clang;"
        , " precedes BINDGEN_EXTRA_CLANG_ARGS; possible targets: "
        , Text.intercalate ", " (map Text.pack triples)
        ]
    ]
  where
    triples :: [String]
    triples = map (`targetTriple` TargetEnvDefault) targets

    targets :: [Target]
    targets = [minBound .. maxBound]

clangArgsFooter :: Help.Doc
clangArgsFooter = Help.vcat [
      "Options passed to Clang have the following order:"
    , "  1. --clang-option-before options"
    , "  2. Clang options managed by hs-bindgen (e.g., -I options)"
    , "  3. --clang-option options"
    , "  4. BINDGEN_EXTRA_CLANG_ARGS options"
    , "  5. --clang-option-after options"
    , "  6. Builtin include directory options"
    ]

selectSliceFooter :: Help.Doc
selectSliceFooter = Help.vcat [
      "Selection and program slicing:"
    , li $ mconcat [
          "Program slicing disabled (default):"
        , " only select declarations according to the selection predicate"
        ]
    , li $ mconcat [
          "Program slicing enabled ('--enable-program-slicing'):"
        , " select declarations using the selection predicate,"
        , " and also select their transitive dependencies"
        ]
    ]

li :: Text -> Help.Doc
li = ("  -" Help.<+>) . Help.align . PP.reflow
