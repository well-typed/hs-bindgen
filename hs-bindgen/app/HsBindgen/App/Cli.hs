{-# LANGUAGE ApplicativeDo #-}
module HsBindgen.App.Cli (
    Cli(..)
  , Mode(..)
  , getCli
  , pureParseModePreprocess
  ) where

import Data.Default
import Options.Applicative

import Clang.Paths
import HsBindgen.App.Common
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCli :: IO Cli
getCli = customExecParser p opts
  where
    p = prefs $ helpShowGlobals <> subparserInline

    opts :: ParserInfo Cli
    opts = info (parseCli <**> helper) $
      mconcat [
          header "hs-bindgen - generate Haskell bindings from C headers"
        ]

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Command line arguments
data Cli = Cli {
      cliGlobalOpts :: GlobalOpts
    , cliMode       :: Mode
    }
  deriving (Show)

data Mode =
    -- | The main mode: preprocess C headers to Haskell modules
    ModePreprocess {
        preprocessInput           :: CHeaderIncludePath
      , preprocessTranslationOpts :: TranslationOpts
      , preprocessModuleOpts      :: HsModuleOpts
      , preprocessRenderOpts      :: HsRenderOpts
      , preprocessOutput          :: Maybe FilePath
      , preprocessGenBindingSpecs :: Maybe FilePath
      }
    -- | Generate tests for generated Haskell code
  | ModeGenTests {
        genTestsInput      :: CHeaderIncludePath
      , genTestsModuleOpts :: HsModuleOpts
      , genTestsRenderOpts :: HsRenderOpts
      , genTestsOutput     :: FilePath
      }
  | ModeLiterate FilePath FilePath
  deriving (Show)

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseCli :: Parser Cli
parseCli =
    Cli
      <$> parseGlobalOpts
      <*> parseMode

pureParseModePreprocess :: [String] -> Maybe Cli
pureParseModePreprocess =
      getParseResult
    . execParserPure (prefs subparserInline) (info parseCli mempty)
    . ("preprocess" :)

{-------------------------------------------------------------------------------
  Mode selection
-------------------------------------------------------------------------------}

parseMode :: Parser Mode
parseMode = subparser $ mconcat [
      cmd "preprocess" parseModePreprocess $ mconcat [
          progDesc "Generate Haskell module from C header"
        ]
    , cmd' "literate" parseModeLiterate $ mconcat [
          progDesc "Generate Haskell module from C header, acting as literate Haskell preprocessor"
        ]
    , cmd "gentests" parseModeGenTests $ mconcat [
          progDesc "Generate tests for generated Haskell code"
        ]
    ]

{-------------------------------------------------------------------------------
  Regular modes
-------------------------------------------------------------------------------}

parseModePreprocess :: Parser Mode
parseModePreprocess =
    ModePreprocess
      <$> parseInput
      <*> parseTranslationOpts
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseOutput
      <*> optional parseGenBindingSpecs

parseModeGenTests :: Parser Mode
parseModeGenTests =
    ModeGenTests
      <$> parseInput
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseGenTestsOutput

parseModeLiterate :: Parser Mode
parseModeLiterate = do
    _ <- strOption @String $ mconcat [ short 'h', metavar "IGNORED" ]

    input <- strArgument $ mconcat [ metavar "IN" ]
    output <- strArgument $ mconcat [ metavar "OUT" ]
    return (ModeLiterate input output)

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

parseTranslationOpts :: Parser TranslationOpts
parseTranslationOpts = pure defaultTranslationOpts

parseHsModuleOpts :: Parser HsModuleOpts
parseHsModuleOpts =
    HsModuleOpts
      <$> strOption (mconcat [
              help "Name of the generated Haskell module"
            , metavar "NAME"
            , long "module"
            , showDefault
            , value "Generated"
            ])

{-------------------------------------------------------------------------------
  Process output
-------------------------------------------------------------------------------}

parseHsRenderOpts :: Parser HsRenderOpts
parseHsRenderOpts =
    HsRenderOpts
      <$> option auto (mconcat [
              help "Maximum length line"
            , long "render-line-length"
            , showDefault
            , value $ hsLineLength def
            ])

parseOutput :: Parser (Maybe FilePath)
parseOutput =
    optional $ strOption $ mconcat [
        help "Output path for the Haskell module"
      , metavar "PATH"
      , long "output"
      , short 'o'
      ]

parseGenTestsOutput :: Parser FilePath
parseGenTestsOutput =
    strOption $ mconcat [
        help "Output directory for the test suite"
      , metavar "PATH"
      , long "output"
      , short 'o'
      , showDefault
      , value "test-hs-bindgen"
      ]

parseGenBindingSpecs :: Parser FilePath
parseGenBindingSpecs =
    strOption $ mconcat [
        help "Binding specifications to generate"
      , metavar "PATH"
      , long "gen-binding-specs"
      ]
