{-# LANGUAGE ApplicativeDo #-}
module HsBindgen.App.Cli (
    Cli(..)
  , Mode(..)
  , getCli
  , pureParseModePreprocess
  , BindingSpecMode(..)
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
        , footerDoc (Just $ environmentVariablesFooter p)
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
        preprocessTranslationOpts :: TranslationOpts
      , preprocessModuleOpts      :: HsModuleOpts
      , preprocessRenderOpts      :: HsRenderOpts
      , preprocessOutput          :: Maybe FilePath
      , preprocessGenExtBindings  :: Maybe FilePath
      , preprocessInputs          :: [CHeaderIncludePath]
      }
    -- | Generate tests for generated Haskell code
  | ModeGenTests {
        genTestsModuleOpts :: HsModuleOpts
      , genTestsRenderOpts :: HsRenderOpts
      , genTestsOutput     :: FilePath
      , genTestsInputs     :: [CHeaderIncludePath]
      }
  | ModeLiterate FilePath FilePath
  | ModeBindingSpec {
        modeBindingSpec :: BindingSpecMode
      }
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
    , cmd "binding-spec" (ModeBindingSpec <$> parseBindingSpecMode) $ mconcat [
          progDesc "Binding specification commands"
        ]
    ]

{-------------------------------------------------------------------------------
  Regular modes
-------------------------------------------------------------------------------}

parseModePreprocess :: Parser Mode
parseModePreprocess =
    ModePreprocess
      <$> parseTranslationOpts
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseOutput
      <*> optional parseGenExtBindings
      <*> some parseInput

parseModeGenTests :: Parser Mode
parseModeGenTests =
    ModeGenTests
      <$> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseGenTestsOutput
      <*> some parseInput

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
parseTranslationOpts = pure def

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

parseGenExtBindings :: Parser FilePath
parseGenExtBindings =
    strOption $ mconcat [
        help "External bindings configuration to generate"
      , metavar "PATH"
      , long "gen-external-bindings"
      ]

{-------------------------------------------------------------------------------
  Binding spec mode
-------------------------------------------------------------------------------}

data BindingSpecMode =
    BindingSpecModeStdlib
  deriving (Show)

parseBindingSpecMode :: Parser BindingSpecMode
parseBindingSpecMode = subparser $ mconcat [
      cmd "stdlib" (pure BindingSpecModeStdlib) $ mconcat [
          progDesc "Write stdlib external binding specification"
        ]
    ]
