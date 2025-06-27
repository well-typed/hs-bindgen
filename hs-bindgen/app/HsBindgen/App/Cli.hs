{-# LANGUAGE ApplicativeDo #-}
module HsBindgen.App.Cli (
    Cli(..)
  , CliMode(..)
  , PreprocessMode(..)
  , GenTestsMode(..)
  , LiterateMode(..)
  , BindingSpecMode(..)
  , ResolveMode(..)
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
        , footerDoc (Just $ footerWith p)
        ]

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Command line arguments
data Cli = Cli {
      cliGlobalOpts :: GlobalOpts
    , cliMode       :: CliMode
    }
  deriving (Show)

parseCli :: Parser Cli
parseCli =
    Cli
      <$> parseGlobalOpts
      <*> parseCliMode

data CliMode =
    -- | The main mode: preprocess C headers to Haskell modules
    CliModePreprocess  PreprocessMode
    -- | Generate tests for generated Haskell code
  | CliModeGenTests    GenTestsMode
  | CliModeLiterate    LiterateMode
  | CliModeBindingSpec BindingSpecMode
  | CliModeResolve     ResolveMode
  deriving (Show)

parseCliMode :: Parser CliMode
parseCliMode = subparser $ mconcat [
      cmd "preprocess" (CliModePreprocess <$> parsePreprocessMode) $ mconcat [
          progDesc "Generate Haskell module from C header"
        ]
    , cmd "gentests" (CliModeGenTests <$> parseGenTestsMode) $ mconcat [
          progDesc "Generate tests for generated Haskell code"
        ]
    , cmd' "literate" (CliModeLiterate <$> parseLiterateMode) $ mconcat [
          progDesc "Generate Haskell module from C header, acting as literate Haskell preprocessor"
        ]
    , cmd "binding-spec" (CliModeBindingSpec <$> parseBindingSpecMode) $ mconcat [
          progDesc "Binding specification commands"
        ]
    , cmd "resolve" (CliModeResolve <$> parseResolveMode) $ mconcat [
          progDesc "Resolve C headers to source paths, for debugging"
        ]
    ]

pureParseModePreprocess :: [String] -> Maybe Cli
pureParseModePreprocess =
      getParseResult
    . execParserPure (prefs subparserInline) (info parseCli mempty)
    . ("preprocess" :)

{-------------------------------------------------------------------------------
  Preprocess mode
-------------------------------------------------------------------------------}

data PreprocessMode = PreprocessMode {
      preprocessTranslationOpts :: TranslationOpts
    , preprocessModuleOpts      :: HsModuleOpts
    , preprocessRenderOpts      :: HsRenderOpts
    , preprocessOutput          :: Maybe FilePath
    , preprocessGenBindingSpec  :: Maybe FilePath
    , preprocessInputs          :: [CHeaderIncludePath]
    }
  deriving (Show)

parsePreprocessMode :: Parser PreprocessMode
parsePreprocessMode =
    PreprocessMode
      <$> parseTranslationOpts
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseOutput
      <*> optional parseGenBindingSpec
      <*> some parseInput

{-------------------------------------------------------------------------------
  Test generation mode
-------------------------------------------------------------------------------}

data GenTestsMode = GenTestsMode {
      genTestsTranslationOpts :: TranslationOpts
    , genTestsModuleOpts      :: HsModuleOpts
    , genTestsRenderOpts      :: HsRenderOpts
    , genTestsOutput          :: FilePath
    , genTestsInputs          :: [CHeaderIncludePath]
    }
  deriving (Show)

parseGenTestsMode :: Parser GenTestsMode
parseGenTestsMode =
    GenTestsMode
      <$> parseTranslationOpts
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseGenTestsOutput
      <*> some parseInput

{-------------------------------------------------------------------------------
  Literate mode
-------------------------------------------------------------------------------}

data LiterateMode = LiterateMode {
      literateInput  :: FilePath
    , literateOutput :: FilePath
    }
  deriving (Show)

parseLiterateMode :: Parser LiterateMode
parseLiterateMode = do
    _ <- strOption @String $ mconcat [ short 'h', metavar "IGNORED" ]

    input  <- strArgument $ mconcat [ metavar "IN" ]
    output <- strArgument $ mconcat [ metavar "OUT" ]
    return (LiterateMode input output)

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

{-------------------------------------------------------------------------------
  Resolve mode
-------------------------------------------------------------------------------}

data ResolveMode = ResolveMode {
      resolveInputs :: [CHeaderIncludePath]
    }
  deriving (Show)

parseResolveMode :: Parser ResolveMode
parseResolveMode =
    ResolveMode
      <$> some parseInput

{-------------------------------------------------------------------------------
  Translation options
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
  Output options
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

parseGenBindingSpec :: Parser FilePath
parseGenBindingSpec =
    strOption $ mconcat [
        help "Binding specification to generate"
      , metavar "PATH"
      , long "gen-binding-spec"
      ]
