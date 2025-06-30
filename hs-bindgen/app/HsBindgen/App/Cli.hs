{-# LANGUAGE ApplicativeDo #-}
module HsBindgen.App.Cli (
    Cli(..)
  , CliCmd(..)
  , PreprocessOpts(..)
  , GenTestsOpts(..)
  , LiterateOpts(..)
  , BindingSpecCmd(..)
  , ResolveOpts(..)
  , getCli
  , pureParseCmdPreprocess
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
    , cliCmd        :: CliCmd
    }
  deriving (Show)

parseCli :: Parser Cli
parseCli =
    Cli
      <$> parseGlobalOpts
      <*> parseCliCmd

data CliCmd =
    -- | The main command: preprocess C headers to Haskell modules
    CliCmdPreprocess  PreprocessOpts
    -- | Generate tests for generated Haskell code
  | CliCmdGenTests    GenTestsOpts
  | CliCmdLiterate    LiterateOpts
  | CliCmdBindingSpec BindingSpecCmd
  | CliCmdResolve     ResolveOpts
  deriving (Show)

parseCliCmd :: Parser CliCmd
parseCliCmd = subparser $ mconcat [
      cmd "preprocess" (CliCmdPreprocess <$> parsePreprocessOpts) $ mconcat [
          progDesc "Generate Haskell module from C headers"
        ]
    , cmd "gentests" (CliCmdGenTests <$> parseGenTestsOpts) $ mconcat [
          progDesc "Generate tests for generated Haskell code"
        ]
    , cmd' "literate" (CliCmdLiterate <$> parseLiterateOpts) $ mconcat [
          progDesc "Generate Haskell module from C header, acting as literate Haskell preprocessor"
        ]
    , cmd "binding-spec" (CliCmdBindingSpec <$> parseBindingSpecCmd) $ mconcat [
          progDesc "Binding specification commands"
        ]
    , cmd "resolve" (CliCmdResolve <$> parseResolveOpts) $ mconcat [
          progDesc "Resolve C headers to source paths, for debugging"
        ]
    ]

pureParseCmdPreprocess :: [String] -> Maybe Cli
pureParseCmdPreprocess =
      getParseResult
    . execParserPure (prefs subparserInline) (info parseCli mempty)
    . ("preprocess" :)

{-------------------------------------------------------------------------------
  Preprocess command
-------------------------------------------------------------------------------}

data PreprocessOpts = PreprocessOpts {
      preprocessTranslationOpts :: TranslationOpts
    , preprocessModuleOpts      :: HsModuleOpts
    , preprocessRenderOpts      :: HsRenderOpts
    , preprocessOutput          :: Maybe FilePath
    , preprocessGenBindingSpec  :: Maybe FilePath
    , preprocessInputs          :: [CHeaderIncludePath]
    }
  deriving (Show)

parsePreprocessOpts :: Parser PreprocessOpts
parsePreprocessOpts =
    PreprocessOpts
      <$> parseTranslationOpts
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseOutput
      <*> optional parseGenBindingSpec
      <*> parseInputs

{-------------------------------------------------------------------------------
  Test generation command
-------------------------------------------------------------------------------}

data GenTestsOpts = GenTestsOpts {
      genTestsTranslationOpts :: TranslationOpts
    , genTestsModuleOpts      :: HsModuleOpts
    , genTestsRenderOpts      :: HsRenderOpts
    , genTestsOutput          :: FilePath
    , genTestsInputs          :: [CHeaderIncludePath]
    }
  deriving (Show)

parseGenTestsOpts :: Parser GenTestsOpts
parseGenTestsOpts =
    GenTestsOpts
      <$> parseTranslationOpts
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseGenTestsOutput
      <*> parseInputs

{-------------------------------------------------------------------------------
  Literate command
-------------------------------------------------------------------------------}

data LiterateOpts = LiterateOpts {
      literateInput  :: FilePath
    , literateOutput :: FilePath
    }
  deriving (Show)

parseLiterateOpts :: Parser LiterateOpts
parseLiterateOpts = do
    _ <- strOption @String $ mconcat [ short 'h', metavar "IGNORED" ]

    input  <- strArgument $ mconcat [ metavar "IN" ]
    output <- strArgument $ mconcat [ metavar "OUT" ]
    return (LiterateOpts input output)

{-------------------------------------------------------------------------------
  Binding spec commands
-------------------------------------------------------------------------------}

data BindingSpecCmd =
    BindingSpecCmdStdlib
  deriving (Show)

parseBindingSpecCmd :: Parser BindingSpecCmd
parseBindingSpecCmd = subparser $ mconcat [
      cmd "stdlib" (pure BindingSpecCmdStdlib) $ mconcat [
          progDesc "Write stdlib external binding specification"
        ]
    ]

{-------------------------------------------------------------------------------
  Resolve command
-------------------------------------------------------------------------------}

data ResolveOpts = ResolveOpts {
      resolveInputs :: [CHeaderIncludePath]
    }
  deriving (Show)

parseResolveOpts :: Parser ResolveOpts
parseResolveOpts =
    ResolveOpts
      <$> parseInputs

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
