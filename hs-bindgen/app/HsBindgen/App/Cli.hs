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

import GHC.Generics (Generic)
import Options.Applicative

import Clang.Paths
import HsBindgen.Lib

import HsBindgen.App.Common

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
    CliCmdPreprocess  PreprocessOpts
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
          progDesc $ mconcat [
              "Generate Haskell module from C header, acting as literate Haskell preprocessor; "
            , "used to incorporate hs-bindgen into the cabal compilation pipeline; "
            , "not meant to be used directly but is invoked by cabal-install"
            ]
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
      config            :: Config
    , inputs            :: [CHeaderIncludePath]
    , output            :: Maybe FilePath
    , bindingSpecConfig :: BindingSpecConfig
    , genBindingSpec    :: Maybe FilePath
    }
  deriving stock (Show, Generic)

parsePreprocessOpts :: Parser PreprocessOpts
parsePreprocessOpts =
    PreprocessOpts
      <$> parseConfig
      <*> parseInputs
      <*> parseOutput
      <*> parseBindingSpecConfig
      <*> optional parseGenBindingSpec

{-------------------------------------------------------------------------------
  Test generation command
-------------------------------------------------------------------------------}

data GenTestsOpts = GenTestsOpts {
      config            :: Config
    , output            :: FilePath
    , inputs            :: [CHeaderIncludePath]
    , bindingSpecConfig :: BindingSpecConfig
    }
  deriving stock (Show, Generic)

parseGenTestsOpts :: Parser GenTestsOpts
parseGenTestsOpts =
    GenTestsOpts
      <$> parseConfig
      <*> parseGenTestsOutput
      <*> parseInputs
      <*> parseBindingSpecConfig

{-------------------------------------------------------------------------------
  Literate command
-------------------------------------------------------------------------------}

data LiterateOpts = LiterateOpts {
      input  :: FilePath
    , output :: FilePath
    }
  deriving stock (Show, Generic)

parseLiterateOpts :: Parser LiterateOpts
parseLiterateOpts = do
    -- When @cabal-install@ calls GHC and the preprocessor, it passes some
    -- standard flags, which we do not (all) use. In particular, it passes
    -- @-hide-all-packages@.
    _ <- strOption @String $ mconcat [
             short 'h'
           , metavar "IGNORED"
           , help "Ignore some preprocessor options provided by cabal-install"
           ]

    input  <- strArgument $ mconcat [ metavar "IN" ]
    output <- strArgument $ mconcat [ metavar "OUT" ]
    return LiterateOpts {..}

{-------------------------------------------------------------------------------
  Binding spec commands
-------------------------------------------------------------------------------}

data BindingSpecCmd =
    BindingSpecCmdStdlib {
      clangArgs :: ClangArgs
    }
  deriving stock (Show, Generic)

parseBindingSpecCmd :: Parser BindingSpecCmd
parseBindingSpecCmd = subparser $ mconcat [
      cmd "stdlib" parseBindingSpecCmdStdlib $ mconcat [
          progDesc "Write stdlib external binding specification"
        ]
    ]

parseBindingSpecCmdStdlib :: Parser BindingSpecCmd
parseBindingSpecCmdStdlib = BindingSpecCmdStdlib <$> parseClangArgs

{-------------------------------------------------------------------------------
  Resolve command
-------------------------------------------------------------------------------}

data ResolveOpts = ResolveOpts {
      inputs    :: [CHeaderIncludePath]
    , clangArgs :: ClangArgs
    }
  deriving stock (Show, Generic)

parseResolveOpts :: Parser ResolveOpts
parseResolveOpts =
    ResolveOpts
      <$> parseInputs
      <*> parseClangArgs

{-------------------------------------------------------------------------------
  Output options
-------------------------------------------------------------------------------}

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
