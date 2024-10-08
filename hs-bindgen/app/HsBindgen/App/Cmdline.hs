module HsBindgen.App.Cmdline (
    Cmdline(..)
  , Mode(..)
  , DevMode(..)
  , getCmdline
  ) where

import Data.Default
import Options.Applicative

import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = execParser opts
  where
    opts :: ParserInfo Cmdline
    opts = info (parseCmdline <**> helper) $ mconcat [
          header "hs-bindgen - generate Haskell bindings from C headers"
        ]

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Command line arguments
data Cmdline = Cmdline {
      cmdVerbosity :: Bool
    , cmdPredicate :: Predicate
    , cmdClangArgs :: ClangArgs
    , cmdMode      :: Mode
    }
  deriving (Show)

data Mode =
    -- | The main mode: preprocess C headers to Haskell modules
    ModePreprocess {
        input      :: FilePath
      , moduleOpts :: HsModuleOpts
      , renderOpts :: HsRenderOpts
      , output     :: Maybe FilePath
      }

  | Dev DevMode
  deriving (Show)

data DevMode =
    -- | Just parse the C header
    DevModeParseCHeader FilePath

    -- | Generate prelude (bootstrap)
  | DevModePrelude
  deriving (Show)

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline =
    Cmdline
      <$> parseVerbosity
      <*> parsePredicate
      <*> parseClangArgs
      <*> parseMode

{-------------------------------------------------------------------------------
  Mode selection
-------------------------------------------------------------------------------}

parseMode :: Parser Mode
parseMode = subparser $ mconcat [
      cmd "preprocess" parseModePreprocess $ mconcat [
          progDesc "Generate Haskell module from C header"
        ]
    , cmd "dev" parseDevMode $ mconcat [
          progDesc "Tools for the development of hs-bindgen itself"
        ]
    ]

parseDevMode :: Parser Mode
parseDevMode = fmap Dev $ subparser $ mconcat [
      cmd "parse" parseDevModeParseCHeader $ mconcat [
          progDesc "Parse C header (primarily for debugging hs-bindgen itself)"
        ]
    , cmd "prelude" parseDevModePrelude $ mconcat [
          progDesc "Trawl the C standard libraries to generate the hs-bindgen prelude"
       ]
    ]

{-------------------------------------------------------------------------------
  Regular modes
-------------------------------------------------------------------------------}

parseModePreprocess :: Parser Mode
parseModePreprocess =
    ModePreprocess
      <$> parseInput
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseOutput

{-------------------------------------------------------------------------------
  Dev modes
-------------------------------------------------------------------------------}

parseDevModeParseCHeader :: Parser DevMode
parseDevModeParseCHeader = DevModeParseCHeader <$> parseInput

parseDevModePrelude :: Parser DevMode
parseDevModePrelude = pure DevModePrelude

{-------------------------------------------------------------------------------
  Prepare input
-------------------------------------------------------------------------------}

parseVerbosity :: Parser Bool
parseVerbosity =
    switch $ mconcat [
        short 'v'
      , long "verbose"
      , help "Verbose output"
      ]

parseClangArgs :: Parser ClangArgs
parseClangArgs =
    fmap aux . many $ strOption $ mconcat [
        long "clang-option"
      , help "Pass option to libclang"
      ]
  where
    aux :: [String] -> ClangArgs
    aux args = defaultClangArgs{clangOtherArgs = args}

parseInput :: Parser FilePath
parseInput =
    strOption $ mconcat [
        help "Input path to the C header"
      , metavar "PATH"
      , long "input"
      , short 'i'
      ]

parsePredicate :: Parser Predicate
parsePredicate = fmap aux . many . asum $ [
      flag' SelectAll $ mconcat [
          long "select-all"
        , help "Process all elements"
        ]
    , fmap SelectByFileName $ strOption $ mconcat [
          long "select-by-filename"
        , help "Match filename against PCRE"
        ]
    , fmap SelectByElementName $ strOption $ mconcat [
          long "select-by-element-name"
        , help "Match element name against PCRE"
        ]
    , flag' SelectFromMainFile $ mconcat [
          long "select-from-main-file"
        , help "Only process elements from the main file (this is the default)"
        ]
    ]
  where
    aux :: [Predicate] -> Predicate
    aux [] = SelectFromMainFile
    aux ps = mconcat ps

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Internal: optparse-applicative auxiliary
-------------------------------------------------------------------------------}

cmd :: String -> Parser a -> InfoMod a -> Mod CommandFields a
cmd name p = command name . info (p <**> helper)
