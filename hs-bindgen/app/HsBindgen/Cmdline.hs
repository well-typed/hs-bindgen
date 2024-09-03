module HsBindgen.Cmdline (
    Cmdline(..)
  , Mode(..)
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
      verbosity :: Bool
    , predicate :: Predicate
    , clangArgs :: ClangArgs
    , mode      :: Mode
    }
  deriving (Show)

data Mode =
    -- | The main mode: preprocess C headers to Haskell modules
    Preprocess {
        input      :: FilePath
      , moduleOpts :: HsModuleOpts
      , renderOpts :: HsRenderOpts
      , output     :: Maybe FilePath
      }

    -- | Just parse the C header
  | ParseCHeader {
        input :: FilePath
      }

    -- | Show the raw @libclang@ AST
  | ShowClangAST {
        input :: FilePath
      }
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

parseMode :: Parser Mode
parseMode = subparser $ mconcat [
      cmd "preprocess" parseModePreprocess $ mconcat [
          progDesc "Generate Haskell module from C header"
        ]
    , cmd "parse" parseModeParseCHeader $ mconcat [
          progDesc "Parse C header (primarily for debugging hs-bindgen itself)"
        ]
    , cmd "show-clang-ast" parseModeDumpClangAST $ mconcat [
          progDesc "Show the libclang AST (primarily for development of hs-bindgen itself)"
        ]
    ]

parseModePreprocess :: Parser Mode
parseModePreprocess =
    Preprocess
      <$> parseInput
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseOutput

parseModeParseCHeader :: Parser Mode
parseModeParseCHeader =
    ParseCHeader
      <$> parseInput

parseModeDumpClangAST :: Parser Mode
parseModeDumpClangAST =
    ShowClangAST
      <$> parseInput

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
    many $ strOption $ mconcat [
        long "clang-option"
      , help "Pass option to libclang"
      ]

parseInput :: Parser FilePath
parseInput =
    strOption $ mconcat [
        help "Input path to the C header"
      , metavar "PATH"
      , long "input"
      , short 'i'
      ]

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
