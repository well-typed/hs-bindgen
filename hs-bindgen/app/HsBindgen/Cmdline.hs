module HsBindgen.Cmdline (
    Cmdline(..)
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
      cmdSpec :: Spec (IO ())
    }

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline =
    Cmdline
      <$> parseSpec

parseSpec :: Parser (Spec (IO ()))
parseSpec = subparser $ mconcat [
      cmd "process" parseCmdProcess $ mconcat [
          progDesc "Generate Haskell module from C header"
        ]
    , cmd "parse" parseCmdParse $ mconcat [
          progDesc "Parse C header (primarily for debugging hs-bindgen itself)"
        ]
    ]

parseCmdProcess :: Parser (Spec (IO ()))
parseCmdProcess =
    Preprocess
      <$> parsePrepareInput
      <*> parseTranslation
      <*> parseProcessHsOutput

parseCmdParse :: Parser (Spec (IO ()))
parseCmdParse =
    Preprocess
      <$> parsePrepareInput
      <*> pure ParseOnly
      <*> parseProcessCOutput

{-------------------------------------------------------------------------------
  Prepare input
-------------------------------------------------------------------------------}

parsePrepareInput :: Parser (PrepareInput CHeader)
parsePrepareInput =
    ParseCHeader
      <$> parseTracer
      <*> parseClangArgs
      <*> parseInput

parseTracer :: Parser (Tracer IO String)
parseTracer = mkTracerIO <$> parseVerbosity

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

parseTranslation :: Parser (Translation CHeader HsModule)
parseTranslation =
    GenModule
      <$> parseHsModuleOpts

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

parseProcessCOutput :: Parser (ProcessOutput CHeader)
parseProcessCOutput = pure PrettyC

parseProcessHsOutput :: Parser (ProcessOutput HsModule)
parseProcessHsOutput =
    PrettyHs
      <$> parseHsRenderOpts
      <*> parseOutput

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
