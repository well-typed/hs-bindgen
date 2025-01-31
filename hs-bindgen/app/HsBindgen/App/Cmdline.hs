module HsBindgen.App.Cmdline (
    Cmdline(..)
  , Mode(..)
  , DevMode(..)
  , getCmdline
  ) where

import Data.Char qualified as Char
import Data.Default
import Data.List qualified as List
import Options.Applicative
import System.FilePath
import System.Info qualified

import HsBindgen.Lib

import Paths_hs_bindgen

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = do
    dataDir <- getDataDir

    let opts :: ParserInfo Cmdline
        opts = info (parseCmdline dataDir <**> helper) $ mconcat [
              header "hs-bindgen - generate Haskell bindings from C headers"
            ]

    execParser opts

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
        preprocessInput           :: FilePath
      , preprocessTranslationOpts :: TranslationOpts
      , preprocessModuleOpts      :: HsModuleOpts
      , preprocessRenderOpts      :: HsRenderOpts
      , preprocessOutput          :: Maybe FilePath
      }
    -- | Generate tests for generated Haskell code
  | ModeGenTests {
        genTestsInput      :: FilePath
      , genTestsModuleOpts :: HsModuleOpts
      , genTestsRenderOpts :: HsRenderOpts
      , genTestsOutput     :: FilePath
      }
  | Dev DevMode
  deriving (Show)

data DevMode =
    -- | Just parse the C header
    DevModeParseCHeader {
        parseCHeaderInput :: FilePath
      }
    -- | Generate prelude (bootstrap)
  | DevModePrelude {
        preludeInput      :: FilePath
      , preludeIncludeDir :: FilePath
      }
  deriving (Show)

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseCmdline :: FilePath -> Parser Cmdline
parseCmdline dataDir =
    Cmdline
      <$> parseVerbosity
      <*> parsePredicate
      <*> parseClangArgs
      <*> parseMode dataDir

{-------------------------------------------------------------------------------
  Mode selection
-------------------------------------------------------------------------------}

parseMode :: FilePath -> Parser Mode
parseMode dataDir = subparser $ mconcat [
      cmd "preprocess" parseModePreprocess $ mconcat [
          progDesc "Generate Haskell module from C header"
        ]
    , cmd "gentests" parseModeGenTests $ mconcat [
          progDesc "Generate tests for generated Haskell code"
        ]
    , cmd "dev" (parseDevMode dataDir) $ mconcat [
          progDesc "Tools for the development of hs-bindgen itself"
        ]
    ]

parseDevMode :: FilePath -> Parser Mode
parseDevMode dataDir = fmap Dev $ subparser $ mconcat [
      cmd "parse" parseDevModeParseCHeader $ mconcat [
          progDesc "Parse C header (primarily for debugging hs-bindgen itself)"
        ]
    , cmd "prelude" (parseDevModePrelude dataDir) $ mconcat [
          progDesc "Trawl the C standard libraries to generate the hs-bindgen prelude"
       ]
    ]

{-------------------------------------------------------------------------------
  Regular modes
-------------------------------------------------------------------------------}

parseModePreprocess :: Parser Mode
parseModePreprocess =
    ModePreprocess
      <$> parseInput Nothing
      <*> parseTranslationOpts
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseOutput

parseModeGenTests :: Parser Mode
parseModeGenTests =
    ModeGenTests
      <$> parseInput Nothing
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseGenTestsOutput

{-------------------------------------------------------------------------------
  Dev modes
-------------------------------------------------------------------------------}

parseDevModeParseCHeader :: Parser DevMode
parseDevModeParseCHeader =
    DevModeParseCHeader
      <$> parseInput Nothing

parseDevModePrelude :: FilePath -> Parser DevMode
parseDevModePrelude dataDir =
    DevModePrelude
      <$> parseInput (Just stdHeaders)
      <*> parseIncludeDir includeDir
  where
    stdHeaders :: FilePath
    stdHeaders = dataDir </> "bootstrap" </> "standard_headers.h"

    includeDir :: Maybe FilePath
    includeDir = case System.Info.arch of
      "aarch64" -> Just $ dataDir </> "musl-include" </> "aarch64"
      "i386"    -> Just $ dataDir </> "musl-include" </> "i386"
      "x86_64"  -> Just $ dataDir </> "musl-include" </> "x86_64"
      _other    -> Nothing

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
    ClangArgs
      <$> parseTarget
      <*> fmap Just parseCStandard
      <*> parseGnuOption
      <*> parseOtherArgs
  where
    parseTarget :: Parser (Maybe String)
    parseTarget = optional . strOption $ mconcat [
        long "target"
      , metavar "TARGET"
      , help "Target architecture (triplet)"
      ]

    parseCStandard :: Parser CStandard
    parseCStandard = option (eitherReader readCStandard) $ mconcat [
        long "standard"
      , metavar "STANDARD"
      , value defaultCStandard
      , help $ concat [
            "C standard (default: "
          , renderCStandard defaultCStandard
          , "; supported: "
          , List.intercalate ", " (map fst cStandards)
          , ")"
          ]
      ]

    defaultCStandard :: CStandard
    defaultCStandard = C17

    renderCStandard :: CStandard -> String
    renderCStandard = map Char.toLower . show

    cStandards :: [(String, CStandard)]
    cStandards = [
        (renderCStandard cStandard, cStandard)
      | cStandard <- [minBound ..]
      ]

    readCStandard :: String -> Either String CStandard
    readCStandard s = case List.lookup s cStandards of
      Just cStandard -> Right cStandard
      Nothing -> Left $ "unknown C standard: " ++ s

    parseGnuOption :: Parser Bool
    parseGnuOption = switch $ mconcat [
        long "gnu"
      , help "Enable GNU extensions"
      ]

    parseOtherArgs :: Parser [String]
    parseOtherArgs = many . option (eitherReader readOtherArg) $ mconcat [
        long "clang-option"
      , metavar "OPTION"
      , help "Pass option to libclang"
      ]

    readOtherArg :: String -> Either String String
    readOtherArg s
      | s == "-std" || "-std=" `List.isPrefixOf` s =
          Left "C standard must be set using hs-bindgen --standard option"
      | s == "--target" || "--target=" `List.isPrefixOf` s =
          Left "Target must be set using hs-bindgen --target option"
      | otherwise = Right s

parseInput :: Maybe FilePath -> Parser FilePath
parseInput mDefault =
    strOption $ mconcat $ [
           help "Input path to the C header"
         , metavar "PATH"
         , long "input"
         , short 'i'
         ]
      ++ case mDefault of
           Nothing -> []
           Just d  -> [
               showDefault
             , value d
             ]

parseIncludeDir :: Maybe FilePath -> Parser FilePath
parseIncludeDir mDefault =
    strOption $ mconcat $ [
           help "Include directory"
         , metavar "PATH"
         , long "include"
         , short 'I'
         ]
      ++ case mDefault of
           Nothing -> []
           Just d  -> [
               showDefault
             , value d
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

{-------------------------------------------------------------------------------
  Internal: optparse-applicative auxiliary
-------------------------------------------------------------------------------}

cmd :: String -> Parser a -> InfoMod a -> Mod CommandFields a
cmd name p = command name . info (p <**> helper)
