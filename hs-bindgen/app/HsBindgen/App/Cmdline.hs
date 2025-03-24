{-# LANGUAGE ApplicativeDo #-}
module HsBindgen.App.Cmdline (
    Cmdline(..)
  , Mode(..)
  , DevMode(..)
  , getCmdline
  , pureParseModePreprocess
  ) where

import Control.Exception (Exception(displayException))
import Data.Bifunctor (first)
import Data.Char qualified as Char
import Data.Default
import Data.List qualified as List
import Options.Applicative
import Options.Applicative.Extra (helperWith)

import Clang.Paths
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = customExecParser p opts
  where
    p = prefs $ helpShowGlobals <> subparserInline

    opts :: ParserInfo Cmdline
    opts = info (parseCmdline <**> helper) $
      mconcat [
          header "hs-bindgen - generate Haskell bindings from C headers"
        ]

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Command line arguments
data Cmdline = Cmdline {
      cmdVerbosity   :: Bool
    , cmdPredicate   :: Predicate
    , cmdClangArgs   :: ClangArgs
    , cmdExtBindings :: [FilePath]
    , cmdMode        :: Mode
    }
  deriving (Show)

data Mode =
    -- | The main mode: preprocess C headers to Haskell modules
    ModePreprocess {
        preprocessInput           :: CHeaderIncludePath
      , preprocessTranslationOpts :: TranslationOpts
      , preprocessPackageName     :: Maybe HsPackageName
      , preprocessModuleOpts      :: HsModuleOpts
      , preprocessRenderOpts      :: HsRenderOpts
      , preprocessOutput          :: Maybe FilePath
      , preprocessGenExtBindings  :: Maybe FilePath
      }
    -- | Generate tests for generated Haskell code
  | ModeGenTests {
        genTestsInput      :: CHeaderIncludePath
      , genTestsModuleOpts :: HsModuleOpts
      , genTestsRenderOpts :: HsRenderOpts
      , genTestsOutput     :: FilePath
      }
  | ModeLiterate FilePath FilePath
  | Dev DevMode
  deriving (Show)

data DevMode =
    -- | Just parse the C header
    DevModeParseCHeader {
        parseCHeaderInput :: CHeaderIncludePath
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
      <*> parseExtBindings
      <*> parseMode

pureParseModePreprocess :: [String] -> Maybe Cmdline
pureParseModePreprocess =
      getParseResult
    . execParserPure (prefs subparserInline) (info parseCmdline mempty)
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
    , cmd "dev" parseDevMode $ mconcat [
          progDesc "Tools for the development of hs-bindgen itself"
        ]
    ]

parseDevMode :: Parser Mode
parseDevMode = fmap Dev $ subparser $ mconcat [
      cmd "parse" parseDevModeParseCHeader $ mconcat [
          progDesc "Parse C header (primarily for debugging hs-bindgen itself)"
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
      <*> optional parseHsPackageName
      <*> parseHsModuleOpts
      <*> parseHsRenderOpts
      <*> parseOutput
      <*> optional parseGenExtBindings

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
  Dev modes
-------------------------------------------------------------------------------}

parseDevModeParseCHeader :: Parser DevMode
parseDevModeParseCHeader =
    DevModeParseCHeader
      <$> parseInput

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
parseClangArgs = do
    -- ApplicativeDo to be able reorder arguments for --help
    -- and uses record construction (i.a. to avoid bool or string/path blindness) instead of positional one.
    clangTarget <- optional parseTarget
    clangCStandard <- fmap Just parseCStandard
    clangStdInc <- fmap not parseNoStdInc
    clangEnableGnu <-parseGnuOption
    clangSystemIncludePathDirs <- parseSystemIncludeDirOptions
    clangQuoteIncludePathDirs <- parseQuoteIncludeDirOptions
    clangOtherArgs <- parseOtherArgs
    pure ClangArgs {..}

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
  where
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

parseNoStdInc :: Parser Bool
parseNoStdInc = switch $ mconcat [
      long "no-stdinc"
    , help "Disable standard include directories"
    ]

parseSystemIncludeDirOptions :: Parser [CIncludePathDir]
parseSystemIncludeDirOptions = many . strOption $ mconcat [
      long "system-include-path"
    , metavar "DIR"
    , help "System include search path directory"
    ]

parseQuoteIncludeDirOptions :: Parser [CIncludePathDir]
parseQuoteIncludeDirOptions = many . strOption $ mconcat [
      short 'I'
    , long "include-path"
    , metavar "DIR"
    , help "Quote include search path directory"
    ]

parseOtherArgs :: Parser [String]
parseOtherArgs = many . option (eitherReader readOtherArg) $ mconcat [
      long "clang-option"
    , metavar "OPTION"
    , help "Pass option to libclang"
    ]
  where
    readOtherArg :: String -> Either String String
    readOtherArg s
      | "-I" `List.isPrefixOf` s =
          Left "Include path must be set using hs-bindgen --include-path options"
      | "-isystem" `List.isPrefixOf` s =
          Left "System include path must be set using hs-bindgen --system-include-path options"
      | s == "-nostdinc" =
          Left "No standard includes option must be set using hs-bindgen --no-stdinc option"
      | s == "-std" || "-std=" `List.isPrefixOf` s =
          Left "C standard must be set using hs-bindgen --standard option"
      | s == "--target" || "--target=" `List.isPrefixOf` s =
          Left "Target must be set using hs-bindgen --target option"
      | otherwise = Right s

parseExtBindings :: Parser [FilePath]
parseExtBindings = many . strOption $ mconcat [
      long "external-bindings"
    , metavar "FILE"
    , help "External bindings configuration (YAML file)"
    ]

parseTarget :: Parser (Target, TargetEnv)
parseTarget = option (maybeReader readTarget) $ mconcat [
      long "target"
    , metavar "TRIPLE"
    , help $ concat [
          "Target (for cross-compilation); supported: "
        , List.intercalate ", " (map fst targets)
        ]
    ]
  where
    targets :: [(String, Target)]
    targets = [
        (targetTriple target TargetEnvDefault, target)
      | target <- [minBound ..]
      ]

    readTarget :: String -> Maybe (Target, TargetEnv)
    readTarget s = asum [
          (, TargetEnvDefault) <$> lookup s targets
        , do (rest, env) <- trySplitOffEnv s
             (, TargetEnvOverride env) <$> lookup rest targets
        ]

    trySplitOffEnv :: String -> Maybe (String, String)
    trySplitOffEnv s =
        case break (== '-') (reverse s) of
          (_   , []    ) -> Nothing
          (env , _:rest) -> Just (reverse rest, reverse env)

parseInput :: Parser CHeaderIncludePath
parseInput =
    option (eitherReader $ first displayException . parseCHeaderIncludePath) $
      mconcat $ [
          help "Input C header, relative to an include path directory"
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

parseTranslationOpts :: Parser TranslationOpts
parseTranslationOpts = pure defaultTranslationOpts

parseHsPackageName :: Parser HsPackageName
parseHsPackageName =
    HsPackageName
      <$> strOption (mconcat [
              help "Package name for generated external bindings"
            , metavar "NAME"
            , long "package"
            ])

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
  Internal: optparse-applicative auxiliary
-------------------------------------------------------------------------------}

cmd :: String -> Parser a -> InfoMod a -> Mod CommandFields a
cmd name p = command name . info (p <**> helper)

-- | Like cmd but without '-h'
cmd' :: String -> Parser a -> InfoMod a -> Mod CommandFields a
cmd' name p = command name . info (p <**> helper') where
  helper' :: Parser (a -> a)
  helper' =
    helperWith (mconcat [
      long "help",
      help "Show this help text"
    ])
