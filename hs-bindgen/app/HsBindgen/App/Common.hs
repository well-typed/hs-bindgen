{-# LANGUAGE ApplicativeDo #-}
module HsBindgen.App.Common (
    -- * Global options
    GlobalOpts(..)
  , parseGlobalOpts
    -- * HsBindgen 'Config'
  , parseConfig
    -- * Clang-related options
  , parseClangArgs
    -- * Binding specifications
  , BindingSpecConfig(..)
  , parseBindingSpecConfig
    -- * Input option
  , parseInputs
    -- * Auxiliary hs-bindgen functions
  , fromMaybeWithFatalError
  , loadBindingSpecs
  , footerWith
    -- * Auxiliary optparse-applicative functions
  , cmd
  , cmd'
  ) where

import Control.Exception (Exception (displayException))
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (Bifunctor (bimap), first, second)
import Data.Char qualified as Char
import Data.Either (partitionEithers)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative
import Options.Applicative.Extra (helperWith)
import Options.Applicative.Help (Doc, align, extractChunk, pretty, tabulate,
                                 vcat, (<+>))
import Prettyprinter.Util (reflow)

import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Options and arguments
-------------------------------------------------------------------------------}

data GlobalOpts = GlobalOpts {
      tracerConfig :: TracerConfig
    }
  deriving stock (Show)

parseGlobalOpts :: Parser GlobalOpts
parseGlobalOpts = GlobalOpts <$> parseTracerConfig

{-------------------------------------------------------------------------------
  Tracer configuration
-------------------------------------------------------------------------------}

parseTracerConfig :: Parser TracerConfig
parseTracerConfig =
    TracerConfig
      <$> parseVerbosity
      <*> parseShowTimeStamp
      <*> parseShowCallStack

parseVerbosity :: Parser Verbosity
parseVerbosity =
  nToVerbosity <$>
    (option auto $
      mconcat [ short 'v'
              , long "verbosity"
              , metavar "INT"
              , value 2
              , help "Specify verbosity (0: error, 1: warning, 2: notice, 3: info, 4: debug);"
              , showDefault
              ])

  where
    nToVerbosity :: Int -> Verbosity
    nToVerbosity = Verbosity . \case
      n | n <= 0 -> Error
      1          -> Warning
      2          -> Notice
      3          -> Info
      _otherwise -> Debug

parseShowTimeStamp :: Parser ShowTimeStamp
parseShowTimeStamp = flag DisableTimeStamp EnableTimeStamp $ mconcat [
      short 't'
    , long "show-time"
    , help "Show time stamps in traces"
    ]

parseShowCallStack :: Parser ShowCallStack
parseShowCallStack = flag DisableCallStack EnableCallStack $ mconcat [
      short 's'
    , long "show-call-stack"
    , help "Show call stacks in traces"
    ]

{-------------------------------------------------------------------------------
  HsBindgen Config
-------------------------------------------------------------------------------}

parseConfig :: Parser Config
parseConfig = Config
    <$> parseClangArgs
    <*> parseTranslationOpts
    <*> parsePredicate
    <*> parseProgramSlicing
    <*> parseHsModuleOpts
    <*> parseHsRenderOpts
  where
    parseTranslationOpts :: Parser TranslationOpts
    parseTranslationOpts = pure def

{-------------------------------------------------------------------------------
  Clang arguments
-------------------------------------------------------------------------------}

parseClangArgs :: Parser ClangArgs
parseClangArgs = do
    -- ApplicativeDo to be able to reorder arguments for --help, and to use
    -- record construction (i.e., to avoid bool or string/path blindness)
    -- instead of positional one.
    clangTarget                <- optional parseTarget
    clangCStandard             <- Just <$> parseCStandard
    clangStdInc                <- not <$> parseNoStdInc
    clangEnableGnu             <- parseGnuOption
    clangSystemIncludePathDirs <- parseSystemIncludeDirOptions
    clangQuoteIncludePathDirs  <- parseQuoteIncludeDirOptions
    clangEnableBlocks          <- parseEnableBlocks
    clangOtherArgs             <- parseOtherArgs
    pure ClangArgs {..}

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

parseNoStdInc :: Parser Bool
parseNoStdInc = switch $ mconcat [
      long "no-stdinc"
    , help "Disable standard include directories"
    ]

parseGnuOption :: Parser Bool
parseGnuOption = switch $ mconcat [
      long "gnu"
    , help "Enable GNU extensions"
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

-- TODO: Perhaps we should mimick clang's @-f@ parameter?
parseEnableBlocks :: Parser Bool
parseEnableBlocks = switch $ mconcat [
      long "enable-blocks"
    , help "Enable the 'blocks' language feature"
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

{-------------------------------------------------------------------------------
  Translation options
-------------------------------------------------------------------------------}

parseHsModuleOpts :: Parser HsModuleOpts
parseHsModuleOpts =
    HsModuleOpts
      <$> strOption (mconcat [
              help "Name of the generated Haskell module"
            , metavar "NAME"
            , long "module"
            , showDefault
            , value $ hsModuleOptsName def
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

{-------------------------------------------------------------------------------
  Binding specifications
-------------------------------------------------------------------------------}

data BindingSpecConfig = BindingSpecConfig {
      stdlibSpec              :: EnableStdlibBindingSpec
    , extBindingSpecs         :: [FilePath]
    , prescriptiveBindingSpec :: Maybe FilePath
    }
  deriving stock (Show)

parseBindingSpecConfig :: Parser BindingSpecConfig
parseBindingSpecConfig =
    BindingSpecConfig
    <$> parseEnableStdlibBindingSpec
    <*> parseExtBindingSpecs
    <*> parsePrescriptiveBindingSpec

parseEnableStdlibBindingSpec :: Parser EnableStdlibBindingSpec
parseEnableStdlibBindingSpec = flag EnableStdlibBindingSpec DisableStdlibBindingSpec $
    mconcat [
        long "no-stdlib"
      , help "Do not automatically use stdlib external binding specification"
      ]

parseExtBindingSpecs :: Parser [FilePath]
parseExtBindingSpecs = many . strOption $ mconcat [
      long "external-binding-spec"
    , metavar "FILE"
    , help "External binding specification (YAML file)"
    ]

parsePrescriptiveBindingSpec :: Parser (Maybe FilePath)
parsePrescriptiveBindingSpec = optional $ strOption $ mconcat [
      long "prescriptive-binding-spec"
    , metavar "FILE"
    , help "Prescriptive binding specification (YAML file)"
    ]

{-------------------------------------------------------------------------------
  Other options and command line arguments
-------------------------------------------------------------------------------}

parsePredicate :: Parser Predicate
parsePredicate = fmap aux . many . asum $ [
      flag' (Right SelectAll) $ mconcat [
          long "select-all"
        , help "Process all elements"
        ]
    , fmap (Right . SelectByFileName) $ strOption $ mconcat [
          long "select-by-filename"
        , help "Select files with names that match PCRE"
        , metavar "PCRE"
        ]
    , fmap (Left . SelectByFileName) $ strOption $ mconcat [
          long "skip-by-filename"
        , help "Skip files with names that match PCRE"
        , metavar "PCRE"
        ]
    , fmap (Right . SelectByElementName) $ strOption $ mconcat [
          long "select-by-element-name"
        , help "Select elements with names that match PCRE"
        , metavar "PCRE"
        ]
    , fmap (Left . SelectByElementName) $ strOption $ mconcat [
          long "skip-by-element-name"
        , help "Skip elements with names that match PCRE"
        , metavar "PCRE"
        ]
    , flag' (Right SelectFromMainFiles) $ mconcat [
          long "select-from-main-files"
        , help "Only process elements from main files (default)"
        ]
    ]
  where
    defSelectFromMainFiles :: [Predicate] -> [Predicate]
    defSelectFromMainFiles [] = [SelectFromMainFiles]
    defSelectFromMainFiles ps = ps
    aux :: [Either Predicate Predicate] -> Predicate
    aux ps = uncurry mergePredicates $
               second defSelectFromMainFiles $
                 partitionEithers ps

parseProgramSlicing :: Parser ProgramSlicing
parseProgramSlicing = flag DisableProgramSlicing EnableProgramSlicing $ mconcat [
      long "enable-program-slicing"
    , help $ "Enable program slicing: "
        <> "Select declarations using the selection predicate, "
        <> "and also select their transitive dependencies"
    ]

{-------------------------------------------------------------------------------
  Input arguments
-------------------------------------------------------------------------------}

-- | Parse one or more input header arguments
--
-- This uses standard syntax for one or more arguments, which
-- @optparse-applicative@ does not get right when just using 'some'.
parseInputs :: Parser [CHeaderIncludePath]
parseInputs = some . argument (eitherReader parseHeader) $ mconcat [
      help "Input C header(s), relative to an include path directory"
    , metavar "HEADER..."
    ]
  where
    parseHeader :: String -> Either String CHeaderIncludePath
    parseHeader = first displayException . parseCHeaderIncludePath

{-------------------------------------------------------------------------------
  Auxiliary hs-bindgen functions
-------------------------------------------------------------------------------}

-- | Extract the result or exit gracefully with an error message.
--
-- Helper function to be used in conjunction with 'withTracer'. We carefully
-- separate running actions from error handling; before we continue to process
-- the result.
fromMaybeWithFatalError :: MonadIO m => Maybe b -> m b
fromMaybeWithFatalError k = maybe fatalError pure k

loadBindingSpecs ::
     Tracer IO TraceMsg
  -> ClangArgs
  -> BindingSpecConfig
  -> IO (BindingSpec, BindingSpec)
loadBindingSpecs tracer clangArgs opts = do
    extSpecs <- loadExtBindingSpecs
                  tracer
                  clangArgs
                  opts.stdlibSpec
                  opts.extBindingSpecs
    pSpec <- case opts.prescriptiveBindingSpec of
               Just path -> loadPrescriptiveBindingSpec
                              tracer
                              clangArgs
                              path
               Nothing   -> pure emptyBindingSpec
    pure (extSpecs, pSpec)

-- | Footer of command line help.
footerWith :: ParserPrefs -> Doc
footerWith p = vcat [ environmentVariablesFooter p
                    , ""
                    , selectSliceFooter p
                    ]

selectSliceFooter :: ParserPrefs -> Doc
selectSliceFooter _ =
   vcat [ pretty ("Selection and program slicing:" :: String)
        , "-" <+> align (reflow $ mconcat [
            "Program slicing disabled (default): "
          , "Only select declarations according to the selection predicate."
          ])
        , "-" <+> align (reflow $ mconcat [
            "Program slicing enabled ('--enable-program-slicing'): "
            , "Select declarations using the selection predicate, "
            , "and also select their transitive dependencies."
          ])
        ]

environmentVariablesFooter :: ParserPrefs -> Doc
environmentVariablesFooter p =
  vcat [ pretty ("Environment variables:" :: String)
       , prettyEnvVars
       ]
  where
    prettyEnvVars :: Doc
    prettyEnvVars = extractChunk $ tabulate (prefTabulateFill p) envVarsDocs

    targets :: [Target]
    targets = [ minBound .. maxBound ]

    triples :: [String]
    triples = map (`targetTriple` TargetEnvDefault) targets

    envVarsDocs :: [(Doc, Doc)]
    envVarsDocs = map (bimap pretty  (align . reflow)) envVars

    envVars :: [(Text, Text)]
    envVars = [ ("BINDGEN_EXTRA_CLANG_ARGS",
                 "Extra command line arguments passed to `libclang`")
              , ("BINDGEN_EXTRA_CLANG_ARGS_<TARGET>",
                 "Per-target arguments passed to `libclang`"
                 <> ", precedes BINDGEN_EXTRA_CLANG_ARGS if using a specific target"
                 <> "; possible targets: "
                 <> Text.intercalate ", " (map Text.pack triples) )
              ]

{-------------------------------------------------------------------------------
  Auxiliary optparse-applicative functions
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
