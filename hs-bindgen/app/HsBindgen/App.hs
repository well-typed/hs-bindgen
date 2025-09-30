{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLabels #-}

-- | @hs-bindgen@ application common types and functions
module HsBindgen.App (
    -- * Global options
    GlobalOpts(..)
  , parseGlobalOpts

    -- * Argument/option parsers
    -- ** Bindgen configuration
  , Config
  , parseConfig
  , parseConfigPP
    -- ** Clang arguments
  , parseClangArgsConfig
    -- ** Output options
  , parseHsOutputDir
  , parseGenBindingSpec
  , parseGenTestsOutput
    -- ** Input arguments
  , parseInputs

    -- * Auxiliary optparse-applicative functions
  , cmd
  , cmd_
  ) where

import Data.Char qualified as Char
import Data.Default (Default (..))
import Data.Either (partitionEithers)
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Options.Applicative
import Options.Applicative.Extra (helperWith)

import HsBindgen.Backend.Hs.Haddock.Config
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.Backend.UniqueId
import HsBindgen.BindingSpec
import HsBindgen.Config
import HsBindgen.Config.ClangArgs
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.RootHeader (UncheckedHashIncludeArg)
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Global options
-------------------------------------------------------------------------------}

data GlobalOpts = GlobalOpts {
      tracerConfig        :: TracerConfig IO Level TraceMsg
    }

parseGlobalOpts :: Parser GlobalOpts
parseGlobalOpts = GlobalOpts <$> parseTracerConfig

{-------------------------------------------------------------------------------
  Tracer configuration
-------------------------------------------------------------------------------}

parseTracerConfig :: Parser (TracerConfig IO Level TraceMsg)
parseTracerConfig =
    TracerConfig
      <$> parseVerbosity
      <*> pure def
      <*> parseCustomLogLevel
      <*> parseShowTimeStamp
      <*> parseShowCallStack

parseVerbosity :: Parser Verbosity
parseVerbosity = fmap nToVerbosity . option auto $ mconcat [
      short 'v'
    , long "verbosity"
    , metavar "INT"
    , value 2
    , help "Verbosity (0: error, 1: warning, 2: notice, 3: info, 4: debug)"
    , showDefault
    ]
  where
    nToVerbosity :: Int -> Verbosity
    nToVerbosity = Verbosity . \case
      n | n <= 0 -> Error
      1          -> Warning
      2          -> Notice
      3          -> Info
      _otherwise -> Debug

parseCustomLogLevel :: Parser (CustomLogLevel Level TraceMsg)
parseCustomLogLevel = do
    -- Generic setters
    makeTraceInfos    <- many $ parseMakeTrace Info
    makeTraceWarnings <- many $ parseMakeTrace Warning
    makeTraceErrors   <- many $ parseMakeTrace Error
    -- Generic modifiers
    makeWarningsErrors <- optional parseMakeWarningsErrors
    -- Specific setters
    enableMacroWarnings <- optional parseEnableMacroWarnings
    pure $ getCustomLogLevel $ catMaybes [
        enableMacroWarnings
      , makeWarningsErrors
      ]
      ++ makeTraceInfos
      ++ makeTraceWarnings
      ++ makeTraceErrors
  where
    parseMakeTrace :: Level -> Parser CustomLogLevelSetting
    parseMakeTrace level =
      let levelStr = map Char.toLower $ show level
      in  fmap (MakeTrace level) . strOption $ mconcat [
              long $ "log-as-" <> levelStr
            , metavar "TRACE_ID"
            , help $ "Set log level of traces with TRACE_ID to " <> levelStr
            ]

    parseMakeWarningsErrors :: Parser CustomLogLevelSetting
    parseMakeWarningsErrors = flag' MakeWarningsErrors $ mconcat [
        long "log-as-error-warnings"
      , help "Set log level of warnings to error"
      ]

    parseEnableMacroWarnings :: Parser CustomLogLevelSetting
    parseEnableMacroWarnings = flag' EnableMacroWarnings $ mconcat [
        long "log-enable-macro-warnings"
      , help $ concat [
            "Set log level of macro reparse and typecheck errors to warning"
          , " (default: info)"
          ]
      ]

parseShowTimeStamp :: Parser ShowTimeStamp
parseShowTimeStamp = flag DisableTimeStamp EnableTimeStamp $ mconcat [
      long "log-show-time"
    , help "Show time stamps in traces"
    ]

parseShowCallStack :: Parser ShowCallStack
parseShowCallStack = flag DisableCallStack EnableCallStack $ mconcat [
      long "log-show-call-stack"
    , help "Show call stacks in traces"
    ]

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

type Config = Config_ FilePath

parseConfig :: Parser Config
parseConfig = Config
    <$> parseClangArgsConfig
    <*> parseBindingSpec
    <*> parseParsePredicate
    <*> parseSelectPredicate
    <*> parseProgramSlicing
    <*> parsePathStyle

parseConfigPP :: Parser ConfigPP
parseConfigPP = ConfigPP
    <$> optional parseUniqueId
    <*> parseHsModuleName

{-------------------------------------------------------------------------------
  Binding specifications
-------------------------------------------------------------------------------}

parseBindingSpec :: Parser BindingSpecConfig
parseBindingSpec = BindingSpecConfig
  <$> parseEnableStdlibBindingSpec
  <*> parseBindingSpecAllowNewer
  <*> many parseExtBindingSpec
  <*> optional parsePrescriptiveBindingSpec

parseEnableStdlibBindingSpec :: Parser EnableStdlibBindingSpec
parseEnableStdlibBindingSpec =
    flag EnableStdlibBindingSpec DisableStdlibBindingSpec $ mconcat [
        long "no-stdlib"
      , help "Do not automatically use stdlib external binding specification"
      ]

parseBindingSpecAllowNewer :: Parser BindingSpecCompatibility
parseBindingSpecAllowNewer =
    flag BindingSpecStrict BindingSpecAllowNewer $ mconcat [
        long "binding-spec-allow-newer"
      , help "Parse binding specifications with newer minor version"
      ]

parseExtBindingSpec :: Parser FilePath
parseExtBindingSpec = strOption $ mconcat [
      long "external-binding-spec"
    , metavar "FILE"
    , help "External binding specification (YAML file)"
    ]

parsePrescriptiveBindingSpec :: Parser FilePath
parsePrescriptiveBindingSpec = strOption $ mconcat [
      long "prescriptive-binding-spec"
    , metavar "FILE"
    , help "Prescriptive binding specification (YAML file)"
    ]

{-------------------------------------------------------------------------------
  Builtin include directory
-------------------------------------------------------------------------------}

parseBuiltinIncDirConfig :: Parser BuiltinIncDirConfig
parseBuiltinIncDirConfig = option (eitherReader auxParse) $ mconcat [
      long "builtin-include-dir"
    , metavar "MODE"
    , showDefaultWith auxRender
    , value def
    , help
        "Configure builtin include directory (supported: clang, disable)"
    ]
  where
    auxParse :: String -> Either String BuiltinIncDirConfig
    auxParse = \case
      "clang"   -> Right BuiltinIncDirClang
      "disable" -> Right BuiltinIncDirDisable
      other     -> Left $ "invalid builtin include directory mode: " ++ other

    auxRender :: BuiltinIncDirConfig -> String
    auxRender = \case
      BuiltinIncDirClang   -> "clang"
      BuiltinIncDirDisable -> "disable"

{-------------------------------------------------------------------------------
  Clang arguments
-------------------------------------------------------------------------------}

parseClangArgsConfig :: Parser (ClangArgsConfig FilePath)
parseClangArgsConfig = do
    -- ApplicativeDo to be able to reorder arguments for --help, and to use
    -- record construction (i.e., to avoid bool or string/path blindness)
    -- instead of positional one.
    target           <- optional parseTarget
    cStandard        <- Just <$> parseCStandard
    gnu              <- parseGnu
    enableBlocks     <- parseEnableBlocks
    builtinIncDir    <- parseBuiltinIncDirConfig
    extraIncludeDirs <- many parseIncludeDir
    defineMacros     <- many parseDefineMacro
    argsBefore       <- many parseClangOptionBefore
    argsInner        <- many parseClangOptionInner
    argsAfter        <- many parseClangOptionAfter
    pure $ ClangArgsConfig {..}

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

parseGnu :: Parser Gnu
parseGnu = flag DisableGnu EnableGnu $ mconcat [
      long "gnu"
    , help "Enable GNU extensions"
    ]

-- TODO: Perhaps we should mimick Clang's @-f@ parameter?
parseEnableBlocks :: Parser Bool
parseEnableBlocks = switch $ mconcat [
      long "enable-blocks"
    , help "Enable the 'blocks' language feature"
    ]

parseIncludeDir :: Parser FilePath
parseIncludeDir = strOption $ mconcat [
      short 'I'
    , metavar "DIR"
    , help "Include search path directory"
    ]

parseDefineMacro :: Parser String
parseDefineMacro = strOption $ mconcat [
      short 'D'
    , long "define-macro"
    , metavar "<macro>=<value>"
    , help "Define <macro> to <value> (or 1 if <value> omitted)"
    ]

parseClangOptionBefore :: Parser String
parseClangOptionBefore = strOption $ mconcat [
      long "clang-option-before"
    , metavar "OPTION"
    , help "Prepend option when calling Clang; see also --clang-option"
    ]

parseClangOptionInner :: Parser String
parseClangOptionInner = strOption $ mconcat [
      long "clang-option"
    , metavar "OPTION"
    , help "Pass option to Clang"
    ]

parseClangOptionAfter :: Parser String
parseClangOptionAfter = strOption $ mconcat [
      long "clang-option-after"
    , metavar "OPTION"
    , help "Append option when calling Clang; see also --clang-option"
    ]

{-------------------------------------------------------------------------------
  Predicates and slicing
-------------------------------------------------------------------------------}

parseParsePredicate :: Parser ParsePredicate
parseParsePredicate = fmap aux . many . asum $ [
      flag' (Right PTrue) $ mconcat [
          long "parse-all"
        , help "Parse all headers"
        ]
    , flag' (Right (PIf FromMainHeaders)) $ mconcat [
          long "parse-from-main-headers"
        , help "Parse main headers"
        ]
    , flag' (Right (PIf FromMainHeaderDirs)) $ mconcat [
          long "parse-from-main-header-dirs"
        , help "Parse headers in main header directories (default)"
        ]
    , fmap (Right . PIf . HeaderPathMatches) $ strOption $ mconcat [
          long "parse-by-header-path"
        , metavar "PCRE"
        , help "Parse headers with paths that match PCRE"
        ]
    , fmap (Left . PIf . HeaderPathMatches) $ strOption $ mconcat [
          long "parse-except-by-header-path"
        , metavar "PCRE"
        , help "Parse except headers with paths that match PCRE"
        ]
    ]
  where
    aux :: [Either ParsePredicate ParsePredicate] -> ParsePredicate
    aux = uncurry mergePredicates . fmap applyDefault . partitionEithers

    applyDefault :: [ParsePredicate] -> [ParsePredicate]
    applyDefault = \case
      [] -> [def]
      ps -> ps

parseSelectPredicate :: Parser SelectPredicate
parseSelectPredicate = fmap aux . many . asum $ [
      flag' (Right PTrue) $ mconcat [
          long "select-all"
        , help "Select all declarations"
        ]
    , flag' (Right (PIf (Left FromMainHeaders))) $ mconcat [
          long "select-from-main-headers"
        , help "Select declarations in main headers (default)"
        ]
    , flag' (Right (PIf (Left FromMainHeaderDirs))) $ mconcat [
          long "select-from-main-header-dirs"
        , help "Select declarations in main header directories"
        ]
    , fmap (Right . PIf . Left . HeaderPathMatches) $ strOption $ mconcat [
          long "select-by-header-path"
        , metavar "PCRE"
        , help "Select declarations in headers with paths that match PCRE"
        ]
    , fmap (Left . PIf . Left . HeaderPathMatches) $ strOption $ mconcat [
          long "select-except-by-header-path"
        , metavar "PCRE"
        , help $ concat [
              "Select except declarations in headers with paths that match"
            , " PCRE"
            ]
        ]
    , fmap (Right . PIf . Right . DeclNameMatches) $ strOption $ mconcat [
          long "select-by-decl-name"
        , metavar "PCRE"
        , help "Select declarations with C names that match PCRE"
        ]
    , fmap (Left . PIf . Right . DeclNameMatches) $ strOption $ mconcat [
          long "select-except-by-decl-name"
        , metavar "PCRE"
        , help "Select except declarations with C names that match PCRE"
        ]
    ]
  where
    aux :: [Either SelectPredicate SelectPredicate] -> SelectPredicate
    aux = uncurry mergePredicates . fmap applyDefault . partitionEithers

    applyDefault :: [SelectPredicate] -> [SelectPredicate]
    applyDefault = \case
      [] -> [def]
      ps -> ps

parseProgramSlicing :: Parser ProgramSlicing
parseProgramSlicing =
    flag DisableProgramSlicing EnableProgramSlicing $ mconcat [
        long "enable-program-slicing"
      , help $ concat [
            "Enable program slicing:"
          , " Select declarations using the selection predicate,"
          , " and also select their transitive dependencies"
          ]
      ]

{-------------------------------------------------------------------------------
  Translation options
-------------------------------------------------------------------------------}

parseUniqueId :: Parser UniqueId
parseUniqueId = fmap UniqueId . strOption $ mconcat [
      long "unique-id"
    , metavar "ID"
    , help $ concat [
          "Use unique ID to discriminate global C identifiers"
        , " (default: empty string)"
        ]
    ]

{-------------------------------------------------------------------------------
  Pretty printer options
-------------------------------------------------------------------------------}

parseHsModuleName :: Parser Hs.ModuleName
parseHsModuleName = strOption $ mconcat [
      long "module"
    , metavar "NAME"
    , showDefault
    , value $ hsModuleOptsBaseName def
    , help "Base name of the generated Haskell modules"
    ]

{-------------------------------------------------------------------------------
  Output options
-------------------------------------------------------------------------------}

parseHsOutputDir :: Parser FilePath
parseHsOutputDir = strOption $ mconcat [
      long "hs-output-dir"
    , metavar "PATH"
    , help "Output directory of generated Haskell modules"
    ]

parseGenBindingSpec :: Parser FilePath
parseGenBindingSpec = strOption $ mconcat [
      long "gen-binding-spec"
    , metavar "PATH"
    , help "Binding specification to generate"
    ]

parseGenTestsOutput :: Parser FilePath
parseGenTestsOutput = strOption $ mconcat [
      short 'o'
    , long "output"
    , metavar "PATH"
    , showDefault
    , value "test-hs-bindgen"
    , help "Output directory for the test suite"
    ]

{-------------------------------------------------------------------------------
  Input arguments
-------------------------------------------------------------------------------}

-- | Parse one or more input header arguments
--
-- This uses standard syntax for one or more arguments, which
-- @optparse-applicative@ does not get right when just using 'some'.
parseInputs :: Parser [UncheckedHashIncludeArg]
parseInputs = some . strArgument $ mconcat [
      metavar "HEADER..."
    , help "Input C header(s), relative to an include path directory"
    ]

{-------------------------------------------------------------------------------
  Haddock options
-------------------------------------------------------------------------------}

parsePathStyle :: Parser PathStyle
parsePathStyle = option readPathStyle $ mconcat [
      long "path-style"
    , metavar "STYLE"
    , help "Path display style (short|full)"
    , showDefault
    , value Short
    , help "Render style of file paths in Haddock comments"
    ]
  where
    readPathStyle :: ReadM PathStyle
    readPathStyle = eitherReader $ \s -> case s of
      "full"  -> Right Full
      "short" -> Right Short
      _       -> Left $ "Invalid path style: " ++ s ++ ". Expected 'full' or 'short'"

{-------------------------------------------------------------------------------
  Auxiliary optparse-applicative functions
-------------------------------------------------------------------------------}

-- | Command with @-h@ and @--help@
cmd ::
     String     -- ^ Name
  -> (a -> c)   -- ^ Constructor
  -> Parser a   -- ^ Options parser
  -> InfoMod c  -- ^ Information
  -> Mod CommandFields c
cmd name mk parser = command name . info (helper <*> (mk <$> parser))

-- | Command with @--help@ (but no @-h@)
cmd_ ::
     String     -- ^ Name
  -> (a -> c)   -- ^ Constructor
  -> Parser a   -- ^ Options parser
  -> InfoMod c  -- ^ Information
  -> Mod CommandFields c
cmd_ name mk parser = command name . info (helper' <*> (mk <$> parser))
  where
    helper' :: Parser (a -> a)
    helper' = helperWith $ mconcat [
        long "help"
      , help "Show this help text"
      ]
