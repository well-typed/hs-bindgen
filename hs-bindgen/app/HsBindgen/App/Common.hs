{-# LANGUAGE ApplicativeDo #-}
module HsBindgen.App.Common (
    -- * Global options
    GlobalOpts(..)
  , parseGlobalOpts
    -- * Input option
  , parseInput
    -- * Auxiliary hs-bindgen functions
  , loadExtBindings'
  , environmentVariablesFooter
    -- * Auxiliary optparse-applicative functions
  , cmd
  , cmd'
  ) where

import Control.Exception (Exception (displayException))
import Control.Tracer (Tracer)
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Options.Applicative
import Options.Applicative.Extra (helperWith)
import Options.Applicative.Help (Doc, align, extractChunk, pretty, tabulate,
                                 vcat)
import Prettyprinter.Util (reflow)

import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Global options
-------------------------------------------------------------------------------}

data GlobalOpts = GlobalOpts {
      globalOptsTracerConf  :: TracerConf
    , globalOptsPredicate   :: Predicate
    , globalOptsClangArgs   :: ClangArgs
    , globalOptsStdlibSpecs :: StdlibBindingSpecs
    , globalOptsExtBindings :: [FilePath]
    }
  deriving stock (Show)

parseGlobalOpts :: Parser GlobalOpts
parseGlobalOpts =
    GlobalOpts
      <$> parseTracerConf
      <*> parsePredicate
      <*> parseClangArgs
      <*> parseStdlibSpecs
      <*> parseExtBindings

parseTracerConf :: Parser TracerConf
parseTracerConf = TracerConf <$> parseVerbosity
                             <*> parseShowTimeStamp
                             <*> parseShowCallStack

parseVerbosity :: Parser Verbosity
parseVerbosity =
  nToVerbosity <$>
    (option auto $
      mconcat [ short 'v'
              , long "verbosity"
              , metavar "INT"
              , value 1
              , help "Specify verbosity (0: error, 1: warning, 2: info, 3: debug);"
              , showDefault
              ])

  where
    nToVerbosity :: Int -> Verbosity
    nToVerbosity = Verbosity . \case
      n | n <= 0 -> Error
      1          -> Warning
      2          -> Info
      -- n | n >= 3 -- (But exhaustive checker complains).
      _nGe3      -> Debug

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
    , flag' SelectFromMainFiles $ mconcat [
          long "select-from-main-files"
        , help "Only process elements from the main files (this is the default)"
        ]
    ]
  where
    aux :: [Predicate] -> Predicate
    aux [] = SelectFromMainFiles
    aux ps = mconcat ps

parseClangArgs :: Parser ClangArgs
parseClangArgs = do
    -- ApplicativeDo to be able to reorder arguments for --help, and to use
    -- record construction (i.e., to avoid bool or string/path blindness)
    -- instead of positional one.
    clangTarget <- optional parseTarget
    clangCStandard <- fmap Just parseCStandard
    clangStdInc <- fmap not parseNoStdInc
    clangEnableGnu <-parseGnuOption
    clangSystemIncludePathDirs <- parseSystemIncludeDirOptions
    clangQuoteIncludePathDirs <- parseQuoteIncludeDirOptions
    clangOtherArgs <- parseOtherArgs
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

parseStdlibSpecs :: Parser StdlibBindingSpecs
parseStdlibSpecs = flag UseStdlibBindingSpecs NoStdlibBindingSpecs $ mconcat [
      long "no-stdlib"
    , help "Do not automatically use stdlib descriptive binding specifications"
    ]

parseExtBindings :: Parser [FilePath]
parseExtBindings = many . strOption $ mconcat [
      long "external-bindings"
    , metavar "FILE"
    , help "External bindings configuration (YAML file)"
    ]

{-------------------------------------------------------------------------------
  Input option
-------------------------------------------------------------------------------}

parseInput :: Parser CHeaderIncludePath
parseInput =
    argument (eitherReader $ first displayException . parseCHeaderIncludePath) $
      mconcat $ [
          help "Input C header, relative to an include path directory"
        , metavar "HEADER"
        ]

{-------------------------------------------------------------------------------
  Auxiliary hs-bindgen functions
-------------------------------------------------------------------------------}

-- | Load exernal bindings, tracing any errors
loadExtBindings' :: HasCallStack =>
     Tracer IO (TraceWithCallStack Trace)
  -> GlobalOpts
  -> IO ResolvedBindingSpec
loadExtBindings' tracer GlobalOpts{..} = do
    (resolveErrs, extBindings) <-
      loadExtBindings
        (useTrace TraceExtraClangArgs tracer)
        globalOptsClangArgs
        globalOptsStdlibSpecs
        globalOptsExtBindings
    mapM_ submitTrace resolveErrs
    return extBindings
  where submitTrace = traceWithCallStack (useTrace TraceResolveHeader tracer)

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
