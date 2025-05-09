{-# LANGUAGE ApplicativeDo #-}
module HsBindgen.App.Common (
    -- * Global options
    GlobalOpts(..)
  , parseGlobalOpts
    -- * Input option
  , parseInput
    -- * Auxiliary hs-bindgen functions
  , loadExtBindingSpecs'
    -- * Auxiliary optparse-applicative functions
  , cmd
  , cmd'
  ) where

import Control.Exception (Exception(displayException))
import Data.Bifunctor (first)
import Data.Char qualified as Char
import Data.List qualified as List
import Options.Applicative
import Options.Applicative.Extra (helperWith)

import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Global options
-------------------------------------------------------------------------------}

data GlobalOpts = GlobalOpts {
      globalOptsVerbosity   :: Bool
    , globalOptsPredicate   :: Predicate
    , globalOptsClangArgs   :: ClangArgs
    , globalOptsExtBindings :: [FilePath]
    }
  deriving (Show)

parseGlobalOpts :: Parser GlobalOpts
parseGlobalOpts =
    GlobalOpts
      <$> parseVerbosity
      <*> parsePredicate
      <*> parseClangArgs
      <*> parseExtBindings

parseVerbosity :: Parser Bool
parseVerbosity =
    switch $ mconcat [
        short 'v'
      , long "verbose"
      , help "Verbose output"
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

parseClangArgs :: Parser ClangArgs
parseClangArgs = do
    -- ApplicativeDo to be able reorder arguments for --help, and uses record
    -- construction (i.a. to avoid bool or string/path blindness) instead of
    -- positional one.
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

parseExtBindings :: Parser [FilePath]
parseExtBindings = many . strOption $ mconcat [
      long "external-bindings"
    , metavar "FILE"
    , help "External bindings specification (YAML file)"
    ]

{-------------------------------------------------------------------------------
  Input option
-------------------------------------------------------------------------------}

parseInput :: Parser CHeaderIncludePath
parseInput =
    option (eitherReader $ first displayException . parseCHeaderIncludePath) $
      mconcat $ [
          help "Input C header, relative to an include path directory"
        , metavar "PATH"
        , long "input"
        , short 'i'
        ]

{-------------------------------------------------------------------------------
  Auxiliary hs-bindgen functions
-------------------------------------------------------------------------------}

-- | Load exernal binding specifications, tracing any errors
loadExtBindingSpecs' ::
     Tracer IO String
  -> GlobalOpts
  -> IO BindingSpecs
loadExtBindingSpecs' tracer GlobalOpts{..} = do
    (resolveErrs, extBindingSpecs) <-
      loadBindingSpecs globalOptsClangArgs globalOptsExtBindings
    mapM_ (traceWith tracer Warning . displayException) resolveErrs
    return extBindingSpecs

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
