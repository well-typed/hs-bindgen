module HsBindgen.App.Dev (
    Dev(..)
  , Mode(..)
  , getDev
  , BindingSpecMode(..)
  ) where

import Options.Applicative

import HsBindgen.App.Common
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getDev :: IO Dev
getDev = customExecParser p opts
  where
    p = prefs $ helpShowGlobals <> subparserInline

    opts :: ParserInfo Dev
    opts = info (parseDev <**> helper) $
      mconcat [
          header "hs-bindgen development utilities"
        , footerDoc (Just $ environmentVariablesFooter p)
        ]

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Command line arguments
data Dev = Dev {
      devGlobalOpts :: GlobalOpts
    , devMode       :: Mode
    }
  deriving (Show)

data Mode =
    -- | Just parse the C headers
    ModeParse {
        parseInputPaths :: [CHeaderIncludePath]
      }
  | ModeBindingSpec {
        modeBindingSpec :: BindingSpecMode
      }
  deriving (Show)

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseDev :: Parser Dev
parseDev =
    Dev
      <$> parseGlobalOpts
      <*> parseMode

{-------------------------------------------------------------------------------
  Mode selection
-------------------------------------------------------------------------------}

parseMode :: Parser Mode
parseMode = subparser $ mconcat [
      cmd "parse" parseModeParse $ mconcat [
          progDesc "Parse C header (primarily for debugging hs-bindgen itself)"
        ]
    , cmd "binding-spec" (ModeBindingSpec <$> parseBindingSpecMode) $ mconcat [
          progDesc "Binding specification commands"
        ]
    ]

{-------------------------------------------------------------------------------
  Dev modes
-------------------------------------------------------------------------------}

parseModeParse :: Parser Mode
parseModeParse =
    ModeParse
      <$> some parseInput

{-------------------------------------------------------------------------------
  Binding spec mode
-------------------------------------------------------------------------------}

data BindingSpecMode =
    BindingSpecModeBase
  | BindingSpecModeRuntime
  deriving (Show)

parseBindingSpecMode :: Parser BindingSpecMode
parseBindingSpecMode = subparser $ mconcat [
      cmd "base" (pure BindingSpecModeBase) $ mconcat [
          progDesc "Write base external binding specification"
        ]
    , cmd "runtime" (pure BindingSpecModeRuntime) $ mconcat [
          progDesc "Write hs-bindgen-runtime external binding specification"
        ]
    ]
