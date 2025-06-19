module HsBindgen.App.Dev (
    Dev(..)
  , Mode(..)
  , getDev
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

newtype Mode =
    -- | Just parse the C headers
    ModeParse {
        parseInputPaths :: [CHeaderIncludePath]
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
    ]

{-------------------------------------------------------------------------------
  Dev modes
-------------------------------------------------------------------------------}

parseModeParse :: Parser Mode
parseModeParse =
    ModeParse
      <$> some parseInput
