module HsBindgen.App.Dev (
    Dev(..)
  , DevMode(..)
  , ParseMode(..)
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
        , footerDoc (Just $ footerWith p)
        ]

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Command line arguments
data Dev = Dev {
      devGlobalOpts :: GlobalOpts
    , devMode       :: DevMode
    }
  deriving (Show)

parseDev :: Parser Dev
parseDev =
    Dev
      <$> parseGlobalOpts
      <*> parseDevMode

data DevMode =
    -- | Just parse the C headers
    DevModeParse ParseMode
  deriving (Show)

parseDevMode :: Parser DevMode
parseDevMode = subparser $ mconcat [
      cmd "parse" (DevModeParse <$> parseParseMode) $ mconcat [
          progDesc "Parse C header (primarily for debugging hs-bindgen itself)"
        ]
    ]

{-------------------------------------------------------------------------------
  Parse mode
-------------------------------------------------------------------------------}

data ParseMode = ParseMode {
      parseInputPaths :: [CHeaderIncludePath]
    }
  deriving (Show)

parseParseMode :: Parser ParseMode
parseParseMode =
    ParseMode
      <$> some parseInput
