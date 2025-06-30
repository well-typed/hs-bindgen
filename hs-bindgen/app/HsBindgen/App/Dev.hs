module HsBindgen.App.Dev (
    Dev(..)
  , DevCmd(..)
  , ParseOpts(..)
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
    , devCmd        :: DevCmd
    }
  deriving (Show)

parseDev :: Parser Dev
parseDev =
    Dev
      <$> parseGlobalOpts
      <*> parseDevCmd

data DevCmd =
    -- | Just parse the C headers
    DevCmdParse ParseOpts
  deriving (Show)

parseDevCmd :: Parser DevCmd
parseDevCmd = subparser $ mconcat [
      cmd "parse" (DevCmdParse <$> parseParseOpts) $ mconcat [
          progDesc "Parse C header (primarily for debugging hs-bindgen itself)"
        ]
    ]

{-------------------------------------------------------------------------------
  Parse command
-------------------------------------------------------------------------------}

newtype ParseOpts = ParseOpts {
      parseInputPaths :: [CHeaderIncludePath]
    }
  deriving (Show)

parseParseOpts :: Parser ParseOpts
parseParseOpts =
    ParseOpts
      <$> parseInputs
