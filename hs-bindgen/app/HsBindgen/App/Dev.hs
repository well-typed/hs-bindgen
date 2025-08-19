module HsBindgen.App.Dev (
    Dev(..)
  , DevCmd(..)
  , ParseOpts(..)
  , getDev
  ) where

import Options.Applicative

import HsBindgen.Lib

import HsBindgen.App.Common

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

parseDev :: Parser Dev
parseDev =
    Dev
      <$> parseGlobalOpts
      <*> parseDevCmd

data DevCmd =
    DevCmdClang ClangArgs
  | DevCmdParse ParseOpts
  deriving (Show)

parseDevCmd :: Parser DevCmd
parseDevCmd = subparser $ mconcat [
      cmd "clang" (DevCmdClang <$> parseClangArgs) $ mconcat [
          progDesc "Run Clang with empty input (to debug with options such as -v)"
        ]
    , cmd "parse" (DevCmdParse <$> parseParseOpts) $ mconcat [
          progDesc "Parse C header (primarily for debugging hs-bindgen itself)"
        ]
    ]

{-------------------------------------------------------------------------------
  Parse command
-------------------------------------------------------------------------------}

data ParseOpts = ParseOpts {
      bindgenConfig :: BindgenConfig
    , inputs        :: [UncheckedHashIncludeArg]
    }
  deriving (Show, Eq)

parseParseOpts :: Parser ParseOpts
parseParseOpts =
    ParseOpts
      <$> parseBindgenConfig
      <*> parseInputs
