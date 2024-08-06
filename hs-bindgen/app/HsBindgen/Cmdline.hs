module HsBindgen.Cmdline (
    Cmdline(..)
  , getCmdline
  ) where

import Options.Applicative

import HsBindgen.Preprocessor.Render
import HsBindgen.Spec qualified as Unresolved

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Command line arguments
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/75>
-- We might want to support multiple Haskell modules; in this case, we'll
-- probably want to bundle an 'Unresolved.Spec' with a 'FilePath' (output); we
-- might also want to split 'Unresolved.Spec' into a section that would be
-- common to /all/ modules we want to generate, and then specify that only once
-- on the command line.
data Cmdline = Cmdline {
      cmdInput         :: Unresolved.Spec
    , cmdOutput        :: FilePath
    , cmdRenderOptions :: RenderOptions
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = execParser opts
  where
    opts :: ParserInfo Cmdline
    opts = info (parseCmdline <**> helper) $ mconcat [
          header "hs-bindgen - generate Haskell bindings from C headers"
        ]

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline =
    Cmdline
      <$> parseInput
      <*> parseOutput
      <*> parseRenderOptions

parseInput :: Parser Unresolved.Spec
parseInput =
    Unresolved.Spec
      <$> strOption (mconcat [
              help "Input path to the C header"
            , metavar "PATH"
            , long "input"
            , short 'i'
            ])
      <*> strOption (mconcat [
              help "Name of the generated Haskell module"
            , metavar "NAME"
            , long "module"
            , showDefault
            , value "Generated"
            ])

parseOutput :: Parser FilePath
parseOutput =
    strOption $ mconcat [
        help "Output path for the Haskell module"
      , metavar "PATH"
      , long "output"
      , short 'o'
      ]

parseRenderOptions :: Parser RenderOptions
parseRenderOptions =
    RenderOptions
      <$> option auto (mconcat [
              help "Maximum length line"
            , long "render-line-length"
            , showDefault
            , value $ renderLineLength defaultRenderOptions
            ])
