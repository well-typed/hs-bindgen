-- | @hs-bindgen-cli dev parse@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Dev.Parse qualified as Parse
module HsBindgen.Cli.Dev.Parse (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Options.Applicative hiding (info)

import HsBindgen.Lib

import HsBindgen.App

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Parse C headers"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      bindgenConfig :: BindgenConfig
    , inputs        :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseBindgenConfig
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    let artefacts = ReifiedC :* Nil
    (I decls :* Nil) <-
      hsBindgen tracerConfig bindgenConfig inputs artefacts
    print decls
