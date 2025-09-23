-- | @hs-bindgen-cli gentests@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.GenTests qualified as GenTests
module HsBindgen.Cli.GenTests (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Control.Monad (void)
import Options.Applicative hiding (info)

import HsBindgen.App
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Generate tests for generated Haskell code"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      bindgenConfig :: BindgenConfig
    , output        :: FilePath
    , inputs        :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseBindgenConfig
      <*> parseGenTestsOutput
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    let artefacts = writeTests output :* Nil
    void $ hsBindgen tracerConfig tracerConfigBackend bindgenConfig inputs artefacts
