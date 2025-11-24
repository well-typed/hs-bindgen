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
import Data.Default (Default (..))
import Options.Applicative hiding (info)

import HsBindgen
import HsBindgen.App
import HsBindgen.Config
import HsBindgen.Frontend.RootHeader

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Generate tests for generated Haskell code"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config         :: Config
    , uniqueId       :: UniqueId
    , baseModuleName :: BaseModuleName
    , output         :: FilePath
    , inputs         :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseUniqueId
      <*> parseBaseModuleName
      <*> parseGenTestsOutput
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    let artefact = writeTests output
        bindgenConfig = toBindgenConfig config uniqueId baseModuleName def
    void $ hsBindgen tracerConfig bindgenConfig inputs artefact
