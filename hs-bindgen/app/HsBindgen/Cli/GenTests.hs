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

import HsBindgen
import HsBindgen.App
import HsBindgen.Config
import HsBindgen.Frontend.RootHeader
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Generate tests for generated Haskell code"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config       :: Config
    , uniqueId     :: UniqueId
    , hsModuleName :: Hs.ModuleName
    , output       :: FilePath
    , inputs       :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseUniqueId
      <*> parseHsModuleName
      <*> parseGenTestsOutput
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    let artefact = writeTests output
        bindgenConfig = toBindgenConfig config uniqueId defBaseModuleName
    void $ hsBindgen tracerConfig bindgenConfig inputs artefact
