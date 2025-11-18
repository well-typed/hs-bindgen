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
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.Backend.UniqueId
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
    let artefacts = writeTests output :* Nil
        bindgenConfig = toBindgenConfig config uniqueId defHsModuleName
    void $ hsBindgen tracerConfig bindgenConfig inputs artefacts
