-- | @hs-bindgen-cli preprocess@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Preprocess qualified as Preprocess
module HsBindgen.Cli.Preprocess (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Control.Monad (void)
import Data.Maybe (maybeToList)
import GHC.Generics (Generic)
import Options.Applicative hiding (info)

import HsBindgen
import HsBindgen.App
import HsBindgen.Artefact
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Frontend.RootHeader

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Generate Haskell module from C headers"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config            :: Config
    , configPP          :: ConfigPP
    , hsOutputDir       :: FilePath
    , outputBindingSpec :: Maybe FilePath
    , inputs            :: [UncheckedHashIncludeArg]
    -- NOTE inputs (arguments) must be last, options must go before it
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseConfigPP
      <*> parseHsOutputDir
      <*> optional parseGenBindingSpec
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = void $ run $ (sequenceArtefacts artefacts) :* Nil
  where
    bindgenConfig :: BindgenConfig
    bindgenConfig = toBindgenConfigPP config configPP

    run :: Artefacts as -> IO (NP I as)
    run = hsBindgen tracerConfig bindgenConfig inputs

    artefacts :: [Artefact ()]
    artefacts =
          writeBindingsMultiple hsOutputDir
      : [ writeBindingSpec file | file <- maybeToList outputBindingSpec ]
