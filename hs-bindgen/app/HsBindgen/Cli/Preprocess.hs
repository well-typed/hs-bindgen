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

import Options.Applicative hiding (info)

import HsBindgen.Imports
import HsBindgen.Lib

import HsBindgen.App

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Generate Haskell module from C headers"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      bindgenConfig     :: BindgenConfig
    , output            :: Maybe FilePath
    , outputBindingSpec :: Maybe FilePath
    , inputs            :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseBindgenConfig
      <*> optional parseOutput
      <*> optional parseGenBindingSpec
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    case outputBindingSpec of
      -- NOTE: We can not assemble the heterogeneous list of artefacts before
      -- evaluating `hsBindgen`. The types don't line up. (We even have to pull
      -- 'void' inside the case statement).
      Nothing ->
        let artefacts = writeBindings output :* Nil
        in  void $ run artefacts
      Just file ->
        let artefacts = writeBindings output :* writeBindingSpec file :* Nil
        in  void $ run $ artefacts
  where
    run :: Artefacts as -> IO (NP I as)
    run = hsBindgen tracerConfig bindgenConfig inputs
