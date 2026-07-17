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

import Data.Default (Default (..))
import Options.Applicative hiding (info)

import HsBindgen
import HsBindgen.App
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro

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
    , inputs         :: [C.UncheckedHashIncludeArg]
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
exec global opts =
    hsBindgen
      global.unsafe
      global.safe
      bindgenConfig
      opts.inputs
      artefact
  where
    artefact :: Artefact CExpr ()
    artefact = writeTests opts.output

    bindgenConfig :: BindgenConfig
    bindgenConfig =
        toBindgenConfig
          opts.config
          opts.uniqueId
          opts.baseModuleName
          def
