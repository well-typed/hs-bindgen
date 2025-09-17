-- | @hs-bindgen-cli internal parse@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Internal.Parse qualified as Parse
module HsBindgen.Cli.Internal.Parse (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Options.Applicative hiding (info)

import HsBindgen.App
import HsBindgen.Lib

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
