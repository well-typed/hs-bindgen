-- | @hs-bindgen-cli binding-spec stdlib@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.BindingSpec.StdLib qualified as StdLib
module HsBindgen.Cli.BindingSpec.StdLib (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Control.Monad ((<=<))
import Data.ByteString qualified as BS
import Options.Applicative hiding (info)

import HsBindgen.App
import HsBindgen.Boot (getClangArgs)
import HsBindgen.Imports
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Write stdlib external binding specification"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

newtype Opts = Opts {
      clangArgsConfig :: ClangArgsConfig
    }

parseOpts :: Parser Opts
parseOpts = Opts <$> parseClangArgsConfig

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    spec <- either throwIO pure <=< withTracer tracerConfig $ \tracer -> do
      clangArgs <- getClangArgs (contramap TraceBoot tracer) clangArgsConfig
      getStdlibBindingSpec
        (contramap (TraceBoot . BootBindingSpec) tracer)
        clangArgs
    BS.putStr $ encodeBindingSpecYaml spec
