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
import HsBindgen.BindingSpec
import HsBindgen.BindingSpec.Private.Stdlib qualified as Stdlib
import HsBindgen.Boot
import HsBindgen.Config.ClangArgs hiding (getClangArgs)
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Write stdlib external binding specification"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      clangArgsConfig :: ClangArgsConfig FilePath
    , output          :: Maybe FilePath
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseClangArgsConfig
      <*> optional parseOutput'
  where
    parseOutput' :: Parser FilePath
    parseOutput' = strOption $ mconcat [
        short 'o'
      , long "output"
      , metavar "PATH"
      , help "Output path for the binding specification"
      ]

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
    maybe BS.putStr BS.writeFile output $
      encodeBindingSpecYaml Stdlib.hsModuleName spec
