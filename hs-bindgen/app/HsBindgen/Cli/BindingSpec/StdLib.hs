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

data Opts = Opts {
      clangArgsConfig :: ClangArgsConfig
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
    case output of
      Just path -> BS.writeFile path $ encodeBindingSpecYaml spec
      Nothing   -> BS.putStr         $ encodeBindingSpecYaml spec
