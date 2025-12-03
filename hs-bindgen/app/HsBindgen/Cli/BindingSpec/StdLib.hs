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

import Data.ByteString qualified as BS
import Options.Applicative hiding (info)
import System.Exit (exitFailure)

import HsBindgen.App
import HsBindgen.BindingSpec
import HsBindgen.Boot
import HsBindgen.Config.ClangArgs
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
    mSpec <- withTracer tracerConfig $ \tracer -> do
        (clangArgs, _target) <-
          getClangArgsAndTarget (contramap TraceBoot tracer) clangArgsConfig
        getStdlibBindingSpec
          (contramap (TraceBoot . BootBindingSpec) tracer)
          clangArgs
    case mSpec of
      Left _ -> do
        putStrLn $ "An error happened (see above)"
        exitFailure
      Right spec ->
        maybe BS.putStr BS.writeFile output $ encodeBindingSpecYaml spec
