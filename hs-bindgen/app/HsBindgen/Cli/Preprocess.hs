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

import Control.Exception (Exception (..), throwIO)
import Control.Monad (unless, void)
import Data.Maybe (maybeToList)
import GHC.Generics (Generic)
import Options.Applicative hiding (info)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

import HsBindgen
import HsBindgen.App
import HsBindgen.Artefact
import HsBindgen.Backend.UniqueId
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Errors
import HsBindgen.Frontend.RootHeader
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Generate Haskell module from C headers"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config          :: Config
    , uniqueId        :: UniqueId
    , hsModuleName    :: Hs.ModuleName
    , hsOutputDir     :: FilePath
    , outputDirPolicy :: OutputDirPolicy
    -- NOTE: Inputs (arguments) must be last, options must go before it.
    , inputs          :: [UncheckedHashIncludeArg]
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseUniqueId
      <*> parseHsModuleName
      <*> parseHsOutputDir
      <*> parseOutputDirPolicy
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    -- Validate or create output directory based on policy
    case outputDirPolicy of
      CreateDirStructure      ->
        createDirectoryIfMissing True hsOutputDir
      DoNotCreateDirStructure -> do
        exists <- doesDirectoryExist hsOutputDir
        unless exists $
          throwIO (OutputDirectoryMissingException hsOutputDir)

    void $ run $ (sequenceArtefacts artefacts) :* Nil
  where
    bindgenConfig :: BindgenConfig
    bindgenConfig = toBindgenConfig config uniqueId hsModuleName

    run :: Artefacts as -> IO (NP I as)
    run = hsBindgen tracerConfig bindgenConfig inputs

    artefacts :: [Artefact ()]
    artefacts =
          writeBindingsMultiple hsOutputDir
      : [ writeBindingSpec file | file <- maybeToList config.outputBindingSpec ]

{-------------------------------------------------------------------------------
  Exception
-------------------------------------------------------------------------------}

data OutputDirectoryMissingException =
  OutputDirectoryMissingException FilePath
  deriving Show

instance Exception OutputDirectoryMissingException where
    toException   = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException (OutputDirectoryMissingException path) = unlines
        [ "Output directory does not exist: " ++ path
        , ""
        , "Use --create-output-dirs to create it automatically, or create the directory manually."
        ]
