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
  , ConfigCLI(..)
    -- * Execution
  , exec
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Options.Applicative hiding (info)
import System.Exit (ExitCode (..), exitWith)

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions (..),
                             buildCategoryChoice, parseOutputOptions)
import HsBindgen.ArtefactM
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Library (Library (..), LibraryError)
import HsBindgen.Library qualified as Library
import HsBindgen.Macro
import HsBindgen.Util.Tracer (prettyForTrace)

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Generate Haskell module from C headers"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config              :: Config
      -- | The selection flags as they were given
      --
      -- 'config' has the predicate that they stand for when one module is
      -- generated.
    , selectionFlags      :: SelectionFlags
    , configCLI           :: ConfigCLI
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts = mkOpts <$> parseConfigWithSelection <*> parseConfigCLI
  where
    mkOpts :: (Config, SelectionFlags) -> ConfigCLI -> Opts
    mkOpts (config, selectionFlags) configCLI = Opts{
          config         = config
        , selectionFlags = selectionFlags
        , configCLI      = configCLI
        }

-- | CLI options; the TH equivalent of ConfigCLI' is 'HsBindgen.Config.ConfigTH'.
data ConfigCLI = ConfigCLI {
      uniqueId          :: UniqueId
    , baseModuleName    :: BaseModuleName
    , library           :: [FilePath]
    , qualifiedStyle    :: QualifiedStyle
    , outputOptions     :: OutputOptions
    , hsOutputDir       :: FilePath
    , outputBindingSpec :: Maybe FilePath
    , dirPolicy         :: DirPolicy
    , filePolicy        :: FilePolicy
    , dryRun            :: Bool
    -- NOTE: Inputs (arguments) must be last, options must go before it.
    , inputs            :: [C.UncheckedRootDirective]
    }
  deriving (Generic)

parseConfigCLI :: Parser ConfigCLI
parseConfigCLI =
    ConfigCLI
      <$> parseUniqueId
      <*> parseBaseModuleName
      <*> parseLibrary
      <*> parseQualifiedStyle
      <*> parseOutputOptions FilePerModule
      <*> parseHsOutputDir
      <*> optional parseGenBindingSpec
      <*> parseDirPolicy
      <*> parseFilePolicy
      <*> parseDryRun
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts = case NonEmpty.nonEmpty opts.configCLI.library of
    Nothing   -> run oneModule
    Just dirs -> do
      let library = Library{dirs = dirs}
      checkLibraryOptions library
      run (modulePerHeader library) >>= \case
        Right _written -> pure ()
        Left err       -> do
          print $ prettyForTrace err
          -- The exit code of any other error that is specific to
          -- @hs-bindgen@
          exitWith (ExitFailure 4)
  where
    run :: Artefact CExpr a -> IO a
    run artefact =
        hsBindgen
          global.unsafe
          global.safe
          bindgenConfig
          opts.configCLI.inputs
          (artefact <* when opts.configCLI.dryRun (Lift dryRun))

    bindgenConfig :: BindgenConfig
    bindgenConfig =
        toBindgenConfig
          opts.config
          opts.configCLI.uniqueId
          opts.configCLI.baseModuleName
          (buildCategoryChoice opts.configCLI.outputOptions)

    mrc :: ModuleRenderConfig
    mrc = ModuleRenderConfig {
        qualifiedStyle = opts.configCLI.qualifiedStyle
      }

    -- The given headers as one module
    oneModule :: Artefact CExpr ()
    oneModule = do
      case opts.configCLI.outputOptions of
        OutputOptions (SingleFile _) ->
          writeBindingsSingle
            mrc
            opts.configCLI.filePolicy
            opts.configCLI.dirPolicy
            opts.configCLI.hsOutputDir
        _ ->
          writeBindingsMultiple
            mrc
            opts.configCLI.filePolicy
            opts.configCLI.dirPolicy
            opts.configCLI.hsOutputDir

      forM_ opts.configCLI.outputBindingSpec $ \path ->
        writeBindingSpec
          opts.configCLI.filePolicy
          opts.configCLI.dirPolicy
          path

    -- One module for each header of the library
    modulePerHeader :: Library -> Artefact CExpr (Either LibraryError [Written])
    modulePerHeader library =
      writeLibrary library BTrue $
        case opts.configCLI.outputOptions of
          OutputOptions (SingleFile _) ->
            getBindingsFileSingle
              mrc
              opts.configCLI.filePolicy
              opts.configCLI.dirPolicy
              opts.configCLI.hsOutputDir
          _ ->
            getBindingsFilesMultiple
              mrc
              opts.configCLI.filePolicy
              opts.configCLI.dirPolicy
              opts.configCLI.hsOutputDir

    -- What cannot be asked for together with @--library@
    checkLibraryOptions :: Library -> IO ()
    checkLibraryOptions library = do
      unless (null opts.selectionFlags.selected && null opts.selectionFlags.excepted) $
        usageError $ concat [
            "--library cannot be combined with --select-... flags:"
          , " every library header that is reached gets a module"
          ]
      when (isJust opts.configCLI.outputBindingSpec) $
        usageError $ concat [
            "--library cannot be combined with --gen-binding-spec:"
          , " there is more than one module"
          ]
      missing <- Library.missingDirs library
      forM_ missing $ \dir ->
        usageError $ "--library: directory does not exist: " ++ dir
