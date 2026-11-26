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

import Data.List qualified as List
import GHC.Exception (Exception (..))
import Options.Applicative hiding (info)
import System.Directory (doesDirectoryExist, doesFileExist)

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions (..),
                             buildCategoryChoice, parseOutputOptions)
import HsBindgen.ArtefactM
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Library (OutputFile, outputFilePath)
import HsBindgen.Macro

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
    , configCLI           :: ConfigCLI
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseConfigCLI

-- | CLI options; the TH equivalent of ConfigCLI' is 'HsBindgen.Config.ConfigTH'.
data ConfigCLI = ConfigCLI {
      uniqueId          :: UniqueId
    , baseModuleName    :: BaseModuleName
    , qualifiedStyle    :: QualifiedStyle
    , outputOptions     :: OutputOptions
    , hsOutputDir       :: FilePath
    , outputBindingSpec :: Maybe FilePath
    , bindingSpecDir    :: Maybe FilePath
    , dirPolicy         :: DirPolicy
    , filePolicy        :: FilePolicy
    , executionMode     :: ExecutionMode
    , libraryDirs       :: [FilePath]
    -- NOTE: Inputs (arguments) must be last, options must go before it.
    , inputs            :: [C.UncheckedRootDirective]
    }
  deriving (Generic)

parseConfigCLI :: Parser ConfigCLI
parseConfigCLI =
    ConfigCLI
      <$> parseUniqueId
      <*> parseBaseModuleName
      <*> parseQualifiedStyle
      <*> parseOutputOptions FilePerModule
      <*> parseHsOutputDir
      <*> optional parseGenBindingSpec
      <*> optional parseGenBindingSpecDir
      <*> parseDirPolicy
      <*> parseFilePolicy
      <*> parseExecutionMode
      <*> parseLibraryDirs
      <*> parseInputs

parseLibraryDirs :: Parser [FilePath]
parseLibraryDirs = many $ strOption $ mconcat [
      long "library"
    , metavar "DIR"
    , help (  "Generate modules for the headers under DIR that the input "
           ++ "headers include, directly or indirectly (repeatable). "
           ++ "By default, only the input headers on the command line "
           ++ "control the selection. Use --select-from-library-headers "
           ++ "to select the library headers directly"
           )
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts
  | not (null opts.configCLI.libraryDirs) = execLibrary
  | otherwise                             = execSingle
  where
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

    -- | Single-header mode, without @--library@: one set of modules for all
    -- the declarations of the run
    execSingle :: IO ()
    execSingle = do
        forM_ opts.configCLI.bindingSpecDir $ \_ ->
          throwIO GenBindingSpecDirWithoutLibrary
        case opts.configCLI.executionMode of
          RealRun ->
            run RealRun artefact
          DryRun -> do
            mModuleHeaders <- run DryRun $ artefact *> getModuleHeaders
            forM_ mModuleHeaders $ putStrLn . renderModuleLine
      where
        run :: ExecutionMode -> Artefact CExpr a -> IO a
        run mode =
          hsBindgen
            global.unsafe
            global.safe
            bindgenConfig
            mode
            opts.configCLI.inputs

        artefact :: Artefact CExpr ()
        artefact = do
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

    -- | Library mode: a set of modules for each header under the @--library@
    -- directories that the run parses (see "HsBindgen.Library")
    execLibrary :: IO ()
    execLibrary = do
        case opts.configCLI.outputOptions of
          OutputOptions (SingleFile _) ->
            throwIO LibraryWithSingleFile
          _otherwise ->
            pure ()
        forM_ opts.configCLI.outputBindingSpec $ \_ ->
          throwIO GenBindingSpecWithLibrary
        forM_ opts.configCLI.libraryDirs $ \dir -> do
          isDir <- doesDirectoryExist dir
          unless isDir $ do
            isFile <- doesFileExist dir
            throwIO $
              if isFile then LibraryDirIsFile dir
                        else LibraryDirNotFound dir

        files <- run opts.configCLI.executionMode

        case opts.configCLI.executionMode of
          -- One line per file the real run would write, whatever the
          -- verbosity.
          DryRun ->
            mapM_ putStrLn $ List.sort [
                outputFilePath
                  opts.configCLI.hsOutputDir
                  opts.configCLI.bindingSpecDir
                  file
              | file <- files
              ]
          RealRun -> pure ()
      where
        run :: ExecutionMode -> IO [OutputFile]
        run mode =
          hsBindgenLibrary
            global.unsafe
            global.safe
            bindgenConfig
            mode
            opts.configCLI.inputs
            opts.configCLI.libraryDirs
            mrc
            opts.configCLI.filePolicy
            opts.configCLI.dirPolicy
            opts.configCLI.hsOutputDir
            opts.configCLI.bindingSpecDir

{-------------------------------------------------------------------------------
  Exception
-------------------------------------------------------------------------------}

-- | Options that @preprocess@ cannot combine, and early validation failures
-- for @--library@ directory arguments.
data PreprocessException =
    LibraryWithSingleFile
  | LibraryDirNotFound FilePath
  | LibraryDirIsFile   FilePath
  | GenBindingSpecWithLibrary
  | GenBindingSpecDirWithoutLibrary
  deriving Show

instance Exception PreprocessException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException LibraryWithSingleFile =
      "--library is not available with --single-file"
    displayException (LibraryDirNotFound path) =
      "--library: directory does not exist: " ++ path
    displayException (LibraryDirIsFile path) =
      "--library: not a directory (regular file): " ++ path
    displayException GenBindingSpecWithLibrary =
      "--gen-binding-spec is not available with --library; use --gen-binding-spec-dir"
    displayException GenBindingSpecDirWithoutLibrary =
      "--gen-binding-spec-dir is only available with --library; use --gen-binding-spec"

{-------------------------------------------------------------------------------
  Dry run
-------------------------------------------------------------------------------}

-- | The line a dry run prints for a module, such as @M: a.h, b.h@
--
-- A module with no headers prints as @M:@.
renderModuleLine :: ModuleHeaders -> String
renderModuleLine m =
    baseModuleNameToString m.moduleName ++ ":" ++ headers
  where
    headers :: String
    headers = case m.headers of
      [] -> ""
      hs -> ' ' : List.intercalate ", " (map (.path) hs)
