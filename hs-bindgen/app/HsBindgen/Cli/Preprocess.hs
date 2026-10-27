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
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Exception (Exception (..))
import Options.Applicative hiding (info)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions (..),
                             buildCategoryChoice, parseOutputOptions)
import HsBindgen.ArtefactM
import HsBindgen.BindingSpec (BindingSpecConfig (..))
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Errors
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Library (LibraryDirs, getLibraryDirs)
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
      -- | The configuration, given the selection when there are no @--select@
      -- flags
      config              :: Boolean SelectionPredicate -> Config
    , configCLI           :: ConfigCLI
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfigWith
      <*> parseConfigCLI

-- | CLI options; the TH equivalent of ConfigCLI' is 'HsBindgen.Config.ConfigTH'.
data ConfigCLI = ConfigCLI {
      uniqueId          :: UniqueId
    , baseModuleName    :: BaseModuleName
    , qualifiedStyle    :: QualifiedStyle
    , outputOptions     :: OutputOptions
    , hsOutputDir       :: FilePath
    , outputBindingSpec :: Maybe FilePath
    , dirPolicy         :: DirPolicy
    , filePolicy        :: FilePolicy
    , hsBootPolicy      :: HsBootPolicy
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
      <*> parseDirPolicy
      <*> parseFilePolicy
      <*> parseHsBootPolicy
      <*> parseExecutionMode
      <*> parseLibraryDirs
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts = do
    mDirs <- forM mLibraryDirs $ \dirs ->
      getLibraryDirs dirs >>= \case
        Right libraryDirs | null libraryOptionErrors ->
          pure libraryDirs
        eLibraryDirs -> do
          -- Usage errors, like those of the parser
          mapM_ (hPutStrLn stderr) $
               [ "--library: no such directory: " ++ dir
               | Left missing <- [eLibraryDirs]
               , dir <- NonEmpty.toList missing
               ]
            ++ libraryOptionErrors
          exitWith (ExitFailure 2)
    case (opts.configCLI.outputOptions, opts.configCLI.hsBootPolicy) of
      (OutputOptions (SingleFile _), GenerateHsBoot) ->
        throwIO HsBootWithSingleFile
      _otherwise ->
        pure ()
    let -- One result for each module
        run :: ExecutionMode -> Artefact CExpr a -> IO [a]
        run mode = case mDirs of
          Nothing   -> fmap pure . runModule mode
          Just dirs -> runLibrary dirs mode
    case opts.configCLI.executionMode of
      RealRun ->
        void $ run RealRun artefact
      DryRun -> do
        mModuleHeaders <- run DryRun $ artefact *> getModuleHeaders
        forM_ (catMaybes mModuleHeaders) $ putStrLn . renderModuleLine
  where
    mLibraryDirs :: Maybe (NonEmpty FilePath)
    mLibraryDirs = NonEmpty.nonEmpty opts.configCLI.libraryDirs

    runModule :: ExecutionMode -> Artefact CExpr a -> IO a
    runModule mode =
      hsBindgen
        global.unsafe
        global.safe
        bindgenConfig
        mode
        opts.configCLI.inputs

    runLibrary :: LibraryDirs -> ExecutionMode -> Artefact CExpr a -> IO [a]
    runLibrary dirs mode =
      hsBindgenLibrary
        global.unsafe
        global.safe
        bindgenConfig
        mode
        dirs
        opts.configCLI.inputs

    -- Without @--select@ flags, a library selects every library header (any
    -- header, restricted to the library ones in "HsBindgen.Library").
    bindgenConfig :: BindgenConfig
    bindgenConfig =
        toBindgenConfig
          (opts.config $ case mLibraryDirs of
             Nothing     -> def
             Just _dirs  -> BIf (SelectHeader FromAllHeaders))
          opts.configCLI.uniqueId
          opts.configCLI.baseModuleName
          (buildCategoryChoice opts.configCLI.outputOptions)
          opts.configCLI.hsBootPolicy

    -- The options that @--library@ does not combine with
    libraryOptionErrors :: [String]
    libraryOptionErrors = concat [
          [ "--single-file is not available with --library"
          | OutputOptions (SingleFile _) <- [opts.configCLI.outputOptions]
          ]
        , [ "--gen-binding-spec is not available with --library"
          | isJust opts.configCLI.outputBindingSpec
          ]
        , [ "--prescriptive-binding-spec is not available with --library"
          | isJust (opts.config def).bindingSpec.prescriptiveBindingSpec
          ]
        ]

    mrc :: ModuleRenderConfig
    mrc = ModuleRenderConfig {
        qualifiedStyle = opts.configCLI.qualifiedStyle
      }

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

{-------------------------------------------------------------------------------
  Exception
-------------------------------------------------------------------------------}

-- | Options that @preprocess@ cannot combine
data PreprocessException = HsBootWithSingleFile
  deriving Show

instance Exception PreprocessException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException HsBootWithSingleFile =
      "--generate-hs-boot is not available with --single-file"

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
