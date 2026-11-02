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
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Library.Runner (LibraryModuleOutput (..))
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

parseLibraryDirs :: Parser [FilePath]
parseLibraryDirs = many $ strOption $ mconcat [
      long "library"
    , metavar "DIR"
    , help "Library directory containing C headers (repeatable)"
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
          opts.configCLI.hsBootPolicy

    mrc :: ModuleRenderConfig
    mrc = ModuleRenderConfig {
        qualifiedStyle = opts.configCLI.qualifiedStyle
      }

    -- | Single-header mode (the original behavior).
    execSingle :: IO ()
    execSingle = do
        case (opts.configCLI.outputOptions, opts.configCLI.hsBootPolicy) of
          (OutputOptions (SingleFile _), GenerateHsBoot) ->
            throwIO HsBootWithSingleFile
          _otherwise ->
            pure ()
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

    -- | Library mode: process all headers under the library directories,
    -- producing one Haskell module per header.
    execLibrary :: IO ()
    execLibrary = do
        case opts.configCLI.outputOptions of
          OutputOptions (SingleFile _) ->
            throwIO LibraryWithSingleFile
          _otherwise ->
            pure ()
        outputs <- run opts.configCLI.executionMode
        case opts.configCLI.executionMode of
          DryRun  -> forM_ (filter hasContent outputs) $
                       putStrLn . renderLibraryModuleLine
          RealRun -> pure ()
      where
        run :: ExecutionMode -> IO [LibraryModuleOutput]
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

{-------------------------------------------------------------------------------
  Exception
-------------------------------------------------------------------------------}

-- | Options that @preprocess@ cannot combine
data PreprocessException =
    HsBootWithSingleFile
  | LibraryWithSingleFile
  deriving Show

instance Exception PreprocessException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException HsBootWithSingleFile =
      "--generate-hs-boot is not available with --single-file"
    displayException LibraryWithSingleFile =
      "--library is not available with --single-file"

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

-- | Whether a library module produced any output (at least one binding
-- category has rendered content).
hasContent :: LibraryModuleOutput -> Bool
hasContent output = any isJust (toList output.rendered)

-- | The line a library dry run prints per module, such as
-- @Lib.Geo.Point: include\/geo\/point.h@
renderLibraryModuleLine :: LibraryModuleOutput -> String
renderLibraryModuleLine output =
    Hs.moduleNameToString output.moduleName ++ ": " ++ show output.headerPath
