module HsBindgen.DelayedIO (
    -- * Policies
    FileOverwritePolicy(..)
  , OutputDirPolicy(..)
  , checkPolicy
    -- * File description
  , FileDescription(..)
  , FileLocation(..)
  , fileLocationToPath
  , RelativeToOutputDir(..)
  , FileContent(..)
    -- * DelayedIOM monad
  , DelayedIOM -- opaque
  , runDelayedIOM
  , runCached
    -- ** Actions
  , delay
  , DelayedIO(..)
  , executeFileSystemActions
    -- * Errors
  , DelayedIOError(..)
  ) where
import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.State (StateT (..), modify)
import Data.ByteString qualified as BSS
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         doesFileExist)
import System.FilePath (takeDirectory, (</>))
import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.BindingSpec.Private.V1 (UnresolvedBindingSpec)
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.Cache
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Policies
-------------------------------------------------------------------------------}

data FileOverwritePolicy
  = AllowFileOverwrite
  | DoNotOverwriteFiles
  deriving (Show, Eq)

instance Default FileOverwritePolicy where
  def = DoNotOverwriteFiles

data OutputDirPolicy
  = CreateOutputDirs
  | DoNotCreateOutputDirs
  deriving (Show, Eq)

instance Default OutputDirPolicy where
  def = DoNotCreateOutputDirs

checkPolicy :: DelayedIO -> ExceptT DelayedIOError IO ()
checkPolicy (WriteToStdOut   _) = pure ()
checkPolicy (WriteToFile fd) = case fd.location of
  UserSpecified path -> do
    let baseDir = takeDirectory path
    dirExists  <- liftIO $ doesDirectoryExist baseDir
    fileExists <- liftIO $ doesFileExist path
    unless dirExists $
      throwError $ DirectoryDoesNotExist baseDir
    when (fileExists && fd.fileOverwritePolicy == DoNotOverwriteFiles) $
      throwError $ FileAlreadyExists path
  RelativeFileLocation RelativeToOutputDir{..} -> do
    let path = outputDir </> localPath
    dirExists  <- liftIO $ doesDirectoryExist outputDir
    fileExists <- liftIO $ doesFileExist path
    unless (dirExists || outputDirPolicy == CreateOutputDirs ) $
      throwError $ DirectoryDoesNotExist outputDir
    when (fileExists && fd.fileOverwritePolicy == DoNotOverwriteFiles) $
      throwError $ FileAlreadyExists path

{-------------------------------------------------------------------------------
  File description
-------------------------------------------------------------------------------}

data FileDescription = FileDescription {
      description         :: String
    , location            :: FileLocation
    , fileOverwritePolicy :: FileOverwritePolicy
    , content             :: FileContent
    }

data FileLocation =
      -- | We never create directories for user-specified file paths.
      UserSpecified FilePath
    | RelativeFileLocation RelativeToOutputDir

data RelativeToOutputDir = RelativeToOutputDir {
      outputDir       :: FilePath
    , localPath       :: FilePath
    , outputDirPolicy :: OutputDirPolicy
    }

fileLocationToPath :: FileLocation -> FilePath
fileLocationToPath = \case
  UserSpecified p -> p
  RelativeFileLocation (RelativeToOutputDir d p _) -> d </> p

-- | Content to be written to a file
--
data FileContent
    = TextContent String
    | BindingSpecContent UnresolvedBindingSpec
    deriving Show

{-------------------------------------------------------------------------------
  DelayedIOM monad
-------------------------------------------------------------------------------}

newtype DelayedIOM a = WrapDelayedIOM {
    unwrapDelayedIOM :: StateT [DelayedIO] IO a
  }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    )

runDelayedIOM :: DelayedIOM a -> IO (a, [DelayedIO])
runDelayedIOM = flip runStateT [] . unwrapDelayedIOM

-- | Private (i.e., /not public/) API :-).
artefactIO :: IO a -> DelayedIOM a
artefactIO = WrapDelayedIOM . liftIO

runCached :: Cached a -> DelayedIOM a
runCached = artefactIO . getCached

{-------------------------------------------------------------------------------
  Actions
-------------------------------------------------------------------------------}

-- | Register a delayed IO action. The action will only be performed if the
--   artefacts are obtained without Error traces, and if the output policies
--   are met.
delay :: DelayedIO -> DelayedIOM ()
delay a = WrapDelayedIOM $ modify (a :)

-- | Delayed IO action
data DelayedIO =
      WriteToStdOut  FileContent
    | WriteToFile    FileDescription

executeFileSystemActions :: Tracer DelayedIOMsg -> [DelayedIO] -> IO ()
executeFileSystemActions tracer as =
  forM_ as $ \case
    WriteToStdOut x -> case x of
      TextContent str        -> putStrLn str
      BindingSpecContent ubs -> BSS.putStr $ BindingSpec.encodeYaml ubs
    WriteToFile  fd -> do
      let path = fileLocationToPath fd.location
      traceWith tracer $ DelayedIOWriteToFile path fd.description
      -- Creating the directory is justified by checking the policy first.
      createDirectoryIfMissing True (takeDirectory path)
      case fd.content of
        TextContent str        -> writeFile path str
        BindingSpecContent ubs -> BindingSpec.writeFile path ubs

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data DelayedIOError =
      DirectoryDoesNotExist FilePath
    | FileAlreadyExists     FilePath
  deriving Show

instance PrettyForTrace DelayedIOError where
  prettyForTrace = \case
    DirectoryDoesNotExist fp -> PP.vsep [
        "Output directory does not exist:" <+> PP.string fp
      , "Use --create-output-dirs to create it automatically, or create the directory manually."
      ]
    FileAlreadyExists fp -> PP.vsep [
        "Output file already exists:" <+> PP.string fp
      , "Use --overwrite-files to allow overwriting existing files, or delete the file manually."
      ]

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

data DelayedIOMsg = DelayedIOWriteToFile FilePath String
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedIOMsg where
  prettyForTrace = \case
    DelayedIOWriteToFile path what ->
      "Writing" <+> PP.showToCtxDoc what <+> "to file" <+> PP.showToCtxDoc path

instance IsTrace SafeLevel DelayedIOMsg where
  getDefaultLogLevel = \case
    DelayedIOWriteToFile{} -> SafeInfo
  getSource = const HsBindgen
  getTraceId = \case
    DelayedIOWriteToFile{} -> "delayedio-write-file"
