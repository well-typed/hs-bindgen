module HsBindgen.ArtefactM (
    -- * Policies
    FilePolicy(..)
  , DirPolicy(..)
  , checkPolicy
    -- * File description
  , FileDescription(..)
  , FileLocation(..)
  , fileLocationToPath
  , RelativeToOutputDir(..)
  , FileContent(..)
    -- * ArtefactM monad
  , ArtefactM -- opaque
  , runArtefactM
  , askConfig
  , runCached
  , emitTrace
    -- ** Actions
  , delay
  , DelayedIO(..)
  , executeDelayedIOActions
    -- * Errors
  , DelayedIOError(..)
    -- * Traces
  , DelayedIOMsg(..)
  ) where

import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (StateT (..), modify)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BSS
import System.Directory qualified as Dir
import System.FilePath (takeDirectory, (</>))
import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Cache
import HsBindgen.Config.Internal
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Policies
-------------------------------------------------------------------------------}

data FilePolicy
  = AllowFileOverwrite
  | DoNotOverwriteFiles
  deriving (Show, Eq)

data DirPolicy
  = CreateOutputDirs
  | DoNotCreateOutputDirs
  deriving (Show, Eq)

checkPolicy :: DelayedIO -> ExceptT DelayedIOError IO ()
checkPolicy = \case
  WriteToStdOut{} -> pure ()
  WriteToFile fd -> case fd.location of
    UserSpecified path -> do
      let baseDir = takeDirectory path
      dirExists  <- liftIO $ Dir.doesDirectoryExist baseDir
      fileExists <- liftIO $ Dir.doesFileExist path
      unless (dirExists || fd.dirPolicy == CreateOutputDirs) $
        throwError $ DirectoryDoesNotExist baseDir
      when (fileExists && fd.filePolicy == DoNotOverwriteFiles) $
        throwError $ FileAlreadyExists path
    RelativeFileLocation relative -> do
      let path = relative.outputDir </> relative.localPath
      dirExists  <- liftIO $ Dir.doesDirectoryExist relative.outputDir
      fileExists <- liftIO $ Dir.doesFileExist path
      unless (dirExists || fd.dirPolicy == CreateOutputDirs) $
        throwError $ DirectoryDoesNotExist relative.outputDir
      when (fileExists && fd.filePolicy == DoNotOverwriteFiles) $
        throwError $ FileAlreadyExists path

{-------------------------------------------------------------------------------
  File description
-------------------------------------------------------------------------------}

data FileDescription = FileDescription {
      description :: String
    , location    :: FileLocation
    , filePolicy  :: FilePolicy
    , dirPolicy   :: DirPolicy
    , content     :: FileContent
    }

data FileLocation =
      UserSpecified FilePath
    | RelativeFileLocation RelativeToOutputDir
  deriving stock (Show, Generic)

data RelativeToOutputDir = RelativeToOutputDir {
      outputDir :: FilePath
    , localPath :: FilePath
    }
  deriving stock (Show, Generic)

fileLocationToPath :: FileLocation -> FilePath
fileLocationToPath = \case
    UserSpecified p -> p
    RelativeFileLocation relative -> relative.outputDir </> relative.localPath

-- | Content to be written to a file
--
data FileContent =
    StringContent     String
  | ByteStringContent ByteString
  deriving Show

{-------------------------------------------------------------------------------
  ArtefactM monad
-------------------------------------------------------------------------------}

newtype ArtefactM a =
  WrapArtefactM (StateT [DelayedIO] (ReaderT BindgenConfig IO) a)
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadReader BindgenConfig
    )

runArtefactM :: ArtefactM a -> BindgenConfig -> IO (a, [DelayedIO])
runArtefactM (WrapArtefactM ma) = runReaderT (runStateT ma [])

askConfig :: ArtefactM BindgenConfig
askConfig = ask

-- | Private (i.e., /not public/) API :-).
unsafeIO :: IO a -> ArtefactM a
unsafeIO = WrapArtefactM . liftIO

-- | Emit a trace while running artefacts.
emitTrace :: Tracer a -> a -> ArtefactM ()
emitTrace t = unsafeIO . traceWith t

runCached :: Cached a -> ArtefactM a
runCached = unsafeIO . getCached

{-------------------------------------------------------------------------------
  Actions
-------------------------------------------------------------------------------}

-- | Register a delayed IO action. The action will only be performed if the
--   artefacts are obtained without Error traces, and if the output policies
--   are met.
delay :: DelayedIO -> ArtefactM ()
delay a = WrapArtefactM $ modify (a :)

-- | Delayed IO action
data DelayedIO =
      WriteToStdOut  FileContent
    | WriteToFile    FileDescription

executeDelayedIOActions :: Tracer DelayedIOMsg -> [DelayedIO] -> IO ()
executeDelayedIOActions tracer as =
  forM_ as $ \case
    WriteToStdOut x -> case x of
      StringContent     s  -> putStrLn s
      ByteStringContent bs -> BSS.putStr bs
    WriteToFile  fd -> do
      let path = fileLocationToPath fd.location
      traceWith tracer $ DelayedIOWriteToFile path fd.description
      -- Creating the directory is justified by checking the policy first.
      Dir.createDirectoryIfMissing True (takeDirectory path)
      case fd.content of
        StringContent     s  -> writeFile path s
        ByteStringContent bs -> BSS.writeFile path bs

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

data DelayedIOMsg =
      DelayedIOWriteToFile FilePath String
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedIOMsg where
  prettyForTrace = \case
    DelayedIOWriteToFile path what ->
      "Writing" <+> PP.show what <+> "to file" <+> PP.show path

instance IsTrace SafeLevel DelayedIOMsg where
  getDefaultLogLevel = \case
    DelayedIOWriteToFile{} -> SafeInfo
  getSource = const HsBindgen
  getTraceId = \case
    DelayedIOWriteToFile{} -> "delayedio-write-file"
