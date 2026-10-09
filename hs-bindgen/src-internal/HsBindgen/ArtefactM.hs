module HsBindgen.ArtefactM (
    -- * Policies
    FilePolicy(..)
  , DirPolicy(..)
  , checkPolicies
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
  , dryRun
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
import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import System.Directory qualified as Dir
import System.FilePath (normalise, takeDirectory, (</>))
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

-- | Check that the delayed actions may be executed
--
-- No file is written twice, and every write agrees with its policies.
--
-- Two paths that differ only in case count as one file, as they are one file
-- on the default file systems of macOS and Windows.
checkPolicies :: [DelayedIO] -> ExceptT DelayedIOError IO ()
checkPolicies as = do
    forM_ writtenTwice $ \fds ->
      throwError $
        FileWrittenTwice
          (fileLocationToPath (NonEmpty.head fds).location)
          ((.description) <$> fds)
    mapM_ checkPolicy as
  where
    writtenTwice :: [NonEmpty FileDescription]
    writtenTwice = [
          fds
        | fds@(_ :| _ : _) <- Map.elems $ Map.fromListWith (flip (<>)) [
              (sameFile fd, fd :| [])
            | Just fd <- map outputFile as
            ]
        ]

    sameFile :: FileDescription -> FilePath
    sameFile fd = map Char.toLower $ normalise $ fileLocationToPath fd.location

checkPolicy :: DelayedIO -> ExceptT DelayedIOError IO ()
checkPolicy = mapM_ checkFile . outputFile
  where
    checkFile :: FileDescription -> ExceptT DelayedIOError IO ()
    checkFile fd = case fd.location of
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
emitTrace t = unsafeIO . traceWith t . withCallStack

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

-- | Turn the file writes registered so far into a dry run
--
-- The files are still checked against the output policies, and each one is
-- reported in place of being written.  A write registered afterwards is not
-- affected.
dryRun :: ArtefactM ()
dryRun = WrapArtefactM $ modify $ map $ \case
    a@WriteToStdOut{} -> a
    WriteToFile fd    -> DryRunFile fd
    a@DryRunFile{}    -> a

-- | Delayed IO action
data DelayedIO =
      WriteToStdOut  FileContent
    | WriteToFile    FileDescription
      -- | A file that is checked like one to be written, and then reported
    | DryRunFile     FileDescription

-- | The file that an action writes, or would write if it were not a dry run
outputFile :: DelayedIO -> Maybe FileDescription
outputFile = \case
    WriteToStdOut{} -> Nothing
    WriteToFile fd  -> Just fd
    DryRunFile fd   -> Just fd

executeDelayedIOActions :: Tracer DelayedIOMsg -> [DelayedIO] -> IO ()
executeDelayedIOActions tracer as =
  forM_ as $ \case
    WriteToStdOut x -> case x of
      StringContent     s  -> putStrLn s
      ByteStringContent bs -> BSS.putStr bs
    WriteToFile  fd -> do
      let path = fileLocationToPath fd.location
      traceWith tracer $ withCallStack $ DelayedIOWriteToFile path fd.description
      -- Creating the directory is justified by checking the policy first.
      Dir.createDirectoryIfMissing True (takeDirectory path)
      case fd.content of
        StringContent     s  -> writeFile path s
        ByteStringContent bs -> BSS.writeFile path bs
    DryRunFile fd ->
      traceWith tracer $ withCallStack $
        DelayedIODryRun (fileLocationToPath fd.location) fd.description

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data DelayedIOError =
      DirectoryDoesNotExist FilePath
    | FileAlreadyExists     FilePath
      -- | The file, and a description of each output that goes to it
    | FileWrittenTwice      FilePath (NonEmpty String)
  deriving Show

instance PrettyForTrace DelayedIOError where
  prettyForTrace = \case
    FileWrittenTwice fp descriptions ->
      PP.hangs' ("More than one output goes to file:" <+> PP.string fp) 2 $
        map PP.string (NonEmpty.toList descriptions)
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
    | DelayedIODryRun      FilePath String
  deriving stock (Show, Generic)

instance PrettyForTrace DelayedIOMsg where
  prettyForTrace = \case
    DelayedIOWriteToFile path what ->
      "Writing" <+> PP.show what <+> "to file" <+> PP.show path
    DelayedIODryRun path what ->
      "Dry run: not writing" <+> PP.show what <+> "to file" <+> PP.show path

instance IsTrace SafeLevel DelayedIOMsg where
  getDefaultLogLevel = \case
    DelayedIOWriteToFile{} -> SafeInfo
    DelayedIODryRun{}      -> SafeNotice
  getSource = const HsBindgen
  getTraceId = \case
    DelayedIOWriteToFile{} -> "delayedio-write-file"
    DelayedIODryRun{}      -> "delayedio-dry-run"
