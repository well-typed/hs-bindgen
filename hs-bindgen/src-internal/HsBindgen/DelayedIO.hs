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
import Control.Monad.State (StateT (..), modify)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BSS
import System.Directory qualified as Dir
import System.FilePath (takeDirectory, (</>))
import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

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
checkPolicy = \case
  WriteToStdOut{} -> pure ()
  WriteToFile fd -> case fd.location of
    UserSpecified path -> do
      let baseDir = takeDirectory path
      dirExists  <- liftIO $ Dir.doesDirectoryExist baseDir
      fileExists <- liftIO $ Dir.doesFileExist path
      unless dirExists $
        throwError $ DirectoryDoesNotExist baseDir
      when (fileExists && fd.fileOverwritePolicy == DoNotOverwriteFiles) $
        throwError $ FileAlreadyExists path
    RelativeFileLocation RelativeToOutputDir{..} -> do
      let path = outputDir </> localPath
      dirExists  <- liftIO $ Dir.doesDirectoryExist outputDir
      fileExists <- liftIO $ Dir.doesFileExist path
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
  deriving stock (Show, Generic)

data RelativeToOutputDir = RelativeToOutputDir {
      outputDir       :: FilePath
    , localPath       :: FilePath
    , outputDirPolicy :: OutputDirPolicy
    }
  deriving stock (Show, Generic)

fileLocationToPath :: FileLocation -> FilePath
fileLocationToPath = \case
  UserSpecified p -> p
  RelativeFileLocation (RelativeToOutputDir d p _) -> d </> p

-- | Content to be written to a file
--
data FileContent
    = StringContent     String
    | ByteStringContent ByteString
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
unsafeIO :: IO a -> DelayedIOM a
unsafeIO = WrapDelayedIOM . liftIO

-- | Emit a trace while running artefacts.
emitTrace :: Tracer a -> a -> DelayedIOM ()
emitTrace t = unsafeIO . traceWith t

runCached :: Cached a -> DelayedIOM a
runCached = unsafeIO . getCached

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
