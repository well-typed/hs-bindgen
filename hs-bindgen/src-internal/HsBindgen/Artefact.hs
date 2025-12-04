module HsBindgen.Artefact (
    -- * Artefacts
    Artefact(..)
  , runArtefacts
    -- * Traces
  , ArtefactMsg(..)
    -- * Errors
  , RunArtefactError(..)
  , FileSystemError(..)
    -- * Policies
  , FileOverwritePolicy(..)
  , OutputDirPolicy(..)
    -- * File descriptions
  , FileDescription(..)
  , FileLocation(..)
  , RelativeToOutputDir(..)
  , FileContent(..)
    -- * Artefact monad
  , ArtefactM -- opaque
  , ArtefactEnv(..)
    -- ** Actions
  , delayWriteFile
  )
where

import Control.Monad (liftM)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT,
                             withExceptT)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..), modify)
import Data.IORef (IORef)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         doesFileExist)
import System.FilePath (takeDirectory, (</>))
import Text.SimplePrettyPrint ((<+>))
import Text.SimplePrettyPrint qualified as PP

import Clang.Paths

import HsBindgen.Backend
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.CallConv (UserlandCapiWrapper)
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.BindingSpec.Private.V1 (UnresolvedBindingSpec)
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.Boot
import HsBindgen.Config
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Frontend
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.ProcessIncludes qualified as ProcessIncludes
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Artefact
-------------------------------------------------------------------------------}

-- | Build artefact.
data Artefact (a :: Star) where
  -- * Boot
  Target              :: Artefact ClangArgs.Target
  HashIncludeArgs     :: Artefact [HashIncludeArg]
  -- * Frontend
  IncludeGraph        :: Artefact (IncludeGraph.Predicate, IncludeGraph.IncludeGraph)
  GetMainHeaders      :: Artefact ProcessIncludes.GetMainHeaders
  DeclIndex           :: Artefact DeclIndex.DeclIndex
  UseDeclGraph        :: Artefact UseDeclGraph.UseDeclGraph
  DeclUseGraph        :: Artefact DeclUseGraph.DeclUseGraph
  OmitTypes           :: Artefact [(C.QualName, SourcePath)]
  ReifiedC            :: Artefact [C.Decl]
  Dependencies        :: Artefact [SourcePath]
  -- * Backend
  HsDecls             :: Artefact (ByCategory [Hs.Decl])
  FinalDecls          :: Artefact (ByCategory ([UserlandCapiWrapper], [SHs.SDecl]))
  FinalModuleBaseName :: Artefact BaseModuleName
  FinalModuleSafe     :: Artefact HsModule
  FinalModuleUnsafe   :: Artefact HsModule
  FinalModules        :: Artefact (ByCategory HsModule)
  -- * Lift and sequence artefacts
  Lift                :: ArtefactM a -> Artefact a
  Bind                :: Artefact b  -> (b -> Artefact c ) -> Artefact c

instance Functor Artefact where
  fmap :: (a -> b) -> Artefact a -> Artefact b
  fmap = liftM

instance Applicative Artefact where
  pure :: a -> Artefact a
  pure = Lift . pure

  (<*>) :: Artefact (a -> b) -> Artefact a -> Artefact b
  (<*>) = ap

instance Monad Artefact where
  (>>=) :: Artefact a -> (a -> Artefact b) -> Artefact b
  (>>=) = Bind

instance MonadIO Artefact where
  liftIO :: IO a -> Artefact a
  liftIO = Lift . liftIO

{-------------------------------------------------------------------------------
  Run artefacts
-------------------------------------------------------------------------------}

-- | Compute the results of a list of artefacts.
--
-- All top-level artefacts will be cached (this is not true for computed
-- artefacts, using, for example, the 'Functor' interface, or 'Lift').
runArtefacts :: forall a.
     Tracer ArtefactMsg
  -> IORef TracerState
  -> BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefact a
  -> IO (Either RunArtefactError a)
runArtefacts
  tracerSafe
  tracerUnsafeStateRef
  BootArtefact{..}
  FrontendArtefact{..}
  BackendArtefact{..}
  artefact = runExceptT run
  where
    env :: ArtefactEnv
    env = ArtefactEnv tracerSafe

    run :: ExceptT RunArtefactError IO a
    run = do
      -- The 'Bind' operator of 'Artefact' checks for 'Error' traces.
      (result, actions)  <-
        withExceptT (const ErrorReported) $
          runArtefactM env $ runArtefact artefact

      -- Before creating directories or writing output files, we verify
      -- adherence to the provided policies.
      mapM_ checkPolicy actions

      liftIO $ executeFileSystemActions actions
      pure result

    runArtefact :: forall x. Artefact x -> ArtefactM x
    runArtefact a = do
      r <- case a of
        --Boot.
        Target              -> liftIO bootTarget
        HashIncludeArgs     -> liftIO bootHashIncludeArgs
        -- Frontend.
        IncludeGraph        -> liftIO frontendIncludeGraph
        GetMainHeaders      -> liftIO frontendGetMainHeaders
        DeclIndex           -> liftIO frontendIndex
        UseDeclGraph        -> liftIO frontendUseDeclGraph
        DeclUseGraph        -> liftIO frontendDeclUseGraph
        OmitTypes           -> liftIO frontendOmitTypes
        ReifiedC            -> liftIO frontendCDecls
        Dependencies        -> liftIO frontendDependencies
        -- Backend.
        HsDecls             -> liftIO backendHsDecls
        FinalDecls          -> liftIO backendFinalDecls
        FinalModuleBaseName -> pure backendFinalModuleBaseName
        FinalModuleSafe     -> liftIO backendFinalModuleSafe
        FinalModuleUnsafe   -> liftIO backendFinalModuleUnsafe
        FinalModules        -> liftIO backendFinalModules
        -- Lift and sequence.
        (Lift   f)          -> f
        (Bind x f)          -> runArtefact x >>= runArtefact . f
      -- After each step, check tracer state for 'Error' traces.
      mbError <- checkTracerState tracerUnsafeStateRef
      case mbError of
        Just err -> throwError err
        Nothing  -> pure r

    checkPolicy :: FileSystemAction -> ExceptT RunArtefactError IO ()
    checkPolicy (WriteFile fd) = case fd.location of
      UserSpecified path -> do
        let baseDir = takeDirectory path
        dirExists  <- liftIO $ doesDirectoryExist baseDir
        fileExists <- liftIO $ doesFileExist path
        unless dirExists $
          throwError $ FileSystemError $ DirectoryDoesNotExist baseDir
        when (fileExists && fd.fileOverwritePolicy == DoNotOverwriteFiles) $
          throwError $ FileSystemError $ FileAlreadyExists path
      RelativeFileLocation RelativeToOutputDir{..} -> do
        let path = outputDir </> localPath
        dirExists  <- liftIO $ doesDirectoryExist outputDir
        fileExists <- liftIO $ doesFileExist path
        unless (dirExists || outputDirPolicy == CreateOutputDirs ) $
          throwError $ FileSystemError $ DirectoryDoesNotExist outputDir
        when (fileExists && fd.fileOverwritePolicy == DoNotOverwriteFiles) $
          throwError $ FileSystemError $ FileAlreadyExists path

    executeFileSystemActions :: [FileSystemAction] -> IO ()
    executeFileSystemActions as =
      forM_ as $ \case
        WriteFile fd -> do
          let path = fileLocationToPath fd.location
          traceWith tracerSafe $ RunArtefactWriteFile path fd.description
          -- Creating the directory is justified by checking the policy first.
          createDirectoryIfMissing True (takeDirectory path)
          case fd.content of
            TextContent str        -> writeFile path str
            BindingSpecContent ubs -> BindingSpec.writeFile path ubs

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

data ArtefactMsg =
    RunArtefactCreateDir FilePath
  | RunArtefactWriteFile FilePath String
  deriving stock (Show, Generic)

instance PrettyForTrace ArtefactMsg where
  prettyForTrace = \case
    RunArtefactCreateDir path ->
      "Creating directory" <+> PP.showToCtxDoc path
    RunArtefactWriteFile path what ->
      "Writing" <+> PP.showToCtxDoc what <+> "to file" <+> PP.showToCtxDoc path

instance IsTrace SafeLevel ArtefactMsg where
  getDefaultLogLevel = \case
    RunArtefactCreateDir{} -> SafeInfo
    RunArtefactWriteFile{} -> SafeInfo
  getSource = const HsBindgen
  getTraceId = \case
    RunArtefactCreateDir{} -> "run-artefact-create-dir"
    RunArtefactWriteFile{} -> "run-artefact-write-file"

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data RunArtefactError =
      ErrorReported
    | FileSystemError FileSystemError
  deriving stock Show

instance PrettyForTrace RunArtefactError where
  prettyForTrace = \case
    ErrorReported     -> "An error happened (see above)"
    FileSystemError e -> prettyForTrace e

data FileSystemError =
      DirectoryDoesNotExist FilePath
    | FileAlreadyExists     FilePath
  deriving Show

instance PrettyForTrace FileSystemError where
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
  Artefact monad
-------------------------------------------------------------------------------}

newtype ArtefactM a = WrapArtefactM {
    unwrapArtefactM ::
      StateT [FileSystemAction]
        (ReaderT ArtefactEnv
          (ExceptT AnErrorHappened IO))
        a
  }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError AnErrorHappened
    )

data ArtefactEnv = ArtefactEnv {
      artefactTracer :: Tracer ArtefactMsg
    }

runArtefactM ::
  ArtefactEnv -> ArtefactM a -> ExceptT AnErrorHappened IO (a, [FileSystemAction])
runArtefactM env = flip runReaderT env . flip runStateT [] . unwrapArtefactM

{-------------------------------------------------------------------------------
  Actions
-------------------------------------------------------------------------------}

-- | Delayed directory create.
delayWriteFile :: FileDescription -> ArtefactM ()
delayWriteFile fd = WrapArtefactM $ modify (WriteFile fd :)

-- | A file system action to be executed
data FileSystemAction = WriteFile FileDescription
