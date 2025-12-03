module HsBindgen.Artefact (
    -- * Artefact
    Artefact(..)
  , runArtefacts
  , ArtefactMsg(..)
    -- ** Error
  , RunArtefactError(..)
  , FileSystemError(..)
    -- * ArtefactM
  , ArtefactM -- opaque
  , ArtefactEnv(..)
    -- ** Actions
  , FileContent(..)
  , delayMkDir
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
import System.FilePath (takeDirectory)
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

-- | A file system action to be executed
data FileSystemAction =
      MkDir FilePath
    | WriteFile String FilePath FileContent

runArtefactM ::
  ArtefactEnv -> ArtefactM a -> ExceptT AnErrorHappened IO (a, [FileSystemAction])
runArtefactM env = flip runReaderT env . flip runStateT [] . unwrapArtefactM

-- * Delayed directory create.
delayMkDir :: FilePath -> ArtefactM ()
delayMkDir fp =
  WrapArtefactM $ modify (MkDir fp :)

-- * Delayed directory create.
delayWriteFile :: String -> FilePath -> FileContent -> ArtefactM ()
delayWriteFile what fp content =
  WrapArtefactM $ modify (WriteFile what fp content :)

-- | Content to be written to a file
--
data FileContent
    = TextContent String
    | BindingSpecContent UnresolvedBindingSpec
    deriving Show

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
  -> BackendConfig
  -> BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefact a
  -> IO (Either RunArtefactError a)
runArtefacts
  tracerSafe
  tracerUnsafeStateRef
  BackendConfig{..}
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
      checkOutputDirPolicy     actions
      checkFileOverwritePolicy actions

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

    lookForExistingDir :: [FileSystemAction] -> IO (Maybe FilePath)
    lookForExistingDir [] = pure Nothing
    lookForExistingDir (MkDir path : as) = do
      fileExists <- doesDirectoryExist path
      if fileExists
         then pure (Just path)
         else lookForExistingFile as
    lookForExistingDir (_ : as) = lookForExistingDir as

    checkOutputDirPolicy :: [FileSystemAction] -> ExceptT RunArtefactError IO ()
    checkOutputDirPolicy as = do
      case backendOutputDirPolicy of
        CreateDirStructure -> pure ()
        DoNotCreateDirStructure -> do
          -- Get the first directory path that exists if any
          mbDirPath <- liftIO $ lookForExistingDir as
          case mbDirPath of
            Nothing -> pure ()
            Just d  ->
              let err = FileSystemError (DirectoryAlreadyExists d)
              in  throwError err

    lookForExistingFile :: [FileSystemAction] -> IO (Maybe FilePath)
    lookForExistingFile [] = pure Nothing
    lookForExistingFile (WriteFile _ path _ : as) = do
      fileExists <- doesFileExist path
      if fileExists
         then pure (Just path)
         else lookForExistingFile as
    lookForExistingFile (_ : as) = lookForExistingFile as

    checkFileOverwritePolicy :: [FileSystemAction] -> ExceptT RunArtefactError IO ()
    checkFileOverwritePolicy as = do
      case backendFileOverwrite of
        AllowFileOverwrite -> pure ()
        ProtectExistingFiles -> do
          -- Get the first file path that exists if any
          mbFilePath <- liftIO $ lookForExistingFile as
          case mbFilePath of
            Nothing -> pure ()
            Just f  ->
              let err = FileSystemError (FileAlreadyExists f)
              in  throwError err

    executeFileSystemActions :: [FileSystemAction] -> IO ()
    executeFileSystemActions as =
      forM_ as $ \case
        MkDir outputDir -> do
          traceWith tracerSafe $ RunArtefactCreateDir outputDir
          createDirectoryIfMissing True outputDir
        WriteFile what path content -> do
          traceWith tracerSafe $ RunArtefactWriteFile path what
          createDirectoryIfMissing True (takeDirectory path)
          case content of
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
      DirectoryAlreadyExists FilePath
    | FileAlreadyExists      FilePath
  deriving Show

instance PrettyForTrace FileSystemError where
  prettyForTrace = \case
    DirectoryAlreadyExists fp -> PP.vsep [
        "Output directory already exists:" <+> PP.string fp
      , "Use --create-output-dirs to create it automatically, or create the directory manually."
      ]
    FileAlreadyExists fp -> PP.vsep [
        "Output file already exists:" <+> PP.string fp
      , "Use --overwrite-files to allow overwriting existing files, or delete the file manually."
      ]
