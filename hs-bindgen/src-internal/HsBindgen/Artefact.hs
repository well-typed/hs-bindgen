module HsBindgen.Artefact (
    -- * Artefacts
    Artefact(..)
  , runArtefacts
    -- * Policies
  , FileOverwritePolicy(..)
  , OutputDirPolicy(..)
    -- * File description
  , FileDescription(..)
  , FileLocation(..)
  , fileLocationToPath
  , RelativeToOutputDir(..)
  , FileContent(..)
    -- * ArtefactM monad
  , ArtefactM -- opaque
    -- ** Actions
  , delay
  , DelayedIO(..)
    -- * Errors
  , DelayedIOError(..)
  )
where

import Control.Monad (liftM)
import Control.Monad.State (StateT (..), modify)
import System.FilePath ((</>))
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
import HsBindgen.Boot
import HsBindgen.Cache (Cached (getCached))
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

{-------------------------------------------------------------------------------
  Run artefacts
-------------------------------------------------------------------------------}

-- | Compute the results of a list of artefacts.
--
-- All top-level artefacts will be cached (this is not true for computed
-- artefacts, using, for example, the 'Functor' interface, or 'Lift').
runArtefacts :: forall a.
     BootArtefact
  -> FrontendArtefact
  -> BackendArtefact
  -> Artefact a
  -> IO (a, [DelayedIO])
runArtefacts
  BootArtefact{..}
  FrontendArtefact{..}
  BackendArtefact{..}
  artefact = runArtefactM $ runArtefact artefact
  where
    runArtefact :: forall x. Artefact x -> ArtefactM x
    runArtefact = \case
        --Boot.
        Target              -> aGetCached bootTarget
        HashIncludeArgs     -> aGetCached bootHashIncludeArgs
        -- Frontend.
        IncludeGraph        -> aGetCached frontendIncludeGraph
        GetMainHeaders      -> aGetCached frontendGetMainHeaders
        DeclIndex           -> aGetCached frontendIndex
        UseDeclGraph        -> aGetCached frontendUseDeclGraph
        DeclUseGraph        -> aGetCached frontendDeclUseGraph
        OmitTypes           -> aGetCached frontendOmitTypes
        ReifiedC            -> aGetCached frontendCDecls
        Dependencies        -> aGetCached frontendDependencies
        -- Backend.
        HsDecls             -> aGetCached backendHsDecls
        FinalDecls          -> aGetCached backendFinalDecls
        FinalModuleBaseName -> pure backendFinalModuleBaseName
        FinalModuleSafe     -> aGetCached backendFinalModuleSafe
        FinalModuleUnsafe   -> aGetCached backendFinalModuleUnsafe
        FinalModules        -> aGetCached backendFinalModules
        -- Lift and sequence.
        (Lift   f)          -> f
        (Bind x f)          -> runArtefact x >>= runArtefact . f

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
  ArtefactM monad
-------------------------------------------------------------------------------}

newtype ArtefactM a = WrapArtefactM {
    unwrapArtefactM :: StateT [DelayedIO] IO a
  }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    )

runArtefactM :: ArtefactM a -> IO (a, [DelayedIO])
runArtefactM = flip runStateT [] . unwrapArtefactM

-- | Private (i.e., /not public/) API :-).
artefactIO :: IO a -> ArtefactM a
artefactIO = WrapArtefactM . liftIO

aGetCached :: Cached a -> ArtefactM a
aGetCached = artefactIO . getCached

{-------------------------------------------------------------------------------
  Actions
-------------------------------------------------------------------------------}

-- | Register a delayed IO action. The action will only be performed if the
-- | artefacts are obtained without Error traces, and if the output policies
-- | align.
delay :: DelayedIO -> ArtefactM ()
delay a = WrapArtefactM $ modify (a :)

-- | A file system action to be executed
data DelayedIO =
      WriteFile FileDescription
    | PutStrLn  String

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
