{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The @Git@ monad: a thin @ReaderT@ over the repository handle.
--
-- This is the piece the combinators cannot provide. They lift individual calls
-- but thread state only through brackets, so the repository handle and the
-- @git_libgit2_init@ / @git_libgit2_shutdown@ lifecycle are hidden here instead.
-- 'withLibgit2' also pins the work to one OS thread ('runInBoundThread') so
-- libgit2's thread-local error reporting (read in "LibGit2.Error") is reliable.
--
module LibGit2.Git
  ( Git
  , withRepository
  , withNewRepository
  , withLibgit2
  , repository
  , inRepo
  ) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Text qualified as T

import Generated.Global.Safe (git_libgit2_init, git_libgit2_shutdown)
import LibGit2.Error (checkStatus)
import LibGit2.Repository (repositoryOpen)
import LibGit2.Types (Repository)
import LibGit2.Write (repositoryInit)

-- | A computation with read access to an open 'Repository'.
newtype Git a = Git (ReaderT Repository IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Initialise libgit2 for the duration of an action, on a bound OS thread.
withLibgit2 :: IO a -> IO a
withLibgit2 act =
    runInBoundThread $
      bracket_
        (git_libgit2_init >>= checkStatus)
        (() <$ git_libgit2_shutdown)
        act

-- | Open the repository at @path@ and run a 'Git' computation against it.
withRepository :: FilePath -> Git a -> IO a
withRepository path (Git act) =
    withLibgit2 $ do
      repo <- repositoryOpen (T.pack path)
      runReaderT act repo

-- | Initialise a new repository at @path@ and run a 'Git' computation.
withNewRepository :: FilePath -> Git a -> IO a
withNewRepository path (Git act) =
    withLibgit2 $ do
      repo <- repositoryInit (T.pack path)
      runReaderT act repo

-- | The repository the computation runs against.
repository :: Git Repository
repository = Git ask

-- | Lift a repository-taking 'IO' action into 'Git', supplying the repository.
inRepo :: (Repository -> IO a) -> Git a
inRepo f = repository >>= liftIO . f
