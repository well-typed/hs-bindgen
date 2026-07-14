-- | Library lifecycle and the @with@-bracket entry points.
--
-- The combinators lift individual calls into 'IO'; this module owns the one thing
-- they cannot: the @git_libgit2_init@ / @git_libgit2_shutdown@ lifecycle and the
-- bound-thread requirement for libgit2's thread-local error reporting (read in
-- "LibGit2.Error"). 'withRepository' and 'withNewRepository' open a repository and
-- hand it to a callback, so callers write plain 'IO' with no state-threading monad.
-- The repository handle frees itself at GC (it is a 'Foreign.ForeignPtr.ForeignPtr'),
-- so the bracket only owns the init/shutdown pair.
--
module LibGit2.Git
  ( withLibgit2
  , withRepository
  , withNewRepository
  ) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket_)
import Data.Text qualified as T

import Generated.Global.Safe (git_libgit2_init, git_libgit2_shutdown)
import LibGit2.Error (checkStatus)
import LibGit2.Repository (repositoryOpen)
import LibGit2.Types (Repository)
import LibGit2.Write (repositoryInit)

-- | Initialise libgit2 for the duration of an action, on a bound OS thread.
withLibgit2 :: IO a -> IO a
withLibgit2 act =
    runInBoundThread $
      bracket_
        (git_libgit2_init >>= checkStatus)
        (() <$ git_libgit2_shutdown)
        act

-- | Open the repository at @path@ and run an action against it.
withRepository :: FilePath -> (Repository -> IO a) -> IO a
withRepository path act =
    withLibgit2 $ repositoryOpen (T.pack path) >>= act

-- | Initialise a new repository at @path@ and run an action against it.
withNewRepository :: FilePath -> (Repository -> IO a) -> IO a
withNewRepository path act =
    withLibgit2 $ repositoryInit (T.pack path) >>= act
