-- | The revision walker: a stateful libgit2 iterator. Create one with
-- 'revwalkNew', seed it ('revwalkPushHead', 'revwalkSortTime'), then pull commits
-- with 'revwalkNext' or drain the whole walk with 'revwalkToList'.
--
module LibGit2.Revwalk
  ( revwalkNew
  , revwalkPushHead
  , revwalkSortTime
  , revwalkNext
  , revwalkToList
  ) where

import Control.Exception (throwIO)

import HsBindgen.HighLevel (fixed, input, output, resultIO, toHighLevel)

import Generated.Errors (Git_error_code (..), pattern GIT_ITEROVER,
                         pattern GIT_OK)
import Generated.Revwalk (Git_sort_t (..), pattern GIT_SORT_TIME)
import Generated.Revwalk.FunPtr qualified as WF
import Generated.Revwalk.Safe qualified as WS
import LibGit2.Error (checkedStatus, gitError)
import LibGit2.Marshal (handleIn, newHandle, oidOut)
import LibGit2.Types (Oid, Repository, Revwalk)

-- | @git_revwalk_new@: a fresh walker over @repo@.
revwalkNew :: Repository -> IO Revwalk
revwalkNew = newHandle WF.git_revwalk_free (input handleIn) WS.git_revwalk_new

-- | @git_revwalk_push_head@: start the walk at @HEAD@.
revwalkPushHead :: Revwalk -> IO ()
revwalkPushHead = toHighLevel WS.git_revwalk_push_head
                $ input handleIn
                $ checkedStatus

-- | @git_revwalk_sorting@ with @GIT_SORT_TIME@: reverse-chronological, like
-- @git log@.
--
-- The mode is 'fixed' rather than an 'input' because this wrapper is /about/ time
-- sorting; a caller who wants to choose gets a different wrapper. @sort_mode@ is
-- declared @unsigned int@ rather than @git_sort_t@, so the generated constant is
-- unwrapped back to its underlying value here.
revwalkSortTime :: Revwalk -> IO ()
revwalkSortTime = toHighLevel WS.git_revwalk_sorting
                $ input handleIn                         -- git_revwalk *walk
                $ fixed (unwrapGit_sort_t GIT_SORT_TIME) -- unsigned int sort_mode
                $ checkedStatus

-- | @git_revwalk_next@: the next oid, or 'Nothing' once the walk is exhausted.
--
-- Reaching the end of a walk is not a failure, so @GIT_ITEROVER@ becomes 'Nothing'
-- rather than an exception, and only a genuine error throws.
--
-- libgit2 writes the oid slot only when it yields a commit, so at @GIT_ITEROVER@ the
-- value read back is whatever 'Foreign.Marshal.Alloc.alloca' left there. It is
-- discarded unread in that case.
revwalkNext :: Revwalk -> IO (Maybe Oid)
revwalkNext = toHighLevel WS.git_revwalk_next
            $ output oidOut   -- git_oid *out
            $ input  handleIn -- git_revwalk *walk
            $ resultIO nextOid
  where
    nextOid oid status = case Git_error_code status of
      GIT_OK       -> pure (Just oid)
      GIT_ITEROVER -> pure Nothing
      _            -> throwIO =<< gitError status

-- | Drain a walker into a list (the iteration the combinators cannot express).
revwalkToList :: Revwalk -> IO [Oid]
revwalkToList w = go id
  where
    go acc = do
      m <- revwalkNext w
      case m of
        Nothing  -> pure (acc [])
        Just oid -> go (acc . (oid :))
