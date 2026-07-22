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
import Foreign.C.Types (CUInt)

import HsBindgen.HighLevel (input, output, resultPure, toHighLevel)
import HsBindgen.HighLevel.Marshaller (scalar)

import Generated.Revwalk.FunPtr qualified as WF
import Generated.Revwalk.Safe qualified as WS
import LibGit2.Error (checkStatusResult, gitError)
import LibGit2.Marshal (handleIn, newHandle, oidOut)
import LibGit2.Types (Oid, Repository, Revwalk)

-- | @git_revwalk_new@: a fresh walker over @repo@.
revwalkNew :: Repository -> IO Revwalk
revwalkNew repo =
    newHandle WF.git_revwalk_free (input handleIn) WS.git_revwalk_new repo

-- | @git_revwalk_push_head@: start the walk at @HEAD@.
revwalkPushHead :: Revwalk -> IO ()
revwalkPushHead =
    toHighLevel (input handleIn $ checkStatusResult) WS.git_revwalk_push_head

-- | @git_revwalk_sorting@ with @GIT_SORT_TIME@ (reverse-chronological, like
-- @git log@). @GIT_SORT_TIME@ is @1 << 1@ = @2@.
revwalkSortTime :: Revwalk -> IO ()
revwalkSortTime w =
    toHighLevel
      ( input handleIn          -- git_revwalk *walk
      $ input (scalar id)       -- unsigned int sort_mode
      $ checkStatusResult
      ) WS.git_revwalk_sorting w (2 :: CUInt)

-- | @git_revwalk_next@: the next oid, or 'Nothing' at @GIT_ITEROVER@.
revwalkNext :: Revwalk -> IO (Maybe Oid)
revwalkNext walk = do
    (oid, status) <- toHighLevel
      ( output oidOut       -- git_oid *out (peeked unconditionally)
      $ input  handleIn     -- git_revwalk *walk
      $ resultPure id       -- keep the raw status
      ) WS.git_revwalk_next walk
    case fromIntegral status :: Int of
      0   -> pure (Just oid)
      -31 -> pure Nothing                          -- GIT_ITEROVER
      n   -> throwIO =<< gitError (fromIntegral n)

-- | Drain a walker into a list (the iteration the combinators cannot express).
revwalkToList :: Revwalk -> IO [Oid]
revwalkToList w = go id
  where
    go acc = do
      m <- revwalkNext w
      case m of
        Nothing  -> pure (acc [])
        Just oid -> go (acc . (oid :))
