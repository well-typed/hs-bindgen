-- | The revision walker: a stateful libgit2 iterator.
--
-- @git_revwalk_next@ is the interesting case for the combinators: its @git_oid@
-- out-parameter is only valid when the status is @0@, and the terminating
-- @GIT_ITEROVER@ is NOT an error. So the output is peeked unconditionally and
-- the status kept raw ('resultPure' 'id'), then classified by hand into 'Maybe'.
-- The /looping/ ('revwalkToList') is plain Haskell: the per-call combinators
-- cannot express iteration.
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

import HsBindgen.Runtime.HighLevel (input, output, resultPure, toHighLevel)
import HsBindgen.Runtime.HighLevel.Marshaller (scalar)

import Generated.Revwalk.FunPtr qualified as WF
import Generated.Revwalk.Safe qualified as WS
import LibGit2.Error (checkStatusResult, gitError)
import LibGit2.Marshal (handleIn, oidOut, outHandle)
import LibGit2.Types (Oid, Repository, Revwalk)

-- | @git_revwalk_new@: a fresh walker over @repo@.
revwalkNew :: Repository -> IO Revwalk
revwalkNew repo =
    fst <$> toHighLevel
      ( output (outHandle WF.git_revwalk_free)
      $ input  handleIn
      $ checkStatusResult
      ) WS.git_revwalk_new repo

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
