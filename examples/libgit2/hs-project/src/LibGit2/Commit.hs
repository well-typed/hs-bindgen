-- | Commit lookups and accessors.
--
-- @commitLookup@ is the constructor shape (out-handle + inputs + status).
-- The accessors are borrowed-pointer reads: a @const@ accessor returns a pointer
-- owned by the commit, copied out inside the @const T *@ input bracket (see
-- 'borrowedText' / 'borrowedOid'). Note 'commitSummary' takes a NON-const commit,
-- so it needs 'handleIn', a small illustration of the const/mutable split.
--
module LibGit2.Commit
  ( commitLookup
  , commitAuthor
  , commitCommitter
  , commitMessage
  , commitSummary
  , commitTime
  , commitId
  , commitParentCount
  ) where

import Data.Int (Int64)
import Data.Text (Text)

import HsBindgen.Runtime.HighLevel (input, output, resultIO, toHighLevel)

import Generated.Commit.FunPtr qualified as CF
import Generated.Commit.Safe qualified as CS
import LibGit2.Error (checkStatusResult)
import LibGit2.Marshal (borrowedOid, borrowedScalar, borrowedText, handleIn,
                        handleInC, oidInC, outHandle, peekTextConst)
import LibGit2.Signature (peekSignatureConst)
import LibGit2.Types (Commit, Oid, Repository, Signature)

-- | @git_commit_lookup@: the commit object for @oid@.
commitLookup :: Repository -> Oid -> IO Commit
commitLookup repo oid =
    fst <$> toHighLevel
      ( output (outHandle CF.git_commit_free)  -- git_commit **out
      $ input  handleIn                        -- git_repository *repo
      $ input  oidInC                          -- const git_oid *id
      $ checkStatusResult
      ) CS.git_commit_lookup repo oid

commitAuthor :: Commit -> IO Signature
commitAuthor = toHighLevel (input handleInC $ resultIO peekSignatureConst) CS.git_commit_author

commitCommitter :: Commit -> IO Signature
commitCommitter = toHighLevel (input handleInC $ resultIO peekSignatureConst) CS.git_commit_committer

commitMessage :: Commit -> IO Text
commitMessage = borrowedText CS.git_commit_message

-- | NB: @git_commit_summary@ takes a non-const @git_commit *@, so 'handleIn'
-- (not the 'borrowedText' helper, which uses the @const@ form).
commitSummary :: Commit -> IO Text
commitSummary = toHighLevel (input handleIn $ resultIO peekTextConst) CS.git_commit_summary

-- | The commit time (seconds since the epoch).
commitTime :: Commit -> IO Int64
commitTime = borrowedScalar CS.git_commit_time

commitId :: Commit -> IO Oid
commitId = borrowedOid CS.git_commit_id

commitParentCount :: Commit -> IO Int
commitParentCount = borrowedScalar CS.git_commit_parentcount
