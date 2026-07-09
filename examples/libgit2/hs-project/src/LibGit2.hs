-- | High-level libgit2 API: idiomatic Haskell over the generated bindings.
--
-- The handle and value types come from "LibGit2.Types", errors from
-- "LibGit2.Error", the @Git@ monad from "LibGit2.Git", and the operations from
-- the per-topic modules. The few monadic helpers at the bottom thread the
-- repository implicitly so a caller writes @withRepository path $ ...@ and never
-- mentions the handle.
--
module LibGit2
  ( -- * Types and errors
    module LibGit2.Types
  , module LibGit2.Error
    -- * The Git monad
  , Git
  , withRepository
  , withNewRepository
  , withLibgit2
  , repository
  , inRepo
    -- * Repository
  , repositoryOpen
  , repositoryHead
  , repositoryPath
    -- * Objects and references
  , revparseSingle
  , objectId
  , referenceName
  , referenceShorthand
  , referenceTarget
    -- * Revision walking
  , revwalkNew
  , revwalkPushHead
  , revwalkSortTime
  , revwalkNext
  , revwalkToList
    -- * Commits
  , commitLookup
  , commitAuthor
  , commitCommitter
  , commitMessage
  , commitSummary
  , commitTime
  , commitId
  , commitParentCount
    -- * Writing: blobs, trees, commits
  , repositoryInit
  , blobCreateFromBuffer
  , treebuilderNew
  , treebuilderInsert
  , treebuilderWrite
  , treeLookup
  , commitCreate
  , regularFileMode
    -- * Monadic conveniences
  , headReference
  , lookupCommit
  , walkFromHead
  ) where

import Control.Monad.IO.Class (liftIO)

import LibGit2.Commit
import LibGit2.Error
import LibGit2.Git
import LibGit2.Object
import LibGit2.Reference
import LibGit2.Repository
import LibGit2.Revwalk
import LibGit2.Types
import LibGit2.Write

-- | The resolved @HEAD@ reference of the current repository.
headReference :: Git Reference
headReference = inRepo repositoryHead

-- | Look up a commit by oid in the current repository.
lookupCommit :: Oid -> Git Commit
lookupCommit oid = inRepo (\r -> commitLookup r oid)

-- | Every commit oid reachable from @HEAD@, newest first.
walkFromHead :: Git [Oid]
walkFromHead = do
  w <- inRepo revwalkNew
  liftIO $ do
    revwalkSortTime w
    revwalkPushHead w
    revwalkToList w
