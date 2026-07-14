-- | High-level libgit2 API: idiomatic Haskell over the generated bindings.
--
-- The handle and value types come from "LibGit2.Types", errors from
-- "LibGit2.Error", the lifecycle brackets from "LibGit2.Git", and the operations
-- from the per-topic modules. Each operation is a plain @Repository -> ... -> IO@
-- function; 'withRepository' passes the repository to a callback, so a caller
-- writes @withRepository path $ \\repo -> ...@ and never frees a handle.
--
module LibGit2
  ( -- * Types and errors
    module LibGit2.Types
  , module LibGit2.Error
    -- * Lifecycle and entry points
  , withLibgit2
  , withRepository
  , withNewRepository
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
    -- * Conveniences
  , walkFromHead
  ) where

import LibGit2.Commit
import LibGit2.Error
import LibGit2.Git
import LibGit2.Object
import LibGit2.Reference
import LibGit2.Repository
import LibGit2.Revwalk
import LibGit2.Types
import LibGit2.Write

-- | Every commit oid reachable from @HEAD@, newest first. Composes a revwalk:
-- a fresh walker, sort by time, push @HEAD@, then drain.
walkFromHead :: Repository -> IO [Oid]
walkFromHead repo = do
  w <- revwalkNew repo
  revwalkSortTime w
  revwalkPushHead w
  revwalkToList w
