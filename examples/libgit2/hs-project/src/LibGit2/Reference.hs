-- | Reference accessors (all borrowed-pointer reads).
--
module LibGit2.Reference
  ( referenceName
  , referenceShorthand
  , referenceTarget
  ) where

import Data.Text (Text)

import Generated.Refs.Safe qualified as RfS
import LibGit2.Marshal (borrowedOid, borrowedText)
import LibGit2.Types (Oid, Reference)

-- | @git_reference_name@: the full reference name (e.g. @refs/heads/main@).
referenceName :: Reference -> IO Text
referenceName = borrowedText RfS.git_reference_name

-- | @git_reference_shorthand@: the human-friendly name (e.g. @main@).
referenceShorthand :: Reference -> IO Text
referenceShorthand = borrowedText RfS.git_reference_shorthand

-- | @git_reference_target@: the oid a direct reference points at.
referenceTarget :: Reference -> IO Oid
referenceTarget = borrowedOid RfS.git_reference_target
