-- | Generic object lookups (@git_revparse_single@) and the object id accessor.
--
module LibGit2.Object
  ( revparseSingle
  , objectId
  ) where

import Data.Text (Text)

import HsBindgen.Runtime.HighLevel (input, output, toHighLevel)

import Generated.Object.FunPtr qualified as OF
import Generated.Object.Safe qualified as OS
import Generated.Revparse.Safe qualified as RPS
import LibGit2.Error (checkStatusResult)
import LibGit2.Marshal (borrowedOid, handleIn, outHandle, textIn)
import LibGit2.Types (Object, Oid, Repository)

-- | @git_revparse_single@: resolve a revision spec (e.g. @HEAD@, @main~3@, a
-- sha) to an object.
revparseSingle :: Repository -> Text -> IO Object
revparseSingle repo spec =
    fst <$> toHighLevel
      ( output (outHandle OF.git_object_free)  -- git_object **out
      $ input  handleIn                        -- git_repository *repo
      $ input  textIn                          -- const char *spec
      $ checkStatusResult
      ) RPS.git_revparse_single repo spec

-- | @git_object_id@: the object's oid (borrowed; copied out).
objectId :: Object -> IO Oid
objectId = borrowedOid OS.git_object_id
