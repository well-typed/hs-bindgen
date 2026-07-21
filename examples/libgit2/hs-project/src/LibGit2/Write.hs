-- | The write path: initialise a repository, write a blob, build a tree, and
-- create a commit.
--
-- This is where the struct-/write/ combinators get exercised: 'commitCreate'
-- passes a 'Signature' to C through @asArgumentC sigMarshal@ (a 'MarshalStruct'
-- written into a @const git_signature *@). The fixed NULL/0 arguments are
-- supplied with 'fixed', and 'dropTrailingUnit' (or 'newHandle', which bakes it
-- in) removes the status closer's @()@.
--
module LibGit2.Write
  ( repositoryInit
  , blobCreateFromBuffer
  , treebuilderNew
  , treebuilderInsert
  , treebuilderWrite
  , treeLookup
  , commitCreate
  , regularFileMode
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Foreign.C.Types (CSize, CUInt)
import Foreign.Ptr (nullPtr)

import HsBindgen.Runtime.HighLevel (dropTrailingUnit, fixed, input, input2,
                                    output, toHighLevel)
import HsBindgen.Runtime.HighLevel.Defaults (defaultIn)
import HsBindgen.Runtime.HighLevel.Marshaller.Utils (unsafeByteStringLenIn)

import Generated.Blob.Safe qualified as B
import Generated.Commit.Safe qualified as C
import Generated.Repository.FunPtr qualified as RF
import Generated.Repository.Safe qualified as RS
import Generated.Tree.FunPtr qualified as TF
import Generated.Tree.Safe qualified as TS
import Generated.Types (Git_filemode_t, pattern GIT_FILEMODE_BLOB)
import LibGit2.Enums ()
import LibGit2.Error (checkStatusResult)
import LibGit2.Marshal (asArgumentC, handleIn, handleInC, newHandle, nullConst,
                        oidInC, oidOut, textIn)
import LibGit2.Signature (sigMarshal)
import LibGit2.Types (Oid, Repository, Signature, Tree, TreeBuilder)

-- | The filemode for a regular, non-executable file.
regularFileMode :: Git_filemode_t
regularFileMode = GIT_FILEMODE_BLOB

-- | @git_repository_init@ at @path@ (non-bare). Creates the directory.
repositoryInit :: Text -> IO Repository
repositoryInit path =
    newHandle RF.git_repository_free
              ( input textIn         -- const char *path
              . fixed (0 :: CUInt) )  -- unsigned is_bare = 0
              RS.git_repository_init path

-- | @git_blob_create_from_buffer@: write @content@ as a blob; returns its oid.
blobCreateFromBuffer :: Repository -> ByteString -> IO Oid
blobCreateFromBuffer repo content =
    toHighLevel
      ( dropTrailingUnit
      $ output oidOut      -- git_oid *id (out)
      $ input  handleIn              -- git_repository *repo
      $ input2 unsafeByteStringLenIn -- const void *buffer, size_t len
      $ checkStatusResult
      ) B.git_blob_create_from_buffer repo content

-- | @git_treebuilder_new@ with no source tree.
treebuilderNew :: Repository -> IO TreeBuilder
treebuilderNew repo =
    newHandle TF.git_treebuilder_free
              ( input handleIn
              . fixed nullConst )   -- const git_tree *source = NULL
              TS.git_treebuilder_new repo

-- | @git_treebuilder_insert@: add an entry (we discard the returned entry).
treebuilderInsert :: TreeBuilder -> Text -> Oid -> Git_filemode_t -> IO ()
treebuilderInsert =
    toHighLevel
      ( fixed  nullPtr      -- const git_tree_entry **out = NULL
      $ input  handleIn     -- git_treebuilder *bld
      $ input  textIn       -- const char *filename
      $ input  oidInC       -- const git_oid *id
      $ input  defaultIn    -- git_filemode_t filemode (DefaultIn, LibGit2.Enums)
      $ checkStatusResult
      ) TS.git_treebuilder_insert

-- | @git_treebuilder_write@: persist the tree; returns its oid.
treebuilderWrite :: TreeBuilder -> IO Oid
treebuilderWrite bld =
    toHighLevel
      ( dropTrailingUnit $ output oidOut $ input handleIn $ checkStatusResult )
      TS.git_treebuilder_write bld

-- | @git_tree_lookup@.
treeLookup :: Repository -> Oid -> IO Tree
treeLookup repo oid =
    newHandle TF.git_tree_free (input handleIn . input oidInC)
              TS.git_tree_lookup repo oid

-- | @git_commit_create@ with no parents. @updateRef@ (e.g. @\"HEAD\"@) is the
-- ref to move to the new commit.
commitCreate :: Repository -> Text -> Signature -> Signature -> Text -> Tree -> IO Oid
commitCreate repo updateRef author committer message tree =
    toHighLevel
      ( dropTrailingUnit
      $ output oidOut                    -- git_oid *id (out)
      $ input  handleIn                  -- git_repository *repo
      $ input  textIn                    -- const char *update_ref
      $ input  (asArgumentC sigMarshal)  -- const git_signature *author
      $ input  (asArgumentC sigMarshal)  -- const git_signature *committer
      $ fixed  nullConst                 -- const char *message_encoding = NULL
      $ input  textIn                    -- const char *message
      $ input  handleInC                 -- const git_tree *tree
      $ fixed  (0 :: CSize)              -- size_t parent_count = 0
      $ fixed  nullPtr                   -- parents = NULL
      $ checkStatusResult
      ) C.git_commit_create repo updateRef author committer message tree
