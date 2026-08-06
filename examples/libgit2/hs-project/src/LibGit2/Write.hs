-- | The write path: initialise a repository, write a blob, build a tree, and
-- create a commit.
--
-- 'commitCreate' writes a 'Signature' into a @const git_signature *@ through
-- @asArgumentC sigMarshal@; the parent list, message encoding and other arguments
-- this binding does not expose are pinned to NULL or 0.
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
import Foreign.Ptr (nullPtr)

import HsBindgen.HighLevel (asArgumentC, fixed, input, input2, output,
                            toHighLevel)
import HsBindgen.HighLevel.Defaults (defaultIn)
import HsBindgen.HighLevel.Marshaller.Utils (nullConst, unsafeByteStringLenIn)

import Generated.Blob.Safe qualified as B
import Generated.Commit.Safe qualified as C
import Generated.Repository.FunPtr qualified as RF
import Generated.Repository.Safe qualified as RS
import Generated.Tree.FunPtr qualified as TF
import Generated.Tree.Safe qualified as TS
import Generated.Types (Git_filemode_t, pattern GIT_FILEMODE_BLOB)
import LibGit2.Defaults ()
import LibGit2.Error (checkedStatus)
import LibGit2.Marshal (handleIn, handleInC, newHandle, oidInC, oidOut, textIn)
import LibGit2.Signature (sigMarshal)
import LibGit2.Types (Oid, Repository, Signature, Tree, TreeBuilder)

-- | The filemode for a regular, non-executable file.
regularFileMode :: Git_filemode_t
regularFileMode = GIT_FILEMODE_BLOB

-- | @git_repository_init@ at @path@ (non-bare). Creates the directory.
repositoryInit :: Text -> IO Repository
repositoryInit =
    newHandle RF.git_repository_free
              ( input textIn -- const char *path
              . fixed 0      -- unsigned is_bare = 0
              )
              RS.git_repository_init

-- | @git_blob_create_from_buffer@: write @content@ as a blob; returns its oid.
blobCreateFromBuffer :: Repository -> ByteString -> IO Oid
blobCreateFromBuffer = toHighLevel B.git_blob_create_from_buffer
                     $ output oidOut                -- git_oid *id (out)
                     $ input  handleIn              -- git_repository *repo
                     $ input2 unsafeByteStringLenIn -- const void *buffer, size_t len
                     $ checkedStatus

-- | @git_treebuilder_new@ with no source tree.
treebuilderNew :: Repository -> IO TreeBuilder
treebuilderNew =
    newHandle TF.git_treebuilder_free
              ( input handleIn  -- git_repository *repo
              . fixed nullConst -- const git_tree *source = NULL
              )
              TS.git_treebuilder_new

-- | @git_treebuilder_insert@: add an entry (we discard the returned entry).
treebuilderInsert :: TreeBuilder -> Text -> Oid -> Git_filemode_t -> IO ()
treebuilderInsert = toHighLevel TS.git_treebuilder_insert
                  $ fixed  nullPtr   -- const git_tree_entry **out = NULL
                  $ input  handleIn  -- git_treebuilder *bld
                  $ input  textIn    -- const char *filename
                  $ input  oidInC    -- const git_oid *id
                  $ input  defaultIn -- git_filemode_t filemode (DefaultIn, LibGit2.Defaults)
                  $ checkedStatus

-- | @git_treebuilder_write@: persist the tree; returns its oid.
treebuilderWrite :: TreeBuilder -> IO Oid
treebuilderWrite = toHighLevel TS.git_treebuilder_write
                 $ output oidOut   -- git_oid *id (out)
                 $ input  handleIn -- git_treebuilder *bld
                 $ checkedStatus

-- | @git_tree_lookup@.
treeLookup :: Repository -> Oid -> IO Tree
treeLookup =
    newHandle TF.git_tree_free
              ( input handleIn
              . input oidInC
              ) TS.git_tree_lookup

-- | @git_commit_create@ with no parents. @updateRef@ (e.g. @\"HEAD\"@) is the
-- ref to move to the new commit.
commitCreate :: Repository -> Text -> Signature -> Signature -> Text -> Tree -> IO Oid
commitCreate = toHighLevel C.git_commit_create
             $ output oidOut                   -- git_oid *id (out)
             $ input  handleIn                 -- git_repository *repo
             $ input  textIn                   -- const char *update_ref
             $ input  (asArgumentC sigMarshal) -- const git_signature *author
             $ input  (asArgumentC sigMarshal) -- const git_signature *committer
             $ fixed  nullConst                -- const char *message_encoding = NULL
             $ input  textIn                   -- const char *message
             $ input  handleInC                -- const git_tree *tree
             $ fixed  0                        -- size_t parent_count = 0
             $ fixed  nullPtr                  -- const git_commit **parents = NULL
             $ checkedStatus
