-- | High-level wrappers for opening a repository and reading its HEAD.
--
-- @repositoryOpen@ and @repositoryHead@ are the canonical constructor shape:
-- @output (outHandle free)@ for the @git_X **out@ slot, @input@ for the rest,
-- closed by 'checkStatusResult', then @fst@ to drop the @()@ the status closer
-- leaves beside the handle.
--
module LibGit2.Repository
  ( repositoryOpen
  , repositoryHead
  , repositoryPath
  ) where

import Data.Text (Text)

import HsBindgen.Runtime.HighLevel (input, output, toHighLevel)

import Generated.Refs.FunPtr qualified as RefF
import Generated.Repository.FunPtr qualified as RF
import Generated.Repository.Safe qualified as RS
import LibGit2.Error (checkStatusResult)
import LibGit2.Marshal (borrowedText, handleIn, outHandle, textIn)
import LibGit2.Types (Reference, Repository)

-- | @git_repository_open@: open the repository at @path@.
repositoryOpen :: Text -> IO Repository
repositoryOpen path =
    fst <$> toHighLevel
      ( output (outHandle RF.git_repository_free)  -- git_repository **out
      $ input  textIn                              -- const char *path
      $ checkStatusResult                          -- int status
      ) RS.git_repository_open path

-- | @git_repository_head@: the resolved @HEAD@ reference.
repositoryHead :: Repository -> IO Reference
repositoryHead repo =
    fst <$> toHighLevel
      ( output (outHandle RefF.git_reference_free) -- git_reference **out
      $ input  handleIn                            -- git_repository *repo
      $ checkStatusResult
      ) RS.git_repository_head repo

-- | @git_repository_path@: the path to the @.git@ directory (a borrowed string).
repositoryPath :: Repository -> IO Text
repositoryPath = borrowedText RS.git_repository_path
