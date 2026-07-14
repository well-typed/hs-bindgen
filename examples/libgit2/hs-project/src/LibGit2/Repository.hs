-- | High-level wrappers for opening a repository and reading its HEAD.
--
-- @repositoryOpen@ and @repositoryHead@ are the canonical constructor shape:
-- 'newHandle' with the handle's @git_X_free@ and the input chain, then @fst@ to
-- drop the @()@ the status check leaves beside the handle.
--
module LibGit2.Repository
  ( repositoryOpen
  , repositoryHead
  , repositoryPath
  ) where

import Data.Text (Text)

import HsBindgen.Runtime.HighLevel (input)

import Generated.Refs.FunPtr qualified as RefF
import Generated.Repository.FunPtr qualified as RF
import Generated.Repository.Safe qualified as RS
import LibGit2.Marshal (borrowedText, handleIn, newHandle, textIn)
import LibGit2.Types (Reference, Repository)

-- | @git_repository_open@: open the repository at @path@.
repositoryOpen :: Text -> IO Repository
repositoryOpen path =
    fst <$> newHandle RF.git_repository_free (input textIn) RS.git_repository_open path

-- | @git_repository_head@: the resolved @HEAD@ reference.
repositoryHead :: Repository -> IO Reference
repositoryHead repo =
    fst <$> newHandle RefF.git_reference_free (input handleIn) RS.git_repository_head repo

-- | @git_repository_path@: the path to the @.git@ directory (a borrowed string).
repositoryPath :: Repository -> IO Text
repositoryPath = borrowedText RS.git_repository_path
