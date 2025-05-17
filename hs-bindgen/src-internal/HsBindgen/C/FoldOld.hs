-- | Folds over the @clang@ AST
--
-- Intended for unqualified import.
module HsBindgen.C.FoldOld (
    -- * Executing folds (re-exports from @clang@)
    Fold
  , runFoldIdentity
  , runFoldReader
  , runFoldState
    -- * Specific folds
    -- ** AST
  , DeclState
  , initDeclState
  , foldDecls
    -- * Logging
  , Skipped
  ) where

import Clang.HighLevel.Types
import HsBindgen.Eff
import HsBindgen.C.FoldOld.Common
import HsBindgen.C.FoldOld.Decl
