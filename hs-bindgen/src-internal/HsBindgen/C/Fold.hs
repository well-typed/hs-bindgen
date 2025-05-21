-- | Folds over the @clang@ AST
--
-- Intended for unqualified import.
module HsBindgen.C.Fold (
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
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.Decl
