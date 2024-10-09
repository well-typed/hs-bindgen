-- | Folds over the @clang@ AST
--
-- Intended for unqualified import.
module HsBindgen.C.Fold (
    -- * Executing folds (re-exports from @hs-bindgen-libclang@)
    Fold
  , runFoldIdentity
  , runFoldReader
  , runFoldState
    -- * Specific folds
    -- ** AST
  , DeclState
  , initDeclState
  , foldDecls
    -- ** Development tools
  , Element(..)
  , foldRaw
  , Comment(..)
  , foldComments
    -- * Logging
  , Skipped
  ) where

import HsBindgen.C.Fold.Comments
import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.Decl
import HsBindgen.C.Fold.Raw
import HsBindgen.Clang.Util.Fold
