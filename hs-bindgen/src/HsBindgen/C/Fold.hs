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
    -- * Logging
  , Skipped
  ) where

import HsBindgen.C.Fold.Common
import HsBindgen.C.Fold.Decl
import HsBindgen.Clang.Util.Fold
