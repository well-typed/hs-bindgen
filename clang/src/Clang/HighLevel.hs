-- | High-level API to @libclang@
--
-- The functions in this module (intentionally) clash with the corresponding
-- function in "Clang.LowLevel": the hope is that by keeping the names of
-- corresponding functions the same, the API is easier to use. You may therefore
-- wish to import this module qualified. Typical usage:
--
-- > import Clang.HighLevel qualified as HighLevel
-- > import Clang.HighLevel.Types
--
-- The "Clang.HighLevel.Types" module avoids name clashes and is intended for
-- unqualified import.
module Clang.HighLevel (
    -- * Source locations
    -- ** Get single location
    clang_getExpansionLocation
  , clang_getPresumedLocation
  , clang_getSpellingLocation
  , clang_getFileLocation
    -- ** Convenience wrappers
    -- *** for @CXSourceLocation@
  , clang_getDiagnosticLocation
  , clang_getCursorLocation
  , clang_getCursorLocation'
  , clang_getTokenLocation
    -- *** for @CXSourceRange@
  , clang_getDiagnosticRange
  , clang_getDiagnosticFixIt
  , clang_Cursor_getSpellingNameRange
  , clang_getCursorExtent
  , clang_getTokenExtent
    -- * Tokens
  , clang_tokenize
    -- * Diagnostics
  , clang_getDiagnostics
    -- * Folds
  , clang_visitChildren
    -- * User-provided names
  , clang_getCursorSpelling
    -- * Declaration classification
  , classifyDeclaration
    -- * General wrappers
  , withIndex
  , withTranslationUnit
  , withTranslationUnit2
  , withUnsavedFile
  ) where

import Clang.HighLevel.Declaration
import Clang.HighLevel.Diagnostics
import Clang.HighLevel.Fold
import Clang.HighLevel.SourceLoc
import Clang.HighLevel.Tokens
import Clang.HighLevel.UserProvided
import Clang.HighLevel.Wrappers
