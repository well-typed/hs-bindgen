{-# LANGUAGE CApiFFI #-}

-- | Doxygen support
--
-- The routines in this group provide access to information in documentation
-- comments.
--
-- These facilities are distinct from the core and may be subject to their own
-- schedule of stability and deprecation.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html>
module HsBindgen.Clang.Doxygen (
    -- * Top-level
    CXComment
  , CXCommentKind(..)
  , clang_Cursor_getParsedComment
  , clang_Comment_getKind
    -- * Comment type 'CXComment_FullComment'
  , clang_FullComment_getAsHTML
  , clang_FullComment_getAsXML
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Foreign
import Foreign.C

import HsBindgen.Clang.Core
import HsBindgen.Clang.Doxygen.Enums
import HsBindgen.Clang.Doxygen.Instances ()
import HsBindgen.Clang.Util.Bindings
import HsBindgen.Clang.Util.CXString
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | A parsed comment.
data CXComment

foreign import capi unsafe "doxygen_wrappers.h wrap_malloc_Cursor_getParsedComment"
  clang_Cursor_getParsedComment' ::
       Ptr CXCursor
    -> IO (Ptr CXComment)

-- | Given a cursor that represents a documentable entity (e.g., declaration),
-- return the associated parsed comment as a 'CXComment_FullComment' AST node.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gab4f95ae3b2e0bd63b10cecc3727a391e>
clang_Cursor_getParsedComment ::
     ForeignPtr CXCursor
  -> IO (ForeignPtr CXComment)
clang_Cursor_getParsedComment cursor =
    withForeignPtr cursor $ \cursor' -> attachFinalizer =<<
      clang_Cursor_getParsedComment' cursor'

foreign import capi unsafe "doxygen_wrappers.h wrap_Comment_getKind"
  clang_Comment_getKind' ::
       Ptr CXComment
    -> IO (SimpleEnum CXCommentKind)

-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gad7f2a27ab2f69abcb9442e05a21a130f>
clang_Comment_getKind ::
     ForeignPtr CXComment
     -- ^ AST node of any kind.
  -> IO (SimpleEnum CXCommentKind)
     -- ^ the type of the AST node.
clang_Comment_getKind comment =
    withForeignPtr comment $ \comment' ->
      clang_Comment_getKind' comment'

{-------------------------------------------------------------------------------
  Comment type 'CXComment_FullComment'
-------------------------------------------------------------------------------}

foreign import capi unsafe "doxygen_wrappers.h wrap_malloc_FullComment_getAsHTML"
  clang_FullComment_getAsHTML' ::
       Ptr CXComment
    -> IO (Ptr CXString)

-- | Convert a given full parsed comment to an HTML fragment.
clang_FullComment_getAsHTML ::
     ForeignPtr CXComment
  -> IO (Strict.ByteString)
clang_FullComment_getAsHTML comment =
    withForeignPtr comment $ \comment' -> packCXString =<<
      clang_FullComment_getAsHTML' comment'

foreign import capi unsafe "doxygen_wrappers.h wrap_malloc_FullComment_getAsXML"
  clang_FullComment_getAsXML' ::
       Ptr CXComment
    -> IO (Ptr CXString)

-- | Convert a given full parsed comment to an XML document.
clang_FullComment_getAsXML ::
     ForeignPtr CXComment
  -> IO (Strict.ByteString)
clang_FullComment_getAsXML comment =
    withForeignPtr comment $ \comment' -> packCXString =<<
      clang_FullComment_getAsXML' comment'
