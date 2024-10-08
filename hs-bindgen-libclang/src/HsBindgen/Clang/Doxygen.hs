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

import Data.Text (Text)
import Foreign.C

import HsBindgen.Clang.Core
import HsBindgen.Clang.Core.Structs
import HsBindgen.Clang.Doxygen.Enums
import HsBindgen.Clang.Doxygen.Instances ()
import HsBindgen.Clang.Doxygen.Structs
import HsBindgen.Clang.Internal.ByValue
import HsBindgen.Clang.Internal.CXString ()
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | A parsed comment.
newtype CXComment = CXComment (OnHaskellHeap CXComment_)
  deriving newtype (LivesOnHaskellHeap, Preallocate)

foreign import capi unsafe "doxygen_wrappers.h wrap_Cursor_getParsedComment"
  wrap_Cursor_getParsedComment :: R CXCursor_ -> W CXComment_ -> IO ()

foreign import capi unsafe "doxygen_wrappers.h wrap_Comment_getKind"
  wrap_Comment_getKind :: R CXComment_ -> IO (SimpleEnum CXCommentKind)

-- | Given a cursor that represents a documentable entity (e.g., declaration),
-- return the associated parsed comment as a 'CXComment_FullComment' AST node.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gab4f95ae3b2e0bd63b10cecc3727a391e>
clang_Cursor_getParsedComment :: CXCursor -> IO CXComment
clang_Cursor_getParsedComment cursor =
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_Cursor_getParsedComment cursor'

-- | Get the type of an AST node of any kind
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gad7f2a27ab2f69abcb9442e05a21a130f>
clang_Comment_getKind :: CXComment -> IO (SimpleEnum CXCommentKind)
clang_Comment_getKind comment =
    onHaskellHeap comment $ \comment' ->
      wrap_Comment_getKind comment'

{-------------------------------------------------------------------------------
  Comment type 'CXComment_FullComment'
-------------------------------------------------------------------------------}

foreign import capi unsafe "doxygen_wrappers.h wrap_FullComment_getAsHTML"
  wrap_FullComment_getAsHTML :: R CXComment_ -> W CXString_ -> IO ()

foreign import capi unsafe "doxygen_wrappers.h wrap_FullComment_getAsXML"
  wrap_FullComment_getAsXML :: R CXComment_ -> W CXString_ -> IO ()

-- | Convert a given full parsed comment to an HTML fragment.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gafdfc03bbfdddd06c380a2644f16ccba9>
clang_FullComment_getAsHTML :: CXComment -> IO Text
clang_FullComment_getAsHTML comment =
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_FullComment_getAsHTML comment'

-- | Convert a given full parsed comment to an XML document.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gac877b07be05f591fdfea05f466ed9395>
clang_FullComment_getAsXML :: CXComment -> IO Text
clang_FullComment_getAsXML comment =
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_FullComment_getAsXML comment'
