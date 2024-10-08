-- | Doxygen support
--
-- The routines in this group provide access to information in documentation
-- comments.
--
-- These facilities are distinct from the core and may be subject to their own
-- schedule of stability and deprecation.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html>
module HsBindgen.Clang.LowLevel.Doxygen (
    -- * Top-level
    CXComment
  , CXCommentKind(..)
  , clang_Cursor_getParsedComment
  , clang_Comment_getKind
  , clang_Comment_getNumChildren
  , clang_Comment_getChild
  , clang_Comment_isWhitespace
  , clang_InlineContentComment_hasTrailingNewline
    -- * Comment type 'CXComment_Text'
  , clang_TextComment_getText
    -- * Comment type 'CXComment_InlineCommand'
  , clang_InlineCommandComment_getCommandName
  , clang_InlineCommandComment_getRenderKind
  , clang_InlineCommandComment_getNumArgs
  , clang_InlineCommandComment_getArgText
    -- * Comment type 'CXComment_BlockCommand'
  , clang_BlockCommandComment_getCommandName
  , clang_BlockCommandComment_getNumArgs
  , clang_BlockCommandComment_getArgText
  , clang_BlockCommandComment_getParagraph
    -- * Comment type 'CXComment_FullComment'
  , clang_FullComment_getAsHTML
  , clang_FullComment_getAsXML
  ) where

import Data.Text (Text)
import Foreign.C

import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.LowLevel.Core.Structs
import HsBindgen.Clang.LowLevel.Doxygen.Enums
import HsBindgen.Clang.LowLevel.Doxygen.Instances ()
import HsBindgen.Clang.LowLevel.Doxygen.Structs
import HsBindgen.Clang.Internal.ByValue
import HsBindgen.Clang.Internal.CXString ()
import HsBindgen.Clang.Internal.Results
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

foreign import capi unsafe "doxygen_wrappers.h wrap_Comment_getNumChildren"
  wrap_Comment_getNumChildren :: R CXComment_ -> IO CUInt

foreign import capi unsafe "doxygen_wrappers.h wrap_Comment_getChild"
  wrap_Comment_getChild :: R CXComment_ -> CUInt -> W CXComment_ -> IO ()

foreign import capi unsafe "doxygen_wrappers.h wrap_Comment_isWhitespace"
  wrap_Comment_isWhitespace :: R CXComment_ -> IO CUInt

foreign import capi unsafe "doxygen_wrappers.h wrap_InlineContentComment_hasTrailingNewline"
  wrap_InlineContentComment_hasTrailingNewline :: R CXComment_ -> IO CUInt

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
clang_Comment_getKind ::
     CXComment -- ^ AST node of any kind
  -> IO (SimpleEnum CXCommentKind)
clang_Comment_getKind comment =
    onHaskellHeap comment $ \comment' ->
      wrap_Comment_getKind comment'

-- | Get the number of children of the AST node.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gaad4eba69493735a4db462bb4b5bed97a>
clang_Comment_getNumChildren ::
     CXComment -- ^ AST node of any kind
  -> IO CUInt
clang_Comment_getNumChildren comment =
    onHaskellHeap comment $ \comment' ->
      wrap_Comment_getNumChildren comment'

-- | Get the specified child of the AST node.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gad5567ecc26b083562e42b83170c105aa>
clang_Comment_getChild ::
     CXComment -- ^ AST node of any kind
  -> CUInt     -- ^ child index (zero-based)
  -> IO CXComment
clang_Comment_getChild comment childIdx =
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_Comment_getChild comment' childIdx

-- | Determine whether the comment is considered whitespace.
--
-- A @CXComment_Paragraph@ node is considered whitespace if it contains only
-- @CXComment_Text@ nodes that are empty or whitespace.
--
-- Other AST nodes (except @CXComment_Paragraph@ and @CXComment_Text@) are
-- never considered whitespace.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga1193c1dc798aecad92cb30cea78bf71e>
clang_Comment_isWhitespace :: CXComment -> IO Bool
clang_Comment_isWhitespace comment =
    onHaskellHeap comment $ \comment' ->
      cToBool <$> wrap_Comment_isWhitespace comment'

-- | Determine whether the comment is inline content and has a newline
-- immediately following it in the comment text.
--
-- Newlines between paragraphs do not count.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gacbc2924271ca86226c024e859e0a75c8>
clang_InlineContentComment_hasTrailingNewline :: CXComment -> IO Bool
clang_InlineContentComment_hasTrailingNewline comment =
    onHaskellHeap comment $ \comment' ->
      cToBool <$> wrap_InlineContentComment_hasTrailingNewline comment'

{-------------------------------------------------------------------------------
  Comment type 'CXComment_Text'
-------------------------------------------------------------------------------}

foreign import capi unsafe "doxygen_wrappers.h wrap_TextComment_getText"
  wrap_TextComment_getText :: R CXComment_ -> W CXString_ -> IO ()

-- | Get the text contained in the AST node.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gae9a27e851356181beac36bbff6e638e2>
clang_TextComment_getText :: CXComment -> IO Text
clang_TextComment_getText comment =
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_TextComment_getText comment'

{-------------------------------------------------------------------------------
  Comment type 'CXComment_InlineCommand'
-------------------------------------------------------------------------------}

foreign import capi unsafe "doxygen_wrappers.h wrap_InlineCommandComment_getCommandName"
  wrap_InlineCommandComment_getCommandName ::
       R CXComment_
    -> W CXString_
    -> IO ()

foreign import capi unsafe "doxygen_wrappers.h wrap_InlineCommandComment_getRenderKind"
  wrap_InlineCommandComment_getRenderKind ::
       R CXComment_
    -> IO (SimpleEnum CXCommentInlineCommandRenderKind)

foreign import capi unsafe "doxygen_wrappers.h wrap_InlineCommandComment_getNumArgs"
  wrap_InlineCommandComment_getNumArgs :: R CXComment_ -> IO CUInt

foreign import capi unsafe "doxygen_wrappers.h wrap_InlineCommandComment_getArgText"
  wrap_InlineCommandComment_getArgText ::
       R CXComment_
    -> CUInt
    -> W CXString_
    -> IO ()

-- | Get the name of the inline command.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga77f5b160e7d73190ac518298c1e79d05>
clang_InlineCommandComment_getCommandName :: CXComment -> IO Text
clang_InlineCommandComment_getCommandName comment =
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_InlineCommandComment_getCommandName comment'

-- | Get the most appropriate rendering mode, chosen on command semantics in
-- Doxygen.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga3dd54ce1288d09c408cac8c887da2ebd>
clang_InlineCommandComment_getRenderKind ::
     CXComment
  -> IO (SimpleEnum CXCommentInlineCommandRenderKind)
clang_InlineCommandComment_getRenderKind comment =
    onHaskellHeap comment $ \comment' ->
      wrap_InlineCommandComment_getRenderKind comment'

-- | Get the number of command arguments.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga78db1049239be9649c2829cdeb83c544>
clang_InlineCommandComment_getNumArgs :: CXComment -> IO CUInt
clang_InlineCommandComment_getNumArgs comment =
    onHaskellHeap comment $ \comment' ->
      wrap_InlineCommandComment_getNumArgs comment'

-- | Get the text of the specified argument.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga6824f3cdcb42edbd143db77a657fe888>
clang_InlineCommandComment_getArgText ::
     CXComment
  -> CUInt -- ^ argument index (zero-based)
  -> IO Text
clang_InlineCommandComment_getArgText comment argIdx =
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_InlineCommandComment_getArgText comment' argIdx

{-------------------------------------------------------------------------------
  Comment type 'CXComment_BlockCommand'
-------------------------------------------------------------------------------}

foreign import capi unsafe "doxygen_wrappers.h wrap_BlockCommandComment_getCommandName"
  wrap_BlockCommandComment_getCommandName ::
       R CXComment_
    -> W CXString_
    -> IO ()

foreign import capi unsafe "doxygen_wrappers.h wrap_BlockCommandComment_getNumArgs"
  wrap_BlockCommandComment_getNumArgs :: R CXComment_ -> IO CUInt

foreign import capi unsafe "doxygen_wrappers.h wrap_BlockCommandComment_getArgText"
  wrap_BlockCommandComment_getArgText ::
       R CXComment_
    -> CUInt
    -> W CXString_
    -> IO ()

foreign import capi unsafe "doxygen_wrappers.h wrap_BlockCommandComment_getParagraph"
  wrap_BlockCommandComment_getParagraph :: R CXComment_ -> W CXComment_ -> IO ()

-- | Get the name of the block command.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga8fdde998537370477362a4f84bc03420>
clang_BlockCommandComment_getCommandName :: CXComment -> IO Text
clang_BlockCommandComment_getCommandName comment =
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_BlockCommandComment_getCommandName comment'

-- | Get the number of word-like arguments.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gacb447968ce9efdfdabbfca8918540cdf>
clang_BlockCommandComment_getNumArgs :: CXComment -> IO CUInt
clang_BlockCommandComment_getNumArgs comment =
    onHaskellHeap comment $ \comment' ->
      wrap_BlockCommandComment_getNumArgs comment'

-- | Get the text of the specified word-like argument.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga9faf08601d88c809a9a97a9826051990>
clang_BlockCommandComment_getArgText ::
     CXComment
  -> CUInt -- ^ argument index (zero-based)
  -> IO Text
clang_BlockCommandComment_getArgText comment argIdx =
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_BlockCommandComment_getArgText comment' argIdx

-- | Get the paragraph argument of the block command.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gac6f2ffc8fdbe9394bd4bb7d54327c968>
clang_BlockCommandComment_getParagraph ::
     CXComment
  -- ^ a @CXComment_BlockCommand@ or @CXComment_VerbatimBlockCommand@ AST node
  -> IO CXComment
clang_BlockCommandComment_getParagraph comment =
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_BlockCommandComment_getParagraph comment'

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
