-- | Doxygen support
--
-- The routines in this group provide access to information in documentation
-- comments.
--
-- These facilities are distinct from the core and may be subject to their own
-- schedule of stability and deprecation.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html>
module Clang.LowLevel.Doxygen (
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
  , CXCommentInlineCommandRenderKind(..)
  , clang_InlineCommandComment_getCommandName
  , clang_InlineCommandComment_getRenderKind
  , clang_InlineCommandComment_getNumArgs
  , clang_InlineCommandComment_getArgText
    -- * Comment type 'CXComment_HTMLStartTag' and 'CXComment_HTMLEndTag'
  , clang_HTMLTagComment_getTagName
  , clang_HTMLStartTagComment_isSelfClosing
  , clang_HTMLStartTag_getNumAttrs
  , clang_HTMLStartTag_getAttrName
  , clang_HTMLStartTag_getAttrValue
  , clang_HTMLTagComment_getAsString
    -- * Comment type 'CXComment_BlockCommand'
  , clang_BlockCommandComment_getCommandName
  , clang_BlockCommandComment_getNumArgs
  , clang_BlockCommandComment_getArgText
  , clang_BlockCommandComment_getParagraph
    -- * Comment type 'CXComment_ParamCommand'
  , CXCommentParamPassDirection(..)
  , clang_ParamCommandComment_getParamName
  , clang_ParamCommandComment_isParamIndexValid
  , clang_ParamCommandComment_getParamIndex
  , clang_ParamCommandComment_isDirectionExplicit
  , clang_ParamCommandComment_getDirection
    -- * Comment type 'CXComment_TParamCommand'
  , clang_TParamCommandComment_getParamName
  , clang_TParamCommandComment_isParamPositionValid
  , clang_TParamCommandComment_getDepth
  , clang_TParamCommandComment_getIndex
    -- * Comment type 'CXComment_VerbatimBlockLine'
  , clang_VerbatimBlockLineComment_getText
    -- * Comment type 'CXComment_VerbatimLine'
  , clang_VerbatimLineComment_getText
    -- * Comment type 'CXComment_FullComment'
  , clang_FullComment_getAsHTML
  , clang_FullComment_getAsXML
  ) where

import Control.Monad.IO.Class
import Data.Text (Text)
import Foreign.C

import Clang.Enum.Simple
import Clang.Internal.ByValue
import Clang.Internal.CXString ()
import Clang.Internal.Results
import Clang.LowLevel.Core
import Clang.LowLevel.Core.Structs
import Clang.LowLevel.Doxygen.Enums
import Clang.LowLevel.Doxygen.Instances ()
import Clang.LowLevel.Doxygen.Structs

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
clang_Cursor_getParsedComment :: MonadIO m => CXCursor -> m CXComment
clang_Cursor_getParsedComment cursor = liftIO $
    onHaskellHeap cursor $ \cursor' ->
      preallocate_ $ wrap_Cursor_getParsedComment cursor'

-- | Get the type of an AST node of any kind
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gad7f2a27ab2f69abcb9442e05a21a130f>
clang_Comment_getKind ::
     MonadIO m
  => CXComment -- ^ AST node of any kind
  -> m (SimpleEnum CXCommentKind)
clang_Comment_getKind comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      wrap_Comment_getKind comment'

-- | Get the number of children of the AST node.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gaad4eba69493735a4db462bb4b5bed97a>
clang_Comment_getNumChildren ::
     MonadIO m
  => CXComment -- ^ AST node of any kind
  -> m CUInt
clang_Comment_getNumChildren comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      wrap_Comment_getNumChildren comment'

-- | Get the specified child of the AST node.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gad5567ecc26b083562e42b83170c105aa>
clang_Comment_getChild ::
     MonadIO m
  => CXComment -- ^ AST node of any kind
  -> CUInt     -- ^ child index (zero-based)
  -> m CXComment
clang_Comment_getChild comment childIdx = liftIO $
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
clang_Comment_isWhitespace :: MonadIO m => CXComment -> m Bool
clang_Comment_isWhitespace comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      cToBool <$> wrap_Comment_isWhitespace comment'

-- | Determine whether the comment is inline content and has a newline
-- immediately following it in the comment text.
--
-- Newlines between paragraphs do not count.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gacbc2924271ca86226c024e859e0a75c8>
clang_InlineContentComment_hasTrailingNewline ::
     MonadIO m
  => CXComment -> m Bool
clang_InlineContentComment_hasTrailingNewline comment = liftIO $
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
clang_TextComment_getText :: MonadIO m => CXComment -> m Text
clang_TextComment_getText comment = liftIO $
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
clang_InlineCommandComment_getCommandName :: MonadIO m => CXComment -> m Text
clang_InlineCommandComment_getCommandName comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_InlineCommandComment_getCommandName comment'

-- | Get the most appropriate rendering mode, chosen on command semantics in
-- Doxygen.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga3dd54ce1288d09c408cac8c887da2ebd>
clang_InlineCommandComment_getRenderKind ::
     MonadIO m
  => CXComment
  -> m (SimpleEnum CXCommentInlineCommandRenderKind)
clang_InlineCommandComment_getRenderKind comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      wrap_InlineCommandComment_getRenderKind comment'

-- | Get the number of command arguments.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga78db1049239be9649c2829cdeb83c544>
clang_InlineCommandComment_getNumArgs :: MonadIO m => CXComment -> m CUInt
clang_InlineCommandComment_getNumArgs comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      wrap_InlineCommandComment_getNumArgs comment'

-- | Get the text of the specified argument.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga6824f3cdcb42edbd143db77a657fe888>
clang_InlineCommandComment_getArgText ::
     MonadIO m
  => CXComment
  -> CUInt -- ^ argument index (zero-based)
  -> m Text
clang_InlineCommandComment_getArgText comment argIdx = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_InlineCommandComment_getArgText comment' argIdx

{-------------------------------------------------------------------------------
  Comment type 'CXComment_HTMLStartTag' and 'CXComment_HTMLEndTag'
-------------------------------------------------------------------------------}

foreign import capi unsafe "doxygen_wrappers.h wrap_HTMLTagComment_getTagName"
  wrap_HTMLTagComment_getTagName :: R CXComment_ -> W CXString_ -> IO ()

foreign import capi unsafe "doxygen_wrappers.h wrap_HTMLStartTagComment_isSelfClosing"
  wrap_HTMLStartTagComment_isSelfClosing :: R CXComment_ -> IO CUInt

foreign import capi unsafe "doxygen_wrappers.h wrap_HTMLStartTag_getNumAttrs"
  wrap_HTMLStartTag_getNumAttrs :: R CXComment_ -> IO CUInt

foreign import capi unsafe "doxygen_wrappers.h wrap_HTMLStartTag_getAttrName"
  wrap_HTMLStartTag_getAttrName :: R CXComment_ -> CUInt -> W CXString_ -> IO ()

foreign import capi unsafe "doxygen_wrappers.h wrap_HTMLStartTag_getAttrValue"
  wrap_HTMLStartTag_getAttrValue ::
       R CXComment_
    -> CUInt
    -> W CXString_
    -> IO ()

foreign import capi unsafe "doxygen_wrappers.h wrap_HTMLTagComment_getAsString"
  wrap_HTMLTagComment_getAsString :: R CXComment_ -> W CXString_ -> IO ()

-- | Get the HTML tag name.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga55b84483c67c0629260b1534d4b3f80e>
clang_HTMLTagComment_getTagName ::
     MonadIO m
  => CXComment
  -- ^ a 'CXComment_HTMLStartTag' or 'CXComment_HTMLEndTag' AST node
  -> m Text
clang_HTMLTagComment_getTagName comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_HTMLTagComment_getTagName comment'

-- | Determine whether the tag is self-closing.
--
-- Example: @<br />@ is self-closing
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga052be5f208a0ef2f76e3e9923a96ef19>
clang_HTMLStartTagComment_isSelfClosing ::
     MonadIO m
  => CXComment -- ^ a 'CXComment_HTMLStartTag' AST node
  -> m Bool
clang_HTMLStartTagComment_isSelfClosing comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      cToBool <$> wrap_HTMLStartTagComment_isSelfClosing comment'

-- | Get the number of attributes (name-value pairs) attached to the start tag.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gaffb8098debd5b99c2345840a5f0e63e0>
clang_HTMLStartTag_getNumAttrs ::
     MonadIO m
  => CXComment -- ^ a 'CXComment_HTMLStartTag' AST node
  -> m CUInt
clang_HTMLStartTag_getNumAttrs comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      wrap_HTMLStartTag_getNumAttrs comment'

-- | Get the name of the specified attribute.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga4bdf958af343477fc70eb2b4822cd006>
clang_HTMLStartTag_getAttrName ::
     MonadIO m
  => CXComment -- ^ a 'CXComment_HTMLStartTag' AST node
  -> CUInt     -- ^ attribute index (zero-based)
  -> m Text
clang_HTMLStartTag_getAttrName comment attrIdx = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_HTMLStartTag_getAttrName comment' attrIdx

-- | Get the value of the specified attribute.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gae674a07af38d28d67941c1c54909c5e8>
clang_HTMLStartTag_getAttrValue ::
     MonadIO m
  => CXComment -- ^ a 'CXComment_HTMLStartTag' AST node
  -> CUInt     -- ^ attribute index (zero-based)
  -> m Text
clang_HTMLStartTag_getAttrValue comment attrIdx = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_HTMLStartTag_getAttrValue comment' attrIdx

-- | Convert an HTML tag AST node to string.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga684a46f5993fe907016aba5dbe9d1d9e>
clang_HTMLTagComment_getAsString ::
     MonadIO m
  => CXComment
  -- ^ a 'CXComment_HTMLStartTag' or 'CXComment_HTMLEndTag' AST node
  -> m Text
clang_HTMLTagComment_getAsString comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_HTMLTagComment_getAsString comment'

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
clang_BlockCommandComment_getCommandName :: MonadIO m => CXComment -> m Text
clang_BlockCommandComment_getCommandName comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_BlockCommandComment_getCommandName comment'

-- | Get the number of word-like arguments.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gacb447968ce9efdfdabbfca8918540cdf>
clang_BlockCommandComment_getNumArgs :: MonadIO m => CXComment -> m CUInt
clang_BlockCommandComment_getNumArgs comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      wrap_BlockCommandComment_getNumArgs comment'

-- | Get the text of the specified word-like argument.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga9faf08601d88c809a9a97a9826051990>
clang_BlockCommandComment_getArgText ::
     MonadIO m
  => CXComment
  -> CUInt -- ^ argument index (zero-based)
  -> m Text
clang_BlockCommandComment_getArgText comment argIdx = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_BlockCommandComment_getArgText comment' argIdx

-- | Get the paragraph argument of the block command.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gac6f2ffc8fdbe9394bd4bb7d54327c968>
clang_BlockCommandComment_getParagraph ::
     MonadIO m
  => CXComment
  -- ^ a @CXComment_BlockCommand@ or @CXComment_VerbatimBlockCommand@ AST node
  -> m CXComment
clang_BlockCommandComment_getParagraph comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_BlockCommandComment_getParagraph comment'

{-------------------------------------------------------------------------------
  Comment type 'CXComment_ParamCommand'
-------------------------------------------------------------------------------}

foreign import capi unsafe "doxygen_wrappers.h wrap_ParamCommandComment_getParamName"
  wrap_ParamCommandComment_getParamName :: R CXComment_ -> W CXString_ -> IO ()

foreign import capi unsafe "doxygen_wrappers.h wrap_ParamCommandComment_isParamIndexValid"
  wrap_ParamCommandComment_isParamIndexValid :: R CXComment_ -> IO CUInt

foreign import capi unsafe "doxygen_wrappers.h wrap_ParamCommandComment_getParamIndex"
  wrap_ParamCommandComment_getParamIndex :: R CXComment_ -> IO CUInt

foreign import capi unsafe "doxygen_wrappers.h wrap_ParamCommandComment_isDirectionExplicit"
  wrap_ParamCommandComment_isDirectionExplicit :: R CXComment_ -> IO CUInt

foreign import capi unsafe "doxygen_wrappers.h wrap_ParamCommandComment_getDirection"
  wrap_ParamCommandComment_getDirection ::
       R CXComment_
    -> IO (SimpleEnum CXCommentParamPassDirection)

-- | Get the parameter name.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gaffd7aaf697c5eb3a3d2b508b5d806763>
clang_ParamCommandComment_getParamName :: MonadIO m => CXComment -> m Text
clang_ParamCommandComment_getParamName comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_ParamCommandComment_getParamName comment'

-- | Determine whether the parameter that this AST node represents was found in
-- the function prototype and @clang_ParamCommandComment_getParamIndex@ function
-- will return a meaningful value.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga92e6422da2a3e428b4452a3e8955ff76>
clang_ParamCommandComment_isParamIndexValid :: MonadIO m => CXComment -> m Bool
clang_ParamCommandComment_isParamIndexValid comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      cToBool <$> wrap_ParamCommandComment_isParamIndexValid comment'

-- | Get the zero-based parameter index in function prototype.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gad9d1dc9ebb52dcc9cb7da8ca4c23332a>
clang_ParamCommandComment_getParamIndex :: MonadIO m => CXComment -> m CUInt
clang_ParamCommandComment_getParamIndex comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      wrap_ParamCommandComment_getParamIndex comment'

-- | Determine whether the parameter passing direction was specified explicitly
-- in the comment.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gaf68f19e83ca9b27aec7eb22b065620bd>
clang_ParamCommandComment_isDirectionExplicit ::
     MonadIO m
  => CXComment -> m Bool
clang_ParamCommandComment_isDirectionExplicit comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      cToBool <$> wrap_ParamCommandComment_isDirectionExplicit comment'

-- | Get the parameter passing direction.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gac78b84734e9e6040a001a0036e6aa15c>
clang_ParamCommandComment_getDirection ::
     MonadIO m
  => CXComment
  -> m (SimpleEnum CXCommentParamPassDirection)
clang_ParamCommandComment_getDirection comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      wrap_ParamCommandComment_getDirection comment'

{-------------------------------------------------------------------------------
  Comment type 'CXComment_TParamCommand'
-------------------------------------------------------------------------------}

foreign import capi unsafe "doxygen_wrappers.h wrap_TParamCommandComment_getParamName"
  wrap_TParamCommandComment_getParamName :: R CXComment_ -> W CXString_ -> IO ()

foreign import capi unsafe "doxygen_wrappers.h wrap_TParamCommandComment_isParamPositionValid"
  wrap_TParamCommandComment_isParamPositionValid :: R CXComment_ -> IO CUInt

foreign import capi unsafe "doxygen_wrappers.h wrap_TParamCommandComment_getDepth"
  wrap_TParamCommandComment_getDepth :: R CXComment_ -> IO CUInt

foreign import capi unsafe "doxygen_wrappers.h wrap_TParamCommandComment_getIndex"
  wrap_TParamCommandComment_getIndex :: R CXComment_ -> CUInt -> IO CUInt

-- | Get the template parameter name.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga01f61f1d0dabcaf806eb1b9f21e5e340>
clang_TParamCommandComment_getParamName :: MonadIO m => CXComment -> m Text
clang_TParamCommandComment_getParamName comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_TParamCommandComment_getParamName comment'

-- | Determine whether the parameter that this AST node represents was found in
-- the template parameter list and @clang_TParamCommandComment_getDepth@ and
-- @clang_TParamCommandComment_getIndex@ functions will return a meaningful
-- value.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga1f6e7538a646824f3dde65d634de753f>
clang_TParamCommandComment_isParamPositionValid ::
     MonadIO m
  => CXComment -> m Bool
clang_TParamCommandComment_isParamPositionValid comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      cToBool <$> wrap_TParamCommandComment_isParamPositionValid comment'

-- | Get the zero-based nesting depth of this parameter in the template
-- parameter list.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga88371156eeeb768d0d14eb5630b7c726>
clang_TParamCommandComment_getDepth :: MonadIO m => CXComment -> m CUInt
clang_TParamCommandComment_getDepth comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      wrap_TParamCommandComment_getDepth comment'

-- | Get the zero-based parameter index in the template parameter list at a
-- given nesting depth.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga0b91d26f02a476076b6dc5b5eea59a8f>
clang_TParamCommandComment_getIndex ::
     MonadIO m
  => CXComment
  -> CUInt -- ^ depth
  -> m CUInt
clang_TParamCommandComment_getIndex comment depth = liftIO $
    onHaskellHeap comment $ \comment' ->
      wrap_TParamCommandComment_getIndex comment' depth

{-------------------------------------------------------------------------------
  Comment type 'CXComment_VerbatimBlockLine'
-------------------------------------------------------------------------------}

foreign import capi unsafe "doxygen_wrappers.h wrap_VerbatimBlockLineComment_getText"
  wrap_VerbatimBlockLineComment_getText :: R CXComment_ -> W CXString_ -> IO ()

-- | Get the text contained in the AST node.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga599fad38a1c52917a2458ac10412969f>
clang_VerbatimBlockLineComment_getText :: MonadIO m => CXComment -> m Text
clang_VerbatimBlockLineComment_getText comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_VerbatimBlockLineComment_getText comment'

{-------------------------------------------------------------------------------
  Comment type 'CXComment_VerbatimLine'
-------------------------------------------------------------------------------}

foreign import capi unsafe "doxygen_wrappers.h wrap_VerbatimLineComment_getText"
  wrap_VerbatimLineComment_getText :: R CXComment_ -> W CXString_ -> IO ()

-- | Get the text contained in the AST node.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#ga4eb1de9012b525f14051409427bd8eb2>
clang_VerbatimLineComment_getText :: MonadIO m => CXComment -> m Text
clang_VerbatimLineComment_getText comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_VerbatimLineComment_getText comment'

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
clang_FullComment_getAsHTML :: MonadIO m => CXComment -> m Text
clang_FullComment_getAsHTML comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_FullComment_getAsHTML comment'

-- | Convert a given full parsed comment to an XML document.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__COMMENT.html#gac877b07be05f591fdfea05f466ed9395>
clang_FullComment_getAsXML :: MonadIO m => CXComment -> m Text
clang_FullComment_getAsXML comment = liftIO $
    onHaskellHeap comment $ \comment' ->
      preallocate_ $ wrap_FullComment_getAsXML comment'
