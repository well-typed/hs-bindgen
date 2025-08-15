{-# LANGUAGE RecordWildCards #-}

module Clang.HighLevel.Documentation (
    -- * Definition
    Comment(..)
  , CommentBlockContent(..)
  , CommentInlineContent(..)
  , CXCommentInlineCommandRenderKind(..)
  , CXCommentParamPassDirection(..)
    -- * Top-Level
  , clang_getComment
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Either
import Data.Text (Text)
import Data.Text qualified as Text

import Clang.Enum.Simple
import Clang.LowLevel.Core
import Clang.LowLevel.Doxygen

import GHC.Generics (Generic)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Reified Clang comment
--
-- This type corresponds to a @CXComment_FullComment@ comment.
--
-- The @CXComment_Null@ kind is not represented by this type.
-- @'Maybe' 'Comment'@ is used instead, where a @CXComment_Null@ comment is
-- represented by 'Nothing'.
data Comment = Comment {
      -- | Clang display name of the cursor
      --
      -- This is included so the original C name can be included in the
      -- translated documentation.
      commentCName :: Text

      -- | Children of a the comment
    , commentChildren :: [CommentBlockContent]
    }
  deriving stock (Show, Eq, Generic)

-- | Reified Clang comment block content
data CommentBlockContent =
      Paragraph {
        paragraphContent :: [CommentInlineContent]
      }
    | BlockCommand {
        blockCommandName :: Text
      , blockCommandArgs :: [Text]
      , blockCommandParagraph :: [CommentInlineContent]
      }
    | ParamCommand {
        paramCommandName                :: Text
      , paramCommandIndex               :: Maybe Int
      , paramCommandDirection           :: Maybe CXCommentParamPassDirection
      , paramCommandIsDirectionExplicit :: Bool
      , paramCommandContent             :: [CommentBlockContent]
      }
    | TParamCommand {
        tParamCommandName     :: Text
      , tParamCommandPosition :: Maybe [(Int, Int)]
      , tParamCommandContent  :: [CommentBlockContent]
      }
    | VerbatimBlockCommand {
        verbatimBlockLines :: [Text]
      }
    | VerbatimLine {
        verbatimLine :: Text
      }
  deriving stock (Show, Eq, Generic)

-- | Reified Clang comment inline content
data CommentInlineContent =
      TextContent {
        textContent :: Text
      }
    | InlineCommand {
        inlineCommandName       :: Text
      , inlineCommandRenderKind :: CXCommentInlineCommandRenderKind
      , inlineCommandArgs       :: [Text]
      }
    | HtmlStartTag {
        htmlStartTagName          :: Text
      , htmlStartTagIsSelfClosing :: Bool
      , htmlStartTagAttributes    :: [(Text, Text)]
      }
    | HtmlEndTag {
        htmlEndTagName :: Text
      }
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Reify the Clang comment for a cursor to the Haskell type
--
-- An error is thrown when an unexpected comment kind is encountered, such as
-- block content within inline content.
clang_getComment :: MonadIO m => CXCursor -> m (Maybe Comment)
clang_getComment cursor = do
    comment <- clang_Cursor_getParsedComment cursor
    eCommentKind <- fromSimpleEnum <$> clang_Comment_getKind comment
    case eCommentKind of
      Right CXComment_Null -> pure Nothing
      Right CXComment_FullComment -> do
        commentCName <- clang_getCursorDisplayName cursor
        commentChildren <- getChildren (getBlockContent cursor) comment
        pure $ Just Comment{..}
      Right commentKind ->
        errorWithContext cursor $ "root comment of kind " ++ show commentKind
      Left n ->
        errorWithContext cursor $ "root comment with invalid kind " ++ show n

-- | Reify block content
--
-- An error is thrown when an unexpected comment kind is encountered.
getBlockContent ::
     MonadIO m
  => CXCursor -- ^ cursor to provide context in error messages
  -> CXComment
  -> m CommentBlockContent
getBlockContent cursor comment = do
    eCommentKind <- fromSimpleEnum <$> clang_Comment_getKind comment
    case eCommentKind of
      Right CXComment_Paragraph -> do
        paragraphContent <- getChildren (getInlineContent cursor) comment
        pure Paragraph{..}

      Right CXComment_BlockCommand -> do
        blockCommandName <- Text.strip <$> clang_BlockCommandComment_getCommandName comment
        idxs <- getIdxs <$> clang_BlockCommandComment_getNumArgs comment
        blockCommandArgs <-
              fmap Text.strip
          <$> mapM (clang_BlockCommandComment_getArgText comment) idxs
        blockCommandParagraph <- getChildren (getInlineContent cursor)
          =<< clang_BlockCommandComment_getParagraph comment
        pure BlockCommand{..}

      Right CXComment_ParamCommand -> do
        paramCommandName <- Text.strip <$> clang_ParamCommandComment_getParamName comment
        paramCommandIndex <- do
          isValid <- clang_ParamCommandComment_isParamIndexValid comment
          if isValid
            then
              Just . fromIntegral
                <$> clang_ParamCommandComment_getParamIndex comment
            else pure Nothing
        paramCommandDirection <-
          either (const Nothing) Just . fromSimpleEnum
            <$> clang_ParamCommandComment_getDirection comment
        paramCommandIsDirectionExplicit <-
          clang_ParamCommandComment_isDirectionExplicit comment
        paramCommandContent <- getChildren (getBlockContent cursor) comment
        pure ParamCommand{..}

      Right CXComment_TParamCommand -> do
        tParamCommandName <- Text.strip <$> clang_TParamCommandComment_getParamName comment
        tParamCommandPosition <- do
          isValid <- clang_TParamCommandComment_isParamPositionValid comment
          if isValid
            then fmap Just $ do
              depth <- clang_TParamCommandComment_getDepth comment
              forM [0 .. depth] $ \d ->
                (fromIntegral d,) . fromIntegral
                  <$> clang_TParamCommandComment_getIndex comment d
            else pure Nothing
        tParamCommandContent <- getChildren (getBlockContent cursor) comment
        pure TParamCommand{..}

      Right CXComment_VerbatimBlockCommand -> do
        verbatimBlockLines <- fmap Text.strip
                          <$> getChildren (getVerbatimBlockLine cursor) comment
        pure VerbatimBlockCommand{..}

      Right CXComment_VerbatimLine -> do
        -- rest of line after misused command becomes a verbatim line
        verbatimLine <- Text.strip
                    <$> clang_VerbatimLineComment_getText comment
        pure VerbatimLine{..}

      Right commentKind -> errorWithContext cursor $
        "child comment of non-block kind " ++ show commentKind

      Left n ->
        errorWithContext cursor $ "child comment with invalid kind " ++ show n

-- | Reify inline content
--
-- An error is thrown when an unexpected comment kind is encountered.
getInlineContent ::
     MonadIO m
  => CXCursor -- ^ cursor to provide context in error messages
  -> CXComment
  -> m CommentInlineContent
getInlineContent cursor comment = do
    eCommentKind <- fromSimpleEnum <$> clang_Comment_getKind comment
    case eCommentKind of
      Right CXComment_Text -> do
        textContent <- Text.strip <$> clang_TextComment_getText comment
        pure TextContent{..}

      Right CXComment_InlineCommand -> do
        inlineCommandName <- Text.strip <$> clang_InlineCommandComment_getCommandName comment
        inlineCommandRenderKind <-
          fromRight CXCommentInlineCommandRenderKind_Normal . fromSimpleEnum
            <$> clang_InlineCommandComment_getRenderKind comment
        idxs <- getIdxs <$> clang_InlineCommandComment_getNumArgs comment
        inlineCommandArgs <-
              fmap Text.strip
          <$> mapM (clang_InlineCommandComment_getArgText comment) idxs
        pure InlineCommand{..}

      Right CXComment_HTMLStartTag -> do
        htmlStartTagName <- Text.strip <$> clang_HTMLTagComment_getTagName comment
        htmlStartTagIsSelfClosing <-
          clang_HTMLStartTagComment_isSelfClosing comment
        idxs <- getIdxs <$> clang_HTMLStartTag_getNumAttrs comment
        htmlStartTagAttributes <- forM idxs $ \idx -> do
          attrName  <- Text.strip <$> clang_HTMLStartTag_getAttrName  comment idx
          attrValue <- Text.strip <$> clang_HTMLStartTag_getAttrValue comment idx
          pure (attrName, attrValue)
        pure HtmlStartTag{..}

      Right CXComment_HTMLEndTag -> do
        htmlEndTagName <- Text.strip <$> clang_HTMLTagComment_getTagName comment
        pure HtmlEndTag{..}

      Right commentKind -> errorWithContext cursor $
        "child comment of non-inline kind " ++ show commentKind

      Left n ->
        errorWithContext cursor $ "child comment with invalid kind " ++ show n

-- | Get a verbatim block line as 'Text'
--
-- An error is thrown when an unexpected comment kind is encountered.
getVerbatimBlockLine ::
     MonadIO m
  => CXCursor -- ^ cursor to provide context in error messages
  -> CXComment
  -> m Text
getVerbatimBlockLine cursor comment = do
    eCommentKind <- fromSimpleEnum <$> clang_Comment_getKind comment
    case eCommentKind of
      Right CXComment_VerbatimBlockLine ->
            Text.strip
        <$> clang_VerbatimBlockLineComment_getText comment

      Right commentKind -> errorWithContext cursor $
        "child comment of non-verbatim-block-line kind " ++ show commentKind

      Left n ->
        errorWithContext cursor $ "child comment with invalid kind " ++ show n

-- | Reify children
getChildren :: MonadIO m => (CXComment -> m a) -> CXComment -> m [a]
getChildren f comment = do
    idxs <- getIdxs <$> clang_Comment_getNumChildren comment
    mapM (f <=< clang_Comment_getChild comment) idxs

-- | Get indexes (zero-based)
getIdxs :: (Enum a, Eq a, Num a)
  => a  -- ^ number of items
  -> [a]
getIdxs 0 = []
getIdxs n = [0 .. n - 1]

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

-- See "HsBindgen.Backend.Artefact.HsModule.Render"

{-------------------------------------------------------------------------------
  Auxiliary Functions
-------------------------------------------------------------------------------}

-- | Throw an error with context information
errorWithContext ::
     MonadIO m
  => CXCursor -- ^ cursor to provide context in error messages
  -> String   -- ^ error message
  -> m a
errorWithContext cursor msg = liftIO $ do
    displayName <- clang_getCursorDisplayName cursor
    extent <- clang_getCursorExtent cursor
    (file, startLine, startCol) <-
      clang_getPresumedLocation =<< clang_getRangeStart extent
    (_, endLine, endCol) <-
      clang_getPresumedLocation =<< clang_getRangeEnd extent
    fail $ concat
      [ msg, ": cursor ", show displayName, " in ", show file, " ("
      , show startLine, ":", show startCol, "-", show endLine, ":"
      , show endCol, ")"
      ]
