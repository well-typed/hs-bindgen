{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Clang.HighLevel.Documentation (
    -- * Definition
    Comment(..)
  , CommentBlockContent(..)
  , CommentInlineContent(..)
  , CXCommentInlineCommandRenderKind(..)
  , CXCommentParamPassDirection(..)
    -- * Top-Level
  , clang_getComment
    -- * Translation
  ) where

import Control.Monad
import Data.Either
import Data.Text (Text)

import HsBindgen.Clang.LowLevel.Core
import HsBindgen.Clang.LowLevel.Doxygen
import HsBindgen.Patterns

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
  deriving stock (Show)

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
  deriving stock (Show)

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
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Reify the Clang comment for a cursor to the Haskell type
clang_getComment :: CXCursor -> IO (Maybe Comment)
clang_getComment cursor = do
    comment <- clang_Cursor_getParsedComment cursor
    eCommentKind <- fromSimpleEnum <$> clang_Comment_getKind comment
    case eCommentKind of
      Right CXComment_Null -> pure Nothing
      Right CXComment_FullComment -> do
        commentCName <- clang_getCursorDisplayName cursor
        commentChildren <- getChildren getBlockContent comment
        pure $ Just Comment{..}
      Right{} -> do
        -- should never happen
        commentCName <- clang_getCursorDisplayName cursor
        commentChildren <- getBlockContent comment
        pure $ Just Comment{..}
      Left{} -> pure Nothing

-- | Reify block content
--
-- This function deals with non-block content as follows:
--
-- * Null comments are ignored.
-- * Inline content is returned in a paragraph.
-- * A verbatim block line is returned in a verbatim block.
-- * Children of full comments are returned.  This function returns a list in
--   order to handle this case.
getBlockContent :: CXComment -> IO [CommentBlockContent]
getBlockContent comment = do
    eCommentKind <- fromSimpleEnum <$> clang_Comment_getKind comment
    case eCommentKind of
      Right CXComment_Paragraph -> do
        paragraphContent <- getChildren getInlineContent comment
        pure [Paragraph{..}]

      Right CXComment_BlockCommand -> do
        blockCommandName <- clang_BlockCommandComment_getCommandName comment
        idxs <- getIdxs <$> clang_BlockCommandComment_getNumArgs comment
        blockCommandArgs <-
          mapM (clang_BlockCommandComment_getArgText comment) idxs
        blockCommandParagraph <- getChildren getInlineContent
          =<< clang_BlockCommandComment_getParagraph comment
        pure [BlockCommand{..}]

      Right CXComment_ParamCommand -> do
        paramCommandName <- clang_ParamCommandComment_getParamName comment
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
        paramCommandContent <- getChildren getBlockContent comment
        pure [ParamCommand{..}]

      Right CXComment_TParamCommand -> do
        tParamCommandName <- clang_TParamCommandComment_getParamName comment
        tParamCommandPosition <- do
          isValid <- clang_TParamCommandComment_isParamPositionValid comment
          if isValid
            then fmap Just $ do
              depth <- clang_TParamCommandComment_getDepth comment
              forM [0 .. depth] $ \d ->
                (fromIntegral d,) . fromIntegral
                  <$> clang_TParamCommandComment_getIndex comment d
            else pure Nothing
        tParamCommandContent <- getChildren getBlockContent comment
        pure [TParamCommand{..}]

      Right CXComment_VerbatimBlockCommand -> do
        verbatimBlockLines <- getChildren getVerbatimBlockLine comment
        pure [VerbatimBlockCommand{..}]

      Right CXComment_VerbatimLine -> do
        -- rest of line after misused command becomes a verbatim line
        verbatimLine <- clang_VerbatimLineComment_getText comment
        pure [VerbatimLine{..}]

      Right CXComment_Null ->
        -- should never happen; ignore
        pure mempty

      Right CXComment_Text -> do
        -- should never happen; put in paragraph
        paragraphContent <- getInlineContent comment
        pure [Paragraph{..}]

      Right CXComment_InlineCommand -> do
        -- should never happen; put in paragraph
        paragraphContent <- getInlineContent comment
        pure [Paragraph{..}]

      Right CXComment_HTMLStartTag -> do
        -- should never happen; put in paragraph
        paragraphContent <- getInlineContent comment
        pure [Paragraph{..}]

      Right CXComment_HTMLEndTag -> do
        -- should never happen; put in paragraph
        paragraphContent <- getInlineContent comment
        pure [Paragraph{..}]

      Right CXComment_VerbatimBlockLine -> do
        -- should never happen; put in verbatim block
        verbatimBlockLines <- getVerbatimBlockLine comment
        pure [VerbatimBlockCommand{..}]

      Right CXComment_FullComment -> do
        -- should never happen; return children
        getChildren getBlockContent comment

      Left{} -> pure mempty

-- | Reify inline content
--
-- This function deals with non-inline content as follows:
--
-- * Null comments are ignored.
-- * Children of paragraphs are returned.  This function returns a list in order
--   to handle this case.
-- * Other block content is ignored.
-- * Full comments are ignored.
getInlineContent :: CXComment -> IO [CommentInlineContent]
getInlineContent comment = do
    eCommentKind <- fromSimpleEnum <$> clang_Comment_getKind comment
    case eCommentKind of
      Right CXComment_Text -> do
        textContent <- clang_TextComment_getText comment
        pure [TextContent{..}]

      Right CXComment_InlineCommand -> do
        inlineCommandName <- clang_InlineCommandComment_getCommandName comment
        inlineCommandRenderKind <-
          fromRight CXCommentInlineCommandRenderKind_Normal . fromSimpleEnum
            <$> clang_InlineCommandComment_getRenderKind comment
        idxs <- getIdxs <$> clang_InlineCommandComment_getNumArgs comment
        inlineCommandArgs <-
          mapM (clang_InlineCommandComment_getArgText comment) idxs
        pure [InlineCommand{..}]

      Right CXComment_HTMLStartTag -> do
        htmlStartTagName <- clang_HTMLTagComment_getTagName comment
        htmlStartTagIsSelfClosing <-
          clang_HTMLStartTagComment_isSelfClosing comment
        idxs <- getIdxs <$> clang_HTMLStartTag_getNumAttrs comment
        htmlStartTagAttributes <- forM idxs $ \idx -> do
          attrName  <- clang_HTMLStartTag_getAttrName  comment idx
          attrValue <- clang_HTMLStartTag_getAttrValue comment idx
          pure (attrName, attrValue)
        pure [HtmlStartTag{..}]

      Right CXComment_HTMLEndTag -> do
        htmlEndTagName <- clang_HTMLTagComment_getTagName comment
        pure [HtmlEndTag{..}]

      Right CXComment_Null ->
        -- should never happen; ignore
        pure mempty

      Right CXComment_Paragraph -> do
        -- should never happen; get children
        getChildren getInlineContent comment

      Right CXComment_BlockCommand ->
        -- should never happen; ignore
        pure mempty

      Right CXComment_ParamCommand ->
        -- should never happen; ignore
        pure mempty

      Right CXComment_TParamCommand ->
        -- should never happen; ignore
        pure mempty

      Right CXComment_VerbatimBlockCommand ->
        -- should never happen; ignore
        pure mempty

      Right CXComment_VerbatimBlockLine ->
        -- should never happen; ignore
        pure mempty

      Right CXComment_VerbatimLine ->
        -- should never happen; ignore
        pure mempty

      Right CXComment_FullComment ->
        -- should never happen; ignore
        pure mempty

      Left{} -> pure mempty

-- | Get a verbatim block line as 'Text'
--
-- All other types of comments are ignored.
--
-- This function returns a list so that it can be used with 'getChildren'.
getVerbatimBlockLine :: CXComment -> IO [Text]
getVerbatimBlockLine comment = do
    eCommentKind <- fromSimpleEnum <$> clang_Comment_getKind comment
    case eCommentKind of
      Right CXComment_VerbatimBlockLine ->
        pure <$> clang_VerbatimBlockLineComment_getText comment

      Right{} ->
        -- should never happen; ignore
        pure mempty

      Left{} ->
        -- should never happen; ignore
        pure mempty

-- | Reify children
--
-- The function that reifies each child returns a list in order to handle
-- unexpected cases (such as inline content when block content is expected).
-- The results are concatenated.
getChildren :: (CXComment -> IO [a]) -> CXComment -> IO [a]
getChildren f comment = do
    idxs <- getIdxs <$> clang_Comment_getNumChildren comment
    concat <$> mapM (f <=< clang_Comment_getChild comment) idxs

-- | Get indexes (zero-based)
getIdxs :: (Enum a, Eq a, Num a)
  => a  -- ^ number of items
  -> [a]
getIdxs 0 = []
getIdxs n = [0 .. n - 1]

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

-- TODO need to pass more options/information to translate references
--translateComment :: Comment -> String
--translateComment = undefined
