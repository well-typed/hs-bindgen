module HsBindgen.Backend.Hs.Haddock.Translation
  ( generateHaddocksWithInfo
  , generateHaddocksWithFieldInfo
  , generateHaddocksWithInfoParams
  )
  where

import Data.Char (isDigit)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Natural (Natural)
import System.FilePath (takeFileName)
import Text.Read (readMaybe)

import Clang.HighLevel.Documentation qualified as C
import Clang.HighLevel.Types qualified as C
import Clang.Paths qualified as C

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Config (HaddockConfig (..), PathStyle (..))
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as Hs
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.AST.External (DeclInfo (..), FieldInfo (..),
                                        Name (..), NamePair (..),
                                        Reference (..))
import HsBindgen.Frontend.RootHeader (HashIncludeArg (..))
import HsBindgen.Language.Haskell (HsIdentifier (..), HsName (..))

-- | Convert a Clang comment to a Haddock comment
--
generateHaddocksWithInfo :: HaddockConfig -> DeclInfo -> Maybe Hs.Comment
generateHaddocksWithInfo config DeclInfo{..} =
  fst $ generateHaddocksWithParams config declLoc declHeader declId declComment []

generateHaddocksWithFieldInfo
  :: HaddockConfig
  -> DeclInfo
  -> FieldInfo
  -> Maybe Hs.Comment
generateHaddocksWithFieldInfo config DeclInfo{..} FieldInfo{..} =
  fst $ generateHaddocksWithParams config fieldLoc declHeader fieldName fieldComment []

generateHaddocksWithInfoParams
  :: HaddockConfig
  -> DeclInfo
  -> [Hs.FunctionParameter]
  -> (Maybe Hs.Comment, [Hs.FunctionParameter])
generateHaddocksWithInfoParams config DeclInfo{..} params =
  generateHaddocksWithParams config declLoc declHeader declId declComment params

-- | Convert a Clang comment to a Haddock comment, updating function parameters
--
-- This function processes 'C.ParamCommand' blocks:
-- - If a parameter name matches one in the provided list, attach the comment to it
-- - If no match is found, include the 'ParamCommand' in the resulting comment
--
-- Returns the processed comment and the updated parameters list
--
generateHaddocksWithParams ::
     HaddockConfig
  -> C.SingleLoc
  -> HashIncludeArg
  -> NamePair
  -> Maybe (C.Comment Reference)
  -> [Hs.FunctionParameter]
  -> (Maybe Hs.Comment, [Hs.FunctionParameter])
generateHaddocksWithParams HaddockConfig{..} declLoc declHeader declId Nothing params =
  -- If there's no C.Comment to associate with any function parameter we make
  -- sure to at least add a comment that will show the function parameter name
  -- if it exists.
  --
  ( Just
     Hs.Comment {
       Hs.commentTitle    = Nothing
     , Hs.commentOrigin   = Just (getName (nameC declId))
     , Hs.commentLocation = Just (updateSingleLoc pathStyle declLoc)
     , Hs.commentHeader   = Just declHeader
     , Hs.commentChildren = []
     }
  , map addFunctionParameterComment params)
generateHaddocksWithParams HaddockConfig{..} declLoc header declId (Just C.Comment{..}) params =
  let (commentTitle, commentChildren') =
        case commentChildren of
          (C.Paragraph [C.TextContent ""]:rest) -> (Nothing, rest)
          (C.Paragraph ci:rest)                 -> ( Just $ concatMap convertInlineContent
                                                          $ filter (\case
                                                                      C.TextContent "" -> False
                                                                      _                -> True
                                                                   )
                                                          $ ci
                                                   , rest
                                                   )
          _                                     -> (Nothing, commentChildren)

      -- Separate 'ParamCommands' that match with provided parameters from
      -- other block content
      --
      paramCommands = filterParamCommands commentChildren'

      -- Process parameter commands and update parameters
      --
      -- If there's no C.Comment to associate with any function parameter we make
      -- sure to at least add a comment that will show the function parameter name
      --
      updatedParams = map addFunctionParameterComment
                    . processParamCommands
                    $ paramCommands

      -- Convert remaining content (including unmatched param commands)
      finalChildren = concatMap ( convertBlockContent
                                . (\case
                                      C.Paragraph ci -> C.Paragraph
                                                      $ filter (\case
                                                                  C.TextContent "" -> False
                                                                  _                -> True
                                                               )
                                                      $ ci
                                      cb             -> cb
                                  )
                                )
                    $ commentChildren'

   in ( Just Hs.Comment {
          commentTitle
        , commentOrigin   = Just (getName (nameC declId))
        , commentLocation = Just (updateSingleLoc pathStyle declLoc)
        , commentHeader   = Just header
        , commentChildren = finalChildren
        }
      , updatedParams
      )
  where
    filterParamCommands :: [C.CommentBlockContent Reference]
                        -> [(Hs.Comment, Maybe C.CXCommentParamPassDirection)]
    filterParamCommands = \case
      [] -> []
      (blockContent@C.ParamCommand{..}:cmds)
        | any ( (== Just paramCommandName)
              . fmap getHsName
              . Hs.functionParameterName
              )
        $ params ->
          let comment =
                Hs.Comment {
                  commentTitle    = Nothing
                , commentOrigin   = if Text.null paramCommandName
                                       then Nothing
                                       else Just (Text.strip paramCommandName)
                , commentLocation = Nothing
                , commentHeader   = Nothing
                , commentChildren = convertBlockContent blockContent
                }
           in (comment, paramCommandDirection):filterParamCommands cmds
        | otherwise -> filterParamCommands cmds
      (_:cmds) -> filterParamCommands cmds

    -- Process 'C.ParamCommand and update matching parameter
    --
    processParamCommands :: [(Hs.Comment, Maybe C.CXCommentParamPassDirection)] -> [Hs.FunctionParameter]
    processParamCommands paramCmds =
      go paramCmds params
      where
        go :: [(Hs.Comment, Maybe C.CXCommentParamPassDirection)]
           -> [Hs.FunctionParameter]
           -> [Hs.FunctionParameter]
        go [] currentParams = currentParams
        go ((hsComment, _mbDirection):rest) currentParams =
          let updatedParams =
                map (\fp@Hs.FunctionParameter {..} ->
                       if fmap getHsName functionParameterName == Hs.commentOrigin hsComment
                          then fp { Hs.functionParameterComment = Just hsComment }
                          else fp
                    ) currentParams
           in go rest updatedParams


-- | If the function parameter doesn't have any comments then add a simple
-- comment with just its name (if exists).
--
addFunctionParameterComment :: Hs.FunctionParameter -> Hs.FunctionParameter
addFunctionParameterComment fp@Hs.FunctionParameter {..} =
  case functionParameterName of
    Nothing -> fp
    Just hsName
      | Text.null (getHsName hsName) -> panicPure "function parameter name is null"
      | otherwise ->
        case functionParameterComment of
          Nothing ->
            fp { Hs.functionParameterComment =
                   Just Hs.Comment {
                          commentTitle    = Nothing
                        , commentOrigin   = getHsName <$> functionParameterName
                        , commentLocation = Nothing
                        , commentHeader   = Nothing
                        , commentChildren = []
                        }
               }
          _ -> fp

-- | Convert Clang block content to Haddock block content
--
--
-- TODO: We should make a bigger effort to detect references to identifiers,
-- URLs, paths, so that we can return valid Haddock syntax for those (See
-- issue #947):
--
-- - 'identifier' syntax
-- - "Module" syntax
-- - <URL> syntax
--
-- For now only \dir, \link and \see  are naively supported.
--
convertBlockContent :: C.CommentBlockContent Reference -> [Hs.CommentBlockContent]
convertBlockContent = \case
  C.Paragraph{..} ->
    formatParagraphContent paragraphContent

  C.BlockCommand{..} ->
    let args        = map (Hs.TextContent . Text.strip) blockCommandArgs
        textArgs    = extractTextLines args
        unwordsArgs = Text.unwords textArgs

        inlineComment        = concatMap convertInlineContent blockCommandParagraph
        textInlineComment    = extractTextLines inlineComment
        unwordsInlineComment = Text.unwords textInlineComment

        inlineCommentWithArgs        = args ++ inlineComment
        textInlineCommentWithArgs    = textArgs ++ textInlineComment
        unwordsInlineCommentWithArgs = unwordsArgs <> " " <> unwordsInlineComment
     in -- Only commands that can be associated with declarations are supported.
        -- Other commands are ignored.
        case Text.toLower (Text.strip blockCommandName) of
          -- Headers
          "section"       -> [Hs.Header Hs.Level1 inlineCommentWithArgs]
          "subsection"    -> [Hs.Header Hs.Level2 inlineCommentWithArgs]
          "subsubsection" -> [Hs.Header Hs.Level3 inlineCommentWithArgs]

          -- Code blocks
          "code"     -> [Hs.CodeBlock textInlineCommentWithArgs]
          "verbatim" -> [Hs.CodeBlock textInlineCommentWithArgs]

          -- Properties
          "property" -> [Hs.Property unwordsInlineCommentWithArgs]

          -- Example
          "example" -> [Hs.Example unwordsInlineCommentWithArgs]

          -- inline commands
          "anchor"     -> [Hs.Paragraph [Hs.Anchor unwordsInlineCommentWithArgs]]
          "ref"        -> [Hs.Paragraph [Hs.Link args unwordsInlineComment]]

          -- Supported References
          "sa"         -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "see:"] : inlineCommentWithArgs)]
          "see"        -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "see:"] : inlineCommentWithArgs)]
          "link"       -> [Hs.Paragraph [Hs.Link args unwordsInlineComment]]

          -- Not yet fully supported references
          "dir"        -> [Hs.Paragraph [Hs.Link inlineCommentWithArgs unwordsInlineCommentWithArgs]]
          "headerfile" -> [Hs.Paragraph [Hs.Link args unwordsInlineComment]]
          "image"      -> [Hs.Paragraph [Hs.Link args unwordsInlineComment]]
          "include"    -> [Hs.Paragraph [Hs.Link args unwordsInlineComment]]
          "refitem"    -> [Hs.Paragraph [Hs.Link args unwordsInlineComment]]
          "snippet"    -> [Hs.Paragraph [Hs.Link args unwordsInlineComment]]
          "xrefitem"   -> [Hs.Paragraph [Hs.Link args unwordsInlineComment]]

          -- List item
          "li" -> [Hs.ListItem Hs.BulletList [Hs.Paragraph inlineCommentWithArgs]]

          -- Metadata
          "since" -> [Hs.Paragraph [Hs.Metadata (Hs.Since unwordsInlineCommentWithArgs)]]

          -- Common documentation commands that become regular text
          "attention"       -> [Hs.Paragraph (Hs.Bold [Hs.Emph [Hs.TextContent "ATTENTION:"]] : inlineCommentWithArgs)]
          "brief"           -> [Hs.Paragraph inlineCommentWithArgs]
          "deprecated"      -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "deprecated:"] : inlineCommentWithArgs)]
          "details"         -> [Hs.Paragraph inlineCommentWithArgs]
          "exception"       -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "exception:"] : inlineCommentWithArgs)]
          "important"       -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "important:"] : inlineCommentWithArgs)]
          "invariant"       -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "invariant:"] : inlineCommentWithArgs)]
          "note"            -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "Note:"] : inlineCommentWithArgs)]
          "paragraph"       -> Hs.Paragraph [Hs.Bold [Hs.TextContent unwordsArgs]]
                             : formatParagraphContent blockCommandParagraph
          "par"             -> Hs.Paragraph [Hs.Bold [Hs.TextContent unwordsArgs]]
                             : formatParagraphContent blockCommandParagraph
          "post"            -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "post condition:"] : inlineComment)]
          "pre"             -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "pre condition:"] : inlineComment)]
          "raisewarning"    -> [Hs.Paragraph (Hs.Bold [Hs.Emph [Hs.TextContent "WARNING:"]] : inlineCommentWithArgs)]
          "remark"          -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "remark:"] : inlineCommentWithArgs)]
          "remarks"         -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "remark:"] : inlineCommentWithArgs)]
          "result"          -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "returns:"] : inlineCommentWithArgs)]
          "return"          -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "returns:"] : inlineCommentWithArgs)]
          "returns"         -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "returns:"] : inlineCommentWithArgs)]
          "retval"          -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "returns:"] : inlineCommentWithArgs)]
          "short"           -> [Hs.Paragraph inlineCommentWithArgs]
          "subparagraph"    -> Hs.Paragraph [Hs.Bold [Hs.TextContent unwordsArgs]]
                             : formatParagraphContent blockCommandParagraph
          "subsubparagraph" -> Hs.Paragraph [Hs.Bold [Hs.TextContent unwordsArgs]]
                             : formatParagraphContent blockCommandParagraph
          "throw"           -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "exception:"] : inlineCommentWithArgs)]
          "throws"          -> [Hs.Paragraph (Hs.Bold [Hs.TextContent "exception:"] : inlineCommentWithArgs)]
          "todo"            -> Hs.Paragraph [Hs.Bold [Hs.TextContent "TODO:"]]
                             : formatParagraphContent blockCommandParagraph
          "warning"         -> [Hs.Paragraph (Hs.Bold [Hs.Emph [Hs.TextContent "WARNING:"]] : inlineCommentWithArgs)]

          -- Everything else becomes an empty paragraph
          _ -> []

  -- Here we have access to param information which can be useful to create
  -- high level bindings.
  --
  -- TODO: Take advantage of these annotations to create high-level
  -- binding (See issue #113)
  C.ParamCommand{..} ->
    let direction = case paramCommandDirection of
          Nothing                                  -> Hs.TextContent ""
          Just C.CXCommentParamPassDirection_In    -> Hs.Emph [Hs.TextContent "(input)"]
          Just C.CXCommentParamPassDirection_Out   -> Hs.Emph [Hs.TextContent "(output)"]
          Just C.CXCommentParamPassDirection_InOut -> Hs.Emph [Hs.TextContent "(input,output)"]
        paramNameAndDirection =
          Hs.Bold [ Hs.Monospace [Hs.TextContent (Text.strip paramCommandName)]
                  , direction
                  ]
    in pure
     $ Hs.DefinitionList paramNameAndDirection
                         (concatMap convertBlockContent paramCommandContent)

  C.TParamCommand{..} ->
    -- Template parameters, similar to regular parameters
    let tparamName = Hs.Bold [Hs.Monospace [Hs.TextContent (Text.strip tParamCommandName)]]
    in pure
     $ Hs.DefinitionList tparamName
                         (concatMap convertBlockContent tParamCommandContent)

  C.VerbatimBlockCommand{..} ->
    pure $
    Hs.CodeBlock (map Text.strip verbatimBlockLines)

  C.VerbatimLine{..} ->
    pure $
    Hs.Verbatim (Text.strip verbatimLine)

-- | Convert inline content
--
convertInlineContent :: C.CommentInlineContent Reference -> [Hs.CommentInlineContent]
convertInlineContent = \case
  C.TextContent{..}
    | Text.null textContent -> []
    | otherwise             -> [Hs.TextContent (Text.strip textContent)]

  C.InlineCommand{..} ->
    let args     = map (Hs.TextContent . Text.strip) inlineCommandArgs
        argsText = Text.unwords (map Text.strip inlineCommandArgs)
    in pure
     $ case inlineCommandRenderKind of
        C.CXCommentInlineCommandRenderKind_Normal     -> Hs.TextContent argsText
        C.CXCommentInlineCommandRenderKind_Bold       -> Hs.Bold args
        C.CXCommentInlineCommandRenderKind_Monospaced -> Hs.Monospace args
        C.CXCommentInlineCommandRenderKind_Emphasized -> Hs.Emph args
        C.CXCommentInlineCommandRenderKind_Anchor     -> Hs.Anchor (Text.unwords (map Text.strip inlineCommandArgs))

  C.InlineRefCommand (ById arg) -> [Hs.Identifier (getHsIdentifier (nameHsIdent arg))]

  -- HTML is not currently supported
  --
  -- TODO: See issue #948
  C.HtmlStartTag{} -> []
  C.HtmlEndTag{}   -> []

-- | Extract text lines from inline content
--
extractTextLines :: [Hs.CommentInlineContent] -> [Text]
extractTextLines = filter (not . Text.null)
                 . map extractText
  where
    extractText (Hs.TextContent t) = Text.strip t
    extractText _                  = ""

-- | There might be list items that we want to pretty print. To find them we
-- will group all list paragraphs, if any, and collect all their contents
-- until the next list item marker.
--
-- When formatting the list items, we just have to address that numbered lists
-- can be created by using a '-#'.
--
-- Known Limitations:
--
-- If a list item contains a list marker inside it, e.g.:
--
-- 1. Item A with a - list marker
-- 2. Item B without a list marker
--
-- Then, this function is going to create 3 list items:
--
-- 1. Item A with a
-- - list marker
-- 2. Item B without a list marker
--
-- This limitation is due to the how clang parses the comments so we can only
-- do a best effort in trying to detect these lists.
--
-- Another known limitation is nested lists, which in principle we can detect
-- since we have information about whitespaces and indentation. TODO: See issue
-- #949.
--
formatParagraphContent :: [C.CommentInlineContent Reference] -> [Hs.CommentBlockContent]
formatParagraphContent = processGroups 1 []
                       . groupListParagraphs
                       -- Filter unnecessary spaces that will lead to excess
                       -- of new lines
                       . filter (\case
                                    C.TextContent "" -> False
                                    _                -> True
                                )
  where
    -- | Group inline content by list items
    --
    -- If the paragraphs contains list items, each list item and its content
    -- will be in a separate group. Otherwise, returns a singleton list.
    --
    groupListParagraphs :: [C.CommentInlineContent Reference] -> [[C.CommentInlineContent Reference]]
    groupListParagraphs [] = []
    -- Check if first item is a list marker
    groupListParagraphs (h : rest)
      | isListMarker h =
        let (itemContent, remaining) = break isListMarker rest
         in (h : itemContent) : groupListParagraphs remaining
    -- Not a list item, check if there are any list items later
    groupListParagraphs contents =
      let (nonItemContent, remaining) = break isListMarker contents
       in case remaining of
            [] -> [nonItemContent]
            _  -> nonItemContent : groupListParagraphs remaining

    processGroups :: Natural
                  -> [Hs.CommentBlockContent]
                  -> [[C.CommentInlineContent Reference]]
                  -> [Hs.CommentBlockContent]
    processGroups _ acc [] = reverse acc
    processGroups n acc (group:rest) =
      case group of
        [] -> processGroups n acc rest
        (C.TextContent t : restContent)
          | Just (listType, afterMarker, nextInt) <- detectListMarker n t ->
              let nextN =
                    case nextInt of
                      Nothing -> n
                      Just n' -> n'
                  listItem =
                    Hs.ListItem listType
                                [ Hs.Paragraph
                                    ( Hs.TextContent (Text.strip afterMarker)
                                    : concatMap convertInlineContent restContent
                                    )
                                ]
               in processGroups nextN (listItem : acc) rest
          | otherwise -> processGroups n (Hs.Paragraph (concatMap convertInlineContent group) : acc) rest
        _ -> processGroups n (Hs.Paragraph (concatMap convertInlineContent group) : acc) rest

    -- | Check if text starts with a list marker
    isListMarker :: C.CommentInlineContent Reference -> Bool
    isListMarker (C.TextContent t) = isJust $ detectListMarker 0 t
    isListMarker _                 = False

    -- | Parse a list marker.
    --
    -- Returns its list type, the item text content and the next number on the
    -- numbered list order if the found marker is -#.
    --
    detectListMarker :: Natural -> Text ->  Maybe (Hs.ListType, Text, Maybe Natural)
    detectListMarker i text =
      case Text.unpack text of
        ('-':'#':' ':rest) ->
          Just (Hs.NumberedList i, Text.pack rest, Just (i + 1))
        ('-':' ':rest) -> Just (Hs.BulletList, Text.pack rest, Nothing)
        ('*':' ':rest) -> Just (Hs.BulletList, Text.pack rest, Nothing)
        ('+':' ':rest) -> Just (Hs.BulletList, Text.pack rest, Nothing)
        _ -> case span Data.Char.isDigit (Text.unpack text) of
          (digits@(_:_), '.':' ':rest) -> do
            digits' <- readMaybe digits
            return (Hs.NumberedList digits', Text.pack rest, Nothing)
          _ -> Nothing

-- | Depending on the configured 'PathStyle', update 'SingleLoc'
-- to either have a short or full path name.
--
-- See #966.
updateSingleLoc :: PathStyle -> C.SingleLoc -> C.SingleLoc
updateSingleLoc Short C.SingleLoc{..} =
  C.SingleLoc {
    singleLocPath = C.SourcePath
                  . Text.pack
                  . takeFileName
                  . C.getSourcePath
                  $ singleLocPath
  , ..
  }
updateSingleLoc _     sloc = sloc
