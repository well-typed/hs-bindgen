module HsBindgen.Backend.Hs.Haddock.Translation (
    mkHaddocks
  , mkHaddocksFieldInfo
  , mkHaddocksDecorateParams
  ) where

import Data.Char (isDigit)
import Data.Text qualified as Text
import System.FilePath (takeFileName)
import Text.Read (readMaybe)

import Clang.HighLevel.Documentation qualified as CDoc
import Clang.HighLevel.Types qualified as C
import Clang.Paths qualified as C

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Config (HaddockConfig (..), PathStyle (..))
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | Convert a Clang comment to a Haddock comment
--
mkHaddocks :: HaddockConfig -> C.DeclInfo Final -> Hs.Name ns -> Maybe HsDoc.Comment
mkHaddocks config info name =
    fmap (mbAddUniqueSymbolSource name) .
      fst $ mkHaddocksWithArgs config info Args{
          isField     = False
        , loc         = info.loc
        , nameC       = renderDeclId info.id.cName
        , nameHsIdent = info.id.hsName
        , comment     = info.comment
        , params      = []
        }

mkHaddocksFieldInfo ::
     HaddockConfig
  -> C.DeclInfo Final
  -> C.FieldInfo Final
  -> Maybe HsDoc.Comment
mkHaddocksFieldInfo config declInfo fieldInfo =
    fst $ mkHaddocksWithArgs config declInfo Args{
        isField     = True
      , loc         = fieldInfo.loc
      , nameC       = fieldInfo.name.cName.text
      , nameHsIdent = fieldInfo.name.hsName
      , comment     = fieldInfo.comment
      , params      = []
      }

-- | Extract Haddock documentation for a function; enrich function parameters
--   with parameter-specific documentation
mkHaddocksDecorateParams ::
     HaddockConfig
  -> C.DeclInfo Final
  -> Hs.Name ns
  -> [Hs.FunctionParameter]
  -> (Maybe HsDoc.Comment, [Hs.FunctionParameter])
mkHaddocksDecorateParams config info name params =
    let (mbc, xs) = mkHaddocksWithArgs config info Args{
        isField     = False
      , loc         = info.loc
      , nameC       = renderDeclId info.id.cName
      , nameHsIdent = info.id.hsName
      , comment     = info.comment
      , params
      }
    in  (mbAddUniqueSymbolSource name <$> mbc, xs)

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

-- | Internal
data Args = Args{
      isField     :: Bool
    , loc         :: C.SingleLoc
    , nameC       :: Text
    , nameHsIdent :: Hs.Identifier
    , comment     :: Maybe (C.Comment Final)
    , params      :: [Hs.FunctionParameter]
    }

-- | Convert a Clang comment to a Haddock comment, updating function parameters
--
-- This function processes 'C.ParamCommand' blocks:
-- - If a parameter name matches one in the provided list, attach the comment to it
-- - If no match is found, include the 'ParamCommand' in the resulting comment
--
-- Returns the processed comment and the updated parameters list
--
mkHaddocksWithArgs :: HaddockConfig -> C.DeclInfo Final -> Args -> (Maybe HsDoc.Comment, [Hs.FunctionParameter])
mkHaddocksWithArgs HaddockConfig{..} info Args{comment = Nothing, ..} =
      -- If there's no C.Comment to associate with any function parameter we make
      -- sure to at least add a comment that will show the function parameter name
      -- if it exists.
      --
      ( Just mempty {
           HsDoc.commentOrigin     = Just nameC
         , HsDoc.commentLocation   = Just $ updateSingleLoc pathStyle loc
         , HsDoc.commentHeaderInfo = Just info.headerInfo
         }
      , map addFunctionParameterComment params)
mkHaddocksWithArgs HaddockConfig{..} info Args{comment = Just (C.Comment CDoc.Comment{..}), ..} =
  let commentCName = nameC
      commentLocation = updateSingleLoc pathStyle loc
      (commentTitle, commentChildren') =
        case commentChildren of
          (CDoc.Paragraph [CDoc.TextContent ""]:rest) -> (Nothing, rest)
          (CDoc.Paragraph ci:rest)                    -> ( Just $ concatMap convertInlineContent
                                                                $ filter (\case
                                                                            CDoc.TextContent "" -> False
                                                                            _                   -> True
                                                                         )
                                                                $ ci
                                                         , rest
                                                         )
          _                                           -> (Nothing, commentChildren)

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
                                      CDoc.Paragraph ci -> CDoc.Paragraph
                                                         $ filter (\case
                                                                     CDoc.TextContent "" -> False
                                                                     _                   -> True
                                                                  )
                                                         $ ci
                                      cb                -> cb
                                  )
                                )
                    $ commentChildren'

   in ( Just mempty {
          HsDoc.commentTitle
        , HsDoc.commentOrigin     = Just commentCName
        , HsDoc.commentLocation   = Just commentLocation
        , HsDoc.commentHeaderInfo = Just info.headerInfo
        , HsDoc.commentChildren   = finalChildren
        }
      , updatedParams
      )
  where
    filterParamCommands ::
         [CDoc.CommentBlockContent (C.CommentRef Final)]
      -> [(HsDoc.Comment, Maybe CDoc.CXCommentParamPassDirection)]
    filterParamCommands = \case
      [] -> []
      (blockContent@CDoc.ParamCommand{..}:cmds)
        | any (\p -> fmap Hs.getName p.name == Just paramCommandName) params ->
          let comment =
                mempty {
                  HsDoc.commentOrigin   = if Text.null paramCommandName
                                            then Nothing
                                            else Just (Text.strip paramCommandName)
                , HsDoc.commentChildren = convertBlockContent blockContent
                }
           in (comment, paramCommandDirection):filterParamCommands cmds
        | otherwise -> filterParamCommands cmds
      (_:cmds) -> filterParamCommands cmds

    -- Process 'C.ParamCommand and update matching parameter
    --
    processParamCommands :: [(HsDoc.Comment, Maybe CDoc.CXCommentParamPassDirection)]
                         -> [Hs.FunctionParameter]
    processParamCommands paramCmds =
      go paramCmds params
      where
        go :: [(HsDoc.Comment, Maybe CDoc.CXCommentParamPassDirection)]
           -> [Hs.FunctionParameter]
           -> [Hs.FunctionParameter]
        go [] currentParams = currentParams
        go ((hsComment, _mbDirection):rest) currentParams =
            go rest $ map updateParam currentParams
          where
            updateParam :: Hs.FunctionParameter -> Hs.FunctionParameter
            updateParam fp =
                if fmap Hs.getName fp.name == HsDoc.commentOrigin hsComment
                  then fp & #comment .~ Just hsComment
                  else fp

-- | If the function parameter doesn't have any comments then add a simple
-- comment with just its name (if exists).
--
addFunctionParameterComment :: Hs.FunctionParameter -> Hs.FunctionParameter
addFunctionParameterComment fp =
  case fp.name of
    Nothing -> fp
    Just hsName
      | Text.null (Hs.getName hsName) -> panicPure "function parameter name is null"
      | otherwise ->
        case fp.comment of
          Just{}  -> fp
          Nothing -> fp & #comment .~ Just mempty {
              HsDoc.commentOrigin = Hs.getName <$> fp.name
            }

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
convertBlockContent ::
     CDoc.CommentBlockContent (C.CommentRef Final)
  -> [HsDoc.CommentBlockContent]
convertBlockContent = \case
  CDoc.Paragraph{..} ->
    formatParagraphContent paragraphContent

  CDoc.BlockCommand{..} ->
    let args        = map (HsDoc.TextContent . Text.strip) blockCommandArgs
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
          "section"       -> [HsDoc.Header HsDoc.Level1 inlineCommentWithArgs]
          "subsection"    -> [HsDoc.Header HsDoc.Level2 inlineCommentWithArgs]
          "subsubsection" -> [HsDoc.Header HsDoc.Level3 inlineCommentWithArgs]

          -- Code blocks
          "code"     -> [HsDoc.CodeBlock textInlineCommentWithArgs]
          "verbatim" -> [HsDoc.CodeBlock textInlineCommentWithArgs]

          -- Properties
          "property" -> [HsDoc.Property unwordsInlineCommentWithArgs]

          -- Example
          "example" -> [HsDoc.Example unwordsInlineCommentWithArgs]

          -- inline commands
          "anchor"     -> [HsDoc.Paragraph [HsDoc.Anchor unwordsInlineCommentWithArgs]]
          "ref"        -> [HsDoc.Paragraph [HsDoc.Link args unwordsInlineComment]]

          -- Supported References
          "sa"         -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "see:"] : inlineCommentWithArgs)]
          "see"        -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "see:"] : inlineCommentWithArgs)]
          "link"       -> [HsDoc.Paragraph [HsDoc.Link args unwordsInlineComment]]

          -- Not yet fully supported references
          "dir"        -> [HsDoc.Paragraph [HsDoc.Link inlineCommentWithArgs unwordsInlineCommentWithArgs]]
          "headerfile" -> [HsDoc.Paragraph [HsDoc.Link args unwordsInlineComment]]
          "image"      -> [HsDoc.Paragraph [HsDoc.Link args unwordsInlineComment]]
          "include"    -> [HsDoc.Paragraph [HsDoc.Link args unwordsInlineComment]]
          "refitem"    -> [HsDoc.Paragraph [HsDoc.Link args unwordsInlineComment]]
          "snippet"    -> [HsDoc.Paragraph [HsDoc.Link args unwordsInlineComment]]
          "xrefitem"   -> [HsDoc.Paragraph [HsDoc.Link args unwordsInlineComment]]

          -- List item
          "li" -> [HsDoc.ListItem HsDoc.BulletList [HsDoc.Paragraph inlineCommentWithArgs]]

          -- Metadata
          "since" -> [HsDoc.Paragraph [HsDoc.Metadata (HsDoc.Since unwordsInlineCommentWithArgs)]]

          -- Common documentation commands that become regular text
          "attention"       -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.Emph [HsDoc.TextContent "ATTENTION:"]] : inlineCommentWithArgs)]
          "brief"           -> [HsDoc.Paragraph inlineCommentWithArgs]
          "deprecated"      -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "deprecated:"] : inlineCommentWithArgs)]
          "details"         -> [HsDoc.Paragraph inlineCommentWithArgs]
          "exception"       -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "exception:"] : inlineCommentWithArgs)]
          "important"       -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "important:"] : inlineCommentWithArgs)]
          "invariant"       -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "invariant:"] : inlineCommentWithArgs)]
          "note"            -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "Note:"] : inlineCommentWithArgs)]
          "paragraph"       -> HsDoc.Paragraph [HsDoc.Bold [HsDoc.TextContent unwordsArgs]]
                             : formatParagraphContent blockCommandParagraph
          "par"             -> HsDoc.Paragraph [HsDoc.Bold [HsDoc.TextContent unwordsArgs]]
                             : formatParagraphContent blockCommandParagraph
          "post"            -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "post condition:"] : inlineComment)]
          "pre"             -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "pre condition:"] : inlineComment)]
          "raisewarning"    -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.Emph [HsDoc.TextContent "WARNING:"]] : inlineCommentWithArgs)]
          "remark"          -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "remark:"] : inlineCommentWithArgs)]
          "remarks"         -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "remark:"] : inlineCommentWithArgs)]
          "result"          -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "returns:"] : inlineCommentWithArgs)]
          "return"          -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "returns:"] : inlineCommentWithArgs)]
          "returns"         -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "returns:"] : inlineCommentWithArgs)]
          "retval"          -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "returns:"] : inlineCommentWithArgs)]
          "short"           -> [HsDoc.Paragraph inlineCommentWithArgs]
          "subparagraph"    -> HsDoc.Paragraph [HsDoc.Bold [HsDoc.TextContent unwordsArgs]]
                             : formatParagraphContent blockCommandParagraph
          "subsubparagraph" -> HsDoc.Paragraph [HsDoc.Bold [HsDoc.TextContent unwordsArgs]]
                             : formatParagraphContent blockCommandParagraph
          "throw"           -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "exception:"] : inlineCommentWithArgs)]
          "throws"          -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent "exception:"] : inlineCommentWithArgs)]
          "todo"            -> HsDoc.Paragraph [HsDoc.Bold [HsDoc.TextContent "TODO:"]]
                             : formatParagraphContent blockCommandParagraph
          "warning"         -> [HsDoc.Paragraph (HsDoc.Bold [HsDoc.Emph [HsDoc.TextContent "WARNING:"]] : inlineCommentWithArgs)]

          -- Everything else becomes an empty paragraph
          _ -> []

  -- Here we have access to param information which can be useful to create
  -- high level bindings.
  --
  -- TODO: Take advantage of these annotations to create high-level
  -- binding (See issue #113)
  CDoc.ParamCommand{..} ->
    let direction = case paramCommandDirection of
          Nothing                                     -> HsDoc.TextContent ""
          Just CDoc.CXCommentParamPassDirection_In    -> HsDoc.Emph [HsDoc.TextContent "(input)"]
          Just CDoc.CXCommentParamPassDirection_Out   -> HsDoc.Emph [HsDoc.TextContent "(output)"]
          Just CDoc.CXCommentParamPassDirection_InOut -> HsDoc.Emph [HsDoc.TextContent "(input,output)"]
        paramNameAndDirection =
          HsDoc.Bold [ HsDoc.Monospace [HsDoc.TextContent (Text.strip paramCommandName)]
                     , direction
                     ]
    in pure
     $ HsDoc.DefinitionList paramNameAndDirection
                            (concatMap convertBlockContent paramCommandContent)

  CDoc.TParamCommand{..} ->
    -- Template parameters, similar to regular parameters
    let tparamName = HsDoc.Bold [HsDoc.Monospace [HsDoc.TextContent (Text.strip tParamCommandName)]]
    in pure
     $ HsDoc.DefinitionList tparamName
                            (concatMap convertBlockContent tParamCommandContent)

  CDoc.VerbatimBlockCommand{..} ->
    pure $
    HsDoc.CodeBlock (map Text.strip verbatimBlockLines)

  CDoc.VerbatimLine{..} ->
    pure $
    HsDoc.Verbatim (Text.strip verbatimLine)

-- | Convert inline content
--
convertInlineContent ::
     CDoc.CommentInlineContent (C.CommentRef Final)
  -> [HsDoc.CommentInlineContent]
convertInlineContent = \case
  CDoc.TextContent{..}
    | Text.null textContent -> []
    | otherwise             -> [HsDoc.TextContent (Text.strip textContent)]

  CDoc.InlineCommand{..} ->
    let args     = map (HsDoc.TextContent . Text.strip) inlineCommandArgs
        argsText = Text.unwords (map Text.strip inlineCommandArgs)
    in pure
     $ case inlineCommandRenderKind of
        CDoc.CXCommentInlineCommandRenderKind_Normal     -> HsDoc.TextContent argsText
        CDoc.CXCommentInlineCommandRenderKind_Bold       -> HsDoc.Bold args
        CDoc.CXCommentInlineCommandRenderKind_Monospaced -> HsDoc.Monospace args
        CDoc.CXCommentInlineCommandRenderKind_Emphasized -> HsDoc.Emph args
        CDoc.CXCommentInlineCommandRenderKind_Anchor     -> HsDoc.Anchor (Text.unwords (map Text.strip inlineCommandArgs))

  CDoc.InlineRefCommand (C.CommentRef c mHsIdent) -> [
      case mHsIdent of
        Just namePair -> HsDoc.Identifier namePair.hsName.text
        Nothing       -> HsDoc.Monospace [HsDoc.TextContent c]
    ]

  -- HTML is not currently supported
  --
  -- TODO: See issue #948
  CDoc.HtmlStartTag{} -> []
  CDoc.HtmlEndTag{}   -> []

-- | Extract text lines from inline content
--
extractTextLines :: [HsDoc.CommentInlineContent] -> [Text]
extractTextLines = filter (not . Text.null)
                 . map extractText
  where
    extractText (HsDoc.TextContent t) = Text.strip t
    extractText _                     = ""

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
formatParagraphContent ::
     [CDoc.CommentInlineContent (C.CommentRef Final)]
  -> [HsDoc.CommentBlockContent]
formatParagraphContent = processGroups 1 []
                       . groupListParagraphs
                       -- Filter unnecessary spaces that will lead to excess
                       -- of new lines
                       . filter (\case
                                    CDoc.TextContent "" -> False
                                    _                   -> True
                                )
  where
    -- | Group inline content by list items
    --
    -- If the paragraphs contains list items, each list item and its content
    -- will be in a separate group. Otherwise, returns a singleton list.
    --
    groupListParagraphs ::
         [CDoc.CommentInlineContent (C.CommentRef Final)]
      -> [[CDoc.CommentInlineContent (C.CommentRef Final)]]
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

    processGroups ::
         Natural
      -> [HsDoc.CommentBlockContent]
      -> [[CDoc.CommentInlineContent (C.CommentRef Final)]]
      -> [HsDoc.CommentBlockContent]
    processGroups _ acc [] = reverse acc
    processGroups n acc (group:rest) =
      case group of
        [] -> processGroups n acc rest
        (CDoc.TextContent t : restContent)
          | Just (listType, afterMarker, nextInt) <- detectListMarker n t ->
              let nextN =
                    case nextInt of
                      Nothing -> n
                      Just n' -> n'
                  listItem =
                    HsDoc.ListItem listType
                                [ HsDoc.Paragraph
                                    ( HsDoc.TextContent (Text.strip afterMarker)
                                    : concatMap convertInlineContent restContent
                                    )
                                ]
               in processGroups nextN (listItem : acc) rest
          | otherwise -> processGroups n (HsDoc.Paragraph (concatMap convertInlineContent group) : acc) rest
        _ -> processGroups n (HsDoc.Paragraph (concatMap convertInlineContent group) : acc) rest

    -- | Check if text starts with a list marker
    isListMarker :: CDoc.CommentInlineContent (C.CommentRef Final) -> Bool
    isListMarker (CDoc.TextContent t) = isJust $ detectListMarker 0 t
    isListMarker _                 = False

    -- | Parse a list marker.
    --
    -- Returns its list type, the item text content and the next number on the
    -- numbered list order if the found marker is -#.
    --
    detectListMarker :: Natural -> Text
                     -> Maybe (HsDoc.ListType, Text, Maybe Natural)
    detectListMarker i text =
      case Text.unpack text of
        ('-':'#':' ':rest) ->
          Just (HsDoc.NumberedList i, Text.pack rest, Just (i + 1))
        ('-':' ':rest) -> Just (HsDoc.BulletList, Text.pack rest, Nothing)
        ('*':' ':rest) -> Just (HsDoc.BulletList, Text.pack rest, Nothing)
        ('+':' ':rest) -> Just (HsDoc.BulletList, Text.pack rest, Nothing)
        _ -> case span Data.Char.isDigit (Text.unpack text) of
          (digits@(_:_), '.':' ':rest) -> do
            digits' <- readMaybe digits
            return (HsDoc.NumberedList digits', Text.pack rest, Nothing)
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

mbAddUniqueSymbolSource :: Hs.Name ns -> HsDoc.Comment -> HsDoc.Comment
mbAddUniqueSymbolSource = \case
  Hs.ExportedName _ -> id
  Hs.InternalName x -> (HsDoc.uniqueSymbol x <>)
