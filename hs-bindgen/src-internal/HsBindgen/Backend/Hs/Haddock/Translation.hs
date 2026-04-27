{-# LANGUAGE RecordWildCards #-}

module HsBindgen.Backend.Hs.Haddock.Translation (
    mkHaddocks
  , mkHaddocksFieldInfo
  , mkHaddocksDecorateParams
  ) where

import Data.Text qualified as Text
import System.FilePath (takeFileName)

import Clang.HighLevel.Types qualified as C
import Clang.Paths qualified as C

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Haddock.Config (HaddockConfig (..), PathStyle (..))
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

import Doxygen.Parser.Types (ParamDirection (..), ParamListKind (..),
                             SimpleSectKind (..))
import Doxygen.Parser.Types qualified as Doxy

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | Convert a Doxygen comment to a Haddock comment
--
mkHaddocks ::
     HaddockConfig
  -> C.DeclInfo Final
  -> Maybe HsDoc.Comment
mkHaddocks config info =
    fst $ mkHaddocksWithArgs config info Args{
        isField = False
      , loc     = info.loc
      , cName   = renderDeclId info.id.cName
      , hsName  = info.id.hsName
      , comment = info.comment
      , params  = []
      }

mkHaddocksFieldInfo ::
     HaddockConfig
  -> C.DeclInfo Final
  -> C.FieldInfo Final
  -> Maybe HsDoc.Comment
mkHaddocksFieldInfo config declInfo fieldInfo =
    fst $ mkHaddocksWithArgs config declInfo Args{
        isField = True
      , loc     = fieldInfo.loc
      , cName   = fieldInfo.name.cName.text
      , hsName  = fieldInfo.name.hsName
      , comment = fieldInfo.comment
      , params  = []
      }

-- | Extract Haddock documentation for a function; enrich function parameters
--   with parameter-specific documentation
mkHaddocksDecorateParams ::
     HaddockConfig
  -> C.DeclInfo Final
  -> [(Maybe Text, Hs.FunctionParameter)]
  -> (Maybe HsDoc.Comment, [Hs.FunctionParameter])
mkHaddocksDecorateParams config info params =
    let (mbc, xs) = mkHaddocksWithArgs config info Args{
        isField = False
      , loc     = info.loc
      , cName   = renderDeclId info.id.cName
      , hsName  = info.id.hsName
      , comment = info.comment
      , params  = params
      }
    in  (mbc, xs)

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

data Args = Args{
      isField :: Bool
    , loc     :: C.SingleLoc
    , cName   :: Text
    , hsName  :: Hs.SomeName
    , comment :: Maybe (C.Comment Final)
    , params  :: [(Maybe Text, Hs.FunctionParameter)]
    }

-- | Convert a Doxygen comment to a Haddock comment, updating function
-- parameters with their documentation.
--
mkHaddocksWithArgs :: HaddockConfig -> C.DeclInfo Final -> Args -> (Maybe HsDoc.Comment, [Hs.FunctionParameter])
mkHaddocksWithArgs HaddockConfig{..} info Args{comment = Nothing, ..} =
      ( Just $
          mempty
            & #origin     .~ Just cName
            & #location   .~ Just (updateSingleLoc pathStyle loc)
            & #headerInfo .~ Just info.headerInfo
      , map (uncurry addFunctionParameterComment) params
      )
mkHaddocksWithArgs HaddockConfig{..} info Args{comment = Just (C.Comment Doxy.Comment{..}), ..} =
  let commentCName    = cName
      commentLocation = updateSingleLoc pathStyle loc

      -- The brief description becomes the Haddock title
      commentTitle = case brief of
        []      -> Nothing
        inlines -> Just $ concatMap convertInline inlines

      -- Extract param docs from detailed blocks for attaching to
      -- function parameters
      paramDocs = extractMatchedParams detailed

      -- Match param docs to function parameters
      updatedParams = map (uncurry addFunctionParameterComment)
                    . processParamDocs paramDocs
                    $ params

      -- Convert detailed blocks to Haddock content.
      -- Matched params are kept so they appear in both the function-level
      -- comment and in individual parameter comments.
      commentChildren = concatMap convertBlock detailed

      -- When doxygen puts all text in detailed (e.g. inline enum comments
      -- like /**< Red color */), promote the first simple paragraph to title.
      (finalTitle, finalChildren) = case (commentTitle, commentChildren) of
        (Nothing, HsDoc.Paragraph inlines : rest)
          | all isSimpleInline inlines -> (Just inlines, rest)
        _ -> (commentTitle, commentChildren)

  in  ( Just $
          mempty
            & #title      .~ finalTitle
            & #origin     .~ Just commentCName
            & #location   .~ Just commentLocation
            & #headerInfo .~ Just info.headerInfo
            & #children   .~ finalChildren
      , updatedParams
      )
  where
    -- Extract @param entries from detailed blocks that match provided parameters
    extractMatchedParams :: [Doxy.Block (C.CommentRef Final)]
                         -> [Doxy.Param (C.CommentRef Final)]
    extractMatchedParams [] = []
    extractMatchedParams (Doxy.ParamList ParamListParam ps : rest) =
        filter isMatch ps ++ extractMatchedParams rest
      where
        isMatch p = any ((== Just p.paramName) . fst) params
    extractMatchedParams (_ : rest) = extractMatchedParams rest

    processParamDocs :: [Doxy.Param (C.CommentRef Final)]
                     -> [(Maybe Text, Hs.FunctionParameter)]
                     -> [(Maybe Text, Hs.FunctionParameter)]
    processParamDocs [] currentParams = currentParams
    processParamDocs (dp : rest) currentParams =
        processParamDocs rest $ map (updateParam dp) currentParams
      where
        updateParam dp' (mbName, fp)
          | mbName == Just (Text.strip dp'.paramName)
          = let paramComment :: HsDoc.Comment
                paramComment = mempty
                  & #origin   .~ mbName
                  & #children .~ [convertParam ParamListParam dp']
            in  (mbName, fp & #comment .~ Just paramComment)
          | otherwise = (mbName, fp)

addFunctionParameterComment :: Maybe Text -> Hs.FunctionParameter -> Hs.FunctionParameter
addFunctionParameterComment mbName fp =
  case mbName of
    Nothing -> fp
    Just name
      | Text.null name -> panicPure "Function parameter name is null"
      | otherwise ->
        case fp.comment of
          Just{}  -> fp
          Nothing -> fp & #comment .~ Just (
              mempty & #origin .~ mbName
            )

{-------------------------------------------------------------------------------
  Block content conversion

  Doxy.Block maps directly to HsDoc.CommentBlockContent.  No string matching
  on command names — the Doxygen XML has already classified everything.
-------------------------------------------------------------------------------}

-- | Convert a 'Doxy.Block' to Haddock block content
convertBlock :: Doxy.Block (C.CommentRef Final) -> [HsDoc.CommentBlockContent]
convertBlock = \case
  Doxy.Paragraph inlines ->
    [HsDoc.Paragraph $ concatMap convertInline inlines]

  Doxy.ParamList kind ps ->
    case kind of
      ParamListParam  -> map (convertParam kind) ps
      ParamListRetVal -> concatMap convertRetval ps

  Doxy.SimpleSect kind blocks ->
    convertSimpleSect kind blocks

  Doxy.CodeBlock codeLines ->
    [HsDoc.CodeBlock codeLines]

  Doxy.ItemizedList items ->
    map (\item -> HsDoc.ListItem HsDoc.BulletList (concatMap convertBlock item)) items

  Doxy.OrderedList items ->
    zipWith (\i item -> HsDoc.ListItem (HsDoc.NumberedList i)
                                       (concatMap convertBlock item))
            [1..] items

  Doxy.XRefSect title blocks ->
    prefixed (title <> ":") (concatMap convertBlock blocks)

  Doxy.Tag _tag children -> concatMap convertBlock children

-- | Convert a @\<simplesect\>@ to Haddock content
convertSimpleSect :: SimpleSectKind -> [Doxy.Block (C.CommentRef Final)] -> [HsDoc.CommentBlockContent]
convertSimpleSect kind blocks =
    let content = concatMap convertBlock blocks
    in  case kind of
          SSReturn     -> prefixed "Returns:" content
          SSWarning    -> prefixed "WARNING:" content
          SSNote       -> prefixed "Note:" content
          SSSee        -> prefixed "See:" content
          SSSince      -> [HsDoc.Paragraph [HsDoc.Metadata (HsDoc.Since (extractText blocks))]]
          SSVersion    -> prefixed "Version:" content
          SSPre        -> prefixed "Precondition:" content
          SSPost       -> prefixed "Postcondition:" content
          SSPar title  -> HsDoc.Paragraph [HsDoc.Bold [HsDoc.TextContent title]] : content
          SSDeprecated -> prefixed "Deprecated:" content
          SSRemark     -> prefixed "Remark:" content
          SSAttention  -> prefixed "ATTENTION:" content
          SSTodo       -> prefixed "TODO:" content
          SSInvariant  -> prefixed "Invariant:" content
          SSAuthor     -> prefixed "Author:" content
          SSDate       -> prefixed "Date:" content
  where
    extractText :: [Doxy.Block (C.CommentRef Final)] -> Text
    extractText = Text.strip . Text.unwords . concatMap go
      where
        go (Doxy.Paragraph inlines) = map inlineText inlines
        go _                       = []

        inlineText (Doxy.Text t) = t
        inlineText _            = ""

-- | Check if an inline content element is simple enough to promote to a title
isSimpleInline :: HsDoc.CommentInlineContent -> Bool
isSimpleInline = \case
  HsDoc.TextContent{} -> True
  HsDoc.Monospace{}   -> True
  HsDoc.Emph{}        -> True
  HsDoc.Bold{}        -> True
  _                   -> False

-- | Inline a bold label into the first paragraph of some content
prefixed :: Text -> [HsDoc.CommentBlockContent] -> [HsDoc.CommentBlockContent]
prefixed label cs = case cs of
  (HsDoc.Paragraph inlines : rest) ->
    HsDoc.Paragraph (HsDoc.Bold [HsDoc.TextContent label] : inlines) : rest
  _ -> HsDoc.Paragraph [HsDoc.Bold [HsDoc.TextContent label]] : cs

-- | Convert a documented parameter to a Haddock definition list entry
convertParam :: ParamListKind -> Doxy.Param (C.CommentRef Final) -> HsDoc.CommentBlockContent
convertParam _kind p =
    let paramNameContent = HsDoc.Monospace [HsDoc.TextContent (Text.strip p.paramName)]
        paramNameAndDir  = HsDoc.Bold (paramNameContent : dirInline p.paramDirection)
    in  HsDoc.DefinitionList paramNameAndDir (concatMap convertBlock p.paramDesc)

-- | Convert a @\@retval@ entry to a definition list item:
--   @[__@value@__]: description@
convertRetval :: Doxy.Param (C.CommentRef Final) -> [HsDoc.CommentBlockContent]
convertRetval p =
    let code = HsDoc.Monospace [HsDoc.TextContent (Text.strip p.paramName)]
        desc = concatMap convertBlock p.paramDesc
    in  [HsDoc.DefinitionList (HsDoc.Bold [code]) desc]

-- | Direction annotation for a parameter (empty list when unspecified)
dirInline :: Maybe ParamDirection -> [HsDoc.CommentInlineContent]
dirInline (Just DirIn)    = [HsDoc.Emph [HsDoc.TextContent "(input)"]]
dirInline (Just DirOut)   = [HsDoc.Emph [HsDoc.TextContent "(output)"]]
dirInline (Just DirInOut) = [HsDoc.Emph [HsDoc.TextContent "(input,output)"]]
dirInline Nothing         = []

{-------------------------------------------------------------------------------
  Inline content conversion

  Doxy.Inline maps directly to HsDoc.CommentInlineContent.
  Cross-references (Doxy.Ref) are resolved via the CommentRef that was
  threaded through the pipeline and resolved by MangleNames.
-------------------------------------------------------------------------------}

-- | Convert a 'Doxy.Inline' to Haddock inline content
--
-- Text content is stripped because the Haddock pretty-printer uses 'hsep'
-- to join inline elements (adding its own inter-element spacing).
-- The Doxygen parser preserves XML whitespace in text nodes, so we strip
-- it here at the translation boundary.
--
convertInline :: Doxy.Inline (C.CommentRef Final) -> [HsDoc.CommentInlineContent]
convertInline = \case
  Doxy.Text t
    | Text.null stripped -> []
    | otherwise          -> [HsDoc.TextContent stripped]
    where stripped = Text.strip t

  Doxy.Bold inlines  -> [HsDoc.Bold $ concatMap convertInline inlines]
  Doxy.Emph inlines  -> [HsDoc.Emph $ concatMap convertInline inlines]
  Doxy.Mono inlines  -> [HsDoc.Monospace $ concatMap convertInline inlines]

  Doxy.Ref (C.CommentRef c mHsIdent) _displayText ->
    case mHsIdent of
      Just namePair -> [HsDoc.Identifier namePair.hsName.text]
      Nothing       -> [HsDoc.Monospace [HsDoc.TextContent c]]

  Doxy.Anchor idText -> [HsDoc.Anchor idText]

  Doxy.Link label url -> [HsDoc.Link (concatMap convertInline label) url]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Depending on the configured 'PathStyle', update 'HsBindgen.Clang.HighLevel.Types.SingleLoc'
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
