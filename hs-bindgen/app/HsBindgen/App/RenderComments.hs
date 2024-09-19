{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HsBindgen.App.RenderComments (renderComments) where

import Data.Text (Text)
import Data.Tree
import Text.Blaze qualified as Blaze
import Text.Blaze.Html5 (Html, ToMarkup(..), (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Render comments
-------------------------------------------------------------------------------}

-- | Generate HTML page with comments
renderComments :: Forest (SourceLoc, Text, Maybe Text) -> Html
renderComments comments = H.docTypeHtml $
    H.body $ do
      mapM_ renderNode comments

renderNode :: Tree (SourceLoc, Text, Maybe Text) -> Html
renderNode (Node (sourceLoc, name, mComment) children) = do
    H.b $ Blaze.text name
    " ("
    toMarkup sourceLoc
    ")"
    mapM_ Blaze.text mComment
    H.div ! A.style "margin-left: 1em;" $ do
      mapM_ renderNode children

instance ToMarkup SourceLoc where
  toMarkup SourceLoc{sourceLocFile, sourceLocLine, sourceLocColumn} = do
      Blaze.text sourceLocFile
      ":"
      toMarkup sourceLocLine
      ":"
      toMarkup sourceLocColumn
