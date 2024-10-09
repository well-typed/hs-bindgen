{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HsBindgen.App.RenderComments (renderComments) where

import Data.Tree
import Text.Blaze qualified as Blaze
import Text.Blaze.Html5 (Html, ToMarkup(..), (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

import HsBindgen.C.AST
import HsBindgen.Lib (Comment(..))

{-------------------------------------------------------------------------------
  Render comments
-------------------------------------------------------------------------------}

-- | Generate HTML page with comments
renderComments :: Forest Comment -> Html
renderComments comments = H.docTypeHtml $
    H.body $ do
      mapM_ renderNode comments

renderNode :: Tree Comment -> Html
renderNode (Node Comment{commentNode, commentLoc, commentHTML} children) = do
    H.b $ Blaze.text commentNode
    " ("
    toMarkup (multiLocExpansion commentLoc)
    ")"
    mapM_ Blaze.text commentHTML
    H.div ! A.style "margin-left: 1em;" $ do
      mapM_ renderNode children

instance ToMarkup SourcePath where
  toMarkup = Blaze.text . getSourcePath

instance ToMarkup SingleLoc where
  toMarkup SingleLoc{singleLocPath, singleLocLine, singleLocColumn} = do
      toMarkup singleLocPath
      ":"
      toMarkup singleLocLine
      ":"
      toMarkup singleLocColumn
