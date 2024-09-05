{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HsBindgen.App.RenderComments (renderComments) where

import Data.ByteString (ByteString)
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
renderComments :: Forest (SourceLoc, ByteString, Maybe ByteString) -> Html
renderComments comments = H.docTypeHtml $
    H.body $ do
      mapM_ renderNode comments

renderNode :: Tree (SourceLoc, ByteString, Maybe ByteString) -> Html
renderNode (Node (sourceLoc, name, mComment) children) = do
    H.b $ Blaze.unsafeByteString name
    " ("
    toMarkup sourceLoc
    ")"
    mapM_ Blaze.unsafeByteString mComment
    H.div ! A.style "margin-left: 1em;" $ do
      mapM_ renderNode children

instance ToMarkup SourceLoc where
  toMarkup SourceLoc{sourceLocFile, sourceLocLine, sourceLocColumn} = do
      Blaze.unsafeByteString sourceLocFile
      ":"
      toMarkup sourceLocLine
      ":"
      toMarkup sourceLocColumn
