-- | Parse functions related to comments
module HsBindgen.Frontend.Pass.Parse.Decl.Comment (
    parseCommentReferences
  ) where

import Clang.HighLevel.Documentation qualified as CDoc

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Imports (Text)

parseCommentReferences :: CDoc.Comment Text -> C.Comment Parse
parseCommentReferences comment = C.Comment (fmap auxRefs comment)
  where
    auxRefs :: Text -> C.CommentRef Parse
    auxRefs ref = C.CommentRef ref Nothing
