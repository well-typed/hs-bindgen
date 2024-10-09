module HsBindgen.C.Fold.Comments (
    Comment(..)
  , foldComments
  ) where

import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Tree

import HsBindgen.C.AST
import HsBindgen.C.Fold.Common
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.Clang.Core
import HsBindgen.Clang.Doxygen
import HsBindgen.Clang.Util.Fold
import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
import HsBindgen.Patterns
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Single node in the tree
-------------------------------------------------------------------------------}

data Comment = Comment {
      commentLoc  :: MultiLoc
    , commentNode :: Text
    , commentHTML :: Maybe Text
    }

mkComment :: MonadIO m => CXCursor -> m Comment
mkComment current = liftIO $ do
    commentLoc  <- SourceLoc.clang_getCursorLocation current
    commentNode <- clang_getCursorSpelling current
    comment     <- clang_Cursor_getParsedComment current
    commentKind <- clang_Comment_getKind comment
    commentHTML <- case fromSimpleEnum commentKind of
                     Right CXComment_FullComment -> do
                       Just <$> clang_FullComment_getAsHTML comment
                     _otherwise ->
                       return Nothing
    return Comment{
        commentLoc
      , commentNode
      , commentHTML
      }

{-------------------------------------------------------------------------------
  Fold proper
-------------------------------------------------------------------------------}

-- | Extract Doxygen comments as HTML for all top-level declarations
--
-- This is mostly a debugging/development aid.
foldComments :: Predicate -> Fold Identity (Tree Comment)
foldComments p = checkPredicate nullTracer p $ recurse mkComment
