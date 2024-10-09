-- | Reify the raw @clang@ AST as a Haskell datatype
--
-- This is primarily useful for debugging/development of @hs-bindgen@ itself.
module HsBindgen.C.Fold.Raw (
    Element(..)
  , foldRaw
  ) where

import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Tree (Tree(..))

import HsBindgen.C.AST
import HsBindgen.C.Fold.Common
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.Classification
import HsBindgen.Clang.Util.Fold
import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Single node in the tree
-------------------------------------------------------------------------------}

-- | An element in the @libclang@ AST
data Element = Element {
      elementName         :: !(UserProvided Text)
    , elementLocation     :: !(Range MultiLoc)
    , elementKind         :: !Text
    , elementTypeKind     :: !Text
    , elementRawComment   :: !Text
    , elementIsAnonymous  :: !Bool
    , elementIsDefinition :: !Bool
    }
  deriving stock (Show)

mkElement :: MonadIO m => CXCursor -> m Element
mkElement current = liftIO $ do
    elementName         <- getUserProvidedName             current
    elementLocation     <- SourceLoc.clang_getCursorExtent current
    elementKind         <- clang_getCursorKindSpelling =<<
                                      clang_getCursorKind  current
    elementTypeKind     <- clang_getTypeKindSpelling . cxtKind =<<
                                      clang_getCursorType  current
    elementRawComment   <- clang_Cursor_getRawCommentText  current
    elementIsAnonymous  <- clang_Cursor_isAnonymous        current
    elementIsDefinition <- clang_isCursorDefinition        current

    return Element {
        elementName
      , elementLocation
      , elementKind
      , elementTypeKind
      , elementRawComment
      , elementIsAnonymous
      , elementIsDefinition
      }

{-------------------------------------------------------------------------------
  Fold proper
-------------------------------------------------------------------------------}

-- | Fold that returns the raw @libclang@ AST
--
-- We can use this at the top-level in 'dumpClangAST', but it is also useful
-- more locally when trying to figure out what the ATS looks like underneath
-- a certain node. For example, suppose we are working on `foldTyp`, and we're
-- not exactly sure what the @struct@ case looks like; we might temporarily use
--
-- > foldTyp :: Tracer IO ParseMsg -> Fold Typ
-- > foldTyp tracer current = do
-- >     cursorType <- clang_getCursorType current
-- >     case fromSimpleEnum $ cxtKind cursorType of
-- >       Right CXType_Record -> do
-- >         return $ Recurse foldRaw $ \t -> print t >> return Nothing
--
-- to see the AST under the @struct@ parent node.
foldRaw :: Predicate -> Fold Identity (Tree Element)
foldRaw p = checkPredicate nullTracer p $ recurse mkElement
