-- | Parse functions related to reparsing
module HsBindgen.Frontend.Pass.Parse.Decl.Macro (
    getReparseInfo
  ) where

import Control.Monad.IO.Class (MonadIO)

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (MultiLoc (multiLocExpansion))
import Clang.LowLevel.Core (CXCursor, CXSourceRange, CXTranslationUnit,
                            clang_getCursorExtent, clang_getExpansionLocation,
                            clang_getLocation, clang_getRange,
                            clang_getRangeEnd, clang_getRangeStart)

import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo (..), Tokens)
import HsBindgen.Frontend.Pass.Parse.Monad.Decl (ParseDecl, getMacroExpansions,
                                                 getTranslationUnit)

getReparseInfo :: CXCursor -> ParseDecl (ReparseInfo Tokens)
getReparseInfo = \curr -> do
    extent <- fmap multiLocExpansion <$> HighLevel.clang_getCursorExtent curr
    macroExpansionsMay <- getMacroExpansions extent
    case macroExpansionsMay of
      Nothing -> pure ReparseNotNeeded
      Just macroExpansions -> do
        unit   <- getTranslationUnit
        -- A declaration starting with a macro, such as the field @T x;@ after
        -- @#define T int@, starts in the macro definition; the expansion range
        -- yields @T x@ instead of @int T x@.
        range  <- toExpansionRange unit =<< clang_getCursorExtent curr
        tokens <- HighLevel.clang_tokenize unit range
        pure $ ReparseNeeded tokens macroExpansions

-- | Move both ends of a range to their expansion locations
--
-- An expansion location is the outermost expansion site, and so always lies in
-- a file: the bodies of @-D@ macros in the predefines buffer cannot contain
-- declarations.
toExpansionRange ::
     MonadIO m
  => CXTranslationUnit -> CXSourceRange -> m CXSourceRange
toExpansionRange unit range = do
    start <- toExpansion =<< clang_getRangeStart range
    end   <- toExpansion =<< clang_getRangeEnd   range
    clang_getRange start end
  where
    toExpansion loc = do
      (file, line, column, _offset) <- clang_getExpansionLocation loc
      clang_getLocation unit file line column
