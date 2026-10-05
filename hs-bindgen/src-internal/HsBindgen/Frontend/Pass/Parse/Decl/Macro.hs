-- | Parse functions related to reparsing
module HsBindgen.Frontend.Pass.Parse.Decl.Macro (
    getReparseInfo
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty (NonEmpty)
import Foreign.C.Types (CUInt)

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (MultiLoc (multiLocExpansion), Range (rangeEnd),
                              SingleLoc (singleLocColumn, singleLocLine))
import Clang.LowLevel.Core (CXCursor, CXSourceRange, CXTranslationUnit,
                            clang_getCursorExtent, clang_getExpansionLocation,
                            clang_getLocation, clang_getRange,
                            clang_getRangeEnd, clang_getRangeStart)

import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo (..), Tokens)
import HsBindgen.Frontend.Pass.Parse.Monad.Decl (ParseDecl, getMacroExpansions,
                                                 getTranslationUnit)
import HsBindgen.Macro.Syntax (MacroInvocation (..))

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
        range  <- toExpansionRange unit macroExpansions
                    =<< clang_getCursorExtent curr
        tokens <- HighLevel.clang_tokenize unit range
        pure $ ReparseNeeded tokens macroExpansions

-- | Move both ends of a range to their expansion locations
--
-- An expansion location is the outermost expansion site, and so always lies in
-- a file: the bodies of @-D@ macros in the predefines buffer cannot contain
-- declarations.
--
-- A range ending in a macro argument, such as @T f PARAMS((T a))@ after
-- @#define PARAMS(args) args@, ends at the expansion location of the argument,
-- the start of the invocation; we extend the range to the end of the
-- invocation.
toExpansionRange ::
     MonadIO m
  => CXTranslationUnit
  -> NonEmpty MacroInvocation  -- ^ Macro invocations within the range
  -> CXSourceRange
  -> m CXSourceRange
toExpansionRange unit invocations range = do
    (startFile, startLine, startColumn, _) <-
      clang_getExpansionLocation =<< clang_getRangeStart range
    (endFile, endLine, endColumn, _) <-
      clang_getExpansionLocation =<< clang_getRangeEnd range
    let (endLine', endColumn') =
          max (endLine, endColumn) $ maximum $ invocationEnd <$> invocations
    start <- clang_getLocation unit startFile startLine  startColumn
    end   <- clang_getLocation unit endFile   endLine'   endColumn'
    clang_getRange start end
  where
    invocationEnd :: MacroInvocation -> (CUInt, CUInt)
    invocationEnd invocation = (
        fromIntegral loc.singleLocLine
      , fromIntegral loc.singleLocColumn
      )
      where
        loc = invocation.locRange.rangeEnd.multiLocExpansion
