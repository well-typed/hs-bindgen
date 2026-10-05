-- | Parse functions related to reparsing
module HsBindgen.Frontend.Pass.Parse.Decl.Macro (
    getReparseInfo
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (MultiLoc (..), Range (..), RealPath,
                              SingleLoc (..))
import Clang.LowLevel.Core (CXCursor, CXFile, CXSourceRange, CXTranslationUnit,
                            clang_getCursorExtent, clang_getExpansionLocation,
                            clang_getLocation, clang_getRange,
                            clang_getRangeStart)

import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo (..), Tokens)
import HsBindgen.Frontend.Pass.Parse.Monad.Decl (ParseDecl, getMacroExpansions,
                                                 getMacroExpansionsAt,
                                                 getTranslationUnit)
import HsBindgen.Macro.Syntax (MacroInvocation (..))


-- | Do we need to reparse a declaration because it contains macros that were
-- expanded? If so, what are the original tokens of the declaration before
-- macro expansion?
--
-- See "Reparsing: the source range of a declaration" in @dev/macros.md@.
getReparseInfo :: CXCursor -> ParseDecl (ReparseInfo Tokens)
getReparseInfo = \curr -> do
    range <- getFileRange =<< HighLevel.clang_getCursorExtent curr
    macroExpansionsMay <- getMacroExpansions range
    case macroExpansionsMay of
      Nothing -> pure ReparseNotNeeded
      Just macroExpansions -> do
        unit   <- getTranslationUnit
        -- An expansion location always lies in a file: the bodies of @-D@
        -- macros in the predefines buffer cannot contain declarations.
        (file, _, _, _) <- clang_getExpansionLocation
                             =<< clang_getRangeStart
                             =<< clang_getCursorExtent curr
        tokens <- HighLevel.clang_tokenize unit =<< toSourceRange unit file range
        pure $ ReparseNeeded tokens macroExpansions

-- | The range a declaration occupies in its file
--
-- A declaration starting with a macro, such as the field @T x;@ after
-- @#define T int@, starts in the macro definition; the expansion location is
-- the invocation @T@. The same holds for an end in a macro body.
--
-- A declaration ending in a macro argument, such as @T f PARAMS((T a))@ after
-- @#define PARAMS(args) args@, ends in the file, inside the invocation, and the
-- expansion location is the /start/ of the invocation; we extend the range to
-- its end. Only then does the range contain the invocations nested in the
-- argument.
getFileRange :: Range (MultiLoc RealPath) -> ParseDecl (Range (SingleLoc RealPath))
getFileRange extent = case extent.rangeEnd.multiLocFile of
    -- 'Nothing' if the file location equals the expansion location, which holds
    -- outside macro arguments, or if it has no real path
    Nothing -> pure expansion
    Just _  -> do
      invocationsMay <- getMacroExpansionsAt expansion.rangeEnd
      pure $ case invocationsMay of
        Nothing          -> expansion
        Just invocations -> expansion{
            rangeEnd = maximum $ invocationEnd <$> invocations
          }
  where
    expansion :: Range (SingleLoc RealPath)
    expansion = multiLocExpansion <$> extent

    invocationEnd :: MacroInvocation -> SingleLoc RealPath
    invocationEnd invocation = invocation.locRange.rangeEnd.multiLocExpansion

-- | Convert a range in the given file to a 'CXSourceRange'
toSourceRange ::
     CXTranslationUnit
  -> CXFile
  -> Range (SingleLoc RealPath)
  -> ParseDecl CXSourceRange
toSourceRange unit file range = do
    let toLocation loc = clang_getLocation unit file
          (fromIntegral loc.singleLocLine)
          (fromIntegral loc.singleLocColumn)
    start <- toLocation range.rangeStart
    end   <- toLocation range.rangeEnd
    clang_getRange start end
