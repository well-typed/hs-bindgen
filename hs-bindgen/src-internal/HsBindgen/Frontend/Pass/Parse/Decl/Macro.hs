-- | Parse functions related to reparsing
module HsBindgen.Frontend.Pass.Parse.Decl.Macro (
    getReparseInfo
  ) where

import Data.Set qualified as Set

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (MultiLoc (multiLocExpansion))
import Clang.LowLevel.Core (CXCursor)

import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo (..))
import HsBindgen.Frontend.Pass.Parse.Monad.Decl (ParseDecl, getMacroExpansions,
                                                 getTranslationUnit)

getReparseInfo :: CXCursor -> ParseDecl ReparseInfo
getReparseInfo = \curr -> do
    extent <- fmap multiLocExpansion <$> HighLevel.clang_getCursorExtent curr
    macroExpansions <- getMacroExpansions extent
    if Set.null macroExpansions; then
      pure ReparseNotNeeded
    else do
      unit <- getTranslationUnit
      ReparseNeeded <$>
        HighLevel.clang_tokenize unit extent <*> pure macroExpansions
