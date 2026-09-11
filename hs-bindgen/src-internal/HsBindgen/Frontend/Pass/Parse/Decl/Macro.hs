-- | Parse functions related to reparsing
module HsBindgen.Frontend.Pass.Parse.Decl.Macro (
    getReparseInfo
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (MultiLoc (multiLocExpansion))
import Clang.LowLevel.Core (CXCursor)
import Clang.Paths (getRealPathText)

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
        tokens <- HighLevel.clang_tokenize unit getRealPathText extent
        pure $ ReparseNeeded tokens macroExpansions
