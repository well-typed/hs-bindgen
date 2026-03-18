-- | Parse functions related to reparsing
module HsBindgen.Frontend.Pass.Parse.Decl.Reparse (
    getReparseInfo
  ) where

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types (MultiLoc (multiLocExpansion))
import Clang.LowLevel.Core (CXCursor)

import HsBindgen.Frontend.Pass.Parse.IsPass (ReparseInfo (..))
import HsBindgen.Frontend.Pass.Parse.Monad.Decl (ParseDecl,
                                                 checkHasMacroExpansion,
                                                 getTranslationUnit)

getReparseInfo :: CXCursor -> ParseDecl ReparseInfo
getReparseInfo = \curr -> do
    extent <- fmap multiLocExpansion <$> HighLevel.clang_getCursorExtent curr
    hasMacroExpansion <- checkHasMacroExpansion extent
    if hasMacroExpansion then do
      unit <- getTranslationUnit
      ReparseNeeded <$> HighLevel.clang_tokenize unit extent
    else
      return ReparseNotNeeded
