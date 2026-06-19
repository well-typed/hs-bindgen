module HsBindgen.Frontend.Pass.Parse.Builtin (
    checkIsBuiltin
  ) where

import Clang.LowLevel.Core

import HsBindgen.Imports

-- | Check for built-in definitions
checkIsBuiltin :: MonadIO m => CXCursor -> m (Maybe Text)
checkIsBuiltin curr = do
    mRange <- clang_Cursor_getSpellingNameRange curr 0 0
    case mRange of
      Nothing    -> return Nothing
      Just range -> do
        start <- clang_getRangeStart range
        (file, _col, _line, _offset) <- clang_getExpansionLocation start
        if isNullPtr file
          then Just <$> clang_getCursorSpelling curr
          else return Nothing
