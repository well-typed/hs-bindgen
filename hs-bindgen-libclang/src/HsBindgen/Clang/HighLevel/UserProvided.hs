-- * Distinguish user-provided from @libclang@-provided values
module HsBindgen.Clang.HighLevel.UserProvided (
    UserProvided(..)
  , getUserProvided
  , clang_getCursorSpelling
  ) where

import Data.Text (Text)

import HsBindgen.Clang.LowLevel.Core hiding (clang_getCursorSpelling)
import HsBindgen.Clang.LowLevel.Core qualified as Core

{-------------------------------------------------------------------------------
  Distinguish user-provided from @libclang@-provided values
-------------------------------------------------------------------------------}

data UserProvided a = UserProvided !a | LibclangProvided !a
  deriving stock (Show)

getUserProvided :: UserProvided a -> Maybe a
getUserProvided (UserProvided     x) = Just x
getUserProvided (LibclangProvided _) = Nothing

-- | Get the user supplied name for the node at the cursor, if any
--
-- Given
--
-- > typedef struct {
-- >     char a;
-- > } S3_t;
--
-- @libclang@ fills in a name (\"spelling\") for the struct tag, even though the
-- user did not provide one; recent versions of @llvm@ fill in @S3_t@ (@""@ in
-- older versions).
clang_getCursorSpelling :: CXCursor -> IO (UserProvided Text)
clang_getCursorSpelling cursor = do
    nameSpelling <- Core.clang_getCursorSpelling cursor

    -- We could /ask/ for the @unit@ to be given to us, but the call to
    -- 'clang_getCursorSpelling' is a useful check; for example, it may reveal
    -- that the result of a call to 'clang_getTypeDeclaration' is not a cursor
    -- for which we have a translation unit, and hence not one on which we can
    -- call 'clang_Cursor_getSpellingNameRange'.
    mUnit <- clang_Cursor_getTranslationUnit cursor
    case mUnit of
      Nothing ->
        return $ LibclangProvided nameSpelling
      Just unit -> do
        nameRange  <- clang_Cursor_getSpellingNameRange cursor 0 0
        nameStart  <- clang_getRangeStart nameRange
        mNameToken <- clang_getToken unit nameStart

        case mNameToken of
          Just nameToken -> do
            nameTokenSpelling <- clang_getTokenSpelling unit nameToken
            return $ if nameSpelling == nameTokenSpelling
                       then UserProvided nameSpelling
                       else LibclangProvided nameSpelling
          Nothing ->
            return $ LibclangProvided nameSpelling
