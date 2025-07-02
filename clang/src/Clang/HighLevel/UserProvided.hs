-- * Distinguish user-provided from @libclang@-provided values
module Clang.HighLevel.UserProvided (
    UserProvided(..)
  , ClangGenerated(..)
  , clang_getCursorSpelling
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Foreign.C

import Clang.LowLevel.Core hiding (clang_getCursorSpelling)
import Clang.LowLevel.Core qualified as Core

{-------------------------------------------------------------------------------
  Distinguish user-provided from @libclang@-provided values
-------------------------------------------------------------------------------}

-- | User provided string (one that actually appears in the source code)
newtype UserProvided = UserProvided Text
  deriving stock (Show, Eq, Ord)

-- | Clang-generated string (which does not appear in the source code)
newtype ClangGenerated = ClangGenerated Text
  deriving stock (Show, Eq, Ord)

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
clang_getCursorSpelling :: forall m.
     MonadIO m
  => CXCursor
  -> m (Either ClangGenerated UserProvided)
clang_getCursorSpelling cursor =
    runExceptT getUserProvided
  where
    getUserProvided :: ExceptT ClangGenerated m UserProvided
    getUserProvided = do
        nameSpelling <- Core.clang_getCursorSpelling cursor

        -- We look for the token in the source code at the location of the name.
        -- If we fail to get this token, we conclude the name must be generated.
        let elseIsGen :: Maybe a -> ExceptT ClangGenerated m a
            elseIsGen (Just x) = return x
            elseIsGen Nothing  = throwError $ ClangGenerated nameSpelling

        -- We could /ask/ for the @unit@ to be given to us, but the call to
        -- 'clang_getCursorSpelling' is a useful check; for example, it may
        -- reveal that the result of a call to 'clang_getTypeDeclaration' is not
        -- a cursor for which we have a translation unit, and hence not one on
        -- which we can call 'clang_Cursor_getSpellingNameRange'.
        unit      <- clang_Cursor_getTranslationUnit cursor       >>= elseIsGen
        range     <- clang_Cursor_getSpellingNameRange cursor 0 0 >>= elseIsGen
        start     <- clang_getRangeStart range
        expansion <- colAndLineNo <$> clang_getExpansionLocation start
        spelling  <- colAndLineNo <$> clang_getSpellingLocation  start

        if expansion /= spelling then
          -- If the expansion location and the spelling location /of the name/
          -- are different, this means that the name is constructed using a
          -- macro. This must therefore have been done by the user.
          return $ UserProvided nameSpelling
        else do
          -- Otherwise, check the token at the expansion location. If it matches
          -- the spelling reported by clang, it was user provided.
          token         <- clang_getToken unit start >>= elseIsGen
          tokenSpelling <- clang_getTokenSpelling unit token
          if nameSpelling /= tokenSpelling
            then throwError $ ClangGenerated nameSpelling
            else return $ UserProvided nameSpelling

    colAndLineNo :: (CXFile, CUInt, CUInt, CUInt) -> (CUInt, CUInt)
    colAndLineNo (_file, col, line, _offset) = (col, line)
