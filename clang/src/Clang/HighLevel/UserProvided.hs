{-# LANGUAGE MultiWayIf #-}

-- * Distinguish user-provided from @libclang@-provided values
module Clang.HighLevel.UserProvided (
    CursorSpelling(..)
  , clang_getCursorSpelling
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Foreign.C

import Clang.Internal.Results (isNullPtr)
import Clang.LowLevel.Core hiding (clang_getCursorSpelling)
import Clang.LowLevel.Core qualified as Core
import Clang.LowLevel.Core.Pointers

{-------------------------------------------------------------------------------
  Distinguish user-provided from @libclang@-provided values
-------------------------------------------------------------------------------}

data CursorSpelling =
    -- | User provided string (one that actually appears in the source code)
    UserProvided Text

    -- | Clang-generated string (which does not appear in the source code)
  | ClangGenerated Text

    -- | This is a name of a clang built-in
  | ClangBuiltin Text
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
clang_getCursorSpelling :: forall m. MonadIO m => CXCursor -> m CursorSpelling
clang_getCursorSpelling cursor =
    either ClangGenerated id <$> runExceptT getUserProvided
  where
    getUserProvided :: ExceptT Text m CursorSpelling
    getUserProvided = do
        nameSpelling <- Core.clang_getCursorSpelling cursor

        -- We look for the token in the source code at the location of the name.
        -- If we fail to get this token, we conclude the name must be generated.
        let elseIsGen :: Maybe a -> ExceptT Text m a
            elseIsGen (Just x) = return x
            elseIsGen Nothing  = throwError nameSpelling

        -- We could /ask/ for the @unit@ to be given to us, but the call to
        -- 'clang_getCursorSpelling' is a useful check; for example, it may
        -- reveal that the result of a call to 'clang_getTypeDeclaration' is not
        -- a cursor for which we have a translation unit, and hence not one on
        -- which we can call 'clang_Cursor_getSpellingNameRange'.
        unit      <- clang_Cursor_getTranslationUnit cursor       >>= elseIsGen
        range     <- clang_Cursor_getSpellingNameRange cursor 0 0 >>= elseIsGen
        start     <- clang_getRangeStart range
        expansion <- clang_getExpansionLocation start
        spelling  <- clang_getSpellingLocation  start

        if
            -- Check for builtins
            --
            -- One example of this is
            --
            -- > typedef __builtin_va_list foo;
            --
            -- where @__builtin_va_list@ is a \"predefined typedef\".
            -- See <https://clang.llvm.org/docs/LanguageExtensions.html#variadic-function-builtins>.
          | isNullPtr (fileOf expansion) ->
              return $ ClangBuiltin nameSpelling

            -- If the expansion location and the spelling location /of the name/
            -- are different, this means that the name is constructed using a
            -- macro. This must therefore have been done by the user.
          | locationOf expansion /= locationOf spelling ->
              return $ UserProvided nameSpelling

            -- Otherwise, check the token at the expansion location. If it
            -- matches the spelling reported by clang, it was user provided.
          | otherwise -> do
              token         <- clang_getToken unit start >>= elseIsGen
              tokenSpelling <- clang_getTokenSpelling unit token
              if nameSpelling /= tokenSpelling
                then throwError nameSpelling
                else return $ UserProvided nameSpelling

    fileOf :: (CXFile, CUInt, CUInt, CUInt) -> CXFile
    fileOf (file, _col, _line, _offset) = file

    locationOf :: (CXFile, CUInt, CUInt, CUInt) -> (CUInt, CUInt)
    locationOf (_file, col, line, _offset) = (col, line)
