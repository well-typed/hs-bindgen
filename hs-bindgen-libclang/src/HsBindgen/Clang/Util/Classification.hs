-- | Classification of various parts of the @libclang@ AST
--
-- Intended for unqualified import.
module HsBindgen.Clang.Util.Classification (
    -- * Classifying types
    isPointerType
  , isRecordType
    -- * Distinguish user-provided from @libclang@-provided values
  , UserProvided(..)
  , getUserProvided
  , getUserProvidedName
  ) where

import Data.ByteString (ByteString)

import HsBindgen.Clang.Core
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Classifying types
-------------------------------------------------------------------------------}

-- | Check if this is a pointer type
--
-- Pointer types are types for which we can call 'clang_getPointeeType'.
isPointerType :: SimpleEnum CXTypeKind -> Bool
isPointerType = either (const False) aux . fromSimpleEnum
  where
    aux :: CXTypeKind -> Bool
    aux CXType_Pointer         = True
    aux CXType_LValueReference = True
    aux CXType_RValueReference = True
    aux _                      = False

isRecordType :: SimpleEnum CXTypeKind -> Bool
isRecordType = (== Right CXType_Record) . fromSimpleEnum

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
getUserProvidedName ::
     CXTranslationUnit
  -> CXCursor
  -> IO (UserProvided ByteString)
getUserProvidedName unit cursor = do
    nameSpelling      <- clang_getCursorSpelling cursor
    nameRange         <- clang_Cursor_getSpellingNameRange cursor 0 0
    nameStart         <- clang_getRangeStart nameRange
    nameToken         <- clang_getToken unit nameStart
    nameTokenSpelling <- clang_getTokenSpelling unit nameToken
    return $ if nameSpelling == nameTokenSpelling
               then UserProvided nameSpelling
               else LibclangProvided nameSpelling
