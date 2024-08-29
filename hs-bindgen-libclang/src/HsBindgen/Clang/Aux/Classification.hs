-- | Classification of various parts of the @libclang@ AST
module HsBindgen.Clang.Aux.Classification (
    -- * Classifying types
    isPointerType
  , isRecordType
  ) where

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