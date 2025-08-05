{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Data.List.NonEmpty
import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Eq, IO, Int, Ord, Read, Show, pure, showsPrec)
import qualified Text.Read

$(CAPI.addCSource "#include <program_slicing_selection.h>\nenum FileOperationStatus testmodule_read_file_chunk (FILE *arg1, void *arg2, size_t arg3) { return read_file_chunk(arg1, arg2, arg3); }\n")

newtype FileOperationStatus = FileOperationStatus
  { un_FileOperationStatus :: FC.CInt
  }
  deriving stock (Eq, Ord)

instance F.Storable FileOperationStatus where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure FileOperationStatus
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          FileOperationStatus un_FileOperationStatus2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_FileOperationStatus2

instance HsBindgen.Runtime.CEnum.CEnum FileOperationStatus where

  type CEnumZ FileOperationStatus = FC.CInt

  toCEnum = FileOperationStatus

  fromCEnum = un_FileOperationStatus

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (-1, Data.List.NonEmpty.singleton "CUSTOM_ERROR_OTHER")
                                                     , (0, Data.List.NonEmpty.singleton "SUCCESS")
                                                     , (2, Data.List.NonEmpty.singleton "NOT_FOUND")
                                                     , (12, Data.List.NonEmpty.singleton "OUT_OF_MEMORY")
                                                     , (13, Data.List.NonEmpty.singleton "PERMISSION_DENIED")
                                                     , (22, Data.List.NonEmpty.singleton "INVALID_ARGUMENT")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "FileOperationStatus"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "FileOperationStatus"

instance Show FileOperationStatus where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read FileOperationStatus where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern SUCCESS :: FileOperationStatus
pattern SUCCESS = FileOperationStatus 0

pattern NOT_FOUND :: FileOperationStatus
pattern NOT_FOUND = FileOperationStatus 2

pattern PERMISSION_DENIED :: FileOperationStatus
pattern PERMISSION_DENIED = FileOperationStatus 13

pattern INVALID_ARGUMENT :: FileOperationStatus
pattern INVALID_ARGUMENT = FileOperationStatus 22

pattern OUT_OF_MEMORY :: FileOperationStatus
pattern OUT_OF_MEMORY = FileOperationStatus 12

pattern CUSTOM_ERROR_OTHER :: FileOperationStatus
pattern CUSTOM_ERROR_OTHER = FileOperationStatus (-1)

data FileOperationRecord = FileOperationRecord
  { fileOperationRecord_status :: FileOperationStatus
  , fileOperationRecord_bytes_processed :: HsBindgen.Runtime.Prelude.CSize
  }
  deriving stock (Eq, Show)

instance F.Storable FileOperationRecord where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure FileOperationRecord
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          FileOperationRecord
            fileOperationRecord_status2
            fileOperationRecord_bytes_processed3 ->
                 F.pokeByteOff ptr0 (0 :: Int) fileOperationRecord_status2
              >> F.pokeByteOff ptr0 (8 :: Int) fileOperationRecord_bytes_processed3

foreign import ccall safe "testmodule_read_file_chunk" read_file_chunk :: (F.Ptr HsBindgen.Runtime.Prelude.CFile) -> (F.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO FileOperationStatus
