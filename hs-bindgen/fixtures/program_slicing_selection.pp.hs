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

$(CAPI.addCSource "#include <program_slicing_selection.h>\nenum FileOperationStatus hs_bindgen_test_program_slicing_selection_2e587488135cbef3 (FILE *arg1, void *arg2, size_t arg3) { return read_file_chunk(arg1, arg2, arg3); }\n")

{-| __/Automatically generated from C/__

    __C declaration:__ @FileOperationStatus@

    __defined at:__ @program_slicing_selection.h:7:6@

    __exported by:__ @program_slicing_selection.h@
-}
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

{-| __/Automatically generated from C/__

    __C declaration:__ @SUCCESS@

    __defined at:__ @program_slicing_selection.h:8:3@

    __exported by:__ @program_slicing_selection.h@
-}
pattern SUCCESS :: FileOperationStatus
pattern SUCCESS = FileOperationStatus 0

{-| __/Automatically generated from C/__

    __C declaration:__ @NOT_FOUND@

    __defined at:__ @program_slicing_selection.h:9:3@

    __exported by:__ @program_slicing_selection.h@
-}
pattern NOT_FOUND :: FileOperationStatus
pattern NOT_FOUND = FileOperationStatus 2

{-| __/Automatically generated from C/__

    __C declaration:__ @PERMISSION_DENIED@

    __defined at:__ @program_slicing_selection.h:10:3@

    __exported by:__ @program_slicing_selection.h@
-}
pattern PERMISSION_DENIED :: FileOperationStatus
pattern PERMISSION_DENIED = FileOperationStatus 13

{-| __/Automatically generated from C/__

    __C declaration:__ @INVALID_ARGUMENT@

    __defined at:__ @program_slicing_selection.h:11:3@

    __exported by:__ @program_slicing_selection.h@
-}
pattern INVALID_ARGUMENT :: FileOperationStatus
pattern INVALID_ARGUMENT = FileOperationStatus 22

{-| __/Automatically generated from C/__

    __C declaration:__ @OUT_OF_MEMORY@

    __defined at:__ @program_slicing_selection.h:12:3@

    __exported by:__ @program_slicing_selection.h@
-}
pattern OUT_OF_MEMORY :: FileOperationStatus
pattern OUT_OF_MEMORY = FileOperationStatus 12

{-| __/Automatically generated from C/__

    __C declaration:__ @CUSTOM_ERROR_OTHER@

    __defined at:__ @program_slicing_selection.h:13:3@

    __exported by:__ @program_slicing_selection.h@
-}
pattern CUSTOM_ERROR_OTHER :: FileOperationStatus
pattern CUSTOM_ERROR_OTHER = FileOperationStatus (-1)

{-| __/Automatically generated from C/__

    __C declaration:__ @FileOperationRecord@

    __defined at:__ @program_slicing_selection.h:16:8@

    __exported by:__ @program_slicing_selection.h@
-}
data FileOperationRecord = FileOperationRecord
  { fileOperationRecord_status :: FileOperationStatus
    {- ^ __/Automatically generated from C/__

         __C declaration:__ @status@

         __defined at:__ @program_slicing_selection.h:17:28@

         __exported by:__ @program_slicing_selection.h@
    -}
  , fileOperationRecord_bytes_processed :: HsBindgen.Runtime.Prelude.CSize
    {- ^ __/Automatically generated from C/__

         __C declaration:__ @bytes_processed@

         __defined at:__ @program_slicing_selection.h:18:10@

         __exported by:__ @program_slicing_selection.h@
    -}
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

{-| __/Automatically generated from C/__

    __C declaration:__ @read_file_chunk@

    __defined at:__ @program_slicing_selection.h:21:26@

    __exported by:__ @program_slicing_selection.h@
-}
foreign import ccall safe "hs_bindgen_test_program_slicing_selection_2e587488135cbef3" read_file_chunk
  :: F.Ptr HsBindgen.Runtime.Prelude.CFile
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @file_ptr@
     -}
  -> F.Ptr Void
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @buffer@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @bytes_to_read@
     -}
  -> IO FileOperationStatus
