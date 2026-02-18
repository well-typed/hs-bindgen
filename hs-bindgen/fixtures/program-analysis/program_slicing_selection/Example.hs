{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @enum FileOperationStatus@

    __defined at:__ @program-analysis\/program_slicing_selection.h 7:6@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
newtype FileOperationStatus = FileOperationStatus
  { unwrapFileOperationStatus :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize FileOperationStatus where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw FileOperationStatus where

  readRaw =
    \ptr0 ->
          pure FileOperationStatus
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw FileOperationStatus where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          FileOperationStatus unwrapFileOperationStatus2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapFileOperationStatus2

deriving via Marshal.EquivStorable FileOperationStatus instance RIP.Storable FileOperationStatus

deriving via RIP.CInt instance RIP.Prim FileOperationStatus

instance CEnum.CEnum FileOperationStatus where

  type CEnumZ FileOperationStatus = RIP.CInt

  toCEnum = FileOperationStatus

  fromCEnum = unwrapFileOperationStatus

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (-1, RIP.singleton "CUSTOM_ERROR_OTHER")
                                   , (0, RIP.singleton "SUCCESS")
                                   , (2, RIP.singleton "NOT_FOUND")
                                   , (12, RIP.singleton "OUT_OF_MEMORY")
                                   , (13, RIP.singleton "PERMISSION_DENIED")
                                   , (22, RIP.singleton "INVALID_ARGUMENT")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "FileOperationStatus"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "FileOperationStatus"

instance Show FileOperationStatus where

  showsPrec = CEnum.shows

instance Read FileOperationStatus where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapFileOperationStatus" (RIP.Ptr FileOperationStatus) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFileOperationStatus")

instance HasCField.HasCField FileOperationStatus "unwrapFileOperationStatus" where

  type CFieldType FileOperationStatus "unwrapFileOperationStatus" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SUCCESS@

    __defined at:__ @program-analysis\/program_slicing_selection.h 8:3@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
pattern SUCCESS :: FileOperationStatus
pattern SUCCESS = FileOperationStatus 0

{-| __C declaration:__ @NOT_FOUND@

    __defined at:__ @program-analysis\/program_slicing_selection.h 9:3@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
pattern NOT_FOUND :: FileOperationStatus
pattern NOT_FOUND = FileOperationStatus 2

{-| __C declaration:__ @PERMISSION_DENIED@

    __defined at:__ @program-analysis\/program_slicing_selection.h 10:3@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
pattern PERMISSION_DENIED :: FileOperationStatus
pattern PERMISSION_DENIED = FileOperationStatus 13

{-| __C declaration:__ @INVALID_ARGUMENT@

    __defined at:__ @program-analysis\/program_slicing_selection.h 11:3@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
pattern INVALID_ARGUMENT :: FileOperationStatus
pattern INVALID_ARGUMENT = FileOperationStatus 22

{-| __C declaration:__ @OUT_OF_MEMORY@

    __defined at:__ @program-analysis\/program_slicing_selection.h 12:3@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
pattern OUT_OF_MEMORY :: FileOperationStatus
pattern OUT_OF_MEMORY = FileOperationStatus 12

{-| __C declaration:__ @CUSTOM_ERROR_OTHER@

    __defined at:__ @program-analysis\/program_slicing_selection.h 13:3@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
pattern CUSTOM_ERROR_OTHER :: FileOperationStatus
pattern CUSTOM_ERROR_OTHER = FileOperationStatus (-1)

{-| __C declaration:__ @struct FileOperationRecord@

    __defined at:__ @program-analysis\/program_slicing_selection.h 16:8@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
data FileOperationRecord = FileOperationRecord
  { fileOperationRecord_status :: FileOperationStatus
    {- ^ __C declaration:__ @status@

         __defined at:__ @program-analysis\/program_slicing_selection.h 17:28@

         __exported by:__ @program-analysis\/program_slicing_selection.h@
    -}
  , fileOperationRecord_bytes_processed :: HsBindgen.Runtime.LibC.CSize
    {- ^ __C declaration:__ @bytes_processed@

         __defined at:__ @program-analysis\/program_slicing_selection.h 18:10@

         __exported by:__ @program-analysis\/program_slicing_selection.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize FileOperationRecord where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw FileOperationRecord where

  readRaw =
    \ptr0 ->
          pure FileOperationRecord
      <*> HasCField.readRaw (RIP.Proxy @"fileOperationRecord_status") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"fileOperationRecord_bytes_processed") ptr0

instance Marshal.WriteRaw FileOperationRecord where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          FileOperationRecord
            fileOperationRecord_status2
            fileOperationRecord_bytes_processed3 ->
                 HasCField.writeRaw (RIP.Proxy @"fileOperationRecord_status") ptr0 fileOperationRecord_status2
              >> HasCField.writeRaw (RIP.Proxy @"fileOperationRecord_bytes_processed") ptr0 fileOperationRecord_bytes_processed3

deriving via Marshal.EquivStorable FileOperationRecord instance RIP.Storable FileOperationRecord

instance HasCField.HasCField FileOperationRecord "fileOperationRecord_status" where

  type CFieldType FileOperationRecord "fileOperationRecord_status" =
    FileOperationStatus

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) FileOperationStatus
         ) => RIP.HasField "fileOperationRecord_status" (RIP.Ptr FileOperationRecord) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"fileOperationRecord_status")

instance HasCField.HasCField FileOperationRecord "fileOperationRecord_bytes_processed" where

  type CFieldType FileOperationRecord "fileOperationRecord_bytes_processed" =
    HsBindgen.Runtime.LibC.CSize

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) HsBindgen.Runtime.LibC.CSize
         ) => RIP.HasField "fileOperationRecord_bytes_processed" (RIP.Ptr FileOperationRecord) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"fileOperationRecord_bytes_processed")
