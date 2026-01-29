{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.LibC
import qualified Text.Read
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @enum FileOperationStatus@

    __defined at:__ @program-analysis\/program_slicing_selection.h 7:6@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
newtype FileOperationStatus = FileOperationStatus
  { unwrapFileOperationStatus :: FC.CInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

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
          FileOperationStatus unwrapFileOperationStatus2 ->
            F.pokeByteOff ptr0 (0 :: Int) unwrapFileOperationStatus2

deriving via FC.CInt instance Data.Primitive.Types.Prim FileOperationStatus

instance HsBindgen.Runtime.CEnum.CEnum FileOperationStatus where

  type CEnumZ FileOperationStatus = FC.CInt

  toCEnum = FileOperationStatus

  fromCEnum = unwrapFileOperationStatus

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

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read FileOperationStatus where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FileOperationStatus) "unwrapFileOperationStatus")
         ) => GHC.Records.HasField "unwrapFileOperationStatus" (Ptr.Ptr FileOperationStatus) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFileOperationStatus")

instance HsBindgen.Runtime.HasCField.HasCField FileOperationStatus "unwrapFileOperationStatus" where

  type CFieldType FileOperationStatus "unwrapFileOperationStatus" =
    FC.CInt

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
  deriving stock (Eq, Show)

instance F.Storable FileOperationRecord where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure FileOperationRecord
      <*> HsBindgen.Runtime.HasCField.peek (Data.Proxy.Proxy @"fileOperationRecord_status") ptr0
      <*> HsBindgen.Runtime.HasCField.peek (Data.Proxy.Proxy @"fileOperationRecord_bytes_processed") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          FileOperationRecord
            fileOperationRecord_status2
            fileOperationRecord_bytes_processed3 ->
                 HsBindgen.Runtime.HasCField.poke (Data.Proxy.Proxy @"fileOperationRecord_status") ptr0 fileOperationRecord_status2
              >> HsBindgen.Runtime.HasCField.poke (Data.Proxy.Proxy @"fileOperationRecord_bytes_processed") ptr0 fileOperationRecord_bytes_processed3

instance HsBindgen.Runtime.HasCField.HasCField FileOperationRecord "fileOperationRecord_status" where

  type CFieldType FileOperationRecord "fileOperationRecord_status" =
    FileOperationStatus

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FileOperationRecord) "fileOperationRecord_status")
         ) => GHC.Records.HasField "fileOperationRecord_status" (Ptr.Ptr FileOperationRecord) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"fileOperationRecord_status")

instance HsBindgen.Runtime.HasCField.HasCField FileOperationRecord "fileOperationRecord_bytes_processed" where

  type CFieldType FileOperationRecord "fileOperationRecord_bytes_processed" =
    HsBindgen.Runtime.LibC.CSize

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FileOperationRecord) "fileOperationRecord_bytes_processed")
         ) => GHC.Records.HasField "fileOperationRecord_bytes_processed" (Ptr.Ptr FileOperationRecord) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"fileOperationRecord_bytes_processed")
