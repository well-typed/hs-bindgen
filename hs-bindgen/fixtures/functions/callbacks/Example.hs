{-# LANGUAGE CApiFFI #-}
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

import qualified Data.Array.Byte
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.SizedByteArray
import qualified Text.Read
import Data.Bits (FiniteBits)
import GHC.Prim ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-| Auxiliary type used by 'FileOpenedNotification'

__C declaration:__ @FileOpenedNotification@

__defined at:__ @functions\/callbacks.h 10:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype FileOpenedNotification_Aux = FileOpenedNotification_Aux
  { un_FileOpenedNotification_Aux :: IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

-- __unique:__ @toFileOpenedNotification_Aux@
foreign import ccall safe "wrapper" hs_bindgen_b3b8b1fad168671a ::
     FileOpenedNotification_Aux
  -> IO (Ptr.FunPtr FileOpenedNotification_Aux)

-- __unique:__ @fromFileOpenedNotification_Aux@
foreign import ccall safe "dynamic" hs_bindgen_f3ba5920f34c7f6a ::
     Ptr.FunPtr FileOpenedNotification_Aux
  -> FileOpenedNotification_Aux

instance HsBindgen.Runtime.FunPtr.ToFunPtr FileOpenedNotification_Aux where

  toFunPtr = hs_bindgen_b3b8b1fad168671a

instance HsBindgen.Runtime.FunPtr.FromFunPtr FileOpenedNotification_Aux where

  fromFunPtr = hs_bindgen_f3ba5920f34c7f6a

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FileOpenedNotification_Aux) "un_FileOpenedNotification_Aux")
         ) => GHC.Records.HasField "un_FileOpenedNotification_Aux" (Ptr.Ptr FileOpenedNotification_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_FileOpenedNotification_Aux")

instance HsBindgen.Runtime.HasCField.HasCField FileOpenedNotification_Aux "un_FileOpenedNotification_Aux" where

  type CFieldType FileOpenedNotification_Aux "un_FileOpenedNotification_Aux" =
    IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FileOpenedNotification@

    __defined at:__ @functions\/callbacks.h 10:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype FileOpenedNotification = FileOpenedNotification
  { un_FileOpenedNotification :: Ptr.FunPtr FileOpenedNotification_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FileOpenedNotification) "un_FileOpenedNotification")
         ) => GHC.Records.HasField "un_FileOpenedNotification" (Ptr.Ptr FileOpenedNotification) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_FileOpenedNotification")

instance HsBindgen.Runtime.HasCField.HasCField FileOpenedNotification "un_FileOpenedNotification" where

  type CFieldType FileOpenedNotification "un_FileOpenedNotification" =
    Ptr.FunPtr FileOpenedNotification_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'ProgressUpdate'

__C declaration:__ @ProgressUpdate@

__defined at:__ @functions\/callbacks.h 11:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype ProgressUpdate_Aux = ProgressUpdate_Aux
  { un_ProgressUpdate_Aux :: FC.CInt -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

-- __unique:__ @toProgressUpdate_Aux@
foreign import ccall safe "wrapper" hs_bindgen_d551f31556ffa727 ::
     ProgressUpdate_Aux
  -> IO (Ptr.FunPtr ProgressUpdate_Aux)

-- __unique:__ @fromProgressUpdate_Aux@
foreign import ccall safe "dynamic" hs_bindgen_ccf7f4b62a839a04 ::
     Ptr.FunPtr ProgressUpdate_Aux
  -> ProgressUpdate_Aux

instance HsBindgen.Runtime.FunPtr.ToFunPtr ProgressUpdate_Aux where

  toFunPtr = hs_bindgen_d551f31556ffa727

instance HsBindgen.Runtime.FunPtr.FromFunPtr ProgressUpdate_Aux where

  fromFunPtr = hs_bindgen_ccf7f4b62a839a04

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ProgressUpdate_Aux) "un_ProgressUpdate_Aux")
         ) => GHC.Records.HasField "un_ProgressUpdate_Aux" (Ptr.Ptr ProgressUpdate_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_ProgressUpdate_Aux")

instance HsBindgen.Runtime.HasCField.HasCField ProgressUpdate_Aux "un_ProgressUpdate_Aux" where

  type CFieldType ProgressUpdate_Aux "un_ProgressUpdate_Aux" =
    FC.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @ProgressUpdate@

    __defined at:__ @functions\/callbacks.h 11:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype ProgressUpdate = ProgressUpdate
  { un_ProgressUpdate :: Ptr.FunPtr ProgressUpdate_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ProgressUpdate) "un_ProgressUpdate")
         ) => GHC.Records.HasField "un_ProgressUpdate" (Ptr.Ptr ProgressUpdate) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_ProgressUpdate")

instance HsBindgen.Runtime.HasCField.HasCField ProgressUpdate "un_ProgressUpdate" where

  type CFieldType ProgressUpdate "un_ProgressUpdate" =
    Ptr.FunPtr ProgressUpdate_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'DataValidator'

__C declaration:__ @DataValidator@

__defined at:__ @functions\/callbacks.h 12:15@

__exported by:__ @functions\/callbacks.h@
-}
newtype DataValidator_Aux = DataValidator_Aux
  { un_DataValidator_Aux :: FC.CInt -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

-- __unique:__ @toDataValidator_Aux@
foreign import ccall safe "wrapper" hs_bindgen_c656ca21e63343d6 ::
     DataValidator_Aux
  -> IO (Ptr.FunPtr DataValidator_Aux)

-- __unique:__ @fromDataValidator_Aux@
foreign import ccall safe "dynamic" hs_bindgen_c1e79a4c11ca4033 ::
     Ptr.FunPtr DataValidator_Aux
  -> DataValidator_Aux

instance HsBindgen.Runtime.FunPtr.ToFunPtr DataValidator_Aux where

  toFunPtr = hs_bindgen_c656ca21e63343d6

instance HsBindgen.Runtime.FunPtr.FromFunPtr DataValidator_Aux where

  fromFunPtr = hs_bindgen_c1e79a4c11ca4033

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DataValidator_Aux) "un_DataValidator_Aux")
         ) => GHC.Records.HasField "un_DataValidator_Aux" (Ptr.Ptr DataValidator_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_DataValidator_Aux")

instance HsBindgen.Runtime.HasCField.HasCField DataValidator_Aux "un_DataValidator_Aux" where

  type CFieldType DataValidator_Aux "un_DataValidator_Aux" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @DataValidator@

    __defined at:__ @functions\/callbacks.h 12:15@

    __exported by:__ @functions\/callbacks.h@
-}
newtype DataValidator = DataValidator
  { un_DataValidator :: Ptr.FunPtr DataValidator_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DataValidator) "un_DataValidator")
         ) => GHC.Records.HasField "un_DataValidator" (Ptr.Ptr DataValidator) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_DataValidator")

instance HsBindgen.Runtime.HasCField.HasCField DataValidator "un_DataValidator" where

  type CFieldType DataValidator "un_DataValidator" =
    Ptr.FunPtr DataValidator_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Measurement@

    __defined at:__ @functions\/callbacks.h 21:8@

    __exported by:__ @functions\/callbacks.h@
-}
data Measurement = Measurement
  { measurement_value :: FC.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @functions\/callbacks.h 22:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , measurement_timestamp :: FC.CDouble
    {- ^ __C declaration:__ @timestamp@

         __defined at:__ @functions\/callbacks.h 23:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Measurement where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Measurement
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"measurement_value") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"measurement_timestamp") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Measurement measurement_value2 measurement_timestamp3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"measurement_value") ptr0 measurement_value2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"measurement_timestamp") ptr0 measurement_timestamp3

instance Data.Primitive.Types.Prim Measurement where

  sizeOf# = \_ -> (16#)

  alignment# = \_ -> (8#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Measurement (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Measurement v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Measurement measurement_value4 measurement_timestamp5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) measurement_value4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) measurement_timestamp5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Measurement (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Measurement v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Measurement measurement_value4 measurement_timestamp5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) measurement_value4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) measurement_timestamp5 s6

instance HsBindgen.Runtime.HasCField.HasCField Measurement "measurement_value" where

  type CFieldType Measurement "measurement_value" =
    FC.CDouble

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Measurement) "measurement_value")
         ) => GHC.Records.HasField "measurement_value" (Ptr.Ptr Measurement) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"measurement_value")

instance HsBindgen.Runtime.HasCField.HasCField Measurement "measurement_timestamp" where

  type CFieldType Measurement "measurement_timestamp" =
    FC.CDouble

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Measurement) "measurement_timestamp")
         ) => GHC.Records.HasField "measurement_timestamp" (Ptr.Ptr Measurement) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"measurement_timestamp")

{-| Auxiliary type used by 'MeasurementReceived'

__C declaration:__ @MeasurementReceived@

__defined at:__ @functions\/callbacks.h 26:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived_Aux = MeasurementReceived_Aux
  { un_MeasurementReceived_Aux :: (Ptr.Ptr Measurement) -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

-- __unique:__ @toMeasurementReceived_Aux@
foreign import ccall safe "wrapper" hs_bindgen_9259654df9d40f5b ::
     MeasurementReceived_Aux
  -> IO (Ptr.FunPtr MeasurementReceived_Aux)

-- __unique:__ @fromMeasurementReceived_Aux@
foreign import ccall safe "dynamic" hs_bindgen_383c36bb22947621 ::
     Ptr.FunPtr MeasurementReceived_Aux
  -> MeasurementReceived_Aux

instance HsBindgen.Runtime.FunPtr.ToFunPtr MeasurementReceived_Aux where

  toFunPtr = hs_bindgen_9259654df9d40f5b

instance HsBindgen.Runtime.FunPtr.FromFunPtr MeasurementReceived_Aux where

  fromFunPtr = hs_bindgen_383c36bb22947621

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MeasurementReceived_Aux) "un_MeasurementReceived_Aux")
         ) => GHC.Records.HasField "un_MeasurementReceived_Aux" (Ptr.Ptr MeasurementReceived_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MeasurementReceived_Aux")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementReceived_Aux "un_MeasurementReceived_Aux" where

  type CFieldType MeasurementReceived_Aux "un_MeasurementReceived_Aux" =
    (Ptr.Ptr Measurement) -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MeasurementReceived@

    __defined at:__ @functions\/callbacks.h 26:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived = MeasurementReceived
  { un_MeasurementReceived :: Ptr.FunPtr MeasurementReceived_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MeasurementReceived) "un_MeasurementReceived")
         ) => GHC.Records.HasField "un_MeasurementReceived" (Ptr.Ptr MeasurementReceived) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MeasurementReceived")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementReceived "un_MeasurementReceived" where

  type CFieldType MeasurementReceived "un_MeasurementReceived" =
    Ptr.FunPtr MeasurementReceived_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'MeasurementReceived2'

__C declaration:__ @MeasurementReceived2@

__defined at:__ @functions\/callbacks.h 29:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived2_Aux = MeasurementReceived2_Aux
  { un_MeasurementReceived2_Aux :: Measurement -> IO ()
  }

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MeasurementReceived2_Aux) "un_MeasurementReceived2_Aux")
         ) => GHC.Records.HasField "un_MeasurementReceived2_Aux" (Ptr.Ptr MeasurementReceived2_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MeasurementReceived2_Aux")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementReceived2_Aux "un_MeasurementReceived2_Aux" where

  type CFieldType MeasurementReceived2_Aux "un_MeasurementReceived2_Aux" =
    Measurement -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MeasurementReceived2@

    __defined at:__ @functions\/callbacks.h 29:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived2 = MeasurementReceived2
  { un_MeasurementReceived2 :: Ptr.FunPtr MeasurementReceived2_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MeasurementReceived2) "un_MeasurementReceived2")
         ) => GHC.Records.HasField "un_MeasurementReceived2" (Ptr.Ptr MeasurementReceived2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MeasurementReceived2")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementReceived2 "un_MeasurementReceived2" where

  type CFieldType MeasurementReceived2 "un_MeasurementReceived2" =
    Ptr.FunPtr MeasurementReceived2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'SampleBufferFull'

__C declaration:__ @SampleBufferFull@

__defined at:__ @functions\/callbacks.h 32:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype SampleBufferFull_Aux = SampleBufferFull_Aux
  { un_SampleBufferFull_Aux :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) FC.CInt) -> IO ()
  }

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType SampleBufferFull_Aux) "un_SampleBufferFull_Aux")
         ) => GHC.Records.HasField "un_SampleBufferFull_Aux" (Ptr.Ptr SampleBufferFull_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_SampleBufferFull_Aux")

instance HsBindgen.Runtime.HasCField.HasCField SampleBufferFull_Aux "un_SampleBufferFull_Aux" where

  type CFieldType SampleBufferFull_Aux "un_SampleBufferFull_Aux" =
    ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) FC.CInt) -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SampleBufferFull@

    __defined at:__ @functions\/callbacks.h 32:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype SampleBufferFull = SampleBufferFull
  { un_SampleBufferFull :: Ptr.FunPtr SampleBufferFull_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType SampleBufferFull) "un_SampleBufferFull")
         ) => GHC.Records.HasField "un_SampleBufferFull" (Ptr.Ptr SampleBufferFull) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_SampleBufferFull")

instance HsBindgen.Runtime.HasCField.HasCField SampleBufferFull "un_SampleBufferFull" where

  type CFieldType SampleBufferFull "un_SampleBufferFull" =
    Ptr.FunPtr SampleBufferFull_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct MeasurementHandler@

    __defined at:__ @functions\/callbacks.h 50:8@

    __exported by:__ @functions\/callbacks.h@
-}
data MeasurementHandler = MeasurementHandler
  { measurementHandler_onReceived :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
    {- ^ __C declaration:__ @onReceived@

         __defined at:__ @functions\/callbacks.h 51:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , measurementHandler_validate :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)
    {- ^ __C declaration:__ @validate@

         __defined at:__ @functions\/callbacks.h 52:9@

         __exported by:__ @functions\/callbacks.h@
    -}
  , measurementHandler_onError :: Ptr.FunPtr (FC.CInt -> IO ())
    {- ^ __C declaration:__ @onError@

         __defined at:__ @functions\/callbacks.h 53:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable MeasurementHandler where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure MeasurementHandler
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"measurementHandler_onReceived") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"measurementHandler_validate") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"measurementHandler_onError") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MeasurementHandler
            measurementHandler_onReceived2
            measurementHandler_validate3
            measurementHandler_onError4 ->
                 HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"measurementHandler_onReceived") ptr0 measurementHandler_onReceived2
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"measurementHandler_validate") ptr0 measurementHandler_validate3
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"measurementHandler_onError") ptr0 measurementHandler_onError4

instance HsBindgen.Runtime.HasCField.HasCField MeasurementHandler "measurementHandler_onReceived" where

  type CFieldType MeasurementHandler "measurementHandler_onReceived" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MeasurementHandler) "measurementHandler_onReceived")
         ) => GHC.Records.HasField "measurementHandler_onReceived" (Ptr.Ptr MeasurementHandler) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"measurementHandler_onReceived")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementHandler "measurementHandler_validate" where

  type CFieldType MeasurementHandler "measurementHandler_validate" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MeasurementHandler) "measurementHandler_validate")
         ) => GHC.Records.HasField "measurementHandler_validate" (Ptr.Ptr MeasurementHandler) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"measurementHandler_validate")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementHandler "measurementHandler_onError" where

  type CFieldType MeasurementHandler "measurementHandler_onError" =
    Ptr.FunPtr (FC.CInt -> IO ())

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MeasurementHandler) "measurementHandler_onError")
         ) => GHC.Records.HasField "measurementHandler_onError" (Ptr.Ptr MeasurementHandler) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"measurementHandler_onError")

{-| __C declaration:__ @struct DataPipeline@

    __defined at:__ @functions\/callbacks.h 58:8@

    __exported by:__ @functions\/callbacks.h@
-}
data DataPipeline = DataPipeline
  { dataPipeline_preProcess :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
    {- ^ __C declaration:__ @preProcess@

         __defined at:__ @functions\/callbacks.h 59:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , dataPipeline_process :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
    {- ^ __C declaration:__ @process@

         __defined at:__ @functions\/callbacks.h 60:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , dataPipeline_postProcess :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
    {- ^ __C declaration:__ @postProcess@

         __defined at:__ @functions\/callbacks.h 61:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable DataPipeline where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure DataPipeline
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"dataPipeline_preProcess") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"dataPipeline_process") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"dataPipeline_postProcess") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          DataPipeline
            dataPipeline_preProcess2
            dataPipeline_process3
            dataPipeline_postProcess4 ->
                 HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"dataPipeline_preProcess") ptr0 dataPipeline_preProcess2
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"dataPipeline_process") ptr0 dataPipeline_process3
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"dataPipeline_postProcess") ptr0 dataPipeline_postProcess4

instance HsBindgen.Runtime.HasCField.HasCField DataPipeline "dataPipeline_preProcess" where

  type CFieldType DataPipeline "dataPipeline_preProcess" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DataPipeline) "dataPipeline_preProcess")
         ) => GHC.Records.HasField "dataPipeline_preProcess" (Ptr.Ptr DataPipeline) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"dataPipeline_preProcess")

instance HsBindgen.Runtime.HasCField.HasCField DataPipeline "dataPipeline_process" where

  type CFieldType DataPipeline "dataPipeline_process" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DataPipeline) "dataPipeline_process")
         ) => GHC.Records.HasField "dataPipeline_process" (Ptr.Ptr DataPipeline) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"dataPipeline_process")

instance HsBindgen.Runtime.HasCField.HasCField DataPipeline "dataPipeline_postProcess" where

  type CFieldType DataPipeline "dataPipeline_postProcess" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DataPipeline) "dataPipeline_postProcess")
         ) => GHC.Records.HasField "dataPipeline_postProcess" (Ptr.Ptr DataPipeline) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"dataPipeline_postProcess")

{-| __C declaration:__ @union ProcessorCallback@

    __defined at:__ @functions\/callbacks.h 69:7@

    __exported by:__ @functions\/callbacks.h@
-}
newtype ProcessorCallback = ProcessorCallback
  { un_ProcessorCallback :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 8 instance F.Storable ProcessorCallback

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 8 instance Data.Primitive.Types.Prim ProcessorCallback

{-|

  __See:__ 'set_processorCallback_simple'

__C declaration:__ @simple@

__defined at:__ @functions\/callbacks.h 70:10@

__exported by:__ @functions\/callbacks.h@
-}
get_processorCallback_simple ::
     ProcessorCallback
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
get_processorCallback_simple =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_processorCallback_simple'

-}
set_processorCallback_simple ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
  -> ProcessorCallback
set_processorCallback_simple =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_processorCallback_withValidator'

__C declaration:__ @withValidator@

__defined at:__ @functions\/callbacks.h 71:10@

__exported by:__ @functions\/callbacks.h@
-}
get_processorCallback_withValidator ::
     ProcessorCallback
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
get_processorCallback_withValidator =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_processorCallback_withValidator'

-}
set_processorCallback_withValidator ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
  -> ProcessorCallback
set_processorCallback_withValidator =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_processorCallback_withProgress'

__C declaration:__ @withProgress@

__defined at:__ @functions\/callbacks.h 72:10@

__exported by:__ @functions\/callbacks.h@
-}
get_processorCallback_withProgress ::
     ProcessorCallback
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
get_processorCallback_withProgress =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_processorCallback_withProgress'

-}
set_processorCallback_withProgress ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> ProcessorCallback
set_processorCallback_withProgress =
  HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField ProcessorCallback "processorCallback_simple" where

  type CFieldType ProcessorCallback "processorCallback_simple" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ProcessorCallback) "processorCallback_simple")
         ) => GHC.Records.HasField "processorCallback_simple" (Ptr.Ptr ProcessorCallback) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"processorCallback_simple")

instance HsBindgen.Runtime.HasCField.HasCField ProcessorCallback "processorCallback_withValidator" where

  type CFieldType ProcessorCallback "processorCallback_withValidator" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ProcessorCallback) "processorCallback_withValidator")
         ) => GHC.Records.HasField "processorCallback_withValidator" (Ptr.Ptr ProcessorCallback) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"processorCallback_withValidator")

instance HsBindgen.Runtime.HasCField.HasCField ProcessorCallback "processorCallback_withProgress" where

  type CFieldType ProcessorCallback "processorCallback_withProgress" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ProcessorCallback) "processorCallback_withProgress")
         ) => GHC.Records.HasField "processorCallback_withProgress" (Ptr.Ptr ProcessorCallback) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"processorCallback_withProgress")

{-| __C declaration:__ @enum \@Processor_mode@

    __defined at:__ @functions\/callbacks.h 76:3@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Processor_mode = Processor_mode
  { un_Processor_mode :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance F.Storable Processor_mode where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Processor_mode
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Processor_mode un_Processor_mode2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Processor_mode2

deriving via FC.CUInt instance Data.Primitive.Types.Prim Processor_mode

instance HsBindgen.Runtime.CEnum.CEnum Processor_mode where

  type CEnumZ Processor_mode = FC.CUInt

  toCEnum = Processor_mode

  fromCEnum = un_Processor_mode

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "MODE_SIMPLE")
                                                     , (1, Data.List.NonEmpty.singleton "MODE_VALIDATED")
                                                     , (2, Data.List.NonEmpty.singleton "MODE_PROGRESS")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Processor_mode"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Processor_mode"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Processor_mode where

  minDeclaredValue = MODE_SIMPLE

  maxDeclaredValue = MODE_PROGRESS

instance Show Processor_mode where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Processor_mode where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @MODE_SIMPLE@

    __defined at:__ @functions\/callbacks.h 76:10@

    __exported by:__ @functions\/callbacks.h@
-}
pattern MODE_SIMPLE :: Processor_mode
pattern MODE_SIMPLE = Processor_mode 0

{-| __C declaration:__ @MODE_VALIDATED@

    __defined at:__ @functions\/callbacks.h 76:23@

    __exported by:__ @functions\/callbacks.h@
-}
pattern MODE_VALIDATED :: Processor_mode
pattern MODE_VALIDATED = Processor_mode 1

{-| __C declaration:__ @MODE_PROGRESS@

    __defined at:__ @functions\/callbacks.h 76:39@

    __exported by:__ @functions\/callbacks.h@
-}
pattern MODE_PROGRESS :: Processor_mode
pattern MODE_PROGRESS = Processor_mode 2

{-| __C declaration:__ @struct Processor@

    __defined at:__ @functions\/callbacks.h 75:8@

    __exported by:__ @functions\/callbacks.h@
-}
data Processor = Processor
  { processor_mode :: Processor_mode
    {- ^ __C declaration:__ @mode@

         __defined at:__ @functions\/callbacks.h 76:55@

         __exported by:__ @functions\/callbacks.h@
    -}
  , processor_callback :: ProcessorCallback
    {- ^ __C declaration:__ @callback@

         __defined at:__ @functions\/callbacks.h 77:27@

         __exported by:__ @functions\/callbacks.h@
    -}
  }

instance F.Storable Processor where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Processor
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"processor_mode") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"processor_callback") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Processor processor_mode2 processor_callback3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"processor_mode") ptr0 processor_mode2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"processor_callback") ptr0 processor_callback3

instance HsBindgen.Runtime.HasCField.HasCField Processor "processor_mode" where

  type CFieldType Processor "processor_mode" =
    Processor_mode

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Processor) "processor_mode")
         ) => GHC.Records.HasField "processor_mode" (Ptr.Ptr Processor) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"processor_mode")

instance HsBindgen.Runtime.HasCField.HasCField Processor "processor_callback" where

  type CFieldType Processor "processor_callback" =
    ProcessorCallback

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Processor) "processor_callback")
         ) => GHC.Records.HasField "processor_callback" (Ptr.Ptr Processor) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"processor_callback")

{-| __C declaration:__ @foo@

    __defined at:__ @functions\/callbacks.h 94:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Foo = Foo
  { un_Foo :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "un_Foo")
         ) => GHC.Records.HasField "un_Foo" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Foo")

instance HsBindgen.Runtime.HasCField.HasCField Foo "un_Foo" where

  type CFieldType Foo "un_Foo" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @foo2@

    __defined at:__ @functions\/callbacks.h 95:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Foo2 = Foo2
  { un_Foo2 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo2) "un_Foo2")
         ) => GHC.Records.HasField "un_Foo2" (Ptr.Ptr Foo2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Foo2")

instance HsBindgen.Runtime.HasCField.HasCField Foo2 "un_Foo2" where

  type CFieldType Foo2 "un_Foo2" = FC.CInt

  offset# = \_ -> \_ -> 0

-- __unique:__ @instance ToFunPtr (Foo -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_d2a71f330b782e41 ::
     (Foo -> IO ())
  -> IO (Ptr.FunPtr (Foo -> IO ()))

-- __unique:__ @instance FromFunPtr (Foo -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_c08440542d338bad ::
     Ptr.FunPtr (Foo -> IO ())
  -> Foo -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr (Foo -> IO ()) where

  toFunPtr = hs_bindgen_d2a71f330b782e41

instance HsBindgen.Runtime.FunPtr.FromFunPtr (Foo -> IO ()) where

  fromFunPtr = hs_bindgen_c08440542d338bad

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)@
foreign import ccall safe "wrapper" hs_bindgen_4a7a09e6a9e8c907 ::
     ((Ptr.Ptr Measurement) -> IO FC.CInt)
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt))

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)@
foreign import ccall safe "dynamic" hs_bindgen_2f679442a6d5613f ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)
  -> (Ptr.Ptr Measurement) -> IO FC.CInt

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt) where

  toFunPtr = hs_bindgen_4a7a09e6a9e8c907

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt) where

  fromFunPtr = hs_bindgen_2f679442a6d5613f

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_ca2a1bac1cc0c128 ::
     ((Ptr.Ptr Measurement) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ()))

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_4d8a3980803a90f0 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
  -> (Ptr.Ptr Measurement) -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> IO ()) where

  toFunPtr = hs_bindgen_ca2a1bac1cc0c128

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> IO ()) where

  fromFunPtr = hs_bindgen_4d8a3980803a90f0

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_aa3ec59dec5e1fdf ::
     ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()))

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_db7fc2b6d55d3864 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
  -> (Ptr.Ptr Measurement) -> DataValidator -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()) where

  toFunPtr = hs_bindgen_aa3ec59dec5e1fdf

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()) where

  fromFunPtr = hs_bindgen_db7fc2b6d55d3864

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_b0ef2ac592b19bed ::
     ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()))

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_17d0b0462779e216 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()) where

  toFunPtr = hs_bindgen_b0ef2ac592b19bed

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_17d0b0462779e216

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_2d4b28b099f1cb6b ::
     ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()))

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_3aa04c4e63a856b2 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()) where

  toFunPtr = hs_bindgen_2d4b28b099f1cb6b

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_3aa04c4e63a856b2

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_4e441dd005b8df73 ::
     ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()))

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_fbe9354fa822de59 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> (Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()) where

  toFunPtr = hs_bindgen_4e441dd005b8df73

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()) where

  fromFunPtr = hs_bindgen_fbe9354fa822de59

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_83f0d12162b8410b ::
     ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()))

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_f634a7da5fce9c42 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()) where

  toFunPtr = hs_bindgen_83f0d12162b8410b

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_f634a7da5fce9c42

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_ab767cc7cdbd64cb ::
     ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()))

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_2f73a7e07a90e977 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
  -> (Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()) where

  toFunPtr = hs_bindgen_ab767cc7cdbd64cb

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()) where

  fromFunPtr = hs_bindgen_2f73a7e07a90e977

-- __unique:__ @instance ToFunPtr (Foo2 -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_235fa4a89af25f04 ::
     (Foo2 -> IO ())
  -> IO (Ptr.FunPtr (Foo2 -> IO ()))

-- __unique:__ @instance FromFunPtr (Foo2 -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_8605b223a9ab9562 ::
     Ptr.FunPtr (Foo2 -> IO ())
  -> Foo2 -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr (Foo2 -> IO ()) where

  toFunPtr = hs_bindgen_235fa4a89af25f04

instance HsBindgen.Runtime.FunPtr.FromFunPtr (Foo2 -> IO ()) where

  fromFunPtr = hs_bindgen_8605b223a9ab9562
