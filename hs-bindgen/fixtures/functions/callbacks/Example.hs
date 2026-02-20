{-# LANGUAGE CApiFFI #-}
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
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| Auxiliary type used by 'FileOpenedNotification'

__C declaration:__ @FileOpenedNotification@

__defined at:__ @functions\/callbacks.h 10:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype FileOpenedNotification_Aux = FileOpenedNotification_Aux
  { unwrapFileOpenedNotification_Aux :: IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_b3b8b1fad168671a_base ::
     IO ()
  -> IO (RIP.FunPtr (IO ()))

-- __unique:__ @toFileOpenedNotification_Aux@
hs_bindgen_b3b8b1fad168671a ::
     FileOpenedNotification_Aux
  -> IO (RIP.FunPtr FileOpenedNotification_Aux)
hs_bindgen_b3b8b1fad168671a =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_b3b8b1fad168671a_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_f3ba5920f34c7f6a_base ::
     RIP.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromFileOpenedNotification_Aux@
hs_bindgen_f3ba5920f34c7f6a ::
     RIP.FunPtr FileOpenedNotification_Aux
  -> FileOpenedNotification_Aux
hs_bindgen_f3ba5920f34c7f6a =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_f3ba5920f34c7f6a_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr FileOpenedNotification_Aux where

  toFunPtr = hs_bindgen_b3b8b1fad168671a

instance RIP.FromFunPtr FileOpenedNotification_Aux where

  fromFunPtr = hs_bindgen_f3ba5920f34c7f6a

instance ( ((~) ty) (IO ())
         ) => RIP.HasField "unwrapFileOpenedNotification_Aux" (RIP.Ptr FileOpenedNotification_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFileOpenedNotification_Aux")

instance HasCField.HasCField FileOpenedNotification_Aux "unwrapFileOpenedNotification_Aux" where

  type CFieldType FileOpenedNotification_Aux "unwrapFileOpenedNotification_Aux" =
    IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FileOpenedNotification@

    __defined at:__ @functions\/callbacks.h 10:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype FileOpenedNotification = FileOpenedNotification
  { unwrapFileOpenedNotification :: RIP.FunPtr FileOpenedNotification_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr FileOpenedNotification_Aux)
         ) => RIP.HasField "unwrapFileOpenedNotification" (RIP.Ptr FileOpenedNotification) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFileOpenedNotification")

instance HasCField.HasCField FileOpenedNotification "unwrapFileOpenedNotification" where

  type CFieldType FileOpenedNotification "unwrapFileOpenedNotification" =
    RIP.FunPtr FileOpenedNotification_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'ProgressUpdate'

__C declaration:__ @ProgressUpdate@

__defined at:__ @functions\/callbacks.h 11:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype ProgressUpdate_Aux = ProgressUpdate_Aux
  { unwrapProgressUpdate_Aux :: RIP.CInt -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_d551f31556ffa727_base ::
     (RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr (RIP.Int32 -> IO ()))

-- __unique:__ @toProgressUpdate_Aux@
hs_bindgen_d551f31556ffa727 ::
     ProgressUpdate_Aux
  -> IO (RIP.FunPtr ProgressUpdate_Aux)
hs_bindgen_d551f31556ffa727 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_d551f31556ffa727_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_ccf7f4b62a839a04_base ::
     RIP.FunPtr (RIP.Int32 -> IO ())
  -> RIP.Int32 -> IO ()

-- __unique:__ @fromProgressUpdate_Aux@
hs_bindgen_ccf7f4b62a839a04 ::
     RIP.FunPtr ProgressUpdate_Aux
  -> ProgressUpdate_Aux
hs_bindgen_ccf7f4b62a839a04 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_ccf7f4b62a839a04_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr ProgressUpdate_Aux where

  toFunPtr = hs_bindgen_d551f31556ffa727

instance RIP.FromFunPtr ProgressUpdate_Aux where

  fromFunPtr = hs_bindgen_ccf7f4b62a839a04

instance ( ((~) ty) (RIP.CInt -> IO ())
         ) => RIP.HasField "unwrapProgressUpdate_Aux" (RIP.Ptr ProgressUpdate_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapProgressUpdate_Aux")

instance HasCField.HasCField ProgressUpdate_Aux "unwrapProgressUpdate_Aux" where

  type CFieldType ProgressUpdate_Aux "unwrapProgressUpdate_Aux" =
    RIP.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @ProgressUpdate@

    __defined at:__ @functions\/callbacks.h 11:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype ProgressUpdate = ProgressUpdate
  { unwrapProgressUpdate :: RIP.FunPtr ProgressUpdate_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr ProgressUpdate_Aux)
         ) => RIP.HasField "unwrapProgressUpdate" (RIP.Ptr ProgressUpdate) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapProgressUpdate")

instance HasCField.HasCField ProgressUpdate "unwrapProgressUpdate" where

  type CFieldType ProgressUpdate "unwrapProgressUpdate" =
    RIP.FunPtr ProgressUpdate_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'DataValidator'

__C declaration:__ @DataValidator@

__defined at:__ @functions\/callbacks.h 12:15@

__exported by:__ @functions\/callbacks.h@
-}
newtype DataValidator_Aux = DataValidator_Aux
  { unwrapDataValidator_Aux :: RIP.CInt -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_c656ca21e63343d6_base ::
     (RIP.Int32 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> IO RIP.Int32))

-- __unique:__ @toDataValidator_Aux@
hs_bindgen_c656ca21e63343d6 ::
     DataValidator_Aux
  -> IO (RIP.FunPtr DataValidator_Aux)
hs_bindgen_c656ca21e63343d6 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_c656ca21e63343d6_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_c1e79a4c11ca4033_base ::
     RIP.FunPtr (RIP.Int32 -> IO RIP.Int32)
  -> RIP.Int32 -> IO RIP.Int32

-- __unique:__ @fromDataValidator_Aux@
hs_bindgen_c1e79a4c11ca4033 ::
     RIP.FunPtr DataValidator_Aux
  -> DataValidator_Aux
hs_bindgen_c1e79a4c11ca4033 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_c1e79a4c11ca4033_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr DataValidator_Aux where

  toFunPtr = hs_bindgen_c656ca21e63343d6

instance RIP.FromFunPtr DataValidator_Aux where

  fromFunPtr = hs_bindgen_c1e79a4c11ca4033

instance ( ((~) ty) (RIP.CInt -> IO RIP.CInt)
         ) => RIP.HasField "unwrapDataValidator_Aux" (RIP.Ptr DataValidator_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapDataValidator_Aux")

instance HasCField.HasCField DataValidator_Aux "unwrapDataValidator_Aux" where

  type CFieldType DataValidator_Aux "unwrapDataValidator_Aux" =
    RIP.CInt -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @DataValidator@

    __defined at:__ @functions\/callbacks.h 12:15@

    __exported by:__ @functions\/callbacks.h@
-}
newtype DataValidator = DataValidator
  { unwrapDataValidator :: RIP.FunPtr DataValidator_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr DataValidator_Aux)
         ) => RIP.HasField "unwrapDataValidator" (RIP.Ptr DataValidator) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapDataValidator")

instance HasCField.HasCField DataValidator "unwrapDataValidator" where

  type CFieldType DataValidator "unwrapDataValidator" =
    RIP.FunPtr DataValidator_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Measurement@

    __defined at:__ @functions\/callbacks.h 21:8@

    __exported by:__ @functions\/callbacks.h@
-}
data Measurement = Measurement
  { measurement_value :: RIP.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @functions\/callbacks.h 22:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , measurement_timestamp :: RIP.CDouble
    {- ^ __C declaration:__ @timestamp@

         __defined at:__ @functions\/callbacks.h 23:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Measurement where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Measurement where

  readRaw =
    \ptr0 ->
          pure Measurement
      <*> HasCField.readRaw (RIP.Proxy @"measurement_value") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"measurement_timestamp") ptr0

instance Marshal.WriteRaw Measurement where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Measurement measurement_value2 measurement_timestamp3 ->
               HasCField.writeRaw (RIP.Proxy @"measurement_value") ptr0 measurement_value2
            >> HasCField.writeRaw (RIP.Proxy @"measurement_timestamp") ptr0 measurement_timestamp3

deriving via Marshal.EquivStorable Measurement instance RIP.Storable Measurement

instance HasCField.HasCField Measurement "measurement_value" where

  type CFieldType Measurement "measurement_value" =
    RIP.CDouble

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "measurement_value" (RIP.Ptr Measurement) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"measurement_value")

instance HasCField.HasCField Measurement "measurement_timestamp" where

  type CFieldType Measurement "measurement_timestamp" =
    RIP.CDouble

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "measurement_timestamp" (RIP.Ptr Measurement) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"measurement_timestamp")

{-| Auxiliary type used by 'MeasurementReceived'

__C declaration:__ @MeasurementReceived@

__defined at:__ @functions\/callbacks.h 26:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived_Aux = MeasurementReceived_Aux
  { unwrapMeasurementReceived_Aux :: (RIP.Ptr Measurement) -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_9259654df9d40f5b_base ::
     ((RIP.Ptr RIP.Void) -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO ()))

-- __unique:__ @toMeasurementReceived_Aux@
hs_bindgen_9259654df9d40f5b ::
     MeasurementReceived_Aux
  -> IO (RIP.FunPtr MeasurementReceived_Aux)
hs_bindgen_9259654df9d40f5b =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_9259654df9d40f5b_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_383c36bb22947621_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO ())
  -> (RIP.Ptr RIP.Void) -> IO ()

-- __unique:__ @fromMeasurementReceived_Aux@
hs_bindgen_383c36bb22947621 ::
     RIP.FunPtr MeasurementReceived_Aux
  -> MeasurementReceived_Aux
hs_bindgen_383c36bb22947621 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_383c36bb22947621_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr MeasurementReceived_Aux where

  toFunPtr = hs_bindgen_9259654df9d40f5b

instance RIP.FromFunPtr MeasurementReceived_Aux where

  fromFunPtr = hs_bindgen_383c36bb22947621

instance ( ((~) ty) ((RIP.Ptr Measurement) -> IO ())
         ) => RIP.HasField "unwrapMeasurementReceived_Aux" (RIP.Ptr MeasurementReceived_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMeasurementReceived_Aux")

instance HasCField.HasCField MeasurementReceived_Aux "unwrapMeasurementReceived_Aux" where

  type CFieldType MeasurementReceived_Aux "unwrapMeasurementReceived_Aux" =
    (RIP.Ptr Measurement) -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MeasurementReceived@

    __defined at:__ @functions\/callbacks.h 26:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived = MeasurementReceived
  { unwrapMeasurementReceived :: RIP.FunPtr MeasurementReceived_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr MeasurementReceived_Aux)
         ) => RIP.HasField "unwrapMeasurementReceived" (RIP.Ptr MeasurementReceived) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMeasurementReceived")

instance HasCField.HasCField MeasurementReceived "unwrapMeasurementReceived" where

  type CFieldType MeasurementReceived "unwrapMeasurementReceived" =
    RIP.FunPtr MeasurementReceived_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'MeasurementReceived2'

__C declaration:__ @MeasurementReceived2@

__defined at:__ @functions\/callbacks.h 29:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived2_Aux = MeasurementReceived2_Aux
  { unwrapMeasurementReceived2_Aux :: Measurement -> IO ()
  }
  deriving stock (RIP.Generic)

instance ( ((~) ty) (Measurement -> IO ())
         ) => RIP.HasField "unwrapMeasurementReceived2_Aux" (RIP.Ptr MeasurementReceived2_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMeasurementReceived2_Aux")

instance HasCField.HasCField MeasurementReceived2_Aux "unwrapMeasurementReceived2_Aux" where

  type CFieldType MeasurementReceived2_Aux "unwrapMeasurementReceived2_Aux" =
    Measurement -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MeasurementReceived2@

    __defined at:__ @functions\/callbacks.h 29:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived2 = MeasurementReceived2
  { unwrapMeasurementReceived2 :: RIP.FunPtr MeasurementReceived2_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr MeasurementReceived2_Aux)
         ) => RIP.HasField "unwrapMeasurementReceived2" (RIP.Ptr MeasurementReceived2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMeasurementReceived2")

instance HasCField.HasCField MeasurementReceived2 "unwrapMeasurementReceived2" where

  type CFieldType MeasurementReceived2 "unwrapMeasurementReceived2" =
    RIP.FunPtr MeasurementReceived2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'SampleBufferFull'

__C declaration:__ @SampleBufferFull@

__defined at:__ @functions\/callbacks.h 32:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype SampleBufferFull_Aux = SampleBufferFull_Aux
  { unwrapSampleBufferFull_Aux :: (RIP.Ptr (IsA.Elem ((CA.ConstantArray 10) RIP.CInt))) -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_57d9e30494ae4453_base ::
     ((RIP.Ptr RIP.Void) -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO ()))

-- __unique:__ @toSampleBufferFull_Aux@
hs_bindgen_57d9e30494ae4453 ::
     SampleBufferFull_Aux
  -> IO (RIP.FunPtr SampleBufferFull_Aux)
hs_bindgen_57d9e30494ae4453 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_57d9e30494ae4453_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_2ab7ac6bb756ba7e_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO ())
  -> (RIP.Ptr RIP.Void) -> IO ()

-- __unique:__ @fromSampleBufferFull_Aux@
hs_bindgen_2ab7ac6bb756ba7e ::
     RIP.FunPtr SampleBufferFull_Aux
  -> SampleBufferFull_Aux
hs_bindgen_2ab7ac6bb756ba7e =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_2ab7ac6bb756ba7e_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr SampleBufferFull_Aux where

  toFunPtr = hs_bindgen_57d9e30494ae4453

instance RIP.FromFunPtr SampleBufferFull_Aux where

  fromFunPtr = hs_bindgen_2ab7ac6bb756ba7e

instance ( ((~) ty) ((RIP.Ptr (IsA.Elem ((CA.ConstantArray 10) RIP.CInt))) -> IO ())
         ) => RIP.HasField "unwrapSampleBufferFull_Aux" (RIP.Ptr SampleBufferFull_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapSampleBufferFull_Aux")

instance HasCField.HasCField SampleBufferFull_Aux "unwrapSampleBufferFull_Aux" where

  type CFieldType SampleBufferFull_Aux "unwrapSampleBufferFull_Aux" =
    (RIP.Ptr (IsA.Elem ((CA.ConstantArray 10) RIP.CInt))) -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SampleBufferFull@

    __defined at:__ @functions\/callbacks.h 32:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype SampleBufferFull = SampleBufferFull
  { unwrapSampleBufferFull :: RIP.FunPtr SampleBufferFull_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr SampleBufferFull_Aux)
         ) => RIP.HasField "unwrapSampleBufferFull" (RIP.Ptr SampleBufferFull) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapSampleBufferFull")

instance HasCField.HasCField SampleBufferFull "unwrapSampleBufferFull" where

  type CFieldType SampleBufferFull "unwrapSampleBufferFull" =
    RIP.FunPtr SampleBufferFull_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct MeasurementHandler@

    __defined at:__ @functions\/callbacks.h 50:8@

    __exported by:__ @functions\/callbacks.h@
-}
data MeasurementHandler = MeasurementHandler
  { measurementHandler_onReceived :: RIP.FunPtr ((RIP.Ptr Measurement) -> IO ())
    {- ^ __C declaration:__ @onReceived@

         __defined at:__ @functions\/callbacks.h 51:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , measurementHandler_validate :: RIP.FunPtr ((RIP.Ptr Measurement) -> IO RIP.CInt)
    {- ^ __C declaration:__ @validate@

         __defined at:__ @functions\/callbacks.h 52:9@

         __exported by:__ @functions\/callbacks.h@
    -}
  , measurementHandler_onError :: RIP.FunPtr (RIP.CInt -> IO ())
    {- ^ __C declaration:__ @onError@

         __defined at:__ @functions\/callbacks.h 53:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize MeasurementHandler where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw MeasurementHandler where

  readRaw =
    \ptr0 ->
          pure MeasurementHandler
      <*> HasCField.readRaw (RIP.Proxy @"measurementHandler_onReceived") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"measurementHandler_validate") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"measurementHandler_onError") ptr0

instance Marshal.WriteRaw MeasurementHandler where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MeasurementHandler
            measurementHandler_onReceived2
            measurementHandler_validate3
            measurementHandler_onError4 ->
                 HasCField.writeRaw (RIP.Proxy @"measurementHandler_onReceived") ptr0 measurementHandler_onReceived2
              >> HasCField.writeRaw (RIP.Proxy @"measurementHandler_validate") ptr0 measurementHandler_validate3
              >> HasCField.writeRaw (RIP.Proxy @"measurementHandler_onError") ptr0 measurementHandler_onError4

deriving via Marshal.EquivStorable MeasurementHandler instance RIP.Storable MeasurementHandler

instance HasCField.HasCField MeasurementHandler "measurementHandler_onReceived" where

  type CFieldType MeasurementHandler "measurementHandler_onReceived" =
    RIP.FunPtr ((RIP.Ptr Measurement) -> IO ())

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr Measurement) -> IO ()))
         ) => RIP.HasField "measurementHandler_onReceived" (RIP.Ptr MeasurementHandler) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"measurementHandler_onReceived")

instance HasCField.HasCField MeasurementHandler "measurementHandler_validate" where

  type CFieldType MeasurementHandler "measurementHandler_validate" =
    RIP.FunPtr ((RIP.Ptr Measurement) -> IO RIP.CInt)

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr Measurement) -> IO RIP.CInt))
         ) => RIP.HasField "measurementHandler_validate" (RIP.Ptr MeasurementHandler) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"measurementHandler_validate")

instance HasCField.HasCField MeasurementHandler "measurementHandler_onError" where

  type CFieldType MeasurementHandler "measurementHandler_onError" =
    RIP.FunPtr (RIP.CInt -> IO ())

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) (RIP.FunPtr (RIP.CInt -> IO ()))
         ) => RIP.HasField "measurementHandler_onError" (RIP.Ptr MeasurementHandler) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"measurementHandler_onError")

{-| __C declaration:__ @struct DataPipeline@

    __defined at:__ @functions\/callbacks.h 58:8@

    __exported by:__ @functions\/callbacks.h@
-}
data DataPipeline = DataPipeline
  { dataPipeline_preProcess :: RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ())
    {- ^ __C declaration:__ @preProcess@

         __defined at:__ @functions\/callbacks.h 59:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , dataPipeline_process :: RIP.FunPtr ((RIP.Ptr Measurement) -> IO ())
    {- ^ __C declaration:__ @process@

         __defined at:__ @functions\/callbacks.h 60:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , dataPipeline_postProcess :: RIP.FunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ())
    {- ^ __C declaration:__ @postProcess@

         __defined at:__ @functions\/callbacks.h 61:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize DataPipeline where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw DataPipeline where

  readRaw =
    \ptr0 ->
          pure DataPipeline
      <*> HasCField.readRaw (RIP.Proxy @"dataPipeline_preProcess") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"dataPipeline_process") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"dataPipeline_postProcess") ptr0

instance Marshal.WriteRaw DataPipeline where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          DataPipeline
            dataPipeline_preProcess2
            dataPipeline_process3
            dataPipeline_postProcess4 ->
                 HasCField.writeRaw (RIP.Proxy @"dataPipeline_preProcess") ptr0 dataPipeline_preProcess2
              >> HasCField.writeRaw (RIP.Proxy @"dataPipeline_process") ptr0 dataPipeline_process3
              >> HasCField.writeRaw (RIP.Proxy @"dataPipeline_postProcess") ptr0 dataPipeline_postProcess4

deriving via Marshal.EquivStorable DataPipeline instance RIP.Storable DataPipeline

instance HasCField.HasCField DataPipeline "dataPipeline_preProcess" where

  type CFieldType DataPipeline "dataPipeline_preProcess" =
    RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ())

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ()))
         ) => RIP.HasField "dataPipeline_preProcess" (RIP.Ptr DataPipeline) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dataPipeline_preProcess")

instance HasCField.HasCField DataPipeline "dataPipeline_process" where

  type CFieldType DataPipeline "dataPipeline_process" =
    RIP.FunPtr ((RIP.Ptr Measurement) -> IO ())

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr Measurement) -> IO ()))
         ) => RIP.HasField "dataPipeline_process" (RIP.Ptr DataPipeline) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dataPipeline_process")

instance HasCField.HasCField DataPipeline "dataPipeline_postProcess" where

  type CFieldType DataPipeline "dataPipeline_postProcess" =
    RIP.FunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ())

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ()))
         ) => RIP.HasField "dataPipeline_postProcess" (RIP.Ptr DataPipeline) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dataPipeline_postProcess")

{-| __C declaration:__ @union ProcessorCallback@

    __defined at:__ @functions\/callbacks.h 69:7@

    __exported by:__ @functions\/callbacks.h@
-}
newtype ProcessorCallback = ProcessorCallback
  { unwrapProcessorCallback :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 8) 8 instance Marshal.StaticSize ProcessorCallback

deriving via (RIP.SizedByteArray 8) 8 instance Marshal.ReadRaw ProcessorCallback

deriving via (RIP.SizedByteArray 8) 8 instance Marshal.WriteRaw ProcessorCallback

deriving via Marshal.EquivStorable ProcessorCallback instance RIP.Storable ProcessorCallback

{-|

  __See:__ 'set_processorCallback_simple'

__C declaration:__ @simple@

__defined at:__ @functions\/callbacks.h 70:10@

__exported by:__ @functions\/callbacks.h@
-}
get_processorCallback_simple ::
     ProcessorCallback
  -> RIP.FunPtr ((RIP.Ptr Measurement) -> IO ())
get_processorCallback_simple = RIP.getUnionPayload

{-|

  __See:__ 'get_processorCallback_simple'

-}
set_processorCallback_simple ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> IO ())
  -> ProcessorCallback
set_processorCallback_simple = RIP.setUnionPayload

{-|

  __See:__ 'set_processorCallback_withValidator'

__C declaration:__ @withValidator@

__defined at:__ @functions\/callbacks.h 71:10@

__exported by:__ @functions\/callbacks.h@
-}
get_processorCallback_withValidator ::
     ProcessorCallback
  -> RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ())
get_processorCallback_withValidator =
  RIP.getUnionPayload

{-|

  __See:__ 'get_processorCallback_withValidator'

-}
set_processorCallback_withValidator ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ())
  -> ProcessorCallback
set_processorCallback_withValidator =
  RIP.setUnionPayload

{-|

  __See:__ 'set_processorCallback_withProgress'

__C declaration:__ @withProgress@

__defined at:__ @functions\/callbacks.h 72:10@

__exported by:__ @functions\/callbacks.h@
-}
get_processorCallback_withProgress ::
     ProcessorCallback
  -> RIP.FunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ())
get_processorCallback_withProgress =
  RIP.getUnionPayload

{-|

  __See:__ 'get_processorCallback_withProgress'

-}
set_processorCallback_withProgress ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> ProcessorCallback
set_processorCallback_withProgress =
  RIP.setUnionPayload

instance HasCField.HasCField ProcessorCallback "processorCallback_simple" where

  type CFieldType ProcessorCallback "processorCallback_simple" =
    RIP.FunPtr ((RIP.Ptr Measurement) -> IO ())

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr Measurement) -> IO ()))
         ) => RIP.HasField "processorCallback_simple" (RIP.Ptr ProcessorCallback) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"processorCallback_simple")

instance HasCField.HasCField ProcessorCallback "processorCallback_withValidator" where

  type CFieldType ProcessorCallback "processorCallback_withValidator" =
    RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ())

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ()))
         ) => RIP.HasField "processorCallback_withValidator" (RIP.Ptr ProcessorCallback) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"processorCallback_withValidator")

instance HasCField.HasCField ProcessorCallback "processorCallback_withProgress" where

  type CFieldType ProcessorCallback "processorCallback_withProgress" =
    RIP.FunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ())

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ()))
         ) => RIP.HasField "processorCallback_withProgress" (RIP.Ptr ProcessorCallback) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"processorCallback_withProgress")

{-| __C declaration:__ @enum \@Processor_mode@

    __defined at:__ @functions\/callbacks.h 76:3@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Processor_mode = Processor_mode
  { unwrapProcessor_mode :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Processor_mode where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Processor_mode where

  readRaw =
    \ptr0 ->
          pure Processor_mode
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Processor_mode where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Processor_mode unwrapProcessor_mode2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapProcessor_mode2

deriving via Marshal.EquivStorable Processor_mode instance RIP.Storable Processor_mode

deriving via RIP.CUInt instance RIP.Prim Processor_mode

instance CEnum.CEnum Processor_mode where

  type CEnumZ Processor_mode = RIP.CUInt

  toCEnum = Processor_mode

  fromCEnum = unwrapProcessor_mode

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (0, RIP.singleton "MODE_SIMPLE")
                                   , (1, RIP.singleton "MODE_VALIDATED")
                                   , (2, RIP.singleton "MODE_PROGRESS")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Processor_mode"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Processor_mode"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Processor_mode where

  minDeclaredValue = MODE_SIMPLE

  maxDeclaredValue = MODE_PROGRESS

instance Show Processor_mode where

  showsPrec = CEnum.shows

instance Read Processor_mode where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapProcessor_mode" (RIP.Ptr Processor_mode) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapProcessor_mode")

instance HasCField.HasCField Processor_mode "unwrapProcessor_mode" where

  type CFieldType Processor_mode "unwrapProcessor_mode" =
    RIP.CUInt

  offset# = \_ -> \_ -> 0

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
  deriving stock (RIP.Generic)

instance Marshal.StaticSize Processor where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Processor where

  readRaw =
    \ptr0 ->
          pure Processor
      <*> HasCField.readRaw (RIP.Proxy @"processor_mode") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"processor_callback") ptr0

instance Marshal.WriteRaw Processor where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Processor processor_mode2 processor_callback3 ->
               HasCField.writeRaw (RIP.Proxy @"processor_mode") ptr0 processor_mode2
            >> HasCField.writeRaw (RIP.Proxy @"processor_callback") ptr0 processor_callback3

deriving via Marshal.EquivStorable Processor instance RIP.Storable Processor

instance HasCField.HasCField Processor "processor_mode" where

  type CFieldType Processor "processor_mode" =
    Processor_mode

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Processor_mode
         ) => RIP.HasField "processor_mode" (RIP.Ptr Processor) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"processor_mode")

instance HasCField.HasCField Processor "processor_callback" where

  type CFieldType Processor "processor_callback" =
    ProcessorCallback

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) ProcessorCallback
         ) => RIP.HasField "processor_callback" (RIP.Ptr Processor) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"processor_callback")

{-| __C declaration:__ @foo@

    __defined at:__ @functions\/callbacks.h 94:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Foo = Foo
  { unwrapFoo :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapFoo" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapFoo")

instance HasCField.HasCField Foo "unwrapFoo" where

  type CFieldType Foo "unwrapFoo" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @foo2@

    __defined at:__ @functions\/callbacks.h 95:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Foo2 = Foo2
  { unwrapFoo2 :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapFoo2" (RIP.Ptr Foo2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFoo2")

instance HasCField.HasCField Foo2 "unwrapFoo2" where

  type CFieldType Foo2 "unwrapFoo2" = RIP.CInt

  offset# = \_ -> \_ -> 0

foreign import ccall safe "wrapper" hs_bindgen_d2a71f330b782e41_base ::
     (RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr (RIP.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (Foo -> IO ())@
hs_bindgen_d2a71f330b782e41 ::
     (Foo -> IO ())
  -> IO (RIP.FunPtr (Foo -> IO ()))
hs_bindgen_d2a71f330b782e41 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_d2a71f330b782e41_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_c08440542d338bad_base ::
     RIP.FunPtr (RIP.Int32 -> IO ())
  -> RIP.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (Foo -> IO ())@
hs_bindgen_c08440542d338bad ::
     RIP.FunPtr (Foo -> IO ())
  -> Foo -> IO ()
hs_bindgen_c08440542d338bad =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_c08440542d338bad_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr (Foo -> IO ()) where

  toFunPtr = hs_bindgen_d2a71f330b782e41

instance RIP.FromFunPtr (Foo -> IO ()) where

  fromFunPtr = hs_bindgen_c08440542d338bad

foreign import ccall safe "wrapper" hs_bindgen_521c86051e68bbb3_base ::
     ((RIP.Ptr RIP.Void) -> IO RIP.Int32)
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO RIP.Int32))

-- __unique:__ @instance ToFunPtr ((RIP.Ptr Measurement) -> IO RIP.CInt)@
hs_bindgen_521c86051e68bbb3 ::
     ((RIP.Ptr Measurement) -> IO RIP.CInt)
  -> IO (RIP.FunPtr ((RIP.Ptr Measurement) -> IO RIP.CInt))
hs_bindgen_521c86051e68bbb3 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_521c86051e68bbb3_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_5eb466d1f957f58c_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO RIP.Int32)
  -> (RIP.Ptr RIP.Void) -> IO RIP.Int32

-- __unique:__ @instance FromFunPtr ((RIP.Ptr Measurement) -> IO RIP.CInt)@
hs_bindgen_5eb466d1f957f58c ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> IO RIP.CInt)
  -> (RIP.Ptr Measurement) -> IO RIP.CInt
hs_bindgen_5eb466d1f957f58c =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_5eb466d1f957f58c_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr ((RIP.Ptr Measurement) -> IO RIP.CInt) where

  toFunPtr = hs_bindgen_521c86051e68bbb3

instance RIP.FromFunPtr ((RIP.Ptr Measurement) -> IO RIP.CInt) where

  fromFunPtr = hs_bindgen_5eb466d1f957f58c

foreign import ccall safe "wrapper" hs_bindgen_c094bf633d23be3e_base ::
     ((RIP.Ptr RIP.Void) -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO ()))

-- __unique:__ @instance ToFunPtr ((RIP.Ptr Measurement) -> IO ())@
hs_bindgen_c094bf633d23be3e ::
     ((RIP.Ptr Measurement) -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr Measurement) -> IO ()))
hs_bindgen_c094bf633d23be3e =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_c094bf633d23be3e_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_eb9da650677aa7fe_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO ())
  -> (RIP.Ptr RIP.Void) -> IO ()

-- __unique:__ @instance FromFunPtr ((RIP.Ptr Measurement) -> IO ())@
hs_bindgen_eb9da650677aa7fe ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> IO ())
  -> (RIP.Ptr Measurement) -> IO ()
hs_bindgen_eb9da650677aa7fe =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_eb9da650677aa7fe_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr ((RIP.Ptr Measurement) -> IO ()) where

  toFunPtr = hs_bindgen_c094bf633d23be3e

instance RIP.FromFunPtr ((RIP.Ptr Measurement) -> IO ()) where

  fromFunPtr = hs_bindgen_eb9da650677aa7fe

foreign import ccall safe "wrapper" hs_bindgen_17bc2816a744a35c_base ::
     ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ()))

-- __unique:__ @instance ToFunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ())@
hs_bindgen_17bc2816a744a35c ::
     ((RIP.Ptr Measurement) -> DataValidator -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ()))
hs_bindgen_17bc2816a744a35c =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_17bc2816a744a35c_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_8322d2225284de92_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ())
  -> (RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ()

-- __unique:__ @instance FromFunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ())@
hs_bindgen_8322d2225284de92 ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ())
  -> (RIP.Ptr Measurement) -> DataValidator -> IO ()
hs_bindgen_8322d2225284de92 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_8322d2225284de92_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ()) where

  toFunPtr = hs_bindgen_17bc2816a744a35c

instance RIP.FromFunPtr ((RIP.Ptr Measurement) -> DataValidator -> IO ()) where

  fromFunPtr = hs_bindgen_8322d2225284de92

foreign import ccall safe "wrapper" hs_bindgen_b1c648ee27a0d356_base ::
     ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())@
hs_bindgen_b1c648ee27a0d356 ::
     ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ()))
hs_bindgen_b1c648ee27a0d356 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_b1c648ee27a0d356_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_175c0f889e110bc0_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ())
  -> (RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())@
hs_bindgen_175c0f889e110bc0 ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())
  -> (RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ()
hs_bindgen_175c0f889e110bc0 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_175c0f889e110bc0_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ()) where

  toFunPtr = hs_bindgen_b1c648ee27a0d356

instance RIP.FromFunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_175c0f889e110bc0

foreign import ccall safe "wrapper" hs_bindgen_d3ad3aa63d8f913a_base ::
     ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr ((RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ())@
hs_bindgen_d3ad3aa63d8f913a ::
     ((RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ()))
hs_bindgen_d3ad3aa63d8f913a =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_d3ad3aa63d8f913a_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_9474c84c17b23d2c_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ())
  -> (RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr ((RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ())@
hs_bindgen_9474c84c17b23d2c ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ())
  -> (RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ()
hs_bindgen_9474c84c17b23d2c =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_9474c84c17b23d2c_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr ((RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ()) where

  toFunPtr = hs_bindgen_d3ad3aa63d8f913a

instance RIP.FromFunPtr ((RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_9474c84c17b23d2c

foreign import ccall safe "wrapper" hs_bindgen_57717f4584d0f03b_base ::
     ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ()))

-- __unique:__ @instance ToFunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ())@
hs_bindgen_57717f4584d0f03b ::
     ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ()))
hs_bindgen_57717f4584d0f03b =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_57717f4584d0f03b_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_e2a00fe9c43288de_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ())
  -> (RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ()

-- __unique:__ @instance FromFunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ())@
hs_bindgen_e2a00fe9c43288de ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> (RIP.Ptr Measurement) -> ProgressUpdate -> IO ()
hs_bindgen_e2a00fe9c43288de =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_e2a00fe9c43288de_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ()) where

  toFunPtr = hs_bindgen_57717f4584d0f03b

instance RIP.FromFunPtr ((RIP.Ptr Measurement) -> ProgressUpdate -> IO ()) where

  fromFunPtr = hs_bindgen_e2a00fe9c43288de

foreign import ccall safe "wrapper" hs_bindgen_eff2003c25feccf7_base ::
     ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ())@
hs_bindgen_eff2003c25feccf7 ::
     ((RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ()))
hs_bindgen_eff2003c25feccf7 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_eff2003c25feccf7_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_0e7d3da67c0294df_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ())
  -> (RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ())@
hs_bindgen_0e7d3da67c0294df ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ())
  -> (RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ()
hs_bindgen_0e7d3da67c0294df =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_0e7d3da67c0294df_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ()) where

  toFunPtr = hs_bindgen_eff2003c25feccf7

instance RIP.FromFunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_0e7d3da67c0294df

foreign import ccall safe "wrapper" hs_bindgen_14404b13c0375937_base ::
     ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ()))

-- __unique:__ @instance ToFunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ())@
hs_bindgen_14404b13c0375937 ::
     ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ()))
hs_bindgen_14404b13c0375937 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_14404b13c0375937_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_b8bbcdafa7814660_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ())
  -> (RIP.Ptr RIP.Void) -> (RIP.FunPtr RIP.Void) -> (RIP.FunPtr RIP.Void) -> IO ()

-- __unique:__ @instance FromFunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ())@
hs_bindgen_b8bbcdafa7814660 ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ())
  -> (RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ()
hs_bindgen_b8bbcdafa7814660 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_b8bbcdafa7814660_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ()) where

  toFunPtr = hs_bindgen_14404b13c0375937

instance RIP.FromFunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ()) where

  fromFunPtr = hs_bindgen_b8bbcdafa7814660

foreign import ccall safe "wrapper" hs_bindgen_235fa4a89af25f04_base ::
     (RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr (RIP.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (Foo2 -> IO ())@
hs_bindgen_235fa4a89af25f04 ::
     (Foo2 -> IO ())
  -> IO (RIP.FunPtr (Foo2 -> IO ()))
hs_bindgen_235fa4a89af25f04 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_235fa4a89af25f04_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_8605b223a9ab9562_base ::
     RIP.FunPtr (RIP.Int32 -> IO ())
  -> RIP.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (Foo2 -> IO ())@
hs_bindgen_8605b223a9ab9562 ::
     RIP.FunPtr (Foo2 -> IO ())
  -> Foo2 -> IO ()
hs_bindgen_8605b223a9ab9562 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_8605b223a9ab9562_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr (Foo2 -> IO ()) where

  toFunPtr = hs_bindgen_235fa4a89af25f04

instance RIP.FromFunPtr (Foo2 -> IO ()) where

  fromFunPtr = hs_bindgen_8605b223a9ab9562
