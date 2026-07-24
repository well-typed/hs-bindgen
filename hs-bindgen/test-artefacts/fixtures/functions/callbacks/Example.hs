{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Example
    ( Example.FileOpenedNotification_Aux(..)
    , Example.FileOpenedNotification(..)
    , Example.ProgressUpdate_Aux(..)
    , Example.ProgressUpdate(..)
    , Example.DataValidator_Aux(..)
    , Example.DataValidator(..)
    , Example.Measurement(..)
    , Example.MeasurementReceived_Aux(..)
    , Example.MeasurementReceived(..)
    , Example.MeasurementReceived2_Aux(..)
    , Example.MeasurementReceived2(..)
    , Example.SampleBufferFull_Aux(..)
    , Example.SampleBufferFull(..)
    , Example.MeasurementHandler(..)
    , Example.DataPipeline(..)
    , Example.ProcessorCallback(..)
    , Example.Processor_mode(..)
    , pattern Example.MODE_SIMPLE
    , pattern Example.MODE_VALIDATED
    , pattern Example.MODE_PROGRESS
    , Example.Processor(..)
    , Example.Foo(..)
    , Example.Foo2(..)
    , Example.A(..)
    , Example.B(..)
    , Example.S(..)
    , Example.C(..)
    , Example.U(..)
    , Example.D(..)
    , Example.T(..)
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| Auxiliary type used by 'FileOpenedNotification'

    __C declaration:__ @FileOpenedNotification@

    __defined at:__ @functions\/callbacks.h 10:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype FileOpenedNotification_Aux = FileOpenedNotification_Aux
  { unwrapFileOpenedNotification_Aux :: IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toFileOpenedNotification_Aux@
foreign import ccall safe "wrapper" hs_bindgen_b3b8b1fad168671a_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toFileOpenedNotification_Aux@
hs_bindgen_b3b8b1fad168671a ::
     FileOpenedNotification_Aux
  -> IO (BG.FunPtr FileOpenedNotification_Aux)
hs_bindgen_b3b8b1fad168671a =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_b3b8b1fad168671a_base (BG.toFFIType fun0))

-- __unique:__ @fromFileOpenedNotification_Aux@
foreign import ccall safe "dynamic" hs_bindgen_f3ba5920f34c7f6a_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromFileOpenedNotification_Aux@
hs_bindgen_f3ba5920f34c7f6a ::
     BG.FunPtr FileOpenedNotification_Aux
  -> FileOpenedNotification_Aux
hs_bindgen_f3ba5920f34c7f6a =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_f3ba5920f34c7f6a_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr FileOpenedNotification_Aux where

  toFunPtr = hs_bindgen_b3b8b1fad168671a

instance BG.FromFunPtr FileOpenedNotification_Aux where

  fromFunPtr = hs_bindgen_f3ba5920f34c7f6a

instance ( ty ~ IO ()
         ) => BG.CompatHasField.HasField "unwrapFileOpenedNotification_Aux" FileOpenedNotification_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          FileOpenedNotification_Aux {unwrapFileOpenedNotification_Aux = y1}
      , BG.getField @"unwrapFileOpenedNotification_Aux" x0
      )

instance ( ty ~ IO ()
         ) => BG.HasField "unwrapFileOpenedNotification_Aux" (BG.Ptr FileOpenedNotification_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFileOpenedNotification_Aux")

instance HasCField.HasCField FileOpenedNotification_Aux "unwrapFileOpenedNotification_Aux" where

  type CFieldType FileOpenedNotification_Aux "unwrapFileOpenedNotification_Aux" =
    IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FileOpenedNotification@

    __defined at:__ @functions\/callbacks.h 10:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype FileOpenedNotification = FileOpenedNotification
  { unwrapFileOpenedNotification :: BG.FunPtr FileOpenedNotification_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr FileOpenedNotification_Aux
         ) => BG.CompatHasField.HasField "unwrapFileOpenedNotification" FileOpenedNotification ty where

  hasField =
    \x0 ->
      ( \y1 ->
          FileOpenedNotification {unwrapFileOpenedNotification = y1}
      , BG.getField @"unwrapFileOpenedNotification" x0
      )

instance ( ty ~ BG.FunPtr FileOpenedNotification_Aux
         ) => BG.HasField "unwrapFileOpenedNotification" (BG.Ptr FileOpenedNotification) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFileOpenedNotification")

instance HasCField.HasCField FileOpenedNotification "unwrapFileOpenedNotification" where

  type CFieldType FileOpenedNotification "unwrapFileOpenedNotification" =
    BG.FunPtr FileOpenedNotification_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'ProgressUpdate'

    __C declaration:__ @ProgressUpdate@

    __defined at:__ @functions\/callbacks.h 11:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype ProgressUpdate_Aux = ProgressUpdate_Aux
  { unwrapProgressUpdate_Aux :: BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toProgressUpdate_Aux@
foreign import ccall safe "wrapper" hs_bindgen_d551f31556ffa727_base ::
     (BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> IO ()))

-- __unique:__ @toProgressUpdate_Aux@
hs_bindgen_d551f31556ffa727 ::
     ProgressUpdate_Aux
  -> IO (BG.FunPtr ProgressUpdate_Aux)
hs_bindgen_d551f31556ffa727 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_d551f31556ffa727_base (BG.toFFIType fun0))

-- __unique:__ @fromProgressUpdate_Aux@
foreign import ccall safe "dynamic" hs_bindgen_ccf7f4b62a839a04_base ::
     BG.FunPtr (BG.Int32 -> IO ())
  -> BG.Int32 -> IO ()

-- __unique:__ @fromProgressUpdate_Aux@
hs_bindgen_ccf7f4b62a839a04 ::
     BG.FunPtr ProgressUpdate_Aux
  -> ProgressUpdate_Aux
hs_bindgen_ccf7f4b62a839a04 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_ccf7f4b62a839a04_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr ProgressUpdate_Aux where

  toFunPtr = hs_bindgen_d551f31556ffa727

instance BG.FromFunPtr ProgressUpdate_Aux where

  fromFunPtr = hs_bindgen_ccf7f4b62a839a04

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapProgressUpdate_Aux" ProgressUpdate_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          ProgressUpdate_Aux {unwrapProgressUpdate_Aux = y1}
      , BG.getField @"unwrapProgressUpdate_Aux" x0
      )

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.HasField "unwrapProgressUpdate_Aux" (BG.Ptr ProgressUpdate_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapProgressUpdate_Aux")

instance HasCField.HasCField ProgressUpdate_Aux "unwrapProgressUpdate_Aux" where

  type CFieldType ProgressUpdate_Aux "unwrapProgressUpdate_Aux" =
    BG.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @ProgressUpdate@

    __defined at:__ @functions\/callbacks.h 11:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype ProgressUpdate = ProgressUpdate
  { unwrapProgressUpdate :: BG.FunPtr ProgressUpdate_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr ProgressUpdate_Aux
         ) => BG.CompatHasField.HasField "unwrapProgressUpdate" ProgressUpdate ty where

  hasField =
    \x0 ->
      ( \y1 -> ProgressUpdate {unwrapProgressUpdate = y1}
      , BG.getField @"unwrapProgressUpdate" x0
      )

instance ( ty ~ BG.FunPtr ProgressUpdate_Aux
         ) => BG.HasField "unwrapProgressUpdate" (BG.Ptr ProgressUpdate) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapProgressUpdate")

instance HasCField.HasCField ProgressUpdate "unwrapProgressUpdate" where

  type CFieldType ProgressUpdate "unwrapProgressUpdate" =
    BG.FunPtr ProgressUpdate_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'DataValidator'

    __C declaration:__ @DataValidator@

    __defined at:__ @functions\/callbacks.h 12:15@

    __exported by:__ @functions\/callbacks.h@
-}
newtype DataValidator_Aux = DataValidator_Aux
  { unwrapDataValidator_Aux :: BG.CInt -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toDataValidator_Aux@
foreign import ccall safe "wrapper" hs_bindgen_c656ca21e63343d6_base ::
     (BG.Int32 -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int32 -> IO BG.Int32))

-- __unique:__ @toDataValidator_Aux@
hs_bindgen_c656ca21e63343d6 ::
     DataValidator_Aux
  -> IO (BG.FunPtr DataValidator_Aux)
hs_bindgen_c656ca21e63343d6 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_c656ca21e63343d6_base (BG.toFFIType fun0))

-- __unique:__ @fromDataValidator_Aux@
foreign import ccall safe "dynamic" hs_bindgen_c1e79a4c11ca4033_base ::
     BG.FunPtr (BG.Int32 -> IO BG.Int32)
  -> BG.Int32 -> IO BG.Int32

-- __unique:__ @fromDataValidator_Aux@
hs_bindgen_c1e79a4c11ca4033 ::
     BG.FunPtr DataValidator_Aux
  -> DataValidator_Aux
hs_bindgen_c1e79a4c11ca4033 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_c1e79a4c11ca4033_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr DataValidator_Aux where

  toFunPtr = hs_bindgen_c656ca21e63343d6

instance BG.FromFunPtr DataValidator_Aux where

  fromFunPtr = hs_bindgen_c1e79a4c11ca4033

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapDataValidator_Aux" DataValidator_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          DataValidator_Aux {unwrapDataValidator_Aux = y1}
      , BG.getField @"unwrapDataValidator_Aux" x0
      )

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "unwrapDataValidator_Aux" (BG.Ptr DataValidator_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapDataValidator_Aux")

instance HasCField.HasCField DataValidator_Aux "unwrapDataValidator_Aux" where

  type CFieldType DataValidator_Aux "unwrapDataValidator_Aux" =
    BG.CInt -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @DataValidator@

    __defined at:__ @functions\/callbacks.h 12:15@

    __exported by:__ @functions\/callbacks.h@
-}
newtype DataValidator = DataValidator
  { unwrapDataValidator :: BG.FunPtr DataValidator_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr DataValidator_Aux
         ) => BG.CompatHasField.HasField "unwrapDataValidator" DataValidator ty where

  hasField =
    \x0 ->
      ( \y1 -> DataValidator {unwrapDataValidator = y1}
      , BG.getField @"unwrapDataValidator" x0
      )

instance ( ty ~ BG.FunPtr DataValidator_Aux
         ) => BG.HasField "unwrapDataValidator" (BG.Ptr DataValidator) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapDataValidator")

instance HasCField.HasCField DataValidator "unwrapDataValidator" where

  type CFieldType DataValidator "unwrapDataValidator" =
    BG.FunPtr DataValidator_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Measurement@

    __defined at:__ @functions\/callbacks.h 21:8@

    __exported by:__ @functions\/callbacks.h@
-}
data Measurement = Measurement
  { measurement_value :: BG.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @functions\/callbacks.h 22:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , measurement_timestamp :: BG.CDouble
    {- ^ __C declaration:__ @timestamp@

         __defined at:__ @functions\/callbacks.h 23:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Measurement where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Measurement where

  readRaw =
    \ptr0 ->
          pure Measurement
      <*> HasCField.readRaw (BG.Proxy @"measurement_value") ptr0
      <*> HasCField.readRaw (BG.Proxy @"measurement_timestamp") ptr0

instance Marshal.WriteRaw Measurement where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Measurement measurement_value2 measurement_timestamp3 ->
               HasCField.writeRaw (BG.Proxy @"measurement_value") ptr0 measurement_value2
            >> HasCField.writeRaw (BG.Proxy @"measurement_timestamp") ptr0 measurement_timestamp3

deriving via Marshal.EquivStorable Measurement instance BG.Storable Measurement

deriving via Struct.IsStructViaStorable Measurement instance Struct.IsStruct Measurement

{-| __C declaration:__ @value@

    __defined at:__ @functions\/callbacks.h 22:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "measurement_value" Measurement ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Measurement { measurement_value = y1
                      , measurement_timestamp = BG.getField @"measurement_timestamp" x0
                      }
      , BG.getField @"measurement_value" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "measurement_value" (BG.Ptr Measurement) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"measurement_value")

instance HasCField.HasCField Measurement "measurement_value" where

  type CFieldType Measurement "measurement_value" =
    BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @timestamp@

    __defined at:__ @functions\/callbacks.h 23:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "measurement_timestamp" Measurement ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Measurement { measurement_timestamp = y1
                      , measurement_value = BG.getField @"measurement_value" x0
                      }
      , BG.getField @"measurement_timestamp" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "measurement_timestamp" (BG.Ptr Measurement) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"measurement_timestamp")

instance HasCField.HasCField Measurement "measurement_timestamp" where

  type CFieldType Measurement "measurement_timestamp" =
    BG.CDouble

  offset# = \_ -> \_ -> 8

{-| Auxiliary type used by 'MeasurementReceived'

    __C declaration:__ @MeasurementReceived@

    __defined at:__ @functions\/callbacks.h 26:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived_Aux = MeasurementReceived_Aux
  { unwrapMeasurementReceived_Aux :: BG.Ptr Measurement -> IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toMeasurementReceived_Aux@
foreign import ccall safe "wrapper" hs_bindgen_9259654df9d40f5b_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @toMeasurementReceived_Aux@
hs_bindgen_9259654df9d40f5b ::
     MeasurementReceived_Aux
  -> IO (BG.FunPtr MeasurementReceived_Aux)
hs_bindgen_9259654df9d40f5b =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_9259654df9d40f5b_base (BG.toFFIType fun0))

-- __unique:__ @fromMeasurementReceived_Aux@
foreign import ccall safe "dynamic" hs_bindgen_383c36bb22947621_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @fromMeasurementReceived_Aux@
hs_bindgen_383c36bb22947621 ::
     BG.FunPtr MeasurementReceived_Aux
  -> MeasurementReceived_Aux
hs_bindgen_383c36bb22947621 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_383c36bb22947621_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr MeasurementReceived_Aux where

  toFunPtr = hs_bindgen_9259654df9d40f5b

instance BG.FromFunPtr MeasurementReceived_Aux where

  fromFunPtr = hs_bindgen_383c36bb22947621

instance ( ty ~ (BG.Ptr Measurement -> IO ())
         ) => BG.CompatHasField.HasField "unwrapMeasurementReceived_Aux" MeasurementReceived_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MeasurementReceived_Aux {unwrapMeasurementReceived_Aux = y1}
      , BG.getField @"unwrapMeasurementReceived_Aux" x0
      )

instance ( ty ~ (BG.Ptr Measurement -> IO ())
         ) => BG.HasField "unwrapMeasurementReceived_Aux" (BG.Ptr MeasurementReceived_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMeasurementReceived_Aux")

instance HasCField.HasCField MeasurementReceived_Aux "unwrapMeasurementReceived_Aux" where

  type CFieldType MeasurementReceived_Aux "unwrapMeasurementReceived_Aux" =
    BG.Ptr Measurement -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MeasurementReceived@

    __defined at:__ @functions\/callbacks.h 26:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived = MeasurementReceived
  { unwrapMeasurementReceived :: BG.FunPtr MeasurementReceived_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr MeasurementReceived_Aux
         ) => BG.CompatHasField.HasField "unwrapMeasurementReceived" MeasurementReceived ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MeasurementReceived {unwrapMeasurementReceived = y1}
      , BG.getField @"unwrapMeasurementReceived" x0
      )

instance ( ty ~ BG.FunPtr MeasurementReceived_Aux
         ) => BG.HasField "unwrapMeasurementReceived" (BG.Ptr MeasurementReceived) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMeasurementReceived")

instance HasCField.HasCField MeasurementReceived "unwrapMeasurementReceived" where

  type CFieldType MeasurementReceived "unwrapMeasurementReceived" =
    BG.FunPtr MeasurementReceived_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'MeasurementReceived2'

    __C declaration:__ @MeasurementReceived2@

    __defined at:__ @functions\/callbacks.h 29:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived2_Aux = MeasurementReceived2_Aux
  { unwrapMeasurementReceived2_Aux :: Measurement -> IO ()
  }
  deriving stock (BG.Generic)

instance ( ty ~ (Measurement -> IO ())
         ) => BG.CompatHasField.HasField "unwrapMeasurementReceived2_Aux" MeasurementReceived2_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MeasurementReceived2_Aux {unwrapMeasurementReceived2_Aux = y1}
      , BG.getField @"unwrapMeasurementReceived2_Aux" x0
      )

instance ( ty ~ (Measurement -> IO ())
         ) => BG.HasField "unwrapMeasurementReceived2_Aux" (BG.Ptr MeasurementReceived2_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMeasurementReceived2_Aux")

instance HasCField.HasCField MeasurementReceived2_Aux "unwrapMeasurementReceived2_Aux" where

  type CFieldType MeasurementReceived2_Aux "unwrapMeasurementReceived2_Aux" =
    Measurement -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MeasurementReceived2@

    __defined at:__ @functions\/callbacks.h 29:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived2 = MeasurementReceived2
  { unwrapMeasurementReceived2 :: BG.FunPtr MeasurementReceived2_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr MeasurementReceived2_Aux
         ) => BG.CompatHasField.HasField "unwrapMeasurementReceived2" MeasurementReceived2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MeasurementReceived2 {unwrapMeasurementReceived2 = y1}
      , BG.getField @"unwrapMeasurementReceived2" x0
      )

instance ( ty ~ BG.FunPtr MeasurementReceived2_Aux
         ) => BG.HasField "unwrapMeasurementReceived2" (BG.Ptr MeasurementReceived2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMeasurementReceived2")

instance HasCField.HasCField MeasurementReceived2 "unwrapMeasurementReceived2" where

  type CFieldType MeasurementReceived2 "unwrapMeasurementReceived2" =
    BG.FunPtr MeasurementReceived2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'SampleBufferFull'

    __C declaration:__ @SampleBufferFull@

    __defined at:__ @functions\/callbacks.h 32:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype SampleBufferFull_Aux = SampleBufferFull_Aux
  { unwrapSampleBufferFull_Aux :: BG.Ptr (IsA.Elem (CA.ConstantArray 10 BG.CInt)) -> IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toSampleBufferFull_Aux@
foreign import ccall safe "wrapper" hs_bindgen_57d9e30494ae4453_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @toSampleBufferFull_Aux@
hs_bindgen_57d9e30494ae4453 ::
     SampleBufferFull_Aux
  -> IO (BG.FunPtr SampleBufferFull_Aux)
hs_bindgen_57d9e30494ae4453 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_57d9e30494ae4453_base (BG.toFFIType fun0))

-- __unique:__ @fromSampleBufferFull_Aux@
foreign import ccall safe "dynamic" hs_bindgen_2ab7ac6bb756ba7e_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @fromSampleBufferFull_Aux@
hs_bindgen_2ab7ac6bb756ba7e ::
     BG.FunPtr SampleBufferFull_Aux
  -> SampleBufferFull_Aux
hs_bindgen_2ab7ac6bb756ba7e =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_2ab7ac6bb756ba7e_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr SampleBufferFull_Aux where

  toFunPtr = hs_bindgen_57d9e30494ae4453

instance BG.FromFunPtr SampleBufferFull_Aux where

  fromFunPtr = hs_bindgen_2ab7ac6bb756ba7e

instance ( ty ~ (BG.Ptr (IsA.Elem (CA.ConstantArray 10 BG.CInt)) -> IO ())
         ) => BG.CompatHasField.HasField "unwrapSampleBufferFull_Aux" SampleBufferFull_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SampleBufferFull_Aux {unwrapSampleBufferFull_Aux = y1}
      , BG.getField @"unwrapSampleBufferFull_Aux" x0
      )

instance ( ty ~ (BG.Ptr (IsA.Elem (CA.ConstantArray 10 BG.CInt)) -> IO ())
         ) => BG.HasField "unwrapSampleBufferFull_Aux" (BG.Ptr SampleBufferFull_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapSampleBufferFull_Aux")

instance HasCField.HasCField SampleBufferFull_Aux "unwrapSampleBufferFull_Aux" where

  type CFieldType SampleBufferFull_Aux "unwrapSampleBufferFull_Aux" =
    BG.Ptr (IsA.Elem (CA.ConstantArray 10 BG.CInt)) -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SampleBufferFull@

    __defined at:__ @functions\/callbacks.h 32:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype SampleBufferFull = SampleBufferFull
  { unwrapSampleBufferFull :: BG.FunPtr SampleBufferFull_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr SampleBufferFull_Aux
         ) => BG.CompatHasField.HasField "unwrapSampleBufferFull" SampleBufferFull ty where

  hasField =
    \x0 ->
      ( \y1 ->
          SampleBufferFull {unwrapSampleBufferFull = y1}
      , BG.getField @"unwrapSampleBufferFull" x0
      )

instance ( ty ~ BG.FunPtr SampleBufferFull_Aux
         ) => BG.HasField "unwrapSampleBufferFull" (BG.Ptr SampleBufferFull) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapSampleBufferFull")

instance HasCField.HasCField SampleBufferFull "unwrapSampleBufferFull" where

  type CFieldType SampleBufferFull "unwrapSampleBufferFull" =
    BG.FunPtr SampleBufferFull_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct MeasurementHandler@

    __defined at:__ @functions\/callbacks.h 50:8@

    __exported by:__ @functions\/callbacks.h@
-}
data MeasurementHandler = MeasurementHandler
  { measurementHandler_onReceived :: BG.FunPtr (BG.Ptr Measurement -> IO ())
    {- ^ __C declaration:__ @onReceived@

         __defined at:__ @functions\/callbacks.h 51:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , measurementHandler_validate :: BG.FunPtr (BG.Ptr Measurement -> IO BG.CInt)
    {- ^ __C declaration:__ @validate@

         __defined at:__ @functions\/callbacks.h 52:9@

         __exported by:__ @functions\/callbacks.h@
    -}
  , measurementHandler_onError :: BG.FunPtr (BG.CInt -> IO ())
    {- ^ __C declaration:__ @onError@

         __defined at:__ @functions\/callbacks.h 53:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize MeasurementHandler where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw MeasurementHandler where

  readRaw =
    \ptr0 ->
          pure MeasurementHandler
      <*> HasCField.readRaw (BG.Proxy @"measurementHandler_onReceived") ptr0
      <*> HasCField.readRaw (BG.Proxy @"measurementHandler_validate") ptr0
      <*> HasCField.readRaw (BG.Proxy @"measurementHandler_onError") ptr0

instance Marshal.WriteRaw MeasurementHandler where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MeasurementHandler
            measurementHandler_onReceived2
            measurementHandler_validate3
            measurementHandler_onError4 ->
                 HasCField.writeRaw (BG.Proxy @"measurementHandler_onReceived") ptr0 measurementHandler_onReceived2
              >> HasCField.writeRaw (BG.Proxy @"measurementHandler_validate") ptr0 measurementHandler_validate3
              >> HasCField.writeRaw (BG.Proxy @"measurementHandler_onError") ptr0 measurementHandler_onError4

deriving via Marshal.EquivStorable MeasurementHandler instance BG.Storable MeasurementHandler

deriving via Struct.IsStructViaStorable MeasurementHandler instance Struct.IsStruct MeasurementHandler

{-| __C declaration:__ @onReceived@

    __defined at:__ @functions\/callbacks.h 51:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> IO ())
         ) => BG.CompatHasField.HasField "measurementHandler_onReceived" MeasurementHandler ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MeasurementHandler { measurementHandler_onReceived = y1
                             , measurementHandler_validate = BG.getField @"measurementHandler_validate" x0
                             , measurementHandler_onError = BG.getField @"measurementHandler_onError" x0
                             }
      , BG.getField @"measurementHandler_onReceived" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> IO ())
         ) => BG.HasField "measurementHandler_onReceived" (BG.Ptr MeasurementHandler) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"measurementHandler_onReceived")

instance HasCField.HasCField MeasurementHandler "measurementHandler_onReceived" where

  type CFieldType MeasurementHandler "measurementHandler_onReceived" =
    BG.FunPtr (BG.Ptr Measurement -> IO ())

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @validate@

    __defined at:__ @functions\/callbacks.h 52:9@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "measurementHandler_validate" MeasurementHandler ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MeasurementHandler { measurementHandler_validate = y1
                             , measurementHandler_onReceived = BG.getField @"measurementHandler_onReceived" x0
                             , measurementHandler_onError = BG.getField @"measurementHandler_onError" x0
                             }
      , BG.getField @"measurementHandler_validate" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> IO BG.CInt)
         ) => BG.HasField "measurementHandler_validate" (BG.Ptr MeasurementHandler) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"measurementHandler_validate")

instance HasCField.HasCField MeasurementHandler "measurementHandler_validate" where

  type CFieldType MeasurementHandler "measurementHandler_validate" =
    BG.FunPtr (BG.Ptr Measurement -> IO BG.CInt)

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @onError@

    __defined at:__ @functions\/callbacks.h 53:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "measurementHandler_onError" MeasurementHandler ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MeasurementHandler { measurementHandler_onError = y1
                             , measurementHandler_onReceived = BG.getField @"measurementHandler_onReceived" x0
                             , measurementHandler_validate = BG.getField @"measurementHandler_validate" x0
                             }
      , BG.getField @"measurementHandler_onError" x0
      )

instance ( ty ~ BG.FunPtr (BG.CInt -> IO ())
         ) => BG.HasField "measurementHandler_onError" (BG.Ptr MeasurementHandler) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"measurementHandler_onError")

instance HasCField.HasCField MeasurementHandler "measurementHandler_onError" where

  type CFieldType MeasurementHandler "measurementHandler_onError" =
    BG.FunPtr (BG.CInt -> IO ())

  offset# = \_ -> \_ -> 16

{-| __C declaration:__ @struct DataPipeline@

    __defined at:__ @functions\/callbacks.h 58:8@

    __exported by:__ @functions\/callbacks.h@
-}
data DataPipeline = DataPipeline
  { dataPipeline_preProcess :: BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ())
    {- ^ __C declaration:__ @preProcess@

         __defined at:__ @functions\/callbacks.h 59:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , dataPipeline_process :: BG.FunPtr (BG.Ptr Measurement -> IO ())
    {- ^ __C declaration:__ @process@

         __defined at:__ @functions\/callbacks.h 60:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  , dataPipeline_postProcess :: BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())
    {- ^ __C declaration:__ @postProcess@

         __defined at:__ @functions\/callbacks.h 61:10@

         __exported by:__ @functions\/callbacks.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize DataPipeline where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw DataPipeline where

  readRaw =
    \ptr0 ->
          pure DataPipeline
      <*> HasCField.readRaw (BG.Proxy @"dataPipeline_preProcess") ptr0
      <*> HasCField.readRaw (BG.Proxy @"dataPipeline_process") ptr0
      <*> HasCField.readRaw (BG.Proxy @"dataPipeline_postProcess") ptr0

instance Marshal.WriteRaw DataPipeline where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          DataPipeline
            dataPipeline_preProcess2
            dataPipeline_process3
            dataPipeline_postProcess4 ->
                 HasCField.writeRaw (BG.Proxy @"dataPipeline_preProcess") ptr0 dataPipeline_preProcess2
              >> HasCField.writeRaw (BG.Proxy @"dataPipeline_process") ptr0 dataPipeline_process3
              >> HasCField.writeRaw (BG.Proxy @"dataPipeline_postProcess") ptr0 dataPipeline_postProcess4

deriving via Marshal.EquivStorable DataPipeline instance BG.Storable DataPipeline

deriving via Struct.IsStructViaStorable DataPipeline instance Struct.IsStruct DataPipeline

{-| __C declaration:__ @preProcess@

    __defined at:__ @functions\/callbacks.h 59:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ())
         ) => BG.CompatHasField.HasField "dataPipeline_preProcess" DataPipeline ty where

  hasField =
    \x0 ->
      ( \y1 ->
          DataPipeline { dataPipeline_preProcess = y1
                       , dataPipeline_process = BG.getField @"dataPipeline_process" x0
                       , dataPipeline_postProcess = BG.getField @"dataPipeline_postProcess" x0
                       }
      , BG.getField @"dataPipeline_preProcess" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ())
         ) => BG.HasField "dataPipeline_preProcess" (BG.Ptr DataPipeline) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"dataPipeline_preProcess")

instance HasCField.HasCField DataPipeline "dataPipeline_preProcess" where

  type CFieldType DataPipeline "dataPipeline_preProcess" =
    BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ())

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @process@

    __defined at:__ @functions\/callbacks.h 60:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> IO ())
         ) => BG.CompatHasField.HasField "dataPipeline_process" DataPipeline ty where

  hasField =
    \x0 ->
      ( \y1 ->
          DataPipeline { dataPipeline_process = y1
                       , dataPipeline_preProcess = BG.getField @"dataPipeline_preProcess" x0
                       , dataPipeline_postProcess = BG.getField @"dataPipeline_postProcess" x0
                       }
      , BG.getField @"dataPipeline_process" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> IO ())
         ) => BG.HasField "dataPipeline_process" (BG.Ptr DataPipeline) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"dataPipeline_process")

instance HasCField.HasCField DataPipeline "dataPipeline_process" where

  type CFieldType DataPipeline "dataPipeline_process" =
    BG.FunPtr (BG.Ptr Measurement -> IO ())

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @postProcess@

    __defined at:__ @functions\/callbacks.h 61:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())
         ) => BG.CompatHasField.HasField "dataPipeline_postProcess" DataPipeline ty where

  hasField =
    \x0 ->
      ( \y1 ->
          DataPipeline { dataPipeline_postProcess = y1
                       , dataPipeline_preProcess = BG.getField @"dataPipeline_preProcess" x0
                       , dataPipeline_process = BG.getField @"dataPipeline_process" x0
                       }
      , BG.getField @"dataPipeline_postProcess" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())
         ) => BG.HasField "dataPipeline_postProcess" (BG.Ptr DataPipeline) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"dataPipeline_postProcess")

instance HasCField.HasCField DataPipeline "dataPipeline_postProcess" where

  type CFieldType DataPipeline "dataPipeline_postProcess" =
    BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())

  offset# = \_ -> \_ -> 16

{-| __C declaration:__ @union ProcessorCallback@

    __defined at:__ @functions\/callbacks.h 69:7@

    __exported by:__ @functions\/callbacks.h@
-}
newtype ProcessorCallback = ProcessorCallback
  { unwrapProcessorCallback :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 8 8 instance Marshal.StaticSize ProcessorCallback

deriving via BG.SizedByteArray 8 8 instance Marshal.ReadRaw ProcessorCallback

deriving via BG.SizedByteArray 8 8 instance Marshal.WriteRaw ProcessorCallback

deriving via Marshal.EquivStorable ProcessorCallback instance BG.Storable ProcessorCallback

deriving via BG.SizedByteArray 8 8 instance Union.IsUnion ProcessorCallback

{-| __C declaration:__ @simple@

    __defined at:__ @functions\/callbacks.h 70:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> IO ())
         ) => BG.HasField "processorCallback_simple" ProcessorCallback ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @simple@

    __defined at:__ @functions\/callbacks.h 70:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> IO ())
         ) => BG.CompatHasField.HasField "processorCallback_simple" ProcessorCallback ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"processorCallback_simple" x0)

instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> IO ())
         ) => BG.HasField "processorCallback_simple" (BG.Ptr ProcessorCallback) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"processorCallback_simple")

instance HasCField.HasCField ProcessorCallback "processorCallback_simple" where

  type CFieldType ProcessorCallback "processorCallback_simple" =
    BG.FunPtr (BG.Ptr Measurement -> IO ())

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @withValidator@

    __defined at:__ @functions\/callbacks.h 71:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ())
         ) => BG.HasField "processorCallback_withValidator" ProcessorCallback ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @withValidator@

    __defined at:__ @functions\/callbacks.h 71:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ())
         ) => BG.CompatHasField.HasField "processorCallback_withValidator" ProcessorCallback ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"processorCallback_withValidator" x0)

instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ())
         ) => BG.HasField "processorCallback_withValidator" (BG.Ptr ProcessorCallback) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"processorCallback_withValidator")

instance HasCField.HasCField ProcessorCallback "processorCallback_withValidator" where

  type CFieldType ProcessorCallback "processorCallback_withValidator" =
    BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ())

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @withProgress@

    __defined at:__ @functions\/callbacks.h 72:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())
         ) => BG.HasField "processorCallback_withProgress" ProcessorCallback ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @withProgress@

    __defined at:__ @functions\/callbacks.h 72:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())
         ) => BG.CompatHasField.HasField "processorCallback_withProgress" ProcessorCallback ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"processorCallback_withProgress" x0)

instance ( ty ~ BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())
         ) => BG.HasField "processorCallback_withProgress" (BG.Ptr ProcessorCallback) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"processorCallback_withProgress")

instance HasCField.HasCField ProcessorCallback "processorCallback_withProgress" where

  type CFieldType ProcessorCallback "processorCallback_withProgress" =
    BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @enum \@Processor_mode@

    __defined at:__ @functions\/callbacks.h 76:3@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Processor_mode = Processor_mode
  { unwrapProcessor_mode :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

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

deriving via Marshal.EquivStorable Processor_mode instance BG.Storable Processor_mode

deriving via BG.CUInt instance BG.Prim Processor_mode

instance CEnum.CEnum Processor_mode where

  type CEnumZ Processor_mode = BG.CUInt

  toCEnum = Processor_mode

  fromCEnum = BG.getField @"unwrapProcessor_mode"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (0, BG.singleton "MODE_SIMPLE")
                                   , (1, BG.singleton "MODE_VALIDATED")
                                   , (2, BG.singleton "MODE_PROGRESS")
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

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapProcessor_mode" Processor_mode ty where

  hasField =
    \x0 ->
      ( \y1 -> Processor_mode {unwrapProcessor_mode = y1}
      , BG.getField @"unwrapProcessor_mode" x0
      )

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapProcessor_mode" (BG.Ptr Processor_mode) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapProcessor_mode")

instance HasCField.HasCField Processor_mode "unwrapProcessor_mode" where

  type CFieldType Processor_mode "unwrapProcessor_mode" =
    BG.CUInt

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
  deriving stock (BG.Generic)

instance Marshal.StaticSize Processor where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Processor where

  readRaw =
    \ptr0 ->
          pure Processor
      <*> HasCField.readRaw (BG.Proxy @"processor_mode") ptr0
      <*> HasCField.readRaw (BG.Proxy @"processor_callback") ptr0

instance Marshal.WriteRaw Processor where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Processor processor_mode2 processor_callback3 ->
               HasCField.writeRaw (BG.Proxy @"processor_mode") ptr0 processor_mode2
            >> HasCField.writeRaw (BG.Proxy @"processor_callback") ptr0 processor_callback3

deriving via Marshal.EquivStorable Processor instance BG.Storable Processor

deriving via Struct.IsStructViaStorable Processor instance Struct.IsStruct Processor

{-| __C declaration:__ @mode@

    __defined at:__ @functions\/callbacks.h 76:55@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ Processor_mode
         ) => BG.CompatHasField.HasField "processor_mode" Processor ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Processor {processor_mode = y1, processor_callback = BG.getField @"processor_callback" x0}
      , BG.getField @"processor_mode" x0
      )

instance ( ty ~ Processor_mode
         ) => BG.HasField "processor_mode" (BG.Ptr Processor) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"processor_mode")

instance HasCField.HasCField Processor "processor_mode" where

  type CFieldType Processor "processor_mode" =
    Processor_mode

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @callback@

    __defined at:__ @functions\/callbacks.h 77:27@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ ProcessorCallback
         ) => BG.CompatHasField.HasField "processor_callback" Processor ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Processor {processor_callback = y1, processor_mode = BG.getField @"processor_mode" x0}
      , BG.getField @"processor_callback" x0
      )

instance ( ty ~ ProcessorCallback
         ) => BG.HasField "processor_callback" (BG.Ptr Processor) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"processor_callback")

instance HasCField.HasCField Processor "processor_callback" where

  type CFieldType Processor "processor_callback" =
    ProcessorCallback

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @foo@

    __defined at:__ @functions\/callbacks.h 94:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Foo = Foo
  { unwrapFoo :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapFoo" Foo ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo {unwrapFoo = y1}, BG.getField @"unwrapFoo" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapFoo" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapFoo")

instance HasCField.HasCField Foo "unwrapFoo" where

  type CFieldType Foo "unwrapFoo" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @foo2@

    __defined at:__ @functions\/callbacks.h 95:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Foo2 = Foo2
  { unwrapFoo2 :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapFoo2" Foo2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo2 {unwrapFoo2 = y1}, BG.getField @"unwrapFoo2" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapFoo2" (BG.Ptr Foo2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapFoo2")

instance HasCField.HasCField Foo2 "unwrapFoo2" where

  type CFieldType Foo2 "unwrapFoo2" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @functions\/callbacks.h 102:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype A = A
  { unwrapA :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @functions\/callbacks.h 108:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype B = B
  { unwrapB :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapB" B ty where

  hasField =
    \x0 ->
      (\y1 -> B {unwrapB = y1}, BG.getField @"unwrapB" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapB" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S@

    __defined at:__ @functions\/callbacks.h 109:8@

    __exported by:__ @functions\/callbacks.h@
-}
data S = S
  { s_fn :: BG.FunPtr (B -> IO ())
    {- ^ __C declaration:__ @fn@

         __defined at:__ @functions\/callbacks.h 110:17@

         __exported by:__ @functions\/callbacks.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_fn") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_fn2 ->
            HasCField.writeRaw (BG.Proxy @"s_fn") ptr0 s_fn2

deriving via Marshal.EquivStorable S instance BG.Storable S

deriving via Struct.IsStructViaStorable S instance Struct.IsStruct S

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/callbacks.h 110:17@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ BG.FunPtr (B -> IO ())
         ) => BG.CompatHasField.HasField "s_fn" S ty where

  hasField =
    \x0 -> (\y1 -> S {s_fn = y1}, BG.getField @"s_fn" x0)

instance ( ty ~ BG.FunPtr (B -> IO ())
         ) => BG.HasField "s_fn" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_fn")

instance HasCField.HasCField S "s_fn" where

  type CFieldType S "s_fn" = BG.FunPtr (B -> IO ())

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @C@

    __defined at:__ @functions\/callbacks.h 116:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype C = C
  { unwrapC :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapC" C ty where

  hasField =
    \x0 ->
      (\y1 -> C {unwrapC = y1}, BG.getField @"unwrapC" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapC" (BG.Ptr C) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapC")

instance HasCField.HasCField C "unwrapC" where

  type CFieldType C "unwrapC" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union U@

    __defined at:__ @functions\/callbacks.h 117:7@

    __exported by:__ @functions\/callbacks.h@
-}
newtype U = U
  { unwrapU :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 24 8 instance Marshal.StaticSize U

deriving via BG.SizedByteArray 24 8 instance Marshal.ReadRaw U

deriving via BG.SizedByteArray 24 8 instance Marshal.WriteRaw U

deriving via Marshal.EquivStorable U instance BG.Storable U

deriving via BG.SizedByteArray 24 8 instance Union.IsUnion U

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/callbacks.h 118:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ CA.ConstantArray 3 (BG.FunPtr (C -> IO ()))
         ) => BG.HasField "u_fn" U ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/callbacks.h 118:10@

    __exported by:__ @functions\/callbacks.h@
-}
instance ( ty ~ CA.ConstantArray 3 (BG.FunPtr (C -> IO ()))
         ) => BG.CompatHasField.HasField "u_fn" U ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"u_fn" x0)

instance ( ty ~ CA.ConstantArray 3 (BG.FunPtr (C -> IO ()))
         ) => BG.HasField "u_fn" (BG.Ptr U) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"u_fn")

instance HasCField.HasCField U "u_fn" where

  type CFieldType U "u_fn" =
    CA.ConstantArray 3 (BG.FunPtr (C -> IO ()))

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @D@

    __defined at:__ @functions\/callbacks.h 124:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype D = D
  { unwrapD :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapD" D ty where

  hasField =
    \x0 ->
      (\y1 -> D {unwrapD = y1}, BG.getField @"unwrapD" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapD" (BG.Ptr D) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapD")

instance HasCField.HasCField D "unwrapD" where

  type CFieldType D "unwrapD" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T@

    __defined at:__ @functions\/callbacks.h 125:15@

    __exported by:__ @functions\/callbacks.h@
-}
newtype T = T
  { unwrapT :: BG.FunPtr (D -> IO ()) -> IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toT@
foreign import ccall safe "wrapper" hs_bindgen_b8534912f6256492_base ::
     (BG.FunPtr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.FunPtr BG.Void -> IO ()))

-- __unique:__ @toT@
hs_bindgen_b8534912f6256492 ::
     T
  -> IO (BG.FunPtr T)
hs_bindgen_b8534912f6256492 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_b8534912f6256492_base (BG.toFFIType fun0))

-- __unique:__ @fromT@
foreign import ccall safe "dynamic" hs_bindgen_8f830eadb43a6fe4_base ::
     BG.FunPtr (BG.FunPtr BG.Void -> IO ())
  -> BG.FunPtr BG.Void -> IO ()

-- __unique:__ @fromT@
hs_bindgen_8f830eadb43a6fe4 ::
     BG.FunPtr T
  -> T
hs_bindgen_8f830eadb43a6fe4 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_8f830eadb43a6fe4_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr T where

  toFunPtr = hs_bindgen_b8534912f6256492

instance BG.FromFunPtr T where

  fromFunPtr = hs_bindgen_8f830eadb43a6fe4

instance ( ty ~ (BG.FunPtr (D -> IO ()) -> IO ())
         ) => BG.CompatHasField.HasField "unwrapT" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {unwrapT = y1}, BG.getField @"unwrapT" x0)

instance ( ty ~ (BG.FunPtr (D -> IO ()) -> IO ())
         ) => BG.HasField "unwrapT" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" =
    BG.FunPtr (D -> IO ()) -> IO ()

  offset# = \_ -> \_ -> 0

-- __unique:__ @instance ToFunPtr (A -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_a46c670f88b5e6d2_base ::
     (BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (A -> IO ())@
hs_bindgen_a46c670f88b5e6d2 ::
     (A -> IO ())
  -> IO (BG.FunPtr (A -> IO ()))
hs_bindgen_a46c670f88b5e6d2 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_a46c670f88b5e6d2_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (A -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_542fc348d1df7dff_base ::
     BG.FunPtr (BG.Int32 -> IO ())
  -> BG.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (A -> IO ())@
hs_bindgen_542fc348d1df7dff ::
     BG.FunPtr (A -> IO ())
  -> A -> IO ()
hs_bindgen_542fc348d1df7dff =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_542fc348d1df7dff_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (A -> IO ()) where

  toFunPtr = hs_bindgen_a46c670f88b5e6d2

instance BG.FromFunPtr (A -> IO ()) where

  fromFunPtr = hs_bindgen_542fc348d1df7dff

-- __unique:__ @instance ToFunPtr (B -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_ca9230f035bf19b1_base ::
     (BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (B -> IO ())@
hs_bindgen_ca9230f035bf19b1 ::
     (B -> IO ())
  -> IO (BG.FunPtr (B -> IO ()))
hs_bindgen_ca9230f035bf19b1 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_ca9230f035bf19b1_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (B -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_05635addcbfd7dd4_base ::
     BG.FunPtr (BG.Int32 -> IO ())
  -> BG.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (B -> IO ())@
hs_bindgen_05635addcbfd7dd4 ::
     BG.FunPtr (B -> IO ())
  -> B -> IO ()
hs_bindgen_05635addcbfd7dd4 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_05635addcbfd7dd4_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (B -> IO ()) where

  toFunPtr = hs_bindgen_ca9230f035bf19b1

instance BG.FromFunPtr (B -> IO ()) where

  fromFunPtr = hs_bindgen_05635addcbfd7dd4

-- __unique:__ @instance ToFunPtr (C -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_151c4eb8866d683f_base ::
     (BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (C -> IO ())@
hs_bindgen_151c4eb8866d683f ::
     (C -> IO ())
  -> IO (BG.FunPtr (C -> IO ()))
hs_bindgen_151c4eb8866d683f =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_151c4eb8866d683f_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (C -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_f9f9dcb4f8267450_base ::
     BG.FunPtr (BG.Int32 -> IO ())
  -> BG.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (C -> IO ())@
hs_bindgen_f9f9dcb4f8267450 ::
     BG.FunPtr (C -> IO ())
  -> C -> IO ()
hs_bindgen_f9f9dcb4f8267450 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_f9f9dcb4f8267450_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (C -> IO ()) where

  toFunPtr = hs_bindgen_151c4eb8866d683f

instance BG.FromFunPtr (C -> IO ()) where

  fromFunPtr = hs_bindgen_f9f9dcb4f8267450

-- __unique:__ @instance ToFunPtr (D -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_bb8d99987b90516d_base ::
     (BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (D -> IO ())@
hs_bindgen_bb8d99987b90516d ::
     (D -> IO ())
  -> IO (BG.FunPtr (D -> IO ()))
hs_bindgen_bb8d99987b90516d =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_bb8d99987b90516d_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (D -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_8eef50c371b3a153_base ::
     BG.FunPtr (BG.Int32 -> IO ())
  -> BG.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (D -> IO ())@
hs_bindgen_8eef50c371b3a153 ::
     BG.FunPtr (D -> IO ())
  -> D -> IO ()
hs_bindgen_8eef50c371b3a153 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_8eef50c371b3a153_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (D -> IO ()) where

  toFunPtr = hs_bindgen_bb8d99987b90516d

instance BG.FromFunPtr (D -> IO ()) where

  fromFunPtr = hs_bindgen_8eef50c371b3a153

-- __unique:__ @instance ToFunPtr (Foo -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_d2a71f330b782e41_base ::
     (BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (Foo -> IO ())@
hs_bindgen_d2a71f330b782e41 ::
     (Foo -> IO ())
  -> IO (BG.FunPtr (Foo -> IO ()))
hs_bindgen_d2a71f330b782e41 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_d2a71f330b782e41_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (Foo -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_c08440542d338bad_base ::
     BG.FunPtr (BG.Int32 -> IO ())
  -> BG.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (Foo -> IO ())@
hs_bindgen_c08440542d338bad ::
     BG.FunPtr (Foo -> IO ())
  -> Foo -> IO ()
hs_bindgen_c08440542d338bad =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_c08440542d338bad_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (Foo -> IO ()) where

  toFunPtr = hs_bindgen_d2a71f330b782e41

instance BG.FromFunPtr (Foo -> IO ()) where

  fromFunPtr = hs_bindgen_c08440542d338bad

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> IO BG.CInt)@
foreign import ccall safe "wrapper" hs_bindgen_4064610c89a52f99_base ::
     (BG.Ptr BG.Void -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO BG.Int32))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> IO BG.CInt)@
hs_bindgen_4064610c89a52f99 ::
     (BG.Ptr Measurement -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.Ptr Measurement -> IO BG.CInt))
hs_bindgen_4064610c89a52f99 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_4064610c89a52f99_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> IO BG.CInt)@
foreign import ccall safe "dynamic" hs_bindgen_46b04bdda1fdaafc_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO BG.Int32)
  -> BG.Ptr BG.Void -> IO BG.Int32

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> IO BG.CInt)@
hs_bindgen_46b04bdda1fdaafc ::
     BG.FunPtr (BG.Ptr Measurement -> IO BG.CInt)
  -> BG.Ptr Measurement -> IO BG.CInt
hs_bindgen_46b04bdda1fdaafc =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_46b04bdda1fdaafc_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (BG.Ptr Measurement -> IO BG.CInt) where

  toFunPtr = hs_bindgen_4064610c89a52f99

instance BG.FromFunPtr (BG.Ptr Measurement -> IO BG.CInt) where

  fromFunPtr = hs_bindgen_46b04bdda1fdaafc

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_0e70e3ef331da59c_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> IO ())@
hs_bindgen_0e70e3ef331da59c ::
     (BG.Ptr Measurement -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> IO ()))
hs_bindgen_0e70e3ef331da59c =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_0e70e3ef331da59c_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_900e47c9e85104f3_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> IO ())@
hs_bindgen_900e47c9e85104f3 ::
     BG.FunPtr (BG.Ptr Measurement -> IO ())
  -> BG.Ptr Measurement -> IO ()
hs_bindgen_900e47c9e85104f3 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_900e47c9e85104f3_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (BG.Ptr Measurement -> IO ()) where

  toFunPtr = hs_bindgen_0e70e3ef331da59c

instance BG.FromFunPtr (BG.Ptr Measurement -> IO ()) where

  fromFunPtr = hs_bindgen_900e47c9e85104f3

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> DataValidator -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_ee5115ecd6cd3b7d_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> DataValidator -> IO ())@
hs_bindgen_ee5115ecd6cd3b7d ::
     (BG.Ptr Measurement -> DataValidator -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ()))
hs_bindgen_ee5115ecd6cd3b7d =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_ee5115ecd6cd3b7d_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> DataValidator -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_767024eb494db15e_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> DataValidator -> IO ())@
hs_bindgen_767024eb494db15e ::
     BG.FunPtr (BG.Ptr Measurement -> DataValidator -> IO ())
  -> BG.Ptr Measurement -> DataValidator -> IO ()
hs_bindgen_767024eb494db15e =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_767024eb494db15e_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (BG.Ptr Measurement -> DataValidator -> IO ()) where

  toFunPtr = hs_bindgen_ee5115ecd6cd3b7d

instance BG.FromFunPtr (BG.Ptr Measurement -> DataValidator -> IO ()) where

  fromFunPtr = hs_bindgen_767024eb494db15e

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_9c62e3e4c665b341_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())@
hs_bindgen_9c62e3e4c665b341 ::
     (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()))
hs_bindgen_9c62e3e4c665b341 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_9c62e3e4c665b341_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_be989dc6cfcbaa3c_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())@
hs_bindgen_be989dc6cfcbaa3c ::
     BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ())
  -> BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()
hs_bindgen_be989dc6cfcbaa3c =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_be989dc6cfcbaa3c_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) where

  toFunPtr = hs_bindgen_9c62e3e4c665b341

instance BG.FromFunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_be989dc6cfcbaa3c

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_cbc1799daa163d3d_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())@
hs_bindgen_cbc1799daa163d3d ::
     (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ()))
hs_bindgen_cbc1799daa163d3d =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_cbc1799daa163d3d_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_b313881940dff0dc_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())@
hs_bindgen_b313881940dff0dc ::
     BG.FunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ())
  -> BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ()
hs_bindgen_b313881940dff0dc =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_b313881940dff0dc_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ()) where

  toFunPtr = hs_bindgen_cbc1799daa163d3d

instance BG.FromFunPtr (BG.Ptr Measurement -> FileOpenedNotification -> BG.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_b313881940dff0dc

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_7b1e0c518e5f0b1b_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())@
hs_bindgen_7b1e0c518e5f0b1b ::
     (BG.Ptr Measurement -> ProgressUpdate -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ()))
hs_bindgen_7b1e0c518e5f0b1b =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_7b1e0c518e5f0b1b_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_0a3b532434d7e43c_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())@
hs_bindgen_0a3b532434d7e43c ::
     BG.FunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ())
  -> BG.Ptr Measurement -> ProgressUpdate -> IO ()
hs_bindgen_0a3b532434d7e43c =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_0a3b532434d7e43c_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ()) where

  toFunPtr = hs_bindgen_7b1e0c518e5f0b1b

instance BG.FromFunPtr (BG.Ptr Measurement -> ProgressUpdate -> IO ()) where

  fromFunPtr = hs_bindgen_0a3b532434d7e43c

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_ba073f1e0ec5008b_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())@
hs_bindgen_ba073f1e0ec5008b ::
     (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ()))
hs_bindgen_ba073f1e0ec5008b =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_ba073f1e0ec5008b_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_090d079ea979a39d_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())@
hs_bindgen_090d079ea979a39d ::
     BG.FunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ())
  -> BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ()
hs_bindgen_090d079ea979a39d =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_090d079ea979a39d_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ()) where

  toFunPtr = hs_bindgen_ba073f1e0ec5008b

instance BG.FromFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.CDouble -> BG.CInt -> IO BG.CDouble) -> BG.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_090d079ea979a39d

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_e1d08f76a35007c0_base ::
     (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.FunPtr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())@
hs_bindgen_e1d08f76a35007c0 ::
     (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())
  -> IO (BG.FunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ()))
hs_bindgen_e1d08f76a35007c0 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_e1d08f76a35007c0_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_74f2cb0fd2d13cc1_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.FunPtr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.FunPtr BG.Void -> BG.FunPtr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())@
hs_bindgen_74f2cb0fd2d13cc1 ::
     BG.FunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ())
  -> BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ()
hs_bindgen_74f2cb0fd2d13cc1 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_74f2cb0fd2d13cc1_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ()) where

  toFunPtr = hs_bindgen_e1d08f76a35007c0

instance BG.FromFunPtr (BG.Ptr Measurement -> BG.FunPtr (BG.Ptr Measurement -> DataValidator -> BG.CInt -> IO ()) -> DataValidator -> IO ()) where

  fromFunPtr = hs_bindgen_74f2cb0fd2d13cc1

-- __unique:__ @instance ToFunPtr (Foo2 -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_235fa4a89af25f04_base ::
     (BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (Foo2 -> IO ())@
hs_bindgen_235fa4a89af25f04 ::
     (Foo2 -> IO ())
  -> IO (BG.FunPtr (Foo2 -> IO ()))
hs_bindgen_235fa4a89af25f04 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_235fa4a89af25f04_base (BG.toFFIType fun0))

-- __unique:__ @instance FromFunPtr (Foo2 -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_8605b223a9ab9562_base ::
     BG.FunPtr (BG.Int32 -> IO ())
  -> BG.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (Foo2 -> IO ())@
hs_bindgen_8605b223a9ab9562 ::
     BG.FunPtr (Foo2 -> IO ())
  -> Foo2 -> IO ()
hs_bindgen_8605b223a9ab9562 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_8605b223a9ab9562_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr (Foo2 -> IO ()) where

  toFunPtr = hs_bindgen_235fa4a89af25f04

instance BG.FromFunPtr (Foo2 -> IO ()) where

  fromFunPtr = hs_bindgen_8605b223a9ab9562
