{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import qualified GHC.Generics
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.ByteArray
import qualified HsBindgen.Runtime.Internal.FunPtr
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Internal.SizedByteArray
import qualified HsBindgen.Runtime.Marshal
import qualified Prelude as P
import qualified Text.Read
import Data.Bits (FiniteBits)
import Data.Void (Void)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-| Auxiliary type used by 'FileOpenedNotification'

__C declaration:__ @FileOpenedNotification@

__defined at:__ @functions\/callbacks.h 10:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype FileOpenedNotification_Aux = FileOpenedNotification_Aux
  { unwrapFileOpenedNotification_Aux :: IO ()
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_b3b8b1fad168671a_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

-- __unique:__ @toFileOpenedNotification_Aux@
hs_bindgen_b3b8b1fad168671a ::
     FileOpenedNotification_Aux
  -> IO (Ptr.FunPtr FileOpenedNotification_Aux)
hs_bindgen_b3b8b1fad168671a =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_b3b8b1fad168671a_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_f3ba5920f34c7f6a_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromFileOpenedNotification_Aux@
hs_bindgen_f3ba5920f34c7f6a ::
     Ptr.FunPtr FileOpenedNotification_Aux
  -> FileOpenedNotification_Aux
hs_bindgen_f3ba5920f34c7f6a =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_f3ba5920f34c7f6a_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr FileOpenedNotification_Aux where

  toFunPtr = hs_bindgen_b3b8b1fad168671a

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr FileOpenedNotification_Aux where

  fromFunPtr = hs_bindgen_f3ba5920f34c7f6a

instance ( TyEq ty (IO ())
         ) => GHC.Records.HasField "unwrapFileOpenedNotification_Aux" (Ptr.Ptr FileOpenedNotification_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFileOpenedNotification_Aux")

instance HsBindgen.Runtime.HasCField.HasCField FileOpenedNotification_Aux "unwrapFileOpenedNotification_Aux" where

  type CFieldType FileOpenedNotification_Aux "unwrapFileOpenedNotification_Aux" =
    IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FileOpenedNotification@

    __defined at:__ @functions\/callbacks.h 10:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype FileOpenedNotification = FileOpenedNotification
  { unwrapFileOpenedNotification :: Ptr.FunPtr FileOpenedNotification_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.FunPtr FileOpenedNotification_Aux)
         ) => GHC.Records.HasField "unwrapFileOpenedNotification" (Ptr.Ptr FileOpenedNotification) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFileOpenedNotification")

instance HsBindgen.Runtime.HasCField.HasCField FileOpenedNotification "unwrapFileOpenedNotification" where

  type CFieldType FileOpenedNotification "unwrapFileOpenedNotification" =
    Ptr.FunPtr FileOpenedNotification_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'ProgressUpdate'

__C declaration:__ @ProgressUpdate@

__defined at:__ @functions\/callbacks.h 11:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype ProgressUpdate_Aux = ProgressUpdate_Aux
  { unwrapProgressUpdate_Aux :: FC.CInt -> IO ()
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_d551f31556ffa727_base ::
     (GHC.Int.Int32 -> IO ())
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> IO ()))

-- __unique:__ @toProgressUpdate_Aux@
hs_bindgen_d551f31556ffa727 ::
     ProgressUpdate_Aux
  -> IO (Ptr.FunPtr ProgressUpdate_Aux)
hs_bindgen_d551f31556ffa727 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_d551f31556ffa727_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_ccf7f4b62a839a04_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> IO ())
  -> GHC.Int.Int32 -> IO ()

-- __unique:__ @fromProgressUpdate_Aux@
hs_bindgen_ccf7f4b62a839a04 ::
     Ptr.FunPtr ProgressUpdate_Aux
  -> ProgressUpdate_Aux
hs_bindgen_ccf7f4b62a839a04 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_ccf7f4b62a839a04_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr ProgressUpdate_Aux where

  toFunPtr = hs_bindgen_d551f31556ffa727

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr ProgressUpdate_Aux where

  fromFunPtr = hs_bindgen_ccf7f4b62a839a04

instance ( TyEq ty (FC.CInt -> IO ())
         ) => GHC.Records.HasField "unwrapProgressUpdate_Aux" (Ptr.Ptr ProgressUpdate_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapProgressUpdate_Aux")

instance HsBindgen.Runtime.HasCField.HasCField ProgressUpdate_Aux "unwrapProgressUpdate_Aux" where

  type CFieldType ProgressUpdate_Aux "unwrapProgressUpdate_Aux" =
    FC.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @ProgressUpdate@

    __defined at:__ @functions\/callbacks.h 11:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype ProgressUpdate = ProgressUpdate
  { unwrapProgressUpdate :: Ptr.FunPtr ProgressUpdate_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.FunPtr ProgressUpdate_Aux)
         ) => GHC.Records.HasField "unwrapProgressUpdate" (Ptr.Ptr ProgressUpdate) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapProgressUpdate")

instance HsBindgen.Runtime.HasCField.HasCField ProgressUpdate "unwrapProgressUpdate" where

  type CFieldType ProgressUpdate "unwrapProgressUpdate" =
    Ptr.FunPtr ProgressUpdate_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'DataValidator'

__C declaration:__ @DataValidator@

__defined at:__ @functions\/callbacks.h 12:15@

__exported by:__ @functions\/callbacks.h@
-}
newtype DataValidator_Aux = DataValidator_Aux
  { unwrapDataValidator_Aux :: FC.CInt -> IO FC.CInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_c656ca21e63343d6_base ::
     (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32))

-- __unique:__ @toDataValidator_Aux@
hs_bindgen_c656ca21e63343d6 ::
     DataValidator_Aux
  -> IO (Ptr.FunPtr DataValidator_Aux)
hs_bindgen_c656ca21e63343d6 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_c656ca21e63343d6_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_c1e79a4c11ca4033_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> GHC.Int.Int32 -> IO GHC.Int.Int32

-- __unique:__ @fromDataValidator_Aux@
hs_bindgen_c1e79a4c11ca4033 ::
     Ptr.FunPtr DataValidator_Aux
  -> DataValidator_Aux
hs_bindgen_c1e79a4c11ca4033 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_c1e79a4c11ca4033_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr DataValidator_Aux where

  toFunPtr = hs_bindgen_c656ca21e63343d6

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr DataValidator_Aux where

  fromFunPtr = hs_bindgen_c1e79a4c11ca4033

instance ( TyEq ty (FC.CInt -> IO FC.CInt)
         ) => GHC.Records.HasField "unwrapDataValidator_Aux" (Ptr.Ptr DataValidator_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapDataValidator_Aux")

instance HsBindgen.Runtime.HasCField.HasCField DataValidator_Aux "unwrapDataValidator_Aux" where

  type CFieldType DataValidator_Aux "unwrapDataValidator_Aux" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @DataValidator@

    __defined at:__ @functions\/callbacks.h 12:15@

    __exported by:__ @functions\/callbacks.h@
-}
newtype DataValidator = DataValidator
  { unwrapDataValidator :: Ptr.FunPtr DataValidator_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.FunPtr DataValidator_Aux)
         ) => GHC.Records.HasField "unwrapDataValidator" (Ptr.Ptr DataValidator) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapDataValidator")

instance HsBindgen.Runtime.HasCField.HasCField DataValidator "unwrapDataValidator" where

  type CFieldType DataValidator "unwrapDataValidator" =
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
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Measurement where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Measurement where

  readRaw =
    \ptr0 ->
          pure Measurement
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"measurement_value") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"measurement_timestamp") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Measurement where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Measurement measurement_value2 measurement_timestamp3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"measurement_value") ptr0 measurement_value2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"measurement_timestamp") ptr0 measurement_timestamp3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Measurement instance F.Storable Measurement

instance HsBindgen.Runtime.HasCField.HasCField Measurement "measurement_value" where

  type CFieldType Measurement "measurement_value" =
    FC.CDouble

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CDouble
         ) => GHC.Records.HasField "measurement_value" (Ptr.Ptr Measurement) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"measurement_value")

instance HsBindgen.Runtime.HasCField.HasCField Measurement "measurement_timestamp" where

  type CFieldType Measurement "measurement_timestamp" =
    FC.CDouble

  offset# = \_ -> \_ -> 8

instance ( TyEq ty FC.CDouble
         ) => GHC.Records.HasField "measurement_timestamp" (Ptr.Ptr Measurement) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"measurement_timestamp")

{-| Auxiliary type used by 'MeasurementReceived'

__C declaration:__ @MeasurementReceived@

__defined at:__ @functions\/callbacks.h 26:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived_Aux = MeasurementReceived_Aux
  { unwrapMeasurementReceived_Aux :: (Ptr.Ptr Measurement) -> IO ()
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_9259654df9d40f5b_base ::
     ((Ptr.Ptr Void) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> IO ()))

-- __unique:__ @toMeasurementReceived_Aux@
hs_bindgen_9259654df9d40f5b ::
     MeasurementReceived_Aux
  -> IO (Ptr.FunPtr MeasurementReceived_Aux)
hs_bindgen_9259654df9d40f5b =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_9259654df9d40f5b_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_383c36bb22947621_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> IO ())
  -> (Ptr.Ptr Void) -> IO ()

-- __unique:__ @fromMeasurementReceived_Aux@
hs_bindgen_383c36bb22947621 ::
     Ptr.FunPtr MeasurementReceived_Aux
  -> MeasurementReceived_Aux
hs_bindgen_383c36bb22947621 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_383c36bb22947621_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr MeasurementReceived_Aux where

  toFunPtr = hs_bindgen_9259654df9d40f5b

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr MeasurementReceived_Aux where

  fromFunPtr = hs_bindgen_383c36bb22947621

instance ( TyEq ty ((Ptr.Ptr Measurement) -> IO ())
         ) => GHC.Records.HasField "unwrapMeasurementReceived_Aux" (Ptr.Ptr MeasurementReceived_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMeasurementReceived_Aux")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementReceived_Aux "unwrapMeasurementReceived_Aux" where

  type CFieldType MeasurementReceived_Aux "unwrapMeasurementReceived_Aux" =
    (Ptr.Ptr Measurement) -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MeasurementReceived@

    __defined at:__ @functions\/callbacks.h 26:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived = MeasurementReceived
  { unwrapMeasurementReceived :: Ptr.FunPtr MeasurementReceived_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.FunPtr MeasurementReceived_Aux)
         ) => GHC.Records.HasField "unwrapMeasurementReceived" (Ptr.Ptr MeasurementReceived) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMeasurementReceived")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementReceived "unwrapMeasurementReceived" where

  type CFieldType MeasurementReceived "unwrapMeasurementReceived" =
    Ptr.FunPtr MeasurementReceived_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'MeasurementReceived2'

__C declaration:__ @MeasurementReceived2@

__defined at:__ @functions\/callbacks.h 29:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived2_Aux = MeasurementReceived2_Aux
  { unwrapMeasurementReceived2_Aux :: Measurement -> IO ()
  }
  deriving stock (GHC.Generics.Generic)

instance ( TyEq ty (Measurement -> IO ())
         ) => GHC.Records.HasField "unwrapMeasurementReceived2_Aux" (Ptr.Ptr MeasurementReceived2_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMeasurementReceived2_Aux")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementReceived2_Aux "unwrapMeasurementReceived2_Aux" where

  type CFieldType MeasurementReceived2_Aux "unwrapMeasurementReceived2_Aux" =
    Measurement -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MeasurementReceived2@

    __defined at:__ @functions\/callbacks.h 29:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype MeasurementReceived2 = MeasurementReceived2
  { unwrapMeasurementReceived2 :: Ptr.FunPtr MeasurementReceived2_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.FunPtr MeasurementReceived2_Aux)
         ) => GHC.Records.HasField "unwrapMeasurementReceived2" (Ptr.Ptr MeasurementReceived2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMeasurementReceived2")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementReceived2 "unwrapMeasurementReceived2" where

  type CFieldType MeasurementReceived2 "unwrapMeasurementReceived2" =
    Ptr.FunPtr MeasurementReceived2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'SampleBufferFull'

__C declaration:__ @SampleBufferFull@

__defined at:__ @functions\/callbacks.h 32:16@

__exported by:__ @functions\/callbacks.h@
-}
newtype SampleBufferFull_Aux = SampleBufferFull_Aux
  { unwrapSampleBufferFull_Aux :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) FC.CInt) -> IO ()
  }
  deriving stock (GHC.Generics.Generic)

instance ( TyEq ty (((HsBindgen.Runtime.ConstantArray.ConstantArray 10) FC.CInt) -> IO ())
         ) => GHC.Records.HasField "unwrapSampleBufferFull_Aux" (Ptr.Ptr SampleBufferFull_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapSampleBufferFull_Aux")

instance HsBindgen.Runtime.HasCField.HasCField SampleBufferFull_Aux "unwrapSampleBufferFull_Aux" where

  type CFieldType SampleBufferFull_Aux "unwrapSampleBufferFull_Aux" =
    ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) FC.CInt) -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @SampleBufferFull@

    __defined at:__ @functions\/callbacks.h 32:16@

    __exported by:__ @functions\/callbacks.h@
-}
newtype SampleBufferFull = SampleBufferFull
  { unwrapSampleBufferFull :: Ptr.FunPtr SampleBufferFull_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty (Ptr.FunPtr SampleBufferFull_Aux)
         ) => GHC.Records.HasField "unwrapSampleBufferFull" (Ptr.Ptr SampleBufferFull) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapSampleBufferFull")

instance HsBindgen.Runtime.HasCField.HasCField SampleBufferFull "unwrapSampleBufferFull" where

  type CFieldType SampleBufferFull "unwrapSampleBufferFull" =
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
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize MeasurementHandler where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw MeasurementHandler where

  readRaw =
    \ptr0 ->
          pure MeasurementHandler
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"measurementHandler_onReceived") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"measurementHandler_validate") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"measurementHandler_onError") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw MeasurementHandler where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          MeasurementHandler
            measurementHandler_onReceived2
            measurementHandler_validate3
            measurementHandler_onError4 ->
                 HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"measurementHandler_onReceived") ptr0 measurementHandler_onReceived2
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"measurementHandler_validate") ptr0 measurementHandler_validate3
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"measurementHandler_onError") ptr0 measurementHandler_onError4

deriving via HsBindgen.Runtime.Marshal.EquivStorable MeasurementHandler instance F.Storable MeasurementHandler

instance HsBindgen.Runtime.HasCField.HasCField MeasurementHandler "measurementHandler_onReceived" where

  type CFieldType MeasurementHandler "measurementHandler_onReceived" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())

  offset# = \_ -> \_ -> 0

instance ( TyEq ty (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ()))
         ) => GHC.Records.HasField "measurementHandler_onReceived" (Ptr.Ptr MeasurementHandler) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"measurementHandler_onReceived")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementHandler "measurementHandler_validate" where

  type CFieldType MeasurementHandler "measurementHandler_validate" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)

  offset# = \_ -> \_ -> 8

instance ( TyEq ty (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt))
         ) => GHC.Records.HasField "measurementHandler_validate" (Ptr.Ptr MeasurementHandler) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"measurementHandler_validate")

instance HsBindgen.Runtime.HasCField.HasCField MeasurementHandler "measurementHandler_onError" where

  type CFieldType MeasurementHandler "measurementHandler_onError" =
    Ptr.FunPtr (FC.CInt -> IO ())

  offset# = \_ -> \_ -> 16

instance ( TyEq ty (Ptr.FunPtr (FC.CInt -> IO ()))
         ) => GHC.Records.HasField "measurementHandler_onError" (Ptr.Ptr MeasurementHandler) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"measurementHandler_onError")

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
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize DataPipeline where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw DataPipeline where

  readRaw =
    \ptr0 ->
          pure DataPipeline
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dataPipeline_preProcess") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dataPipeline_process") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dataPipeline_postProcess") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw DataPipeline where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          DataPipeline
            dataPipeline_preProcess2
            dataPipeline_process3
            dataPipeline_postProcess4 ->
                 HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dataPipeline_preProcess") ptr0 dataPipeline_preProcess2
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dataPipeline_process") ptr0 dataPipeline_process3
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dataPipeline_postProcess") ptr0 dataPipeline_postProcess4

deriving via HsBindgen.Runtime.Marshal.EquivStorable DataPipeline instance F.Storable DataPipeline

instance HsBindgen.Runtime.HasCField.HasCField DataPipeline "dataPipeline_preProcess" where

  type CFieldType DataPipeline "dataPipeline_preProcess" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())

  offset# = \_ -> \_ -> 0

instance ( TyEq ty (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()))
         ) => GHC.Records.HasField "dataPipeline_preProcess" (Ptr.Ptr DataPipeline) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dataPipeline_preProcess")

instance HsBindgen.Runtime.HasCField.HasCField DataPipeline "dataPipeline_process" where

  type CFieldType DataPipeline "dataPipeline_process" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())

  offset# = \_ -> \_ -> 8

instance ( TyEq ty (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ()))
         ) => GHC.Records.HasField "dataPipeline_process" (Ptr.Ptr DataPipeline) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dataPipeline_process")

instance HsBindgen.Runtime.HasCField.HasCField DataPipeline "dataPipeline_postProcess" where

  type CFieldType DataPipeline "dataPipeline_postProcess" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())

  offset# = \_ -> \_ -> 16

instance ( TyEq ty (Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()))
         ) => GHC.Records.HasField "dataPipeline_postProcess" (Ptr.Ptr DataPipeline) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dataPipeline_postProcess")

{-| __C declaration:__ @union ProcessorCallback@

    __defined at:__ @functions\/callbacks.h 69:7@

    __exported by:__ @functions\/callbacks.h@
-}
newtype ProcessorCallback = ProcessorCallback
  { unwrapProcessorCallback :: Data.Array.Byte.ByteArray
  }
  deriving stock (GHC.Generics.Generic)

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 8 instance HsBindgen.Runtime.Marshal.StaticSize ProcessorCallback

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 8 instance HsBindgen.Runtime.Marshal.ReadRaw ProcessorCallback

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 8 instance HsBindgen.Runtime.Marshal.WriteRaw ProcessorCallback

deriving via HsBindgen.Runtime.Marshal.EquivStorable ProcessorCallback instance F.Storable ProcessorCallback

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
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_processorCallback_simple'

-}
set_processorCallback_simple ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
  -> ProcessorCallback
set_processorCallback_simple =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

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
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_processorCallback_withValidator'

-}
set_processorCallback_withValidator ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
  -> ProcessorCallback
set_processorCallback_withValidator =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

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
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_processorCallback_withProgress'

-}
set_processorCallback_withProgress ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> ProcessorCallback
set_processorCallback_withProgress =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField ProcessorCallback "processorCallback_simple" where

  type CFieldType ProcessorCallback "processorCallback_simple" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())

  offset# = \_ -> \_ -> 0

instance ( TyEq ty (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ()))
         ) => GHC.Records.HasField "processorCallback_simple" (Ptr.Ptr ProcessorCallback) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"processorCallback_simple")

instance HsBindgen.Runtime.HasCField.HasCField ProcessorCallback "processorCallback_withValidator" where

  type CFieldType ProcessorCallback "processorCallback_withValidator" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())

  offset# = \_ -> \_ -> 0

instance ( TyEq ty (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()))
         ) => GHC.Records.HasField "processorCallback_withValidator" (Ptr.Ptr ProcessorCallback) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"processorCallback_withValidator")

instance HsBindgen.Runtime.HasCField.HasCField ProcessorCallback "processorCallback_withProgress" where

  type CFieldType ProcessorCallback "processorCallback_withProgress" =
    Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())

  offset# = \_ -> \_ -> 0

instance ( TyEq ty (Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()))
         ) => GHC.Records.HasField "processorCallback_withProgress" (Ptr.Ptr ProcessorCallback) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"processorCallback_withProgress")

{-| __C declaration:__ @enum \@Processor_mode@

    __defined at:__ @functions\/callbacks.h 76:3@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Processor_mode = Processor_mode
  { unwrapProcessor_mode :: FC.CUInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize Processor_mode where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Processor_mode where

  readRaw =
    \ptr0 ->
          pure Processor_mode
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw Processor_mode where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Processor_mode unwrapProcessor_mode2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapProcessor_mode2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Processor_mode instance F.Storable Processor_mode

deriving via FC.CUInt instance Data.Primitive.Types.Prim Processor_mode

instance HsBindgen.Runtime.CEnum.CEnum Processor_mode where

  type CEnumZ Processor_mode = FC.CUInt

  toCEnum = Processor_mode

  fromCEnum = unwrapProcessor_mode

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

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read Processor_mode where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty FC.CUInt
         ) => GHC.Records.HasField "unwrapProcessor_mode" (Ptr.Ptr Processor_mode) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapProcessor_mode")

instance HsBindgen.Runtime.HasCField.HasCField Processor_mode "unwrapProcessor_mode" where

  type CFieldType Processor_mode "unwrapProcessor_mode" =
    FC.CUInt

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
  deriving stock (GHC.Generics.Generic)

instance HsBindgen.Runtime.Marshal.StaticSize Processor where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Processor where

  readRaw =
    \ptr0 ->
          pure Processor
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"processor_mode") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"processor_callback") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Processor where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Processor processor_mode2 processor_callback3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"processor_mode") ptr0 processor_mode2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"processor_callback") ptr0 processor_callback3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Processor instance F.Storable Processor

instance HsBindgen.Runtime.HasCField.HasCField Processor "processor_mode" where

  type CFieldType Processor "processor_mode" =
    Processor_mode

  offset# = \_ -> \_ -> 0

instance ( TyEq ty Processor_mode
         ) => GHC.Records.HasField "processor_mode" (Ptr.Ptr Processor) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"processor_mode")

instance HsBindgen.Runtime.HasCField.HasCField Processor "processor_callback" where

  type CFieldType Processor "processor_callback" =
    ProcessorCallback

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ProcessorCallback
         ) => GHC.Records.HasField "processor_callback" (Ptr.Ptr Processor) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"processor_callback")

{-| __C declaration:__ @foo@

    __defined at:__ @functions\/callbacks.h 94:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Foo = Foo
  { unwrapFoo :: FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "unwrapFoo" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFoo")

instance HsBindgen.Runtime.HasCField.HasCField Foo "unwrapFoo" where

  type CFieldType Foo "unwrapFoo" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @foo2@

    __defined at:__ @functions\/callbacks.h 95:13@

    __exported by:__ @functions\/callbacks.h@
-}
newtype Foo2 = Foo2
  { unwrapFoo2 :: FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "unwrapFoo2" (Ptr.Ptr Foo2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFoo2")

instance HsBindgen.Runtime.HasCField.HasCField Foo2 "unwrapFoo2" where

  type CFieldType Foo2 "unwrapFoo2" = FC.CInt

  offset# = \_ -> \_ -> 0

foreign import ccall safe "wrapper" hs_bindgen_d2a71f330b782e41_base ::
     (GHC.Int.Int32 -> IO ())
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (Foo -> IO ())@
hs_bindgen_d2a71f330b782e41 ::
     (Foo -> IO ())
  -> IO (Ptr.FunPtr (Foo -> IO ()))
hs_bindgen_d2a71f330b782e41 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_d2a71f330b782e41_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_c08440542d338bad_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> IO ())
  -> GHC.Int.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (Foo -> IO ())@
hs_bindgen_c08440542d338bad ::
     Ptr.FunPtr (Foo -> IO ())
  -> Foo -> IO ()
hs_bindgen_c08440542d338bad =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_c08440542d338bad_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr (Foo -> IO ()) where

  toFunPtr = hs_bindgen_d2a71f330b782e41

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr (Foo -> IO ()) where

  fromFunPtr = hs_bindgen_c08440542d338bad

foreign import ccall safe "wrapper" hs_bindgen_4a7a09e6a9e8c907_base ::
     ((Ptr.Ptr Void) -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> IO GHC.Int.Int32))

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)@
hs_bindgen_4a7a09e6a9e8c907 ::
     ((Ptr.Ptr Measurement) -> IO FC.CInt)
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt))
hs_bindgen_4a7a09e6a9e8c907 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_4a7a09e6a9e8c907_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_2f679442a6d5613f_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> IO GHC.Int.Int32)
  -> (Ptr.Ptr Void) -> IO GHC.Int.Int32

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)@
hs_bindgen_2f679442a6d5613f ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)
  -> (Ptr.Ptr Measurement) -> IO FC.CInt
hs_bindgen_2f679442a6d5613f =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_2f679442a6d5613f_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt) where

  toFunPtr = hs_bindgen_4a7a09e6a9e8c907

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt) where

  fromFunPtr = hs_bindgen_2f679442a6d5613f

foreign import ccall safe "wrapper" hs_bindgen_ca2a1bac1cc0c128_base ::
     ((Ptr.Ptr Void) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> IO ()))

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> IO ())@
hs_bindgen_ca2a1bac1cc0c128 ::
     ((Ptr.Ptr Measurement) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ()))
hs_bindgen_ca2a1bac1cc0c128 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_ca2a1bac1cc0c128_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_4d8a3980803a90f0_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> IO ())
  -> (Ptr.Ptr Void) -> IO ()

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> IO ())@
hs_bindgen_4d8a3980803a90f0 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
  -> (Ptr.Ptr Measurement) -> IO ()
hs_bindgen_4d8a3980803a90f0 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_4d8a3980803a90f0_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> IO ()) where

  toFunPtr = hs_bindgen_ca2a1bac1cc0c128

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> IO ()) where

  fromFunPtr = hs_bindgen_4d8a3980803a90f0

foreign import ccall safe "wrapper" hs_bindgen_aa3ec59dec5e1fdf_base ::
     ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> IO ()))

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())@
hs_bindgen_aa3ec59dec5e1fdf ::
     ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()))
hs_bindgen_aa3ec59dec5e1fdf =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_aa3ec59dec5e1fdf_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_db7fc2b6d55d3864_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> IO ())
  -> (Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> IO ()

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())@
hs_bindgen_db7fc2b6d55d3864 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
  -> (Ptr.Ptr Measurement) -> DataValidator -> IO ()
hs_bindgen_db7fc2b6d55d3864 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_db7fc2b6d55d3864_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()) where

  toFunPtr = hs_bindgen_aa3ec59dec5e1fdf

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()) where

  fromFunPtr = hs_bindgen_db7fc2b6d55d3864

foreign import ccall safe "wrapper" hs_bindgen_b0ef2ac592b19bed_base ::
     ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())@
hs_bindgen_b0ef2ac592b19bed ::
     ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()))
hs_bindgen_b0ef2ac592b19bed =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_b0ef2ac592b19bed_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_17d0b0462779e216_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ())
  -> (Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())@
hs_bindgen_17d0b0462779e216 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()
hs_bindgen_17d0b0462779e216 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_17d0b0462779e216_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()) where

  toFunPtr = hs_bindgen_b0ef2ac592b19bed

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_17d0b0462779e216

foreign import ccall safe "wrapper" hs_bindgen_2d4b28b099f1cb6b_base ::
     ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())@
hs_bindgen_2d4b28b099f1cb6b ::
     ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()))
hs_bindgen_2d4b28b099f1cb6b =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_2d4b28b099f1cb6b_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_3aa04c4e63a856b2_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ())
  -> (Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())@
hs_bindgen_3aa04c4e63a856b2 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()
hs_bindgen_3aa04c4e63a856b2 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_3aa04c4e63a856b2_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()) where

  toFunPtr = hs_bindgen_2d4b28b099f1cb6b

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_3aa04c4e63a856b2

foreign import ccall safe "wrapper" hs_bindgen_4e441dd005b8df73_base ::
     ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> IO ()))

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())@
hs_bindgen_4e441dd005b8df73 ::
     ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()))
hs_bindgen_4e441dd005b8df73 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_4e441dd005b8df73_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_fbe9354fa822de59_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> IO ())
  -> (Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> IO ()

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())@
hs_bindgen_fbe9354fa822de59 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> (Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()
hs_bindgen_fbe9354fa822de59 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_fbe9354fa822de59_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()) where

  toFunPtr = hs_bindgen_4e441dd005b8df73

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()) where

  fromFunPtr = hs_bindgen_fbe9354fa822de59

foreign import ccall safe "wrapper" hs_bindgen_83f0d12162b8410b_base ::
     ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())@
hs_bindgen_83f0d12162b8410b ::
     ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()))
hs_bindgen_83f0d12162b8410b =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_83f0d12162b8410b_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_f634a7da5fce9c42_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ())
  -> (Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())@
hs_bindgen_f634a7da5fce9c42 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()
hs_bindgen_f634a7da5fce9c42 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_f634a7da5fce9c42_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()) where

  toFunPtr = hs_bindgen_83f0d12162b8410b

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()) where

  fromFunPtr = hs_bindgen_f634a7da5fce9c42

foreign import ccall safe "wrapper" hs_bindgen_ab767cc7cdbd64cb_base ::
     ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> (Ptr.FunPtr Void) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> (Ptr.FunPtr Void) -> IO ()))

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())@
hs_bindgen_ab767cc7cdbd64cb ::
     ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()))
hs_bindgen_ab767cc7cdbd64cb =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_ab767cc7cdbd64cb_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_2f73a7e07a90e977_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> (Ptr.FunPtr Void) -> IO ())
  -> (Ptr.Ptr Void) -> (Ptr.FunPtr Void) -> (Ptr.FunPtr Void) -> IO ()

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())@
hs_bindgen_2f73a7e07a90e977 ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
  -> (Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()
hs_bindgen_2f73a7e07a90e977 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_2f73a7e07a90e977_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()) where

  toFunPtr = hs_bindgen_ab767cc7cdbd64cb

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()) where

  fromFunPtr = hs_bindgen_2f73a7e07a90e977

foreign import ccall safe "wrapper" hs_bindgen_235fa4a89af25f04_base ::
     (GHC.Int.Int32 -> IO ())
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> IO ()))

-- __unique:__ @instance ToFunPtr (Foo2 -> IO ())@
hs_bindgen_235fa4a89af25f04 ::
     (Foo2 -> IO ())
  -> IO (Ptr.FunPtr (Foo2 -> IO ()))
hs_bindgen_235fa4a89af25f04 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_235fa4a89af25f04_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_8605b223a9ab9562_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> IO ())
  -> GHC.Int.Int32 -> IO ()

-- __unique:__ @instance FromFunPtr (Foo2 -> IO ())@
hs_bindgen_8605b223a9ab9562 ::
     Ptr.FunPtr (Foo2 -> IO ())
  -> Foo2 -> IO ()
hs_bindgen_8605b223a9ab9562 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_8605b223a9ab9562_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr (Foo2 -> IO ()) where

  toFunPtr = hs_bindgen_235fa4a89af25f04

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr (Foo2 -> IO ()) where

  fromFunPtr = hs_bindgen_8605b223a9ab9562
