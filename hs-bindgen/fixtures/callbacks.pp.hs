{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.Prelude
import qualified HsBindgen.Runtime.SizedByteArray
import qualified Text.Read
import Data.Bits (FiniteBits)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

$(HsBindgen.Runtime.Prelude.addCSource "#include <callbacks.h>\nsigned int hs_bindgen_test_callbacks_a0a59181c714c131 (void (*arg1) (signed int arg1), signed int arg2) { return readFileWithProcessor(arg1, arg2); }\nvoid hs_bindgen_test_callbacks_d59e6698796971ea (void (*arg1) (signed int arg1), signed int arg2) { watchTemperature(arg1, arg2); }\nvoid hs_bindgen_test_callbacks_c9fb8fdc3d0d3978 (FileOpenedNotification arg1) { onFileOpened(arg1); }\nvoid hs_bindgen_test_callbacks_7921ad1b219190e4 (ProgressUpdate arg1) { onProgressChanged(arg1); }\nsigned int hs_bindgen_test_callbacks_ae19d658f098584a (DataValidator arg1, signed int arg2) { return validateInput(arg1, arg2); }\nvoid hs_bindgen_test_callbacks_d2fdffe85523b3ef (MeasurementReceived arg1) { onNewMeasurement(arg1); }\nvoid hs_bindgen_test_callbacks_c5b555bbc07b808d (MeasurementReceived2 arg1) { onNewMeasurement2(arg1); }\nvoid hs_bindgen_test_callbacks_65927c77229ad893 (SampleBufferFull arg1) { onBufferReady(arg1); }\nvoid hs_bindgen_test_callbacks_0b6a9249f49b986f (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, double (*arg2) (double arg1, signed int arg2), signed int arg3)) { transformMeasurement(arg1, arg2); }\nvoid hs_bindgen_test_callbacks_2c3e0e84ae9cde51 (void (*arg1) (struct Measurement *arg1, FileOpenedNotification arg2, signed int arg3)) { processWithCallbacks(arg1); }\nvoid hs_bindgen_test_callbacks_0b172585709f9d48 (struct MeasurementHandler *arg1) { registerHandler(arg1); }\nvoid hs_bindgen_test_callbacks_25a56dfc7b259e7d (struct Measurement *arg1, struct DataPipeline *arg2) { executePipeline(arg1, arg2); }\nvoid hs_bindgen_test_callbacks_5908d37641d70953 (struct Measurement *arg1, struct Processor *arg2) { runProcessor(arg1, arg2); }\nvoid hs_bindgen_test_callbacks_f3c99b4af7808e7f (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, DataValidator arg2, signed int arg3), DataValidator arg3)) { processMeasurementWithValidation(arg1, arg2); }\nvoid hs_bindgen_test_callbacks_fcce70013c76ce8b (void (*arg1) (foo arg1)) { f(arg1); }\nvoid hs_bindgen_test_callbacks_1d043de05a457e90 (void (*arg1) (foo2 const arg1)) { f2(arg1); }\n/* get_readFileWithProcessor_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_callbacks_c4b06d89a94616dd (void)) (void (*arg1) (signed int arg1), signed int arg2) { return &readFileWithProcessor; } \n/* get_watchTemperature_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_22c54726df44b640 (void)) (void (*arg1) (signed int arg1), signed int arg2) { return &watchTemperature; } \n/* get_onFileOpened_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_8167a5b82d621c9d (void)) (FileOpenedNotification arg1) { return &onFileOpened; } \n/* get_onProgressChanged_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_ef51ad75ce9862a3 (void)) (ProgressUpdate arg1) { return &onProgressChanged; } \n/* get_validateInput_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb (void)) (DataValidator arg1, signed int arg2) { return &validateInput; } \n/* get_onNewMeasurement_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_f9f4f5ec3dd82431 (void)) (MeasurementReceived arg1) { return &onNewMeasurement; } \n/* get_onNewMeasurement2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_9c5afeda25ede1ce (void)) (MeasurementReceived2 arg1) { return &onNewMeasurement2; } \n/* get_onBufferReady_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_8091188123328aa8 (void)) (SampleBufferFull arg1) { return &onBufferReady; } \n/* get_transformMeasurement_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_6c9fe4dae03a37fa (void)) (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, double (*arg2) (double arg1, signed int arg2), signed int arg3)) { return &transformMeasurement; } \n/* get_processWithCallbacks_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_2ee8d8889cd31fb7 (void)) (void (*arg1) (struct Measurement *arg1, FileOpenedNotification arg2, signed int arg3)) { return &processWithCallbacks; } \n/* get_registerHandler_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_5a70e34ecc71835b (void)) (struct MeasurementHandler *arg1) { return &registerHandler; } \n/* get_executePipeline_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_1a0881ba01e93710 (void)) (struct Measurement *arg1, struct DataPipeline *arg2) { return &executePipeline; } \n/* get_runProcessor_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_1e7d8fd6cb5a199f (void)) (struct Measurement *arg1, struct Processor *arg2) { return &runProcessor; } \n/* get_processMeasurementWithValidation_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_6621b1bf8ef7af3b (void)) (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, void (*arg2) (struct Measurement *arg1, DataValidator arg2, signed int arg3), DataValidator arg3)) { return &processMeasurementWithValidation; } \n/* get_f_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_c34fd33eedc1490d (void)) (void (*arg1) (foo arg1)) { return &f; } \n/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_490ca7e8c8282a69 (void)) (void (*arg1) (foo2 const arg1)) { return &f2; } \n")

{-| Auxiliary type used by 'FileOpenedNotification'

__defined at:__ @callbacks.h:10:16@

__exported by:__ @callbacks.h@
-}
newtype FileOpenedNotification_Deref = FileOpenedNotification_Deref
  { un_FileOpenedNotification_Deref :: IO ()
  }

foreign import ccall safe "wrapper" toFileOpenedNotification_Deref ::
     FileOpenedNotification_Deref
  -> IO (Ptr.FunPtr FileOpenedNotification_Deref)

foreign import ccall safe "dynamic" fromFileOpenedNotification_Deref ::
     Ptr.FunPtr FileOpenedNotification_Deref
  -> FileOpenedNotification_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr FileOpenedNotification_Deref where

  toFunPtr = toFileOpenedNotification_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr FileOpenedNotification_Deref where

  fromFunPtr = fromFileOpenedNotification_Deref

{-| __C declaration:__ @FileOpenedNotification@

    __defined at:__ @callbacks.h:10:16@

    __exported by:__ @callbacks.h@
-}
newtype FileOpenedNotification = FileOpenedNotification
  { un_FileOpenedNotification :: Ptr.FunPtr FileOpenedNotification_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'ProgressUpdate'

__defined at:__ @callbacks.h:11:16@

__exported by:__ @callbacks.h@
-}
newtype ProgressUpdate_Deref = ProgressUpdate_Deref
  { un_ProgressUpdate_Deref :: FC.CInt -> IO ()
  }

foreign import ccall safe "wrapper" toProgressUpdate_Deref ::
     ProgressUpdate_Deref
  -> IO (Ptr.FunPtr ProgressUpdate_Deref)

foreign import ccall safe "dynamic" fromProgressUpdate_Deref ::
     Ptr.FunPtr ProgressUpdate_Deref
  -> ProgressUpdate_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr ProgressUpdate_Deref where

  toFunPtr = toProgressUpdate_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr ProgressUpdate_Deref where

  fromFunPtr = fromProgressUpdate_Deref

{-| __C declaration:__ @ProgressUpdate@

    __defined at:__ @callbacks.h:11:16@

    __exported by:__ @callbacks.h@
-}
newtype ProgressUpdate = ProgressUpdate
  { un_ProgressUpdate :: Ptr.FunPtr ProgressUpdate_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'DataValidator'

__defined at:__ @callbacks.h:12:15@

__exported by:__ @callbacks.h@
-}
newtype DataValidator_Deref = DataValidator_Deref
  { un_DataValidator_Deref :: FC.CInt -> IO FC.CInt
  }

foreign import ccall safe "wrapper" toDataValidator_Deref ::
     DataValidator_Deref
  -> IO (Ptr.FunPtr DataValidator_Deref)

foreign import ccall safe "dynamic" fromDataValidator_Deref ::
     Ptr.FunPtr DataValidator_Deref
  -> DataValidator_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr DataValidator_Deref where

  toFunPtr = toDataValidator_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr DataValidator_Deref where

  fromFunPtr = fromDataValidator_Deref

{-| __C declaration:__ @DataValidator@

    __defined at:__ @callbacks.h:12:15@

    __exported by:__ @callbacks.h@
-}
newtype DataValidator = DataValidator
  { un_DataValidator :: Ptr.FunPtr DataValidator_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @Measurement@

    __defined at:__ @callbacks.h:21:8@

    __exported by:__ @callbacks.h@
-}
data Measurement = Measurement
  { measurement_value :: FC.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @callbacks.h:22:10@

         __exported by:__ @callbacks.h@
    -}
  , measurement_timestamp :: FC.CDouble
    {- ^ __C declaration:__ @timestamp@

         __defined at:__ @callbacks.h:23:10@

         __exported by:__ @callbacks.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Measurement where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Measurement
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Measurement measurement_value2 measurement_timestamp3 ->
               F.pokeByteOff ptr0 (0 :: Int) measurement_value2
            >> F.pokeByteOff ptr0 (8 :: Int) measurement_timestamp3

{-| Auxiliary type used by 'MeasurementReceived'

__defined at:__ @callbacks.h:26:16@

__exported by:__ @callbacks.h@
-}
newtype MeasurementReceived_Deref = MeasurementReceived_Deref
  { un_MeasurementReceived_Deref :: (Ptr.Ptr Measurement) -> IO ()
  }

foreign import ccall safe "wrapper" toMeasurementReceived_Deref ::
     MeasurementReceived_Deref
  -> IO (Ptr.FunPtr MeasurementReceived_Deref)

foreign import ccall safe "dynamic" fromMeasurementReceived_Deref ::
     Ptr.FunPtr MeasurementReceived_Deref
  -> MeasurementReceived_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr MeasurementReceived_Deref where

  toFunPtr = toMeasurementReceived_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr MeasurementReceived_Deref where

  fromFunPtr = fromMeasurementReceived_Deref

{-| __C declaration:__ @MeasurementReceived@

    __defined at:__ @callbacks.h:26:16@

    __exported by:__ @callbacks.h@
-}
newtype MeasurementReceived = MeasurementReceived
  { un_MeasurementReceived :: Ptr.FunPtr MeasurementReceived_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'MeasurementReceived2'

__defined at:__ @callbacks.h:29:16@

__exported by:__ @callbacks.h@
-}
newtype MeasurementReceived2_Deref = MeasurementReceived2_Deref
  { un_MeasurementReceived2_Deref :: Measurement -> IO ()
  }

{-| __C declaration:__ @MeasurementReceived2@

    __defined at:__ @callbacks.h:29:16@

    __exported by:__ @callbacks.h@
-}
newtype MeasurementReceived2 = MeasurementReceived2
  { un_MeasurementReceived2 :: Ptr.FunPtr MeasurementReceived2_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'SampleBufferFull'

__defined at:__ @callbacks.h:32:16@

__exported by:__ @callbacks.h@
-}
newtype SampleBufferFull_Deref = SampleBufferFull_Deref
  { un_SampleBufferFull_Deref :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) FC.CInt) -> IO ()
  }

{-| __C declaration:__ @SampleBufferFull@

    __defined at:__ @callbacks.h:32:16@

    __exported by:__ @callbacks.h@
-}
newtype SampleBufferFull = SampleBufferFull
  { un_SampleBufferFull :: Ptr.FunPtr SampleBufferFull_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @MeasurementHandler@

    __defined at:__ @callbacks.h:50:8@

    __exported by:__ @callbacks.h@
-}
data MeasurementHandler = MeasurementHandler
  { measurementHandler_onReceived :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
    {- ^ __C declaration:__ @onReceived@

         __defined at:__ @callbacks.h:51:10@

         __exported by:__ @callbacks.h@
    -}
  , measurementHandler_validate :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)
    {- ^ __C declaration:__ @validate@

         __defined at:__ @callbacks.h:52:9@

         __exported by:__ @callbacks.h@
    -}
  , measurementHandler_onError :: Ptr.FunPtr (FC.CInt -> IO ())
    {- ^ __C declaration:__ @onError@

         __defined at:__ @callbacks.h:53:10@

         __exported by:__ @callbacks.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable MeasurementHandler where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure MeasurementHandler
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MeasurementHandler
            measurementHandler_onReceived2
            measurementHandler_validate3
            measurementHandler_onError4 ->
                 F.pokeByteOff ptr0 (0 :: Int) measurementHandler_onReceived2
              >> F.pokeByteOff ptr0 (8 :: Int) measurementHandler_validate3
              >> F.pokeByteOff ptr0 (16 :: Int) measurementHandler_onError4

{-| __C declaration:__ @DataPipeline@

    __defined at:__ @callbacks.h:58:8@

    __exported by:__ @callbacks.h@
-}
data DataPipeline = DataPipeline
  { dataPipeline_preProcess :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
    {- ^ __C declaration:__ @preProcess@

         __defined at:__ @callbacks.h:59:10@

         __exported by:__ @callbacks.h@
    -}
  , dataPipeline_process :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
    {- ^ __C declaration:__ @process@

         __defined at:__ @callbacks.h:60:10@

         __exported by:__ @callbacks.h@
    -}
  , dataPipeline_postProcess :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
    {- ^ __C declaration:__ @postProcess@

         __defined at:__ @callbacks.h:61:10@

         __exported by:__ @callbacks.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable DataPipeline where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure DataPipeline
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          DataPipeline
            dataPipeline_preProcess2
            dataPipeline_process3
            dataPipeline_postProcess4 ->
                 F.pokeByteOff ptr0 (0 :: Int) dataPipeline_preProcess2
              >> F.pokeByteOff ptr0 (8 :: Int) dataPipeline_process3
              >> F.pokeByteOff ptr0 (16 :: Int) dataPipeline_postProcess4

{-| __C declaration:__ @ProcessorCallback@

    __defined at:__ @callbacks.h:69:7@

    __exported by:__ @callbacks.h@
-}
newtype ProcessorCallback = ProcessorCallback
  { un_ProcessorCallback :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 8 instance F.Storable ProcessorCallback

{-|

  __See:__ 'set_processorCallback_simple'

__C declaration:__ @simple@

__defined at:__ @callbacks.h:70:10@

__exported by:__ @callbacks.h@
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

__defined at:__ @callbacks.h:71:10@

__exported by:__ @callbacks.h@
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

__defined at:__ @callbacks.h:72:10@

__exported by:__ @callbacks.h@
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

{-| __defined at:__ @callbacks.h:76:3@

    __exported by:__ @callbacks.h@
-}
newtype Processor_mode = Processor_mode
  { un_Processor_mode :: FC.CUInt
  }
  deriving stock (Eq, Ord)

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

    __defined at:__ @callbacks.h:76:10@

    __exported by:__ @callbacks.h@
-}
pattern MODE_SIMPLE :: Processor_mode
pattern MODE_SIMPLE = Processor_mode 0

{-| __C declaration:__ @MODE_VALIDATED@

    __defined at:__ @callbacks.h:76:23@

    __exported by:__ @callbacks.h@
-}
pattern MODE_VALIDATED :: Processor_mode
pattern MODE_VALIDATED = Processor_mode 1

{-| __C declaration:__ @MODE_PROGRESS@

    __defined at:__ @callbacks.h:76:39@

    __exported by:__ @callbacks.h@
-}
pattern MODE_PROGRESS :: Processor_mode
pattern MODE_PROGRESS = Processor_mode 2

{-| __C declaration:__ @Processor@

    __defined at:__ @callbacks.h:75:8@

    __exported by:__ @callbacks.h@
-}
data Processor = Processor
  { processor_mode :: Processor_mode
    {- ^ __C declaration:__ @mode@

         __defined at:__ @callbacks.h:76:55@

         __exported by:__ @callbacks.h@
    -}
  , processor_callback :: ProcessorCallback
    {- ^ __C declaration:__ @callback@

         __defined at:__ @callbacks.h:77:27@

         __exported by:__ @callbacks.h@
    -}
  }

instance F.Storable Processor where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Processor
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Processor processor_mode2 processor_callback3 ->
               F.pokeByteOff ptr0 (0 :: Int) processor_mode2
            >> F.pokeByteOff ptr0 (8 :: Int) processor_callback3

{-| __C declaration:__ @foo@

    __defined at:__ @callbacks.h:94:13@

    __exported by:__ @callbacks.h@
-}
newtype Foo = Foo
  { un_Foo :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @foo2@

    __defined at:__ @callbacks.h:95:13@

    __exported by:__ @callbacks.h@
-}
newtype Foo2 = Foo2
  { un_Foo2 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall safe "wrapper" funPtr_7a7e54ab_to ::
     (Foo -> IO ())
  -> IO (Ptr.FunPtr (Foo -> IO ()))

foreign import ccall safe "dynamic" funPtr_7a7e54ab_from ::
     Ptr.FunPtr (Foo -> IO ())
  -> Foo -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr (Foo -> IO ()) where

  toFunPtr = funPtr_7a7e54ab_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr (Foo -> IO ()) where

  fromFunPtr = funPtr_7a7e54ab_from

foreign import ccall safe "wrapper" funPtr_527712e4_to ::
     ((Ptr.Ptr Measurement) -> IO FC.CInt)
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt))

foreign import ccall safe "dynamic" funPtr_527712e4_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt)
  -> (Ptr.Ptr Measurement) -> IO FC.CInt

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt) where

  toFunPtr = funPtr_527712e4_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> IO FC.CInt) where

  fromFunPtr = funPtr_527712e4_from

foreign import ccall safe "wrapper" funPtr_2cc53fd3_to ::
     ((Ptr.Ptr Measurement) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ()))

foreign import ccall safe "dynamic" funPtr_2cc53fd3_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
  -> (Ptr.Ptr Measurement) -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> IO ()) where

  toFunPtr = funPtr_2cc53fd3_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> IO ()) where

  fromFunPtr = funPtr_2cc53fd3_from

foreign import ccall safe "wrapper" funPtr_6f6352e1_to ::
     ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()))

foreign import ccall safe "dynamic" funPtr_6f6352e1_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ())
  -> (Ptr.Ptr Measurement) -> DataValidator -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()) where

  toFunPtr = funPtr_6f6352e1_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> IO ()) where

  fromFunPtr = funPtr_6f6352e1_from

foreign import ccall safe "wrapper" funPtr_065333fd_to ::
     ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()))

foreign import ccall safe "dynamic" funPtr_065333fd_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()) where

  toFunPtr = funPtr_065333fd_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ()) where

  fromFunPtr = funPtr_065333fd_from

foreign import ccall safe "wrapper" funPtr_ac6a854e_to ::
     ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()))

foreign import ccall safe "dynamic" funPtr_ac6a854e_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()) where

  toFunPtr = funPtr_ac6a854e_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ()) where

  fromFunPtr = funPtr_ac6a854e_from

foreign import ccall safe "wrapper" funPtr_5e6e3f6b_to ::
     ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()))

foreign import ccall safe "dynamic" funPtr_5e6e3f6b_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ())
  -> (Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()) where

  toFunPtr = funPtr_5e6e3f6b_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> ProgressUpdate -> IO ()) where

  fromFunPtr = funPtr_5e6e3f6b_from

foreign import ccall safe "wrapper" funPtr_8b272628_to ::
     ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()))

foreign import ccall safe "dynamic" funPtr_8b272628_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
  -> (Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()) where

  toFunPtr = funPtr_8b272628_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ()) where

  fromFunPtr = funPtr_8b272628_from

foreign import ccall safe "wrapper" funPtr_30a248a4_to ::
     ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()))

foreign import ccall safe "dynamic" funPtr_30a248a4_from ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
  -> (Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()) where

  toFunPtr = funPtr_30a248a4_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ()) where

  fromFunPtr = funPtr_30a248a4_from

foreign import ccall safe "wrapper" funPtr_a3803a24_to ::
     (Foo2 -> IO ())
  -> IO (Ptr.FunPtr (Foo2 -> IO ()))

foreign import ccall safe "dynamic" funPtr_a3803a24_from ::
     Ptr.FunPtr (Foo2 -> IO ())
  -> Foo2 -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr (Foo2 -> IO ()) where

  toFunPtr = funPtr_a3803a24_to

instance HsBindgen.Runtime.FunPtr.FromFunPtr (Foo2 -> IO ()) where

  fromFunPtr = funPtr_a3803a24_from

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @callbacks.h:4:5@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_a0a59181c714c131" readFileWithProcessor ::
     Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @processLine@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @fileId@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @callbacks.h:5:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_d59e6698796971ea" watchTemperature ::
     Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @onTempChange@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @sensorId@
     -}
  -> IO ()

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @callbacks.h:14:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_c9fb8fdc3d0d3978" onFileOpened ::
     FileOpenedNotification
     {- ^ __C declaration:__ @notify@
     -}
  -> IO ()

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @callbacks.h:15:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_7921ad1b219190e4" onProgressChanged ::
     ProgressUpdate
     {- ^ __C declaration:__ @update@
     -}
  -> IO ()

{-| __C declaration:__ @validateInput@

    __defined at:__ @callbacks.h:16:5@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_ae19d658f098584a" validateInput ::
     DataValidator
     {- ^ __C declaration:__ @validator@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @rawValue@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @callbacks.h:27:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_d2fdffe85523b3ef" onNewMeasurement ::
     MeasurementReceived
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @callbacks.h:30:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_c5b555bbc07b808d" onNewMeasurement2 ::
     MeasurementReceived2
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @callbacks.h:33:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_65927c77229ad893" onBufferReady ::
     SampleBufferFull
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @callbacks.h:38:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_0b6a9249f49b986f" transformMeasurement ::
     Ptr.Ptr Measurement
     {- ^ __C declaration:__ @data'@
     -}
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
     {- ^ __C declaration:__ @transformer@
     -}
  -> IO ()

{-| __C declaration:__ @processWithCallbacks@

    __defined at:__ @callbacks.h:43:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_2c3e0e84ae9cde51" processWithCallbacks ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @registerHandler@

    __defined at:__ @callbacks.h:56:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_0b172585709f9d48" registerHandler ::
     Ptr.Ptr MeasurementHandler
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @executePipeline@

    __defined at:__ @callbacks.h:64:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_25a56dfc7b259e7d" executePipeline ::
     Ptr.Ptr Measurement
     {- ^ __C declaration:__ @data'@
     -}
  -> Ptr.Ptr DataPipeline
     {- ^ __C declaration:__ @pipeline@
     -}
  -> IO ()

{-| __C declaration:__ @runProcessor@

    __defined at:__ @callbacks.h:80:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_5908d37641d70953" runProcessor ::
     Ptr.Ptr Measurement
     {- ^ __C declaration:__ @data'@
     -}
  -> Ptr.Ptr Processor
     {- ^ __C declaration:__ @processor@
     -}
  -> IO ()

{-| __C declaration:__ @processMeasurementWithValidation@

    __defined at:__ @callbacks.h:85:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_f3c99b4af7808e7f" processMeasurementWithValidation ::
     Ptr.Ptr Measurement
     {- ^ __C declaration:__ @data'@
     -}
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
     {- ^ __C declaration:__ @processor@
     -}
  -> IO ()

{-| __C declaration:__ @f@

    __defined at:__ @callbacks.h:96:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_fcce70013c76ce8b" f ::
     Ptr.FunPtr (Foo -> IO ())
     {- ^ __C declaration:__ @callback@
     -}
  -> IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @callbacks.h:97:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_1d043de05a457e90" f2 ::
     Ptr.FunPtr (Foo2 -> IO ())
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

foreign import ccall unsafe "hs_bindgen_test_callbacks_c4b06d89a94616dd" hs_bindgen_test_callbacks_c4b06d89a94616dd ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE readFileWithProcessor_ptr #-}

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @callbacks.h:4:5@

    __exported by:__ @callbacks.h@
-}
readFileWithProcessor_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt)
readFileWithProcessor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_c4b06d89a94616dd

foreign import ccall unsafe "hs_bindgen_test_callbacks_22c54726df44b640" hs_bindgen_test_callbacks_22c54726df44b640 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ()))

{-# NOINLINE watchTemperature_ptr #-}

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @callbacks.h:5:6@

    __exported by:__ @callbacks.h@
-}
watchTemperature_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ())
watchTemperature_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_22c54726df44b640

foreign import ccall unsafe "hs_bindgen_test_callbacks_8167a5b82d621c9d" hs_bindgen_test_callbacks_8167a5b82d621c9d ::
     IO (Ptr.FunPtr (FileOpenedNotification -> IO ()))

{-# NOINLINE onFileOpened_ptr #-}

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @callbacks.h:14:6@

    __exported by:__ @callbacks.h@
-}
onFileOpened_ptr :: Ptr.FunPtr (FileOpenedNotification -> IO ())
onFileOpened_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_8167a5b82d621c9d

foreign import ccall unsafe "hs_bindgen_test_callbacks_ef51ad75ce9862a3" hs_bindgen_test_callbacks_ef51ad75ce9862a3 ::
     IO (Ptr.FunPtr (ProgressUpdate -> IO ()))

{-# NOINLINE onProgressChanged_ptr #-}

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @callbacks.h:15:6@

    __exported by:__ @callbacks.h@
-}
onProgressChanged_ptr :: Ptr.FunPtr (ProgressUpdate -> IO ())
onProgressChanged_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_ef51ad75ce9862a3

foreign import ccall unsafe "hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb" hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb ::
     IO (Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt))

{-# NOINLINE validateInput_ptr #-}

{-| __C declaration:__ @validateInput@

    __defined at:__ @callbacks.h:16:5@

    __exported by:__ @callbacks.h@
-}
validateInput_ptr :: Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt)
validateInput_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb

foreign import ccall unsafe "hs_bindgen_test_callbacks_f9f4f5ec3dd82431" hs_bindgen_test_callbacks_f9f4f5ec3dd82431 ::
     IO (Ptr.FunPtr (MeasurementReceived -> IO ()))

{-# NOINLINE onNewMeasurement_ptr #-}

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @callbacks.h:27:6@

    __exported by:__ @callbacks.h@
-}
onNewMeasurement_ptr :: Ptr.FunPtr (MeasurementReceived -> IO ())
onNewMeasurement_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_f9f4f5ec3dd82431

foreign import ccall unsafe "hs_bindgen_test_callbacks_9c5afeda25ede1ce" hs_bindgen_test_callbacks_9c5afeda25ede1ce ::
     IO (Ptr.FunPtr (MeasurementReceived2 -> IO ()))

{-# NOINLINE onNewMeasurement2_ptr #-}

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @callbacks.h:30:6@

    __exported by:__ @callbacks.h@
-}
onNewMeasurement2_ptr :: Ptr.FunPtr (MeasurementReceived2 -> IO ())
onNewMeasurement2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_9c5afeda25ede1ce

foreign import ccall unsafe "hs_bindgen_test_callbacks_8091188123328aa8" hs_bindgen_test_callbacks_8091188123328aa8 ::
     IO (Ptr.FunPtr (SampleBufferFull -> IO ()))

{-# NOINLINE onBufferReady_ptr #-}

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @callbacks.h:33:6@

    __exported by:__ @callbacks.h@
-}
onBufferReady_ptr :: Ptr.FunPtr (SampleBufferFull -> IO ())
onBufferReady_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_8091188123328aa8

foreign import ccall unsafe "hs_bindgen_test_callbacks_6c9fe4dae03a37fa" hs_bindgen_test_callbacks_6c9fe4dae03a37fa ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())) -> IO ()))

{-# NOINLINE transformMeasurement_ptr #-}

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @callbacks.h:38:6@

    __exported by:__ @callbacks.h@
-}
transformMeasurement_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())) -> IO ())
transformMeasurement_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_6c9fe4dae03a37fa

foreign import ccall unsafe "hs_bindgen_test_callbacks_2ee8d8889cd31fb7" hs_bindgen_test_callbacks_2ee8d8889cd31fb7 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())) -> IO ()))

{-# NOINLINE processWithCallbacks_ptr #-}

{-| __C declaration:__ @processWithCallbacks@

    __defined at:__ @callbacks.h:43:6@

    __exported by:__ @callbacks.h@
-}
processWithCallbacks_ptr :: Ptr.FunPtr ((Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())) -> IO ())
processWithCallbacks_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_2ee8d8889cd31fb7

foreign import ccall unsafe "hs_bindgen_test_callbacks_5a70e34ecc71835b" hs_bindgen_test_callbacks_5a70e34ecc71835b ::
     IO (Ptr.FunPtr ((Ptr.Ptr MeasurementHandler) -> IO ()))

{-# NOINLINE registerHandler_ptr #-}

{-| __C declaration:__ @registerHandler@

    __defined at:__ @callbacks.h:56:6@

    __exported by:__ @callbacks.h@
-}
registerHandler_ptr :: Ptr.FunPtr ((Ptr.Ptr MeasurementHandler) -> IO ())
registerHandler_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_5a70e34ecc71835b

foreign import ccall unsafe "hs_bindgen_test_callbacks_1a0881ba01e93710" hs_bindgen_test_callbacks_1a0881ba01e93710 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr DataPipeline) -> IO ()))

{-# NOINLINE executePipeline_ptr #-}

{-| __C declaration:__ @executePipeline@

    __defined at:__ @callbacks.h:64:6@

    __exported by:__ @callbacks.h@
-}
executePipeline_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr DataPipeline) -> IO ())
executePipeline_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_1a0881ba01e93710

foreign import ccall unsafe "hs_bindgen_test_callbacks_1e7d8fd6cb5a199f" hs_bindgen_test_callbacks_1e7d8fd6cb5a199f ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr Processor) -> IO ()))

{-# NOINLINE runProcessor_ptr #-}

{-| __C declaration:__ @runProcessor@

    __defined at:__ @callbacks.h:80:6@

    __exported by:__ @callbacks.h@
-}
runProcessor_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr Processor) -> IO ())
runProcessor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_1e7d8fd6cb5a199f

foreign import ccall unsafe "hs_bindgen_test_callbacks_6621b1bf8ef7af3b" hs_bindgen_test_callbacks_6621b1bf8ef7af3b ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())) -> IO ()))

{-# NOINLINE processMeasurementWithValidation_ptr #-}

{-| __C declaration:__ @processMeasurementWithValidation@

    __defined at:__ @callbacks.h:85:6@

    __exported by:__ @callbacks.h@
-}
processMeasurementWithValidation_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())) -> IO ())
processMeasurementWithValidation_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_6621b1bf8ef7af3b

foreign import ccall unsafe "hs_bindgen_test_callbacks_c34fd33eedc1490d" hs_bindgen_test_callbacks_c34fd33eedc1490d ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (Foo -> IO ())) -> IO ()))

{-# NOINLINE f_ptr #-}

{-| __C declaration:__ @f@

    __defined at:__ @callbacks.h:96:6@

    __exported by:__ @callbacks.h@
-}
f_ptr :: Ptr.FunPtr ((Ptr.FunPtr (Foo -> IO ())) -> IO ())
f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_c34fd33eedc1490d

foreign import ccall unsafe "hs_bindgen_test_callbacks_490ca7e8c8282a69" hs_bindgen_test_callbacks_490ca7e8c8282a69 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (Foo2 -> IO ())) -> IO ()))

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @callbacks.h:97:6@

    __exported by:__ @callbacks.h@
-}
f2_ptr :: Ptr.FunPtr ((Ptr.FunPtr (Foo2 -> IO ())) -> IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_490ca7e8c8282a69
