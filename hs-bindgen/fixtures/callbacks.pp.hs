{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.ConstantArray
import Prelude ((<*>), (>>), Eq, IO, Int, Ord, Show, pure)

$(CAPI.addCSource "#include <callbacks.h>\nsigned int hs_bindgen_test_callbacks_b5fe3bd1fd70072c (void (*arg1) (signed int arg1), signed int arg2) { return readFileWithProcessor(arg1, arg2); }\n/* get_readFileWithProcessor_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_callbacks_9bff2165e3decc84 (void)) (void (*arg1) (signed int arg1), signed int arg2) { return &readFileWithProcessor; } \nvoid hs_bindgen_test_callbacks_e444e06c0c5d9e82 (void (*arg1) (signed int arg1), signed int arg2) { watchTemperature(arg1, arg2); }\n/* get_watchTemperature_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_c2f8e0ccab10771e (void)) (void (*arg1) (signed int arg1), signed int arg2) { return &watchTemperature; } \nvoid hs_bindgen_test_callbacks_bebfa5cb7ff1a841 (FileOpenedNotification arg1) { onFileOpened(arg1); }\n/* get_onFileOpened_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_b0c6e0e75106190c (void)) (FileOpenedNotification arg1) { return &onFileOpened; } \nvoid hs_bindgen_test_callbacks_f8434b7a5c10a8de (ProgressUpdate arg1) { onProgressChanged(arg1); }\n/* get_onProgressChanged_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_d5b434b3faaf4465 (void)) (ProgressUpdate arg1) { return &onProgressChanged; } \nsigned int hs_bindgen_test_callbacks_41dbe3a1204d2401 (DataValidator arg1, signed int arg2) { return validateInput(arg1, arg2); }\n/* get_validateInput_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_callbacks_b753c64bc11604dd (void)) (DataValidator arg1, signed int arg2) { return &validateInput; } \nvoid hs_bindgen_test_callbacks_80ae1355e2538a88 (MeasurementReceived arg1) { onNewMeasurement(arg1); }\n/* get_onNewMeasurement_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_5ede51ad7cee58ce (void)) (MeasurementReceived arg1) { return &onNewMeasurement; } \nvoid hs_bindgen_test_callbacks_c537c4b0f5f57239 (SampleBufferFull arg1) { onBufferReady(arg1); }\n/* get_onBufferReady_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_514203bcc8a16ea6 (void)) (SampleBufferFull arg1) { return &onBufferReady; } \n")

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @callbacks.h:2:5@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_b5fe3bd1fd70072c" readFileWithProcessor
  :: Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @processLine@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @fileId@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @callbacks.h:2:5@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_9bff2165e3decc84" hs_bindgen_test_callbacks_9bff2165e3decc84
  :: IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE readFileWithProcessor_ptr #-}

readFileWithProcessor_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt)
readFileWithProcessor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_9bff2165e3decc84

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @callbacks.h:3:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_e444e06c0c5d9e82" watchTemperature
  :: Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @onTempChange@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @sensorId@
     -}
  -> IO ()

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @callbacks.h:3:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_c2f8e0ccab10771e" hs_bindgen_test_callbacks_c2f8e0ccab10771e
  :: IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ()))

{-# NOINLINE watchTemperature_ptr #-}

watchTemperature_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ())
watchTemperature_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_c2f8e0ccab10771e

{-| __C declaration:__ @FileOpenedNotification@

    __defined at:__ @callbacks.h:6:16@

    __exported by:__ @callbacks.h@
-}
newtype FileOpenedNotification = FileOpenedNotification
  { un_FileOpenedNotification :: Ptr.FunPtr (IO ())
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  Convert Haskell function @IO ()@ to 'FileOpenedNotification' (C function pointer typedef).

-}
foreign import capi safe "wrapper" mkFileOpenedNotification
  :: IO ()
  -> IO FileOpenedNotification

{-| __C declaration:__ @ProgressUpdate@

    __defined at:__ @callbacks.h:7:16@

    __exported by:__ @callbacks.h@
-}
newtype ProgressUpdate = ProgressUpdate
  { un_ProgressUpdate :: Ptr.FunPtr (FC.CInt -> IO ())
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  Convert Haskell function @FC.CInt -> IO ()@ to 'ProgressUpdate' (C function pointer typedef).

-}
foreign import capi safe "wrapper" mkProgressUpdate
  :: (FC.CInt -> IO ())
  -> IO ProgressUpdate

{-| __C declaration:__ @DataValidator@

    __defined at:__ @callbacks.h:8:15@

    __exported by:__ @callbacks.h@
-}
newtype DataValidator = DataValidator
  { un_DataValidator :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  Convert Haskell function @FC.CInt -> IO FC.CInt@ to 'DataValidator' (C function pointer typedef).

-}
foreign import capi safe "wrapper" mkDataValidator
  :: (FC.CInt -> IO FC.CInt)
  -> IO DataValidator

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @callbacks.h:10:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_bebfa5cb7ff1a841" onFileOpened
  :: FileOpenedNotification
     {- ^ __C declaration:__ @notify@
     -}
  -> IO ()

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @callbacks.h:10:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_b0c6e0e75106190c" hs_bindgen_test_callbacks_b0c6e0e75106190c
  :: IO (Ptr.FunPtr (FileOpenedNotification -> IO ()))

{-# NOINLINE onFileOpened_ptr #-}

onFileOpened_ptr :: Ptr.FunPtr (FileOpenedNotification -> IO ())
onFileOpened_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_b0c6e0e75106190c

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @callbacks.h:11:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_f8434b7a5c10a8de" onProgressChanged
  :: ProgressUpdate
     {- ^ __C declaration:__ @update@
     -}
  -> IO ()

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @callbacks.h:11:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_d5b434b3faaf4465" hs_bindgen_test_callbacks_d5b434b3faaf4465
  :: IO (Ptr.FunPtr (ProgressUpdate -> IO ()))

{-# NOINLINE onProgressChanged_ptr #-}

onProgressChanged_ptr :: Ptr.FunPtr (ProgressUpdate -> IO ())
onProgressChanged_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_d5b434b3faaf4465

{-| __C declaration:__ @validateInput@

    __defined at:__ @callbacks.h:12:5@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_41dbe3a1204d2401" validateInput
  :: DataValidator
     {- ^ __C declaration:__ @validator@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @rawValue@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @validateInput@

    __defined at:__ @callbacks.h:12:5@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_b753c64bc11604dd" hs_bindgen_test_callbacks_b753c64bc11604dd
  :: IO (Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt))

{-# NOINLINE validateInput_ptr #-}

validateInput_ptr :: Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt)
validateInput_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_b753c64bc11604dd

{-| __C declaration:__ @Measurement@

    __defined at:__ @callbacks.h:15:8@

    __exported by:__ @callbacks.h@
-}
data Measurement = Measurement
  { measurement_value :: FC.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @callbacks.h:16:10@

         __exported by:__ @callbacks.h@
    -}
  , measurement_timestamp :: FC.CDouble
    {- ^ __C declaration:__ @timestamp@

         __defined at:__ @callbacks.h:16:17@

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

{-| __C declaration:__ @MeasurementReceived@

    __defined at:__ @callbacks.h:19:16@

    __exported by:__ @callbacks.h@
-}
newtype MeasurementReceived = MeasurementReceived
  { un_MeasurementReceived :: Ptr.FunPtr (Measurement -> IO ())
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  Convert Haskell function @Measurement -> IO ()@ to 'MeasurementReceived' (C function pointer typedef).

-}
foreign import capi safe "wrapper" mkMeasurementReceived
  :: (Measurement -> IO ())
  -> IO MeasurementReceived

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @callbacks.h:20:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_80ae1355e2538a88" onNewMeasurement
  :: MeasurementReceived
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @callbacks.h:20:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_5ede51ad7cee58ce" hs_bindgen_test_callbacks_5ede51ad7cee58ce
  :: IO (Ptr.FunPtr (MeasurementReceived -> IO ()))

{-# NOINLINE onNewMeasurement_ptr #-}

onNewMeasurement_ptr :: Ptr.FunPtr (MeasurementReceived -> IO ())
onNewMeasurement_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_5ede51ad7cee58ce

{-| __C declaration:__ @SampleBufferFull@

    __defined at:__ @callbacks.h:23:16@

    __exported by:__ @callbacks.h@
-}
newtype SampleBufferFull = SampleBufferFull
  { un_SampleBufferFull :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 10) FC.CInt) -> IO ())
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  Convert Haskell function @((HsBindgen.Runtime.ConstantArray.ConstantArray 10) FC.CInt) -> IO ()@ to 'SampleBufferFull' (C function pointer typedef).

-}
foreign import capi safe "wrapper" mkSampleBufferFull
  :: (((HsBindgen.Runtime.ConstantArray.ConstantArray 10) FC.CInt) -> IO ())
  -> IO SampleBufferFull

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @callbacks.h:24:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_c537c4b0f5f57239" onBufferReady
  :: SampleBufferFull
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @callbacks.h:24:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_514203bcc8a16ea6" hs_bindgen_test_callbacks_514203bcc8a16ea6
  :: IO (Ptr.FunPtr (SampleBufferFull -> IO ()))

{-# NOINLINE onBufferReady_ptr #-}

onBufferReady_ptr :: Ptr.FunPtr (SampleBufferFull -> IO ())
onBufferReady_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_514203bcc8a16ea6

{-|

  Convert Haskell function @FC.CInt -> IO ()@ to C function pointer.

-}
foreign import capi safe "wrapper" wrapFunPtr_CInt_to_Unit
  :: (FC.CInt -> IO ())
  -> IO (Ptr.FunPtr (FC.CInt -> IO ()))
