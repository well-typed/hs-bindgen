{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Eq, IO, Int, Ord, Show, pure)

$(HsBindgen.Runtime.Prelude.addCSource "#include <callbacks.h>\nsigned int hs_bindgen_test_callbacks_a0a59181c714c131 (void (*arg1) (signed int arg1), signed int arg2) { return readFileWithProcessor(arg1, arg2); }\nvoid hs_bindgen_test_callbacks_d59e6698796971ea (void (*arg1) (signed int arg1), signed int arg2) { watchTemperature(arg1, arg2); }\nvoid hs_bindgen_test_callbacks_c9fb8fdc3d0d3978 (FileOpenedNotification arg1) { onFileOpened(arg1); }\nvoid hs_bindgen_test_callbacks_7921ad1b219190e4 (ProgressUpdate arg1) { onProgressChanged(arg1); }\nsigned int hs_bindgen_test_callbacks_ae19d658f098584a (DataValidator arg1, signed int arg2) { return validateInput(arg1, arg2); }\nvoid hs_bindgen_test_callbacks_d2fdffe85523b3ef (MeasurementReceived arg1) { onNewMeasurement(arg1); }\n/* get_readFileWithProcessor_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_callbacks_c4b06d89a94616dd (void)) (void (*arg1) (signed int arg1), signed int arg2) { return &readFileWithProcessor; } \n/* get_watchTemperature_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_22c54726df44b640 (void)) (void (*arg1) (signed int arg1), signed int arg2) { return &watchTemperature; } \n/* get_onFileOpened_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_8167a5b82d621c9d (void)) (FileOpenedNotification arg1) { return &onFileOpened; } \n/* get_onProgressChanged_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_ef51ad75ce9862a3 (void)) (ProgressUpdate arg1) { return &onProgressChanged; } \n/* get_validateInput_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb (void)) (DataValidator arg1, signed int arg2) { return &validateInput; } \n/* get_onNewMeasurement_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_callbacks_f9f4f5ec3dd82431 (void)) (MeasurementReceived arg1) { return &onNewMeasurement; } \n")

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
foreign import ccall safe "wrapper" mkFileOpenedNotification
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
foreign import ccall safe "wrapper" mkProgressUpdate
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
foreign import ccall safe "wrapper" mkDataValidator
  :: (FC.CInt -> IO FC.CInt)
  -> IO DataValidator

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
  { un_MeasurementReceived :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> IO ())
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  Convert Haskell function @(Ptr.Ptr Measurement) -> IO ()@ to 'MeasurementReceived' (C function pointer typedef).

-}
foreign import ccall safe "wrapper" mkMeasurementReceived
  :: ((Ptr.Ptr Measurement) -> IO ())
  -> IO MeasurementReceived

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @callbacks.h:2:5@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_a0a59181c714c131" readFileWithProcessor
  :: Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @processLine@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @fileId@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @callbacks.h:3:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_d59e6698796971ea" watchTemperature
  :: Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @onTempChange@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @sensorId@
     -}
  -> IO ()

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @callbacks.h:10:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_c9fb8fdc3d0d3978" onFileOpened
  :: FileOpenedNotification
     {- ^ __C declaration:__ @notify@
     -}
  -> IO ()

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @callbacks.h:11:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_7921ad1b219190e4" onProgressChanged
  :: ProgressUpdate
     {- ^ __C declaration:__ @update@
     -}
  -> IO ()

{-| __C declaration:__ @validateInput@

    __defined at:__ @callbacks.h:12:5@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_ae19d658f098584a" validateInput
  :: DataValidator
     {- ^ __C declaration:__ @validator@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @rawValue@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @callbacks.h:20:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall safe "hs_bindgen_test_callbacks_d2fdffe85523b3ef" onNewMeasurement
  :: MeasurementReceived
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @callbacks.h:2:5@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_c4b06d89a94616dd" hs_bindgen_test_callbacks_c4b06d89a94616dd
  :: IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE readFileWithProcessor_ptr #-}

readFileWithProcessor_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt)
readFileWithProcessor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_c4b06d89a94616dd

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @callbacks.h:3:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_22c54726df44b640" hs_bindgen_test_callbacks_22c54726df44b640
  :: IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ()))

{-# NOINLINE watchTemperature_ptr #-}

watchTemperature_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ())
watchTemperature_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_22c54726df44b640

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @callbacks.h:10:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_8167a5b82d621c9d" hs_bindgen_test_callbacks_8167a5b82d621c9d
  :: IO (Ptr.FunPtr (FileOpenedNotification -> IO ()))

{-# NOINLINE onFileOpened_ptr #-}

onFileOpened_ptr :: Ptr.FunPtr (FileOpenedNotification -> IO ())
onFileOpened_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_8167a5b82d621c9d

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @callbacks.h:11:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_ef51ad75ce9862a3" hs_bindgen_test_callbacks_ef51ad75ce9862a3
  :: IO (Ptr.FunPtr (ProgressUpdate -> IO ()))

{-# NOINLINE onProgressChanged_ptr #-}

onProgressChanged_ptr :: Ptr.FunPtr (ProgressUpdate -> IO ())
onProgressChanged_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_ef51ad75ce9862a3

{-| __C declaration:__ @validateInput@

    __defined at:__ @callbacks.h:12:5@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb" hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb
  :: IO (Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt))

{-# NOINLINE validateInput_ptr #-}

validateInput_ptr :: Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt)
validateInput_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @callbacks.h:20:6@

    __exported by:__ @callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_callbacks_f9f4f5ec3dd82431" hs_bindgen_test_callbacks_f9f4f5ec3dd82431
  :: IO (Ptr.FunPtr (MeasurementReceived -> IO ()))

{-# NOINLINE onNewMeasurement_ptr #-}

onNewMeasurement_ptr :: Ptr.FunPtr (MeasurementReceived -> IO ())
onNewMeasurement_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_callbacks_f9f4f5ec3dd82431
