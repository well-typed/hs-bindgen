{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <callbacks.h>"
  , "/* get_readFileWithProcessor_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_callbacks_c4b06d89a94616dd (void)) ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &readFileWithProcessor;"
  , "}"
  , "/* get_watchTemperature_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_22c54726df44b640 (void)) ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &watchTemperature;"
  , "}"
  , "/* get_onFileOpened_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_8167a5b82d621c9d (void)) ("
  , "  FileOpenedNotification arg1"
  , ")"
  , "{"
  , "  return &onFileOpened;"
  , "}"
  , "/* get_onProgressChanged_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_ef51ad75ce9862a3 (void)) ("
  , "  ProgressUpdate arg1"
  , ")"
  , "{"
  , "  return &onProgressChanged;"
  , "}"
  , "/* get_validateInput_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_callbacks_9eaedb1b1c5b3fdb (void)) ("
  , "  DataValidator arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &validateInput;"
  , "}"
  , "/* get_onNewMeasurement_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_f9f4f5ec3dd82431 (void)) ("
  , "  MeasurementReceived arg1"
  , ")"
  , "{"
  , "  return &onNewMeasurement;"
  , "}"
  , "/* get_onNewMeasurement2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_9c5afeda25ede1ce (void)) ("
  , "  MeasurementReceived2 arg1"
  , ")"
  , "{"
  , "  return &onNewMeasurement2;"
  , "}"
  , "/* get_onBufferReady_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_8091188123328aa8 (void)) ("
  , "  SampleBufferFull arg1"
  , ")"
  , "{"
  , "  return &onBufferReady;"
  , "}"
  , "/* get_transformMeasurement_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_6c9fe4dae03a37fa (void)) ("
  , "  struct Measurement *arg1,"
  , "  void (*arg2) ("
  , "  struct Measurement *arg1,"
  , "  double (*arg2) ("
  , "  double arg1,"
  , "  signed int arg2"
  , "),"
  , "  signed int arg3"
  , ")"
  , ")"
  , "{"
  , "  return &transformMeasurement;"
  , "}"
  , "/* get_processWithCallbacks_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_2ee8d8889cd31fb7 (void)) ("
  , "  void (*arg1) ("
  , "  struct Measurement *arg1,"
  , "  FileOpenedNotification arg2,"
  , "  signed int arg3"
  , ")"
  , ")"
  , "{"
  , "  return &processWithCallbacks;"
  , "}"
  , "/* get_registerHandler_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_5a70e34ecc71835b (void)) ("
  , "  struct MeasurementHandler *arg1"
  , ")"
  , "{"
  , "  return &registerHandler;"
  , "}"
  , "/* get_executePipeline_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_1a0881ba01e93710 (void)) ("
  , "  struct Measurement *arg1,"
  , "  struct DataPipeline *arg2"
  , ")"
  , "{"
  , "  return &executePipeline;"
  , "}"
  , "/* get_runProcessor_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_1e7d8fd6cb5a199f (void)) ("
  , "  struct Measurement *arg1,"
  , "  struct Processor *arg2"
  , ")"
  , "{"
  , "  return &runProcessor;"
  , "}"
  , "/* get_processMeasurementWithValidation_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_6621b1bf8ef7af3b (void)) ("
  , "  struct Measurement *arg1,"
  , "  void (*arg2) ("
  , "  struct Measurement *arg1,"
  , "  void (*arg2) ("
  , "  struct Measurement *arg1,"
  , "  DataValidator arg2,"
  , "  signed int arg3"
  , "),"
  , "  DataValidator arg3"
  , ")"
  , ")"
  , "{"
  , "  return &processMeasurementWithValidation;"
  , "}"
  , "/* get_f_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_c34fd33eedc1490d (void)) ("
  , "  void (*arg1) ("
  , "  foo arg1"
  , ")"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  , "/* get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_callbacks_490ca7e8c8282a69 (void)) ("
  , "  void (*arg1) ("
  , "  foo2 const arg1"
  , ")"
  , ")"
  , "{"
  , "  return &f2;"
  , "}"
  ]))

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
