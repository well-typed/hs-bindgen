{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/callbacks.h>"
  , "/* get_readFileWithProcessor_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionscallbacks_c4b06d89a94616dd (void)) ("
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
  , "void (*hs_bindgen_test_functionscallbacks_22c54726df44b640 (void)) ("
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
  , "void (*hs_bindgen_test_functionscallbacks_8167a5b82d621c9d (void)) ("
  , "  FileOpenedNotification arg1"
  , ")"
  , "{"
  , "  return &onFileOpened;"
  , "}"
  , "/* get_onProgressChanged_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_ef51ad75ce9862a3 (void)) ("
  , "  ProgressUpdate arg1"
  , ")"
  , "{"
  , "  return &onProgressChanged;"
  , "}"
  , "/* get_validateInput_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionscallbacks_9eaedb1b1c5b3fdb (void)) ("
  , "  DataValidator arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &validateInput;"
  , "}"
  , "/* get_onNewMeasurement_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_f9f4f5ec3dd82431 (void)) ("
  , "  MeasurementReceived arg1"
  , ")"
  , "{"
  , "  return &onNewMeasurement;"
  , "}"
  , "/* get_onNewMeasurement2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_9c5afeda25ede1ce (void)) ("
  , "  MeasurementReceived2 arg1"
  , ")"
  , "{"
  , "  return &onNewMeasurement2;"
  , "}"
  , "/* get_onBufferReady_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_8091188123328aa8 (void)) ("
  , "  SampleBufferFull arg1"
  , ")"
  , "{"
  , "  return &onBufferReady;"
  , "}"
  , "/* get_transformMeasurement_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_6c9fe4dae03a37fa (void)) ("
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
  , "void (*hs_bindgen_test_functionscallbacks_2ee8d8889cd31fb7 (void)) ("
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
  , "void (*hs_bindgen_test_functionscallbacks_5a70e34ecc71835b (void)) ("
  , "  struct MeasurementHandler *arg1"
  , ")"
  , "{"
  , "  return &registerHandler;"
  , "}"
  , "/* get_executePipeline_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_1a0881ba01e93710 (void)) ("
  , "  struct Measurement *arg1,"
  , "  struct DataPipeline *arg2"
  , ")"
  , "{"
  , "  return &executePipeline;"
  , "}"
  , "/* get_runProcessor_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_1e7d8fd6cb5a199f (void)) ("
  , "  struct Measurement *arg1,"
  , "  struct Processor *arg2"
  , ")"
  , "{"
  , "  return &runProcessor;"
  , "}"
  , "/* get_processMeasurementWithValidation_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_6621b1bf8ef7af3b (void)) ("
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
  , "void (*hs_bindgen_test_functionscallbacks_c34fd33eedc1490d (void)) ("
  , "  void (*arg1) ("
  , "  foo arg1"
  , ")"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  , "/* get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_490ca7e8c8282a69 (void)) ("
  , "  void (*arg1) ("
  , "  foo2 const arg1"
  , ")"
  , ")"
  , "{"
  , "  return &f2;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_c4b06d89a94616dd" hs_bindgen_test_functionscallbacks_c4b06d89a94616dd_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt))
    )

hs_bindgen_test_functionscallbacks_c4b06d89a94616dd ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt))
hs_bindgen_test_functionscallbacks_c4b06d89a94616dd =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_c4b06d89a94616dd_base

{-# NOINLINE readFileWithProcessor_ptr #-}

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @functions\/callbacks.h:4:5@

    __exported by:__ @functions\/callbacks.h@
-}
readFileWithProcessor_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt)
readFileWithProcessor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_c4b06d89a94616dd

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_22c54726df44b640" hs_bindgen_test_functionscallbacks_22c54726df44b640_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ()))
    )

hs_bindgen_test_functionscallbacks_22c54726df44b640 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ()))
hs_bindgen_test_functionscallbacks_22c54726df44b640 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_22c54726df44b640_base

{-# NOINLINE watchTemperature_ptr #-}

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @functions\/callbacks.h:5:6@

    __exported by:__ @functions\/callbacks.h@
-}
watchTemperature_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ())
watchTemperature_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_22c54726df44b640

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_8167a5b82d621c9d" hs_bindgen_test_functionscallbacks_8167a5b82d621c9d_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (FileOpenedNotification -> IO ()))
    )

hs_bindgen_test_functionscallbacks_8167a5b82d621c9d ::
     IO (Ptr.FunPtr (FileOpenedNotification -> IO ()))
hs_bindgen_test_functionscallbacks_8167a5b82d621c9d =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_8167a5b82d621c9d_base

{-# NOINLINE onFileOpened_ptr #-}

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @functions\/callbacks.h:14:6@

    __exported by:__ @functions\/callbacks.h@
-}
onFileOpened_ptr :: Ptr.FunPtr (FileOpenedNotification -> IO ())
onFileOpened_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_8167a5b82d621c9d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_ef51ad75ce9862a3" hs_bindgen_test_functionscallbacks_ef51ad75ce9862a3_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (ProgressUpdate -> IO ()))
    )

hs_bindgen_test_functionscallbacks_ef51ad75ce9862a3 ::
     IO (Ptr.FunPtr (ProgressUpdate -> IO ()))
hs_bindgen_test_functionscallbacks_ef51ad75ce9862a3 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_ef51ad75ce9862a3_base

{-# NOINLINE onProgressChanged_ptr #-}

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @functions\/callbacks.h:15:6@

    __exported by:__ @functions\/callbacks.h@
-}
onProgressChanged_ptr :: Ptr.FunPtr (ProgressUpdate -> IO ())
onProgressChanged_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_ef51ad75ce9862a3

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_9eaedb1b1c5b3fdb" hs_bindgen_test_functionscallbacks_9eaedb1b1c5b3fdb_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt))
    )

hs_bindgen_test_functionscallbacks_9eaedb1b1c5b3fdb ::
     IO (Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt))
hs_bindgen_test_functionscallbacks_9eaedb1b1c5b3fdb =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_9eaedb1b1c5b3fdb_base

{-# NOINLINE validateInput_ptr #-}

{-| __C declaration:__ @validateInput@

    __defined at:__ @functions\/callbacks.h:16:5@

    __exported by:__ @functions\/callbacks.h@
-}
validateInput_ptr :: Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt)
validateInput_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_9eaedb1b1c5b3fdb

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_f9f4f5ec3dd82431" hs_bindgen_test_functionscallbacks_f9f4f5ec3dd82431_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (MeasurementReceived -> IO ()))
    )

hs_bindgen_test_functionscallbacks_f9f4f5ec3dd82431 ::
     IO (Ptr.FunPtr (MeasurementReceived -> IO ()))
hs_bindgen_test_functionscallbacks_f9f4f5ec3dd82431 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_f9f4f5ec3dd82431_base

{-# NOINLINE onNewMeasurement_ptr #-}

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @functions\/callbacks.h:27:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement_ptr :: Ptr.FunPtr (MeasurementReceived -> IO ())
onNewMeasurement_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_f9f4f5ec3dd82431

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_9c5afeda25ede1ce" hs_bindgen_test_functionscallbacks_9c5afeda25ede1ce_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (MeasurementReceived2 -> IO ()))
    )

hs_bindgen_test_functionscallbacks_9c5afeda25ede1ce ::
     IO (Ptr.FunPtr (MeasurementReceived2 -> IO ()))
hs_bindgen_test_functionscallbacks_9c5afeda25ede1ce =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_9c5afeda25ede1ce_base

{-# NOINLINE onNewMeasurement2_ptr #-}

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @functions\/callbacks.h:30:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement2_ptr :: Ptr.FunPtr (MeasurementReceived2 -> IO ())
onNewMeasurement2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_9c5afeda25ede1ce

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_8091188123328aa8" hs_bindgen_test_functionscallbacks_8091188123328aa8_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (SampleBufferFull -> IO ()))
    )

hs_bindgen_test_functionscallbacks_8091188123328aa8 ::
     IO (Ptr.FunPtr (SampleBufferFull -> IO ()))
hs_bindgen_test_functionscallbacks_8091188123328aa8 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_8091188123328aa8_base

{-# NOINLINE onBufferReady_ptr #-}

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @functions\/callbacks.h:33:6@

    __exported by:__ @functions\/callbacks.h@
-}
onBufferReady_ptr :: Ptr.FunPtr (SampleBufferFull -> IO ())
onBufferReady_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_8091188123328aa8

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_6c9fe4dae03a37fa" hs_bindgen_test_functionscallbacks_6c9fe4dae03a37fa_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())) -> IO ()))
    )

hs_bindgen_test_functionscallbacks_6c9fe4dae03a37fa ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())) -> IO ()))
hs_bindgen_test_functionscallbacks_6c9fe4dae03a37fa =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_6c9fe4dae03a37fa_base

{-# NOINLINE transformMeasurement_ptr #-}

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @functions\/callbacks.h:38:6@

    __exported by:__ @functions\/callbacks.h@
-}
transformMeasurement_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())) -> IO ())
transformMeasurement_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_6c9fe4dae03a37fa

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_2ee8d8889cd31fb7" hs_bindgen_test_functionscallbacks_2ee8d8889cd31fb7_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr ((Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())) -> IO ()))
    )

hs_bindgen_test_functionscallbacks_2ee8d8889cd31fb7 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())) -> IO ()))
hs_bindgen_test_functionscallbacks_2ee8d8889cd31fb7 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_2ee8d8889cd31fb7_base

{-# NOINLINE processWithCallbacks_ptr #-}

{-| __C declaration:__ @processWithCallbacks@

    __defined at:__ @functions\/callbacks.h:43:6@

    __exported by:__ @functions\/callbacks.h@
-}
processWithCallbacks_ptr :: Ptr.FunPtr ((Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())) -> IO ())
processWithCallbacks_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_2ee8d8889cd31fb7

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_5a70e34ecc71835b" hs_bindgen_test_functionscallbacks_5a70e34ecc71835b_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr ((Ptr.Ptr MeasurementHandler) -> IO ()))
    )

hs_bindgen_test_functionscallbacks_5a70e34ecc71835b ::
     IO (Ptr.FunPtr ((Ptr.Ptr MeasurementHandler) -> IO ()))
hs_bindgen_test_functionscallbacks_5a70e34ecc71835b =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_5a70e34ecc71835b_base

{-# NOINLINE registerHandler_ptr #-}

{-| __C declaration:__ @registerHandler@

    __defined at:__ @functions\/callbacks.h:56:6@

    __exported by:__ @functions\/callbacks.h@
-}
registerHandler_ptr :: Ptr.FunPtr ((Ptr.Ptr MeasurementHandler) -> IO ())
registerHandler_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_5a70e34ecc71835b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_1a0881ba01e93710" hs_bindgen_test_functionscallbacks_1a0881ba01e93710_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr DataPipeline) -> IO ()))
    )

hs_bindgen_test_functionscallbacks_1a0881ba01e93710 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr DataPipeline) -> IO ()))
hs_bindgen_test_functionscallbacks_1a0881ba01e93710 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_1a0881ba01e93710_base

{-# NOINLINE executePipeline_ptr #-}

{-| __C declaration:__ @executePipeline@

    __defined at:__ @functions\/callbacks.h:64:6@

    __exported by:__ @functions\/callbacks.h@
-}
executePipeline_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr DataPipeline) -> IO ())
executePipeline_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_1a0881ba01e93710

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_1e7d8fd6cb5a199f" hs_bindgen_test_functionscallbacks_1e7d8fd6cb5a199f_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr Processor) -> IO ()))
    )

hs_bindgen_test_functionscallbacks_1e7d8fd6cb5a199f ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr Processor) -> IO ()))
hs_bindgen_test_functionscallbacks_1e7d8fd6cb5a199f =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_1e7d8fd6cb5a199f_base

{-# NOINLINE runProcessor_ptr #-}

{-| __C declaration:__ @runProcessor@

    __defined at:__ @functions\/callbacks.h:80:6@

    __exported by:__ @functions\/callbacks.h@
-}
runProcessor_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr Processor) -> IO ())
runProcessor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_1e7d8fd6cb5a199f

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_6621b1bf8ef7af3b" hs_bindgen_test_functionscallbacks_6621b1bf8ef7af3b_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())) -> IO ()))
    )

hs_bindgen_test_functionscallbacks_6621b1bf8ef7af3b ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())) -> IO ()))
hs_bindgen_test_functionscallbacks_6621b1bf8ef7af3b =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_6621b1bf8ef7af3b_base

{-# NOINLINE processMeasurementWithValidation_ptr #-}

{-| __C declaration:__ @processMeasurementWithValidation@

    __defined at:__ @functions\/callbacks.h:85:6@

    __exported by:__ @functions\/callbacks.h@
-}
processMeasurementWithValidation_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())) -> IO ())
processMeasurementWithValidation_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_6621b1bf8ef7af3b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_c34fd33eedc1490d" hs_bindgen_test_functionscallbacks_c34fd33eedc1490d_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr ((Ptr.FunPtr (Foo -> IO ())) -> IO ()))
    )

hs_bindgen_test_functionscallbacks_c34fd33eedc1490d ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (Foo -> IO ())) -> IO ()))
hs_bindgen_test_functionscallbacks_c34fd33eedc1490d =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_c34fd33eedc1490d_base

{-# NOINLINE f_ptr #-}

{-| __C declaration:__ @f@

    __defined at:__ @functions\/callbacks.h:96:6@

    __exported by:__ @functions\/callbacks.h@
-}
f_ptr :: Ptr.FunPtr ((Ptr.FunPtr (Foo -> IO ())) -> IO ())
f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_c34fd33eedc1490d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_490ca7e8c8282a69" hs_bindgen_test_functionscallbacks_490ca7e8c8282a69_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr ((Ptr.FunPtr (Foo2 -> IO ())) -> IO ()))
    )

hs_bindgen_test_functionscallbacks_490ca7e8c8282a69 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (Foo2 -> IO ())) -> IO ()))
hs_bindgen_test_functionscallbacks_490ca7e8c8282a69 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionscallbacks_490ca7e8c8282a69_base

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/callbacks.h:97:6@

    __exported by:__ @functions\/callbacks.h@
-}
f2_ptr :: Ptr.FunPtr ((Ptr.FunPtr (Foo2 -> IO ())) -> IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_490ca7e8c8282a69
