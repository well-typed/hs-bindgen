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
  [ "#include <functions/callbacks.h>"
  , "/* Example_get_readFileWithProcessor_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionscallbacks_ed51526023e5367b (void)) ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &readFileWithProcessor;"
  , "}"
  , "/* Example_get_watchTemperature_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_ebca68a54afea5f2 (void)) ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &watchTemperature;"
  , "}"
  , "/* Example_get_onFileOpened_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_85af38f9cdcf654a (void)) ("
  , "  FileOpenedNotification arg1"
  , ")"
  , "{"
  , "  return &onFileOpened;"
  , "}"
  , "/* Example_get_onProgressChanged_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_3121ac2111208ecb (void)) ("
  , "  ProgressUpdate arg1"
  , ")"
  , "{"
  , "  return &onProgressChanged;"
  , "}"
  , "/* Example_get_validateInput_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionscallbacks_ff4c641e9e0a2050 (void)) ("
  , "  DataValidator arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &validateInput;"
  , "}"
  , "/* Example_get_onNewMeasurement_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_761699de313bf4cd (void)) ("
  , "  MeasurementReceived arg1"
  , ")"
  , "{"
  , "  return &onNewMeasurement;"
  , "}"
  , "/* Example_get_onNewMeasurement2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_b8c34b285b7ff16e (void)) ("
  , "  MeasurementReceived2 arg1"
  , ")"
  , "{"
  , "  return &onNewMeasurement2;"
  , "}"
  , "/* Example_get_onBufferReady_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_4ee8e1383e64a5e6 (void)) ("
  , "  SampleBufferFull arg1"
  , ")"
  , "{"
  , "  return &onBufferReady;"
  , "}"
  , "/* Example_get_transformMeasurement_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_73b889e4da817277 (void)) ("
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
  , "/* Example_get_processWithCallbacks_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_2634ebb1a0a580a3 (void)) ("
  , "  void (*arg1) ("
  , "  struct Measurement *arg1,"
  , "  FileOpenedNotification arg2,"
  , "  signed int arg3"
  , ")"
  , ")"
  , "{"
  , "  return &processWithCallbacks;"
  , "}"
  , "/* Example_get_registerHandler_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_305c60d9d549e1e0 (void)) ("
  , "  struct MeasurementHandler *arg1"
  , ")"
  , "{"
  , "  return &registerHandler;"
  , "}"
  , "/* Example_get_executePipeline_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_23aa060e2b36809b (void)) ("
  , "  struct Measurement *arg1,"
  , "  struct DataPipeline *arg2"
  , ")"
  , "{"
  , "  return &executePipeline;"
  , "}"
  , "/* Example_get_runProcessor_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_c4ba3c375fad5cad (void)) ("
  , "  struct Measurement *arg1,"
  , "  struct Processor *arg2"
  , ")"
  , "{"
  , "  return &runProcessor;"
  , "}"
  , "/* Example_get_processMeasurementWithValidation_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_e22a923b06dcd00a (void)) ("
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
  , "/* Example_get_f_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_e684d4bc6355e13a (void)) ("
  , "  void (*arg1) ("
  , "  foo arg1"
  , ")"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  , "/* Example_get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionscallbacks_66763a4fad90fe22 (void)) ("
  , "  void (*arg1) ("
  , "  foo2 const arg1"
  , ")"
  , ")"
  , "{"
  , "  return &f2;"
  , "}"
  ]))

{-| __unique:__ @Example_get_readFileWithProcessor_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_ed51526023e5367b" hs_bindgen_test_functionscallbacks_ed51526023e5367b ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE readFileWithProcessor_ptr #-}

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @functions\/callbacks.h:4:5@

    __exported by:__ @functions\/callbacks.h@
-}
readFileWithProcessor_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt)
readFileWithProcessor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_ed51526023e5367b

{-| __unique:__ @Example_get_watchTemperature_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_ebca68a54afea5f2" hs_bindgen_test_functionscallbacks_ebca68a54afea5f2 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ()))

{-# NOINLINE watchTemperature_ptr #-}

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @functions\/callbacks.h:5:6@

    __exported by:__ @functions\/callbacks.h@
-}
watchTemperature_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ())
watchTemperature_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_ebca68a54afea5f2

{-| __unique:__ @Example_get_onFileOpened_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_85af38f9cdcf654a" hs_bindgen_test_functionscallbacks_85af38f9cdcf654a ::
     IO (Ptr.FunPtr (FileOpenedNotification -> IO ()))

{-# NOINLINE onFileOpened_ptr #-}

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @functions\/callbacks.h:14:6@

    __exported by:__ @functions\/callbacks.h@
-}
onFileOpened_ptr :: Ptr.FunPtr (FileOpenedNotification -> IO ())
onFileOpened_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_85af38f9cdcf654a

{-| __unique:__ @Example_get_onProgressChanged_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_3121ac2111208ecb" hs_bindgen_test_functionscallbacks_3121ac2111208ecb ::
     IO (Ptr.FunPtr (ProgressUpdate -> IO ()))

{-# NOINLINE onProgressChanged_ptr #-}

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @functions\/callbacks.h:15:6@

    __exported by:__ @functions\/callbacks.h@
-}
onProgressChanged_ptr :: Ptr.FunPtr (ProgressUpdate -> IO ())
onProgressChanged_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_3121ac2111208ecb

{-| __unique:__ @Example_get_validateInput_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_ff4c641e9e0a2050" hs_bindgen_test_functionscallbacks_ff4c641e9e0a2050 ::
     IO (Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt))

{-# NOINLINE validateInput_ptr #-}

{-| __C declaration:__ @validateInput@

    __defined at:__ @functions\/callbacks.h:16:5@

    __exported by:__ @functions\/callbacks.h@
-}
validateInput_ptr :: Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt)
validateInput_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_ff4c641e9e0a2050

{-| __unique:__ @Example_get_onNewMeasurement_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_761699de313bf4cd" hs_bindgen_test_functionscallbacks_761699de313bf4cd ::
     IO (Ptr.FunPtr (MeasurementReceived -> IO ()))

{-# NOINLINE onNewMeasurement_ptr #-}

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @functions\/callbacks.h:27:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement_ptr :: Ptr.FunPtr (MeasurementReceived -> IO ())
onNewMeasurement_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_761699de313bf4cd

{-| __unique:__ @Example_get_onNewMeasurement2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_b8c34b285b7ff16e" hs_bindgen_test_functionscallbacks_b8c34b285b7ff16e ::
     IO (Ptr.FunPtr (MeasurementReceived2 -> IO ()))

{-# NOINLINE onNewMeasurement2_ptr #-}

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @functions\/callbacks.h:30:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement2_ptr :: Ptr.FunPtr (MeasurementReceived2 -> IO ())
onNewMeasurement2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_b8c34b285b7ff16e

{-| __unique:__ @Example_get_onBufferReady_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_4ee8e1383e64a5e6" hs_bindgen_test_functionscallbacks_4ee8e1383e64a5e6 ::
     IO (Ptr.FunPtr (SampleBufferFull -> IO ()))

{-# NOINLINE onBufferReady_ptr #-}

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @functions\/callbacks.h:33:6@

    __exported by:__ @functions\/callbacks.h@
-}
onBufferReady_ptr :: Ptr.FunPtr (SampleBufferFull -> IO ())
onBufferReady_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_4ee8e1383e64a5e6

{-| __unique:__ @Example_get_transformMeasurement_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_73b889e4da817277" hs_bindgen_test_functionscallbacks_73b889e4da817277 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())) -> IO ()))

{-# NOINLINE transformMeasurement_ptr #-}

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @functions\/callbacks.h:38:6@

    __exported by:__ @functions\/callbacks.h@
-}
transformMeasurement_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())) -> IO ())
transformMeasurement_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_73b889e4da817277

{-| __unique:__ @Example_get_processWithCallbacks_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_2634ebb1a0a580a3" hs_bindgen_test_functionscallbacks_2634ebb1a0a580a3 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())) -> IO ()))

{-# NOINLINE processWithCallbacks_ptr #-}

{-| __C declaration:__ @processWithCallbacks@

    __defined at:__ @functions\/callbacks.h:43:6@

    __exported by:__ @functions\/callbacks.h@
-}
processWithCallbacks_ptr :: Ptr.FunPtr ((Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())) -> IO ())
processWithCallbacks_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_2634ebb1a0a580a3

{-| __unique:__ @Example_get_registerHandler_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_305c60d9d549e1e0" hs_bindgen_test_functionscallbacks_305c60d9d549e1e0 ::
     IO (Ptr.FunPtr ((Ptr.Ptr MeasurementHandler) -> IO ()))

{-# NOINLINE registerHandler_ptr #-}

{-| __C declaration:__ @registerHandler@

    __defined at:__ @functions\/callbacks.h:56:6@

    __exported by:__ @functions\/callbacks.h@
-}
registerHandler_ptr :: Ptr.FunPtr ((Ptr.Ptr MeasurementHandler) -> IO ())
registerHandler_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_305c60d9d549e1e0

{-| __unique:__ @Example_get_executePipeline_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_23aa060e2b36809b" hs_bindgen_test_functionscallbacks_23aa060e2b36809b ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr DataPipeline) -> IO ()))

{-# NOINLINE executePipeline_ptr #-}

{-| __C declaration:__ @executePipeline@

    __defined at:__ @functions\/callbacks.h:64:6@

    __exported by:__ @functions\/callbacks.h@
-}
executePipeline_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr DataPipeline) -> IO ())
executePipeline_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_23aa060e2b36809b

{-| __unique:__ @Example_get_runProcessor_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_c4ba3c375fad5cad" hs_bindgen_test_functionscallbacks_c4ba3c375fad5cad ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr Processor) -> IO ()))

{-# NOINLINE runProcessor_ptr #-}

{-| __C declaration:__ @runProcessor@

    __defined at:__ @functions\/callbacks.h:80:6@

    __exported by:__ @functions\/callbacks.h@
-}
runProcessor_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr Processor) -> IO ())
runProcessor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_c4ba3c375fad5cad

{-| __unique:__ @Example_get_processMeasurementWithValidation_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_e22a923b06dcd00a" hs_bindgen_test_functionscallbacks_e22a923b06dcd00a ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())) -> IO ()))

{-# NOINLINE processMeasurementWithValidation_ptr #-}

{-| __C declaration:__ @processMeasurementWithValidation@

    __defined at:__ @functions\/callbacks.h:85:6@

    __exported by:__ @functions\/callbacks.h@
-}
processMeasurementWithValidation_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())) -> IO ())
processMeasurementWithValidation_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_e22a923b06dcd00a

{-| __unique:__ @Example_get_f_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_e684d4bc6355e13a" hs_bindgen_test_functionscallbacks_e684d4bc6355e13a ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (Foo -> IO ())) -> IO ()))

{-# NOINLINE f_ptr #-}

{-| __C declaration:__ @f@

    __defined at:__ @functions\/callbacks.h:96:6@

    __exported by:__ @functions\/callbacks.h@
-}
f_ptr :: Ptr.FunPtr ((Ptr.FunPtr (Foo -> IO ())) -> IO ())
f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_e684d4bc6355e13a

{-| __unique:__ @Example_get_f2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_66763a4fad90fe22" hs_bindgen_test_functionscallbacks_66763a4fad90fe22 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (Foo2 -> IO ())) -> IO ()))

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/callbacks.h:97:6@

    __exported by:__ @functions\/callbacks.h@
-}
f2_ptr :: Ptr.FunPtr ((Ptr.FunPtr (Foo2 -> IO ())) -> IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionscallbacks_66763a4fad90fe22
