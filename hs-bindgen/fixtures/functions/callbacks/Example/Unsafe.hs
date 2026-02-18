{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/callbacks.h>"
  , "signed int hs_bindgen_d07f3a3e526e7017 ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return readFileWithProcessor(arg1, arg2);"
  , "}"
  , "void hs_bindgen_cb0219aedd5afed5 ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  watchTemperature(arg1, arg2);"
  , "}"
  , "void hs_bindgen_d96938841a039f9b ("
  , "  FileOpenedNotification arg1"
  , ")"
  , "{"
  , "  onFileOpened(arg1);"
  , "}"
  , "void hs_bindgen_3cb24888fc3e1751 ("
  , "  ProgressUpdate arg1"
  , ")"
  , "{"
  , "  onProgressChanged(arg1);"
  , "}"
  , "signed int hs_bindgen_567ea6dc040b50a1 ("
  , "  DataValidator arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return validateInput(arg1, arg2);"
  , "}"
  , "void hs_bindgen_aab80c08edfa6b4b ("
  , "  MeasurementReceived arg1"
  , ")"
  , "{"
  , "  onNewMeasurement(arg1);"
  , "}"
  , "void hs_bindgen_6c8fae51df7c46a1 ("
  , "  MeasurementReceived2 arg1"
  , ")"
  , "{"
  , "  onNewMeasurement2(arg1);"
  , "}"
  , "void hs_bindgen_d44afeb36d2ae523 ("
  , "  SampleBufferFull arg1"
  , ")"
  , "{"
  , "  onBufferReady(arg1);"
  , "}"
  , "void hs_bindgen_523fee13fb646cad ("
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
  , "  transformMeasurement(arg1, arg2);"
  , "}"
  , "void hs_bindgen_98d0c5bd1271eeb7 ("
  , "  void (*arg1) ("
  , "  struct Measurement *arg1,"
  , "  FileOpenedNotification arg2,"
  , "  signed int arg3"
  , ")"
  , ")"
  , "{"
  , "  processWithCallbacks(arg1);"
  , "}"
  , "void hs_bindgen_b96f4d4d7893e301 ("
  , "  struct MeasurementHandler *arg1"
  , ")"
  , "{"
  , "  registerHandler(arg1);"
  , "}"
  , "void hs_bindgen_c062ded603732aae ("
  , "  struct Measurement *arg1,"
  , "  struct DataPipeline *arg2"
  , ")"
  , "{"
  , "  executePipeline(arg1, arg2);"
  , "}"
  , "void hs_bindgen_02d41a1f48eebff7 ("
  , "  struct Measurement *arg1,"
  , "  struct Processor *arg2"
  , ")"
  , "{"
  , "  runProcessor(arg1, arg2);"
  , "}"
  , "void hs_bindgen_39704c8b14c2ce3c ("
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
  , "  processMeasurementWithValidation(arg1, arg2);"
  , "}"
  , "void hs_bindgen_10c383cdf6eddb0d ("
  , "  void (*arg1) ("
  , "  foo arg1"
  , ")"
  , ")"
  , "{"
  , "  f(arg1);"
  , "}"
  , "void hs_bindgen_831d03bed0065a4e ("
  , "  void (*arg1) ("
  , "  foo2 const arg1"
  , ")"
  , ")"
  , "{"
  , "  f2(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionscallbacks_Example_Unsafe_readFileWithProcessor@
foreign import ccall unsafe "hs_bindgen_d07f3a3e526e7017" hs_bindgen_d07f3a3e526e7017_base ::
     RIP.FunPtr RIP.Void
  -> RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_functionscallbacks_Example_Unsafe_readFileWithProcessor@
hs_bindgen_d07f3a3e526e7017 ::
     RIP.FunPtr (RIP.CInt -> IO ())
  -> RIP.CInt
  -> IO RIP.CInt
hs_bindgen_d07f3a3e526e7017 =
  RIP.fromFFIType hs_bindgen_d07f3a3e526e7017_base

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @functions\/callbacks.h 4:5@

    __exported by:__ @functions\/callbacks.h@
-}
readFileWithProcessor ::
     RIP.FunPtr (RIP.CInt -> IO ())
     -- ^ __C declaration:__ @processLine@
  -> RIP.CInt
     -- ^ __C declaration:__ @fileId@
  -> IO RIP.CInt
readFileWithProcessor = hs_bindgen_d07f3a3e526e7017

-- __unique:__ @test_functionscallbacks_Example_Unsafe_watchTemperature@
foreign import ccall unsafe "hs_bindgen_cb0219aedd5afed5" hs_bindgen_cb0219aedd5afed5_base ::
     RIP.FunPtr RIP.Void
  -> RIP.Int32
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_watchTemperature@
hs_bindgen_cb0219aedd5afed5 ::
     RIP.FunPtr (RIP.CInt -> IO ())
  -> RIP.CInt
  -> IO ()
hs_bindgen_cb0219aedd5afed5 =
  RIP.fromFFIType hs_bindgen_cb0219aedd5afed5_base

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @functions\/callbacks.h 5:6@

    __exported by:__ @functions\/callbacks.h@
-}
watchTemperature ::
     RIP.FunPtr (RIP.CInt -> IO ())
     -- ^ __C declaration:__ @onTempChange@
  -> RIP.CInt
     -- ^ __C declaration:__ @sensorId@
  -> IO ()
watchTemperature = hs_bindgen_cb0219aedd5afed5

-- __unique:__ @test_functionscallbacks_Example_Unsafe_onFileOpened@
foreign import ccall unsafe "hs_bindgen_d96938841a039f9b" hs_bindgen_d96938841a039f9b_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_onFileOpened@
hs_bindgen_d96938841a039f9b ::
     FileOpenedNotification
  -> IO ()
hs_bindgen_d96938841a039f9b =
  RIP.fromFFIType hs_bindgen_d96938841a039f9b_base

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @functions\/callbacks.h 14:6@

    __exported by:__ @functions\/callbacks.h@
-}
onFileOpened ::
     FileOpenedNotification
     -- ^ __C declaration:__ @notify@
  -> IO ()
onFileOpened = hs_bindgen_d96938841a039f9b

-- __unique:__ @test_functionscallbacks_Example_Unsafe_onProgressChanged@
foreign import ccall unsafe "hs_bindgen_3cb24888fc3e1751" hs_bindgen_3cb24888fc3e1751_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_onProgressChanged@
hs_bindgen_3cb24888fc3e1751 ::
     ProgressUpdate
  -> IO ()
hs_bindgen_3cb24888fc3e1751 =
  RIP.fromFFIType hs_bindgen_3cb24888fc3e1751_base

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @functions\/callbacks.h 15:6@

    __exported by:__ @functions\/callbacks.h@
-}
onProgressChanged ::
     ProgressUpdate
     -- ^ __C declaration:__ @update@
  -> IO ()
onProgressChanged = hs_bindgen_3cb24888fc3e1751

-- __unique:__ @test_functionscallbacks_Example_Unsafe_validateInput@
foreign import ccall unsafe "hs_bindgen_567ea6dc040b50a1" hs_bindgen_567ea6dc040b50a1_base ::
     RIP.FunPtr RIP.Void
  -> RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_functionscallbacks_Example_Unsafe_validateInput@
hs_bindgen_567ea6dc040b50a1 ::
     DataValidator
  -> RIP.CInt
  -> IO RIP.CInt
hs_bindgen_567ea6dc040b50a1 =
  RIP.fromFFIType hs_bindgen_567ea6dc040b50a1_base

{-| __C declaration:__ @validateInput@

    __defined at:__ @functions\/callbacks.h 16:5@

    __exported by:__ @functions\/callbacks.h@
-}
validateInput ::
     DataValidator
     -- ^ __C declaration:__ @validator@
  -> RIP.CInt
     -- ^ __C declaration:__ @rawValue@
  -> IO RIP.CInt
validateInput = hs_bindgen_567ea6dc040b50a1

-- __unique:__ @test_functionscallbacks_Example_Unsafe_onNewMeasurement@
foreign import ccall unsafe "hs_bindgen_aab80c08edfa6b4b" hs_bindgen_aab80c08edfa6b4b_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_onNewMeasurement@
hs_bindgen_aab80c08edfa6b4b ::
     MeasurementReceived
  -> IO ()
hs_bindgen_aab80c08edfa6b4b =
  RIP.fromFFIType hs_bindgen_aab80c08edfa6b4b_base

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @functions\/callbacks.h 27:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement ::
     MeasurementReceived
     -- ^ __C declaration:__ @handler@
  -> IO ()
onNewMeasurement = hs_bindgen_aab80c08edfa6b4b

-- __unique:__ @test_functionscallbacks_Example_Unsafe_onNewMeasurement2@
foreign import ccall unsafe "hs_bindgen_6c8fae51df7c46a1" hs_bindgen_6c8fae51df7c46a1_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_onNewMeasurement2@
hs_bindgen_6c8fae51df7c46a1 ::
     MeasurementReceived2
  -> IO ()
hs_bindgen_6c8fae51df7c46a1 =
  RIP.fromFFIType hs_bindgen_6c8fae51df7c46a1_base

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @functions\/callbacks.h 30:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement2 ::
     MeasurementReceived2
     -- ^ __C declaration:__ @handler@
  -> IO ()
onNewMeasurement2 = hs_bindgen_6c8fae51df7c46a1

-- __unique:__ @test_functionscallbacks_Example_Unsafe_onBufferReady@
foreign import ccall unsafe "hs_bindgen_d44afeb36d2ae523" hs_bindgen_d44afeb36d2ae523_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_onBufferReady@
hs_bindgen_d44afeb36d2ae523 ::
     SampleBufferFull
  -> IO ()
hs_bindgen_d44afeb36d2ae523 =
  RIP.fromFFIType hs_bindgen_d44afeb36d2ae523_base

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @functions\/callbacks.h 33:6@

    __exported by:__ @functions\/callbacks.h@
-}
onBufferReady ::
     SampleBufferFull
     -- ^ __C declaration:__ @handler@
  -> IO ()
onBufferReady = hs_bindgen_d44afeb36d2ae523

-- __unique:__ @test_functionscallbacks_Example_Unsafe_transformMeasurement@
foreign import ccall unsafe "hs_bindgen_523fee13fb646cad" hs_bindgen_523fee13fb646cad_base ::
     RIP.Ptr RIP.Void
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_transformMeasurement@
hs_bindgen_523fee13fb646cad ::
     RIP.Ptr Measurement
  -> RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ())
  -> IO ()
hs_bindgen_523fee13fb646cad =
  RIP.fromFFIType hs_bindgen_523fee13fb646cad_base

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @functions\/callbacks.h 38:6@

    __exported by:__ @functions\/callbacks.h@
-}
transformMeasurement ::
     RIP.Ptr Measurement
     -- ^ __C declaration:__ @data@
  -> RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ())
     -- ^ __C declaration:__ @transformer@
  -> IO ()
transformMeasurement = hs_bindgen_523fee13fb646cad

-- __unique:__ @test_functionscallbacks_Example_Unsafe_processWithCallbacks@
foreign import ccall unsafe "hs_bindgen_98d0c5bd1271eeb7" hs_bindgen_98d0c5bd1271eeb7_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_processWithCallbacks@
hs_bindgen_98d0c5bd1271eeb7 ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ())
  -> IO ()
hs_bindgen_98d0c5bd1271eeb7 =
  RIP.fromFFIType hs_bindgen_98d0c5bd1271eeb7_base

{-| __C declaration:__ @processWithCallbacks@

    __defined at:__ @functions\/callbacks.h 43:6@

    __exported by:__ @functions\/callbacks.h@
-}
processWithCallbacks ::
     RIP.FunPtr ((RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ())
     -- ^ __C declaration:__ @handler@
  -> IO ()
processWithCallbacks = hs_bindgen_98d0c5bd1271eeb7

-- __unique:__ @test_functionscallbacks_Example_Unsafe_registerHandler@
foreign import ccall unsafe "hs_bindgen_b96f4d4d7893e301" hs_bindgen_b96f4d4d7893e301_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_registerHandler@
hs_bindgen_b96f4d4d7893e301 ::
     RIP.Ptr MeasurementHandler
  -> IO ()
hs_bindgen_b96f4d4d7893e301 =
  RIP.fromFFIType hs_bindgen_b96f4d4d7893e301_base

{-| __C declaration:__ @registerHandler@

    __defined at:__ @functions\/callbacks.h 56:6@

    __exported by:__ @functions\/callbacks.h@
-}
registerHandler ::
     RIP.Ptr MeasurementHandler
     -- ^ __C declaration:__ @handler@
  -> IO ()
registerHandler = hs_bindgen_b96f4d4d7893e301

-- __unique:__ @test_functionscallbacks_Example_Unsafe_executePipeline@
foreign import ccall unsafe "hs_bindgen_c062ded603732aae" hs_bindgen_c062ded603732aae_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_executePipeline@
hs_bindgen_c062ded603732aae ::
     RIP.Ptr Measurement
  -> RIP.Ptr DataPipeline
  -> IO ()
hs_bindgen_c062ded603732aae =
  RIP.fromFFIType hs_bindgen_c062ded603732aae_base

{-| __C declaration:__ @executePipeline@

    __defined at:__ @functions\/callbacks.h 64:6@

    __exported by:__ @functions\/callbacks.h@
-}
executePipeline ::
     RIP.Ptr Measurement
     -- ^ __C declaration:__ @data@
  -> RIP.Ptr DataPipeline
     -- ^ __C declaration:__ @pipeline@
  -> IO ()
executePipeline = hs_bindgen_c062ded603732aae

-- __unique:__ @test_functionscallbacks_Example_Unsafe_runProcessor@
foreign import ccall unsafe "hs_bindgen_02d41a1f48eebff7" hs_bindgen_02d41a1f48eebff7_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_runProcessor@
hs_bindgen_02d41a1f48eebff7 ::
     RIP.Ptr Measurement
  -> RIP.Ptr Processor
  -> IO ()
hs_bindgen_02d41a1f48eebff7 =
  RIP.fromFFIType hs_bindgen_02d41a1f48eebff7_base

{-| __C declaration:__ @runProcessor@

    __defined at:__ @functions\/callbacks.h 80:6@

    __exported by:__ @functions\/callbacks.h@
-}
runProcessor ::
     RIP.Ptr Measurement
     -- ^ __C declaration:__ @data@
  -> RIP.Ptr Processor
     -- ^ __C declaration:__ @processor@
  -> IO ()
runProcessor = hs_bindgen_02d41a1f48eebff7

-- __unique:__ @test_functionscallbacks_Example_Unsafe_processMeasurementWithValidation@
foreign import ccall unsafe "hs_bindgen_39704c8b14c2ce3c" hs_bindgen_39704c8b14c2ce3c_base ::
     RIP.Ptr RIP.Void
  -> RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_processMeasurementWithValidation@
hs_bindgen_39704c8b14c2ce3c ::
     RIP.Ptr Measurement
  -> RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ())
  -> IO ()
hs_bindgen_39704c8b14c2ce3c =
  RIP.fromFFIType hs_bindgen_39704c8b14c2ce3c_base

{-| __C declaration:__ @processMeasurementWithValidation@

    __defined at:__ @functions\/callbacks.h 85:6@

    __exported by:__ @functions\/callbacks.h@
-}
processMeasurementWithValidation ::
     RIP.Ptr Measurement
     -- ^ __C declaration:__ @data@
  -> RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ())
     -- ^ __C declaration:__ @processor@
  -> IO ()
processMeasurementWithValidation =
  hs_bindgen_39704c8b14c2ce3c

-- __unique:__ @test_functionscallbacks_Example_Unsafe_f@
foreign import ccall unsafe "hs_bindgen_10c383cdf6eddb0d" hs_bindgen_10c383cdf6eddb0d_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_f@
hs_bindgen_10c383cdf6eddb0d ::
     RIP.FunPtr (Foo -> IO ())
  -> IO ()
hs_bindgen_10c383cdf6eddb0d =
  RIP.fromFFIType hs_bindgen_10c383cdf6eddb0d_base

{-| __C declaration:__ @f@

    __defined at:__ @functions\/callbacks.h 96:6@

    __exported by:__ @functions\/callbacks.h@
-}
f ::
     RIP.FunPtr (Foo -> IO ())
     -- ^ __C declaration:__ @callback@
  -> IO ()
f = hs_bindgen_10c383cdf6eddb0d

-- __unique:__ @test_functionscallbacks_Example_Unsafe_f2@
foreign import ccall unsafe "hs_bindgen_831d03bed0065a4e" hs_bindgen_831d03bed0065a4e_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Unsafe_f2@
hs_bindgen_831d03bed0065a4e ::
     RIP.FunPtr (Foo2 -> IO ())
  -> IO ()
hs_bindgen_831d03bed0065a4e =
  RIP.fromFFIType hs_bindgen_831d03bed0065a4e_base

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/callbacks.h 97:6@

    __exported by:__ @functions\/callbacks.h@
-}
f2 ::
     RIP.FunPtr (Foo2 -> IO ())
     -- ^ __C declaration:__ @handler@
  -> IO ()
f2 = hs_bindgen_831d03bed0065a4e
