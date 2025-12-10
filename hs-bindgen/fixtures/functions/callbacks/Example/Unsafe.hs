{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @functions\/callbacks.h:4:5@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_readFileWithProcessor@
-}
foreign import ccall unsafe "hs_bindgen_d07f3a3e526e7017" readFileWithProcessor ::
     Ptr.FunPtr (FC.CInt -> IO ())
     -- ^ __C declaration:__ @processLine@
  -> FC.CInt
     -- ^ __C declaration:__ @fileId@
  -> IO FC.CInt

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @functions\/callbacks.h:5:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_watchTemperature@
-}
foreign import ccall unsafe "hs_bindgen_cb0219aedd5afed5" watchTemperature ::
     Ptr.FunPtr (FC.CInt -> IO ())
     -- ^ __C declaration:__ @onTempChange@
  -> FC.CInt
     -- ^ __C declaration:__ @sensorId@
  -> IO ()

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @functions\/callbacks.h:14:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_onFileOpened@
-}
foreign import ccall unsafe "hs_bindgen_d96938841a039f9b" onFileOpened ::
     FileOpenedNotification
     -- ^ __C declaration:__ @notify@
  -> IO ()

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @functions\/callbacks.h:15:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_onProgressChanged@
-}
foreign import ccall unsafe "hs_bindgen_3cb24888fc3e1751" onProgressChanged ::
     ProgressUpdate
     -- ^ __C declaration:__ @update@
  -> IO ()

{-| __C declaration:__ @validateInput@

    __defined at:__ @functions\/callbacks.h:16:5@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_validateInput@
-}
foreign import ccall unsafe "hs_bindgen_567ea6dc040b50a1" validateInput ::
     DataValidator
     -- ^ __C declaration:__ @validator@
  -> FC.CInt
     -- ^ __C declaration:__ @rawValue@
  -> IO FC.CInt

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @functions\/callbacks.h:27:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_onNewMeasurement@
-}
foreign import ccall unsafe "hs_bindgen_aab80c08edfa6b4b" onNewMeasurement ::
     MeasurementReceived
     -- ^ __C declaration:__ @handler@
  -> IO ()

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @functions\/callbacks.h:30:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_onNewMeasurement2@
-}
foreign import ccall unsafe "hs_bindgen_6c8fae51df7c46a1" onNewMeasurement2 ::
     MeasurementReceived2
     -- ^ __C declaration:__ @handler@
  -> IO ()

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @functions\/callbacks.h:33:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_onBufferReady@
-}
foreign import ccall unsafe "hs_bindgen_d44afeb36d2ae523" onBufferReady ::
     SampleBufferFull
     -- ^ __C declaration:__ @handler@
  -> IO ()

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @functions\/callbacks.h:38:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_transformMeasurement@
-}
foreign import ccall unsafe "hs_bindgen_523fee13fb646cad" transformMeasurement ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data'@
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
     -- ^ __C declaration:__ @transformer@
  -> IO ()

{-| __C declaration:__ @processWithCallbacks@

    __defined at:__ @functions\/callbacks.h:43:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_processWithCallbacks@
-}
foreign import ccall unsafe "hs_bindgen_98d0c5bd1271eeb7" processWithCallbacks ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
     -- ^ __C declaration:__ @handler@
  -> IO ()

{-| __C declaration:__ @registerHandler@

    __defined at:__ @functions\/callbacks.h:56:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_registerHandler@
-}
foreign import ccall unsafe "hs_bindgen_b96f4d4d7893e301" registerHandler ::
     Ptr.Ptr MeasurementHandler
     -- ^ __C declaration:__ @handler@
  -> IO ()

{-| __C declaration:__ @executePipeline@

    __defined at:__ @functions\/callbacks.h:64:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_executePipeline@
-}
foreign import ccall unsafe "hs_bindgen_c062ded603732aae" executePipeline ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data'@
  -> Ptr.Ptr DataPipeline
     -- ^ __C declaration:__ @pipeline@
  -> IO ()

{-| __C declaration:__ @runProcessor@

    __defined at:__ @functions\/callbacks.h:80:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_runProcessor@
-}
foreign import ccall unsafe "hs_bindgen_02d41a1f48eebff7" runProcessor ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data'@
  -> Ptr.Ptr Processor
     -- ^ __C declaration:__ @processor@
  -> IO ()

{-| __C declaration:__ @processMeasurementWithValidation@

    __defined at:__ @functions\/callbacks.h:85:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_processMeasurementWithValidation@
-}
foreign import ccall unsafe "hs_bindgen_39704c8b14c2ce3c" processMeasurementWithValidation ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data'@
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
     -- ^ __C declaration:__ @processor@
  -> IO ()

{-| __C declaration:__ @f@

    __defined at:__ @functions\/callbacks.h:96:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_f@
-}
foreign import ccall unsafe "hs_bindgen_10c383cdf6eddb0d" f ::
     Ptr.FunPtr (Foo -> IO ())
     -- ^ __C declaration:__ @callback@
  -> IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/callbacks.h:97:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Unsafe_f2@
-}
foreign import ccall unsafe "hs_bindgen_831d03bed0065a4e" f2 ::
     Ptr.FunPtr (Foo2 -> IO ())
     -- ^ __C declaration:__ @handler@
  -> IO ()
