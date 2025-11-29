{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/callbacks.h>"
  , "signed int hs_bindgen_test_functionscallbacks_a0a59181c714c131 ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return readFileWithProcessor(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_d59e6698796971ea ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  watchTemperature(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_c9fb8fdc3d0d3978 ("
  , "  FileOpenedNotification arg1"
  , ")"
  , "{"
  , "  onFileOpened(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_7921ad1b219190e4 ("
  , "  ProgressUpdate arg1"
  , ")"
  , "{"
  , "  onProgressChanged(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionscallbacks_ae19d658f098584a ("
  , "  DataValidator arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return validateInput(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_d2fdffe85523b3ef ("
  , "  MeasurementReceived arg1"
  , ")"
  , "{"
  , "  onNewMeasurement(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_c5b555bbc07b808d ("
  , "  MeasurementReceived2 arg1"
  , ")"
  , "{"
  , "  onNewMeasurement2(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_65927c77229ad893 ("
  , "  SampleBufferFull arg1"
  , ")"
  , "{"
  , "  onBufferReady(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_0b6a9249f49b986f ("
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
  , "void hs_bindgen_test_functionscallbacks_2c3e0e84ae9cde51 ("
  , "  void (*arg1) ("
  , "  struct Measurement *arg1,"
  , "  FileOpenedNotification arg2,"
  , "  signed int arg3"
  , ")"
  , ")"
  , "{"
  , "  processWithCallbacks(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_0b172585709f9d48 ("
  , "  struct MeasurementHandler *arg1"
  , ")"
  , "{"
  , "  registerHandler(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_25a56dfc7b259e7d ("
  , "  struct Measurement *arg1,"
  , "  struct DataPipeline *arg2"
  , ")"
  , "{"
  , "  executePipeline(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_5908d37641d70953 ("
  , "  struct Measurement *arg1,"
  , "  struct Processor *arg2"
  , ")"
  , "{"
  , "  runProcessor(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_f3c99b4af7808e7f ("
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
  , "void hs_bindgen_test_functionscallbacks_fcce70013c76ce8b ("
  , "  void (*arg1) ("
  , "  foo arg1"
  , ")"
  , ")"
  , "{"
  , "  f(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_1d043de05a457e90 ("
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

    __unique:__ @ExampleJust SafereadFileWithProcessor@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_a0a59181c714c131" readFileWithProcessor ::
     Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @processLine@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @fileId@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @functions\/callbacks.h:5:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SafewatchTemperature@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_d59e6698796971ea" watchTemperature ::
     Ptr.FunPtr (FC.CInt -> IO ())
     {- ^ __C declaration:__ @onTempChange@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @sensorId@
     -}
  -> IO ()

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @functions\/callbacks.h:14:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SafeonFileOpened@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_c9fb8fdc3d0d3978" onFileOpened ::
     FileOpenedNotification
     {- ^ __C declaration:__ @notify@
     -}
  -> IO ()

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @functions\/callbacks.h:15:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SafeonProgressChanged@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_7921ad1b219190e4" onProgressChanged ::
     ProgressUpdate
     {- ^ __C declaration:__ @update@
     -}
  -> IO ()

{-| __C declaration:__ @validateInput@

    __defined at:__ @functions\/callbacks.h:16:5@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SafevalidateInput@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_ae19d658f098584a" validateInput ::
     DataValidator
     {- ^ __C declaration:__ @validator@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @rawValue@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @functions\/callbacks.h:27:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SafeonNewMeasurement@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_d2fdffe85523b3ef" onNewMeasurement ::
     MeasurementReceived
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @functions\/callbacks.h:30:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SafeonNewMeasurement2@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_c5b555bbc07b808d" onNewMeasurement2 ::
     MeasurementReceived2
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @functions\/callbacks.h:33:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SafeonBufferReady@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_65927c77229ad893" onBufferReady ::
     SampleBufferFull
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @functions\/callbacks.h:38:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SafetransformMeasurement@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_0b6a9249f49b986f" transformMeasurement ::
     Ptr.Ptr Measurement
     {- ^ __C declaration:__ @data'@
     -}
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
     {- ^ __C declaration:__ @transformer@
     -}
  -> IO ()

{-| __C declaration:__ @processWithCallbacks@

    __defined at:__ @functions\/callbacks.h:43:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SafeprocessWithCallbacks@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_2c3e0e84ae9cde51" processWithCallbacks ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @registerHandler@

    __defined at:__ @functions\/callbacks.h:56:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SaferegisterHandler@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_0b172585709f9d48" registerHandler ::
     Ptr.Ptr MeasurementHandler
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @executePipeline@

    __defined at:__ @functions\/callbacks.h:64:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SafeexecutePipeline@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_25a56dfc7b259e7d" executePipeline ::
     Ptr.Ptr Measurement
     {- ^ __C declaration:__ @data'@
     -}
  -> Ptr.Ptr DataPipeline
     {- ^ __C declaration:__ @pipeline@
     -}
  -> IO ()

{-| __C declaration:__ @runProcessor@

    __defined at:__ @functions\/callbacks.h:80:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SaferunProcessor@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_5908d37641d70953" runProcessor ::
     Ptr.Ptr Measurement
     {- ^ __C declaration:__ @data'@
     -}
  -> Ptr.Ptr Processor
     {- ^ __C declaration:__ @processor@
     -}
  -> IO ()

{-| __C declaration:__ @processMeasurementWithValidation@

    __defined at:__ @functions\/callbacks.h:85:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust SafeprocessMeasurementWithValidation@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_f3c99b4af7808e7f" processMeasurementWithValidation ::
     Ptr.Ptr Measurement
     {- ^ __C declaration:__ @data'@
     -}
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
     {- ^ __C declaration:__ @processor@
     -}
  -> IO ()

{-| __C declaration:__ @f@

    __defined at:__ @functions\/callbacks.h:96:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust Safef@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_fcce70013c76ce8b" f ::
     Ptr.FunPtr (Foo -> IO ())
     {- ^ __C declaration:__ @callback@
     -}
  -> IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/callbacks.h:97:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @ExampleJust Safef2@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_1d043de05a457e90" f2 ::
     Ptr.FunPtr (Foo2 -> IO ())
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()
