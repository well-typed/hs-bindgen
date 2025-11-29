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
  , "signed int hs_bindgen_test_functionscallbacks_884542d6013adb09 ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return readFileWithProcessor(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_d19122a26f131513 ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  watchTemperature(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_b6dcfbef4460b7d5 ("
  , "  FileOpenedNotification arg1"
  , ")"
  , "{"
  , "  onFileOpened(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_00cc60fb4c69a48f ("
  , "  ProgressUpdate arg1"
  , ")"
  , "{"
  , "  onProgressChanged(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionscallbacks_a4214e9021ffc15c ("
  , "  DataValidator arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return validateInput(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_04007efa2889fc16 ("
  , "  MeasurementReceived arg1"
  , ")"
  , "{"
  , "  onNewMeasurement(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_1d05ae35a802c6bc ("
  , "  MeasurementReceived2 arg1"
  , ")"
  , "{"
  , "  onNewMeasurement2(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_c54bc46fac50933d ("
  , "  SampleBufferFull arg1"
  , ")"
  , "{"
  , "  onBufferReady(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_fed320ad2dc0ab29 ("
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
  , "void hs_bindgen_test_functionscallbacks_ca13dd5fef3fd5e1 ("
  , "  void (*arg1) ("
  , "  struct Measurement *arg1,"
  , "  FileOpenedNotification arg2,"
  , "  signed int arg3"
  , ")"
  , ")"
  , "{"
  , "  processWithCallbacks(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_df72369d20d7f2c1 ("
  , "  struct MeasurementHandler *arg1"
  , ")"
  , "{"
  , "  registerHandler(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_10f2941c7bf55e6e ("
  , "  struct Measurement *arg1,"
  , "  struct DataPipeline *arg2"
  , ")"
  , "{"
  , "  executePipeline(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_a3b112c943858ab1 ("
  , "  struct Measurement *arg1,"
  , "  struct Processor *arg2"
  , ")"
  , "{"
  , "  runProcessor(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_4b7bbc5e66e9470f ("
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
  , "void hs_bindgen_test_functionscallbacks_59d88239da8fef6b ("
  , "  void (*arg1) ("
  , "  foo arg1"
  , ")"
  , ")"
  , "{"
  , "  f(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_a58b72650fa977ee ("
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

    __unique:__ @Example_Safe_readFileWithProcessor@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_884542d6013adb09" readFileWithProcessor ::
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

    __unique:__ @Example_Safe_watchTemperature@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_d19122a26f131513" watchTemperature ::
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

    __unique:__ @Example_Safe_onFileOpened@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_b6dcfbef4460b7d5" onFileOpened ::
     FileOpenedNotification
     {- ^ __C declaration:__ @notify@
     -}
  -> IO ()

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @functions\/callbacks.h:15:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @Example_Safe_onProgressChanged@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_00cc60fb4c69a48f" onProgressChanged ::
     ProgressUpdate
     {- ^ __C declaration:__ @update@
     -}
  -> IO ()

{-| __C declaration:__ @validateInput@

    __defined at:__ @functions\/callbacks.h:16:5@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @Example_Safe_validateInput@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_a4214e9021ffc15c" validateInput ::
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

    __unique:__ @Example_Safe_onNewMeasurement@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_04007efa2889fc16" onNewMeasurement ::
     MeasurementReceived
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @functions\/callbacks.h:30:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @Example_Safe_onNewMeasurement2@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_1d05ae35a802c6bc" onNewMeasurement2 ::
     MeasurementReceived2
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @functions\/callbacks.h:33:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @Example_Safe_onBufferReady@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_c54bc46fac50933d" onBufferReady ::
     SampleBufferFull
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @functions\/callbacks.h:38:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @Example_Safe_transformMeasurement@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_fed320ad2dc0ab29" transformMeasurement ::
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

    __unique:__ @Example_Safe_processWithCallbacks@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_ca13dd5fef3fd5e1" processWithCallbacks ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @registerHandler@

    __defined at:__ @functions\/callbacks.h:56:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @Example_Safe_registerHandler@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_df72369d20d7f2c1" registerHandler ::
     Ptr.Ptr MeasurementHandler
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @executePipeline@

    __defined at:__ @functions\/callbacks.h:64:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @Example_Safe_executePipeline@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_10f2941c7bf55e6e" executePipeline ::
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

    __unique:__ @Example_Safe_runProcessor@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_a3b112c943858ab1" runProcessor ::
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

    __unique:__ @Example_Safe_processMeasurementWithValidation@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_4b7bbc5e66e9470f" processMeasurementWithValidation ::
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

    __unique:__ @Example_Safe_f@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_59d88239da8fef6b" f ::
     Ptr.FunPtr (Foo -> IO ())
     {- ^ __C declaration:__ @callback@
     -}
  -> IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/callbacks.h:97:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @Example_Safe_f2@
-}
foreign import ccall safe "hs_bindgen_test_functionscallbacks_a58b72650fa977ee" f2 ::
     Ptr.FunPtr (Foo2 -> IO ())
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()
