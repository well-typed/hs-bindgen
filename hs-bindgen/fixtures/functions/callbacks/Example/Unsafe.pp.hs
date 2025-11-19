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
  , "signed int hs_bindgen_test_functionscallbacks_f0d72410d79899b5 ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return readFileWithProcessor(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_a445b9cacb08ed71 ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  watchTemperature(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_b71e59965bcc2316 ("
  , "  FileOpenedNotification arg1"
  , ")"
  , "{"
  , "  onFileOpened(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_013e79fc3cd3b1b4 ("
  , "  ProgressUpdate arg1"
  , ")"
  , "{"
  , "  onProgressChanged(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionscallbacks_697a7b01b3d64c58 ("
  , "  DataValidator arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return validateInput(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_f291b861b36d5a90 ("
  , "  MeasurementReceived arg1"
  , ")"
  , "{"
  , "  onNewMeasurement(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_4f36523b7d965e44 ("
  , "  MeasurementReceived2 arg1"
  , ")"
  , "{"
  , "  onNewMeasurement2(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_92d54aaf9e8a1c8e ("
  , "  SampleBufferFull arg1"
  , ")"
  , "{"
  , "  onBufferReady(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_0e22183e51a42eab ("
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
  , "void hs_bindgen_test_functionscallbacks_9b4727ea289ff135 ("
  , "  void (*arg1) ("
  , "  struct Measurement *arg1,"
  , "  FileOpenedNotification arg2,"
  , "  signed int arg3"
  , ")"
  , ")"
  , "{"
  , "  processWithCallbacks(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_aea76777f06b51ce ("
  , "  struct MeasurementHandler *arg1"
  , ")"
  , "{"
  , "  registerHandler(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_54fc81d3b44b84d5 ("
  , "  struct Measurement *arg1,"
  , "  struct DataPipeline *arg2"
  , ")"
  , "{"
  , "  executePipeline(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_4bb32ee774218053 ("
  , "  struct Measurement *arg1,"
  , "  struct Processor *arg2"
  , ")"
  , "{"
  , "  runProcessor(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_f453b618c9ab0234 ("
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
  , "void hs_bindgen_test_functionscallbacks_b68b2cffacc97c1d ("
  , "  void (*arg1) ("
  , "  foo arg1"
  , ")"
  , ")"
  , "{"
  , "  f(arg1);"
  , "}"
  , "void hs_bindgen_test_functionscallbacks_14361e995fb5684a ("
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
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_f0d72410d79899b5" readFileWithProcessor ::
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
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_a445b9cacb08ed71" watchTemperature ::
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
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_b71e59965bcc2316" onFileOpened ::
     FileOpenedNotification
     {- ^ __C declaration:__ @notify@
     -}
  -> IO ()

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @functions\/callbacks.h:15:6@

    __exported by:__ @functions\/callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_013e79fc3cd3b1b4" onProgressChanged ::
     ProgressUpdate
     {- ^ __C declaration:__ @update@
     -}
  -> IO ()

{-| __C declaration:__ @validateInput@

    __defined at:__ @functions\/callbacks.h:16:5@

    __exported by:__ @functions\/callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_697a7b01b3d64c58" validateInput ::
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
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_f291b861b36d5a90" onNewMeasurement ::
     MeasurementReceived
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @functions\/callbacks.h:30:6@

    __exported by:__ @functions\/callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_4f36523b7d965e44" onNewMeasurement2 ::
     MeasurementReceived2
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @functions\/callbacks.h:33:6@

    __exported by:__ @functions\/callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_92d54aaf9e8a1c8e" onBufferReady ::
     SampleBufferFull
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @functions\/callbacks.h:38:6@

    __exported by:__ @functions\/callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_0e22183e51a42eab" transformMeasurement ::
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
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_9b4727ea289ff135" processWithCallbacks ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @registerHandler@

    __defined at:__ @functions\/callbacks.h:56:6@

    __exported by:__ @functions\/callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_aea76777f06b51ce" registerHandler ::
     Ptr.Ptr MeasurementHandler
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()

{-| __C declaration:__ @executePipeline@

    __defined at:__ @functions\/callbacks.h:64:6@

    __exported by:__ @functions\/callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_54fc81d3b44b84d5" executePipeline ::
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
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_4bb32ee774218053" runProcessor ::
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
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_f453b618c9ab0234" processMeasurementWithValidation ::
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
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_b68b2cffacc97c1d" f ::
     Ptr.FunPtr (Foo -> IO ())
     {- ^ __C declaration:__ @callback@
     -}
  -> IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/callbacks.h:97:6@

    __exported by:__ @functions\/callbacks.h@
-}
foreign import ccall unsafe "hs_bindgen_test_functionscallbacks_14361e995fb5684a" f2 ::
     Ptr.FunPtr (Foo2 -> IO ())
     {- ^ __C declaration:__ @handler@
     -}
  -> IO ()
