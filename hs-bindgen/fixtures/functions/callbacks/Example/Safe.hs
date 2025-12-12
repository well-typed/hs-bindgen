{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/callbacks.h>"
  , "signed int hs_bindgen_99bda9cd8097b0ea ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return readFileWithProcessor(arg1, arg2);"
  , "}"
  , "void hs_bindgen_84b75366c836fc85 ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  watchTemperature(arg1, arg2);"
  , "}"
  , "void hs_bindgen_f2580f574faa3697 ("
  , "  FileOpenedNotification arg1"
  , ")"
  , "{"
  , "  onFileOpened(arg1);"
  , "}"
  , "void hs_bindgen_654057b291ee37ea ("
  , "  ProgressUpdate arg1"
  , ")"
  , "{"
  , "  onProgressChanged(arg1);"
  , "}"
  , "signed int hs_bindgen_5df7aac6996be10f ("
  , "  DataValidator arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return validateInput(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8f1bb1c4d2b5355f ("
  , "  MeasurementReceived arg1"
  , ")"
  , "{"
  , "  onNewMeasurement(arg1);"
  , "}"
  , "void hs_bindgen_d805e39c6cbdd620 ("
  , "  MeasurementReceived2 arg1"
  , ")"
  , "{"
  , "  onNewMeasurement2(arg1);"
  , "}"
  , "void hs_bindgen_8d803591bcf10ba5 ("
  , "  SampleBufferFull arg1"
  , ")"
  , "{"
  , "  onBufferReady(arg1);"
  , "}"
  , "void hs_bindgen_16c298a15b737eb2 ("
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
  , "void hs_bindgen_e6a073138e56764f ("
  , "  void (*arg1) ("
  , "  struct Measurement *arg1,"
  , "  FileOpenedNotification arg2,"
  , "  signed int arg3"
  , ")"
  , ")"
  , "{"
  , "  processWithCallbacks(arg1);"
  , "}"
  , "void hs_bindgen_ece0d4f94c2319f0 ("
  , "  struct MeasurementHandler *arg1"
  , ")"
  , "{"
  , "  registerHandler(arg1);"
  , "}"
  , "void hs_bindgen_d66d7470a7a213b0 ("
  , "  struct Measurement *arg1,"
  , "  struct DataPipeline *arg2"
  , ")"
  , "{"
  , "  executePipeline(arg1, arg2);"
  , "}"
  , "void hs_bindgen_e925d3ce6e5fb395 ("
  , "  struct Measurement *arg1,"
  , "  struct Processor *arg2"
  , ")"
  , "{"
  , "  runProcessor(arg1, arg2);"
  , "}"
  , "void hs_bindgen_1e432e1595a1ef55 ("
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
  , "void hs_bindgen_d5cd030edf2e0364 ("
  , "  void (*arg1) ("
  , "  foo arg1"
  , ")"
  , ")"
  , "{"
  , "  f(arg1);"
  , "}"
  , "void hs_bindgen_a10eec74074627ba ("
  , "  void (*arg1) ("
  , "  foo2 const arg1"
  , ")"
  , ")"
  , "{"
  , "  f2(arg1);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_99bda9cd8097b0ea" readFileWithProcessor_base ::
     Ptr.FunPtr Void
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @functions\/callbacks.h:4:5@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_readFileWithProcessor@
-}
readFileWithProcessor ::
     Ptr.FunPtr (FC.CInt -> IO ())
     -- ^ __C declaration:__ @processLine@
  -> FC.CInt
     -- ^ __C declaration:__ @fileId@
  -> IO FC.CInt
readFileWithProcessor =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType readFileWithProcessor_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_84b75366c836fc85" watchTemperature_base ::
     Ptr.FunPtr Void
  -> FC.CInt
  -> IO ()

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @functions\/callbacks.h:5:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_watchTemperature@
-}
watchTemperature ::
     Ptr.FunPtr (FC.CInt -> IO ())
     -- ^ __C declaration:__ @onTempChange@
  -> FC.CInt
     -- ^ __C declaration:__ @sensorId@
  -> IO ()
watchTemperature =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType watchTemperature_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_f2580f574faa3697" onFileOpened_base ::
     Ptr.FunPtr Void
  -> IO ()

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @functions\/callbacks.h:14:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_onFileOpened@
-}
onFileOpened ::
     FileOpenedNotification
     -- ^ __C declaration:__ @notify@
  -> IO ()
onFileOpened =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType onFileOpened_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_654057b291ee37ea" onProgressChanged_base ::
     Ptr.FunPtr Void
  -> IO ()

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @functions\/callbacks.h:15:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_onProgressChanged@
-}
onProgressChanged ::
     ProgressUpdate
     -- ^ __C declaration:__ @update@
  -> IO ()
onProgressChanged =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType onProgressChanged_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_5df7aac6996be10f" validateInput_base ::
     Ptr.FunPtr Void
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @validateInput@

    __defined at:__ @functions\/callbacks.h:16:5@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_validateInput@
-}
validateInput ::
     DataValidator
     -- ^ __C declaration:__ @validator@
  -> FC.CInt
     -- ^ __C declaration:__ @rawValue@
  -> IO FC.CInt
validateInput =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType validateInput_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_8f1bb1c4d2b5355f" onNewMeasurement_base ::
     Ptr.FunPtr Void
  -> IO ()

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @functions\/callbacks.h:27:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_onNewMeasurement@
-}
onNewMeasurement ::
     MeasurementReceived
     -- ^ __C declaration:__ @handler@
  -> IO ()
onNewMeasurement =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType onNewMeasurement_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_d805e39c6cbdd620" onNewMeasurement2_base ::
     Ptr.FunPtr Void
  -> IO ()

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @functions\/callbacks.h:30:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_onNewMeasurement2@
-}
onNewMeasurement2 ::
     MeasurementReceived2
     -- ^ __C declaration:__ @handler@
  -> IO ()
onNewMeasurement2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType onNewMeasurement2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_8d803591bcf10ba5" onBufferReady_base ::
     Ptr.FunPtr Void
  -> IO ()

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @functions\/callbacks.h:33:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_onBufferReady@
-}
onBufferReady ::
     SampleBufferFull
     -- ^ __C declaration:__ @handler@
  -> IO ()
onBufferReady =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType onBufferReady_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_16c298a15b737eb2" transformMeasurement_base ::
     Ptr.Ptr Void
  -> Ptr.FunPtr Void
  -> IO ()

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @functions\/callbacks.h:38:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_transformMeasurement@
-}
transformMeasurement ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data'@
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
     -- ^ __C declaration:__ @transformer@
  -> IO ()
transformMeasurement =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType transformMeasurement_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_e6a073138e56764f" processWithCallbacks_base ::
     Ptr.FunPtr Void
  -> IO ()

{-| __C declaration:__ @processWithCallbacks@

    __defined at:__ @functions\/callbacks.h:43:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_processWithCallbacks@
-}
processWithCallbacks ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
     -- ^ __C declaration:__ @handler@
  -> IO ()
processWithCallbacks =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType processWithCallbacks_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_ece0d4f94c2319f0" registerHandler_base ::
     Ptr.Ptr Void
  -> IO ()

{-| __C declaration:__ @registerHandler@

    __defined at:__ @functions\/callbacks.h:56:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_registerHandler@
-}
registerHandler ::
     Ptr.Ptr MeasurementHandler
     -- ^ __C declaration:__ @handler@
  -> IO ()
registerHandler =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType registerHandler_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_d66d7470a7a213b0" executePipeline_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

{-| __C declaration:__ @executePipeline@

    __defined at:__ @functions\/callbacks.h:64:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_executePipeline@
-}
executePipeline ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data'@
  -> Ptr.Ptr DataPipeline
     -- ^ __C declaration:__ @pipeline@
  -> IO ()
executePipeline =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType executePipeline_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_e925d3ce6e5fb395" runProcessor_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

{-| __C declaration:__ @runProcessor@

    __defined at:__ @functions\/callbacks.h:80:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_runProcessor@
-}
runProcessor ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data'@
  -> Ptr.Ptr Processor
     -- ^ __C declaration:__ @processor@
  -> IO ()
runProcessor =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType runProcessor_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_1e432e1595a1ef55" processMeasurementWithValidation_base ::
     Ptr.Ptr Void
  -> Ptr.FunPtr Void
  -> IO ()

{-| __C declaration:__ @processMeasurementWithValidation@

    __defined at:__ @functions\/callbacks.h:85:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_processMeasurementWithValidation@
-}
processMeasurementWithValidation ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data'@
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
     -- ^ __C declaration:__ @processor@
  -> IO ()
processMeasurementWithValidation =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType processMeasurementWithValidation_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_d5cd030edf2e0364" f_base ::
     Ptr.FunPtr Void
  -> IO ()

{-| __C declaration:__ @f@

    __defined at:__ @functions\/callbacks.h:96:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_f@
-}
f ::
     Ptr.FunPtr (Foo -> IO ())
     -- ^ __C declaration:__ @callback@
  -> IO ()
f =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_a10eec74074627ba" f2_base ::
     Ptr.FunPtr Void
  -> IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/callbacks.h:97:6@

    __exported by:__ @functions\/callbacks.h@

    __unique:__ @test_functionscallbacks_Example_Safe_f2@
-}
f2 ::
     Ptr.FunPtr (Foo2 -> IO ())
     -- ^ __C declaration:__ @handler@
  -> IO ()
f2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f2_base
