{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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

-- __unique:__ @test_functionscallbacks_Example_Safe_readFileWithProcessor@
foreign import ccall safe "hs_bindgen_99bda9cd8097b0ea" hs_bindgen_99bda9cd8097b0ea_base ::
     Ptr.FunPtr Void
  -> GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_functionscallbacks_Example_Safe_readFileWithProcessor@
hs_bindgen_99bda9cd8097b0ea ::
     Ptr.FunPtr (FC.CInt -> IO ())
  -> FC.CInt
  -> IO FC.CInt
hs_bindgen_99bda9cd8097b0ea =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_99bda9cd8097b0ea_base

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @functions\/callbacks.h 4:5@

    __exported by:__ @functions\/callbacks.h@
-}
readFileWithProcessor ::
     Ptr.FunPtr (FC.CInt -> IO ())
     -- ^ __C declaration:__ @processLine@
  -> FC.CInt
     -- ^ __C declaration:__ @fileId@
  -> IO FC.CInt
readFileWithProcessor = hs_bindgen_99bda9cd8097b0ea

-- __unique:__ @test_functionscallbacks_Example_Safe_watchTemperature@
foreign import ccall safe "hs_bindgen_84b75366c836fc85" hs_bindgen_84b75366c836fc85_base ::
     Ptr.FunPtr Void
  -> GHC.Int.Int32
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_watchTemperature@
hs_bindgen_84b75366c836fc85 ::
     Ptr.FunPtr (FC.CInt -> IO ())
  -> FC.CInt
  -> IO ()
hs_bindgen_84b75366c836fc85 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_84b75366c836fc85_base

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @functions\/callbacks.h 5:6@

    __exported by:__ @functions\/callbacks.h@
-}
watchTemperature ::
     Ptr.FunPtr (FC.CInt -> IO ())
     -- ^ __C declaration:__ @onTempChange@
  -> FC.CInt
     -- ^ __C declaration:__ @sensorId@
  -> IO ()
watchTemperature = hs_bindgen_84b75366c836fc85

-- __unique:__ @test_functionscallbacks_Example_Safe_onFileOpened@
foreign import ccall safe "hs_bindgen_f2580f574faa3697" hs_bindgen_f2580f574faa3697_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_onFileOpened@
hs_bindgen_f2580f574faa3697 ::
     FileOpenedNotification
  -> IO ()
hs_bindgen_f2580f574faa3697 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f2580f574faa3697_base

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @functions\/callbacks.h 14:6@

    __exported by:__ @functions\/callbacks.h@
-}
onFileOpened ::
     FileOpenedNotification
     -- ^ __C declaration:__ @notify@
  -> IO ()
onFileOpened = hs_bindgen_f2580f574faa3697

-- __unique:__ @test_functionscallbacks_Example_Safe_onProgressChanged@
foreign import ccall safe "hs_bindgen_654057b291ee37ea" hs_bindgen_654057b291ee37ea_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_onProgressChanged@
hs_bindgen_654057b291ee37ea ::
     ProgressUpdate
  -> IO ()
hs_bindgen_654057b291ee37ea =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_654057b291ee37ea_base

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @functions\/callbacks.h 15:6@

    __exported by:__ @functions\/callbacks.h@
-}
onProgressChanged ::
     ProgressUpdate
     -- ^ __C declaration:__ @update@
  -> IO ()
onProgressChanged = hs_bindgen_654057b291ee37ea

-- __unique:__ @test_functionscallbacks_Example_Safe_validateInput@
foreign import ccall safe "hs_bindgen_5df7aac6996be10f" hs_bindgen_5df7aac6996be10f_base ::
     Ptr.FunPtr Void
  -> GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_functionscallbacks_Example_Safe_validateInput@
hs_bindgen_5df7aac6996be10f ::
     DataValidator
  -> FC.CInt
  -> IO FC.CInt
hs_bindgen_5df7aac6996be10f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_5df7aac6996be10f_base

{-| __C declaration:__ @validateInput@

    __defined at:__ @functions\/callbacks.h 16:5@

    __exported by:__ @functions\/callbacks.h@
-}
validateInput ::
     DataValidator
     -- ^ __C declaration:__ @validator@
  -> FC.CInt
     -- ^ __C declaration:__ @rawValue@
  -> IO FC.CInt
validateInput = hs_bindgen_5df7aac6996be10f

-- __unique:__ @test_functionscallbacks_Example_Safe_onNewMeasurement@
foreign import ccall safe "hs_bindgen_8f1bb1c4d2b5355f" hs_bindgen_8f1bb1c4d2b5355f_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_onNewMeasurement@
hs_bindgen_8f1bb1c4d2b5355f ::
     MeasurementReceived
  -> IO ()
hs_bindgen_8f1bb1c4d2b5355f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_8f1bb1c4d2b5355f_base

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @functions\/callbacks.h 27:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement ::
     MeasurementReceived
     -- ^ __C declaration:__ @handler@
  -> IO ()
onNewMeasurement = hs_bindgen_8f1bb1c4d2b5355f

-- __unique:__ @test_functionscallbacks_Example_Safe_onNewMeasurement2@
foreign import ccall safe "hs_bindgen_d805e39c6cbdd620" hs_bindgen_d805e39c6cbdd620_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_onNewMeasurement2@
hs_bindgen_d805e39c6cbdd620 ::
     MeasurementReceived2
  -> IO ()
hs_bindgen_d805e39c6cbdd620 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d805e39c6cbdd620_base

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @functions\/callbacks.h 30:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement2 ::
     MeasurementReceived2
     -- ^ __C declaration:__ @handler@
  -> IO ()
onNewMeasurement2 = hs_bindgen_d805e39c6cbdd620

-- __unique:__ @test_functionscallbacks_Example_Safe_onBufferReady@
foreign import ccall safe "hs_bindgen_8d803591bcf10ba5" hs_bindgen_8d803591bcf10ba5_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_onBufferReady@
hs_bindgen_8d803591bcf10ba5 ::
     SampleBufferFull
  -> IO ()
hs_bindgen_8d803591bcf10ba5 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_8d803591bcf10ba5_base

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @functions\/callbacks.h 33:6@

    __exported by:__ @functions\/callbacks.h@
-}
onBufferReady ::
     SampleBufferFull
     -- ^ __C declaration:__ @handler@
  -> IO ()
onBufferReady = hs_bindgen_8d803591bcf10ba5

-- __unique:__ @test_functionscallbacks_Example_Safe_transformMeasurement@
foreign import ccall safe "hs_bindgen_16c298a15b737eb2" hs_bindgen_16c298a15b737eb2_base ::
     Ptr.Ptr Void
  -> Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_transformMeasurement@
hs_bindgen_16c298a15b737eb2 ::
     Ptr.Ptr Measurement
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
  -> IO ()
hs_bindgen_16c298a15b737eb2 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_16c298a15b737eb2_base

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @functions\/callbacks.h 38:6@

    __exported by:__ @functions\/callbacks.h@
-}
transformMeasurement ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data@
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())
     -- ^ __C declaration:__ @transformer@
  -> IO ()
transformMeasurement = hs_bindgen_16c298a15b737eb2

-- __unique:__ @test_functionscallbacks_Example_Safe_processWithCallbacks@
foreign import ccall safe "hs_bindgen_e6a073138e56764f" hs_bindgen_e6a073138e56764f_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_processWithCallbacks@
hs_bindgen_e6a073138e56764f ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
  -> IO ()
hs_bindgen_e6a073138e56764f =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e6a073138e56764f_base

{-| __C declaration:__ @processWithCallbacks@

    __defined at:__ @functions\/callbacks.h 43:6@

    __exported by:__ @functions\/callbacks.h@
-}
processWithCallbacks ::
     Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())
     -- ^ __C declaration:__ @handler@
  -> IO ()
processWithCallbacks = hs_bindgen_e6a073138e56764f

-- __unique:__ @test_functionscallbacks_Example_Safe_registerHandler@
foreign import ccall safe "hs_bindgen_ece0d4f94c2319f0" hs_bindgen_ece0d4f94c2319f0_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_registerHandler@
hs_bindgen_ece0d4f94c2319f0 ::
     Ptr.Ptr MeasurementHandler
  -> IO ()
hs_bindgen_ece0d4f94c2319f0 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ece0d4f94c2319f0_base

{-| __C declaration:__ @registerHandler@

    __defined at:__ @functions\/callbacks.h 56:6@

    __exported by:__ @functions\/callbacks.h@
-}
registerHandler ::
     Ptr.Ptr MeasurementHandler
     -- ^ __C declaration:__ @handler@
  -> IO ()
registerHandler = hs_bindgen_ece0d4f94c2319f0

-- __unique:__ @test_functionscallbacks_Example_Safe_executePipeline@
foreign import ccall safe "hs_bindgen_d66d7470a7a213b0" hs_bindgen_d66d7470a7a213b0_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_executePipeline@
hs_bindgen_d66d7470a7a213b0 ::
     Ptr.Ptr Measurement
  -> Ptr.Ptr DataPipeline
  -> IO ()
hs_bindgen_d66d7470a7a213b0 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d66d7470a7a213b0_base

{-| __C declaration:__ @executePipeline@

    __defined at:__ @functions\/callbacks.h 64:6@

    __exported by:__ @functions\/callbacks.h@
-}
executePipeline ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data@
  -> Ptr.Ptr DataPipeline
     -- ^ __C declaration:__ @pipeline@
  -> IO ()
executePipeline = hs_bindgen_d66d7470a7a213b0

-- __unique:__ @test_functionscallbacks_Example_Safe_runProcessor@
foreign import ccall safe "hs_bindgen_e925d3ce6e5fb395" hs_bindgen_e925d3ce6e5fb395_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_runProcessor@
hs_bindgen_e925d3ce6e5fb395 ::
     Ptr.Ptr Measurement
  -> Ptr.Ptr Processor
  -> IO ()
hs_bindgen_e925d3ce6e5fb395 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e925d3ce6e5fb395_base

{-| __C declaration:__ @runProcessor@

    __defined at:__ @functions\/callbacks.h 80:6@

    __exported by:__ @functions\/callbacks.h@
-}
runProcessor ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data@
  -> Ptr.Ptr Processor
     -- ^ __C declaration:__ @processor@
  -> IO ()
runProcessor = hs_bindgen_e925d3ce6e5fb395

-- __unique:__ @test_functionscallbacks_Example_Safe_processMeasurementWithValidation@
foreign import ccall safe "hs_bindgen_1e432e1595a1ef55" hs_bindgen_1e432e1595a1ef55_base ::
     Ptr.Ptr Void
  -> Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_processMeasurementWithValidation@
hs_bindgen_1e432e1595a1ef55 ::
     Ptr.Ptr Measurement
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
  -> IO ()
hs_bindgen_1e432e1595a1ef55 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_1e432e1595a1ef55_base

{-| __C declaration:__ @processMeasurementWithValidation@

    __defined at:__ @functions\/callbacks.h 85:6@

    __exported by:__ @functions\/callbacks.h@
-}
processMeasurementWithValidation ::
     Ptr.Ptr Measurement
     -- ^ __C declaration:__ @data@
  -> Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())
     -- ^ __C declaration:__ @processor@
  -> IO ()
processMeasurementWithValidation =
  hs_bindgen_1e432e1595a1ef55

-- __unique:__ @test_functionscallbacks_Example_Safe_f@
foreign import ccall safe "hs_bindgen_d5cd030edf2e0364" hs_bindgen_d5cd030edf2e0364_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_f@
hs_bindgen_d5cd030edf2e0364 ::
     Ptr.FunPtr (Foo -> IO ())
  -> IO ()
hs_bindgen_d5cd030edf2e0364 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d5cd030edf2e0364_base

{-| __C declaration:__ @f@

    __defined at:__ @functions\/callbacks.h 96:6@

    __exported by:__ @functions\/callbacks.h@
-}
f ::
     Ptr.FunPtr (Foo -> IO ())
     -- ^ __C declaration:__ @callback@
  -> IO ()
f = hs_bindgen_d5cd030edf2e0364

-- __unique:__ @test_functionscallbacks_Example_Safe_f2@
foreign import ccall safe "hs_bindgen_a10eec74074627ba" hs_bindgen_a10eec74074627ba_base ::
     Ptr.FunPtr Void
  -> IO ()

-- __unique:__ @test_functionscallbacks_Example_Safe_f2@
hs_bindgen_a10eec74074627ba ::
     Ptr.FunPtr (Foo2 -> IO ())
  -> IO ()
hs_bindgen_a10eec74074627ba =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a10eec74074627ba_base

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/callbacks.h 97:6@

    __exported by:__ @functions\/callbacks.h@
-}
f2 ::
     Ptr.FunPtr (Foo2 -> IO ())
     -- ^ __C declaration:__ @handler@
  -> IO ()
f2 = hs_bindgen_a10eec74074627ba
