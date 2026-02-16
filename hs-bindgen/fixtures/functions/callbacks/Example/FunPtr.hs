{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/callbacks.h>"
  , "/* test_functionscallbacks_Example_get_readFileWithProcessor */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_793b9ca86a272b6a (void)) ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &readFileWithProcessor;"
  , "}"
  , "/* test_functionscallbacks_Example_get_watchTemperature */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a2b606291df27114 (void)) ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &watchTemperature;"
  , "}"
  , "/* test_functionscallbacks_Example_get_onFileOpened */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2c00a09a05e4c87b (void)) ("
  , "  FileOpenedNotification arg1"
  , ")"
  , "{"
  , "  return &onFileOpened;"
  , "}"
  , "/* test_functionscallbacks_Example_get_onProgressChanged */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_84066778519d3ea8 (void)) ("
  , "  ProgressUpdate arg1"
  , ")"
  , "{"
  , "  return &onProgressChanged;"
  , "}"
  , "/* test_functionscallbacks_Example_get_validateInput */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4d522e3ec1eb0b9f (void)) ("
  , "  DataValidator arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &validateInput;"
  , "}"
  , "/* test_functionscallbacks_Example_get_onNewMeasurement */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a26d6914af82ade0 (void)) ("
  , "  MeasurementReceived arg1"
  , ")"
  , "{"
  , "  return &onNewMeasurement;"
  , "}"
  , "/* test_functionscallbacks_Example_get_onNewMeasurement2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2c377b597bd80e50 (void)) ("
  , "  MeasurementReceived2 arg1"
  , ")"
  , "{"
  , "  return &onNewMeasurement2;"
  , "}"
  , "/* test_functionscallbacks_Example_get_onBufferReady */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d1ce142d0c667e22 (void)) ("
  , "  SampleBufferFull arg1"
  , ")"
  , "{"
  , "  return &onBufferReady;"
  , "}"
  , "/* test_functionscallbacks_Example_get_transformMeasurement */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2e8d6b201521b14a (void)) ("
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
  , "/* test_functionscallbacks_Example_get_processWithCallbacks */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7f4acb7260277d86 (void)) ("
  , "  void (*arg1) ("
  , "  struct Measurement *arg1,"
  , "  FileOpenedNotification arg2,"
  , "  signed int arg3"
  , ")"
  , ")"
  , "{"
  , "  return &processWithCallbacks;"
  , "}"
  , "/* test_functionscallbacks_Example_get_registerHandler */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_80684ecfa6d982b7 (void)) ("
  , "  struct MeasurementHandler *arg1"
  , ")"
  , "{"
  , "  return &registerHandler;"
  , "}"
  , "/* test_functionscallbacks_Example_get_executePipeline */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1714a267564c9f6f (void)) ("
  , "  struct Measurement *arg1,"
  , "  struct DataPipeline *arg2"
  , ")"
  , "{"
  , "  return &executePipeline;"
  , "}"
  , "/* test_functionscallbacks_Example_get_runProcessor */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fedf80fe8e91dc7a (void)) ("
  , "  struct Measurement *arg1,"
  , "  struct Processor *arg2"
  , ")"
  , "{"
  , "  return &runProcessor;"
  , "}"
  , "/* test_functionscallbacks_Example_get_processMeasurementWithValidation */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_443cb57c681c2cab (void)) ("
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
  , "/* test_functionscallbacks_Example_get_f */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7cf6cffd19682c36 (void)) ("
  , "  void (*arg1) ("
  , "  foo arg1"
  , ")"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  , "/* test_functionscallbacks_Example_get_f2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_81fde2984e04c791 (void)) ("
  , "  void (*arg1) ("
  , "  foo2 const arg1"
  , ")"
  , ")"
  , "{"
  , "  return &f2;"
  , "}"
  ]))

-- __unique:__ @test_functionscallbacks_Example_get_readFileWithProcessor@
foreign import ccall unsafe "hs_bindgen_793b9ca86a272b6a" hs_bindgen_793b9ca86a272b6a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_readFileWithProcessor@
hs_bindgen_793b9ca86a272b6a :: IO (RIP.FunPtr ((RIP.FunPtr (RIP.CInt -> IO ())) -> RIP.CInt -> IO RIP.CInt))
hs_bindgen_793b9ca86a272b6a =
  RIP.fromFFIType hs_bindgen_793b9ca86a272b6a_base

{-# NOINLINE readFileWithProcessor #-}
{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @functions\/callbacks.h 4:5@

    __exported by:__ @functions\/callbacks.h@
-}
readFileWithProcessor :: RIP.FunPtr ((RIP.FunPtr (RIP.CInt -> IO ())) -> RIP.CInt -> IO RIP.CInt)
readFileWithProcessor =
  RIP.unsafePerformIO hs_bindgen_793b9ca86a272b6a

-- __unique:__ @test_functionscallbacks_Example_get_watchTemperature@
foreign import ccall unsafe "hs_bindgen_a2b606291df27114" hs_bindgen_a2b606291df27114_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_watchTemperature@
hs_bindgen_a2b606291df27114 :: IO (RIP.FunPtr ((RIP.FunPtr (RIP.CInt -> IO ())) -> RIP.CInt -> IO ()))
hs_bindgen_a2b606291df27114 =
  RIP.fromFFIType hs_bindgen_a2b606291df27114_base

{-# NOINLINE watchTemperature #-}
{-| __C declaration:__ @watchTemperature@

    __defined at:__ @functions\/callbacks.h 5:6@

    __exported by:__ @functions\/callbacks.h@
-}
watchTemperature :: RIP.FunPtr ((RIP.FunPtr (RIP.CInt -> IO ())) -> RIP.CInt -> IO ())
watchTemperature =
  RIP.unsafePerformIO hs_bindgen_a2b606291df27114

-- __unique:__ @test_functionscallbacks_Example_get_onFileOpened@
foreign import ccall unsafe "hs_bindgen_2c00a09a05e4c87b" hs_bindgen_2c00a09a05e4c87b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_onFileOpened@
hs_bindgen_2c00a09a05e4c87b :: IO (RIP.FunPtr (FileOpenedNotification -> IO ()))
hs_bindgen_2c00a09a05e4c87b =
  RIP.fromFFIType hs_bindgen_2c00a09a05e4c87b_base

{-# NOINLINE onFileOpened #-}
{-| __C declaration:__ @onFileOpened@

    __defined at:__ @functions\/callbacks.h 14:6@

    __exported by:__ @functions\/callbacks.h@
-}
onFileOpened :: RIP.FunPtr (FileOpenedNotification -> IO ())
onFileOpened =
  RIP.unsafePerformIO hs_bindgen_2c00a09a05e4c87b

-- __unique:__ @test_functionscallbacks_Example_get_onProgressChanged@
foreign import ccall unsafe "hs_bindgen_84066778519d3ea8" hs_bindgen_84066778519d3ea8_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_onProgressChanged@
hs_bindgen_84066778519d3ea8 :: IO (RIP.FunPtr (ProgressUpdate -> IO ()))
hs_bindgen_84066778519d3ea8 =
  RIP.fromFFIType hs_bindgen_84066778519d3ea8_base

{-# NOINLINE onProgressChanged #-}
{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @functions\/callbacks.h 15:6@

    __exported by:__ @functions\/callbacks.h@
-}
onProgressChanged :: RIP.FunPtr (ProgressUpdate -> IO ())
onProgressChanged =
  RIP.unsafePerformIO hs_bindgen_84066778519d3ea8

-- __unique:__ @test_functionscallbacks_Example_get_validateInput@
foreign import ccall unsafe "hs_bindgen_4d522e3ec1eb0b9f" hs_bindgen_4d522e3ec1eb0b9f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_validateInput@
hs_bindgen_4d522e3ec1eb0b9f :: IO (RIP.FunPtr (DataValidator -> RIP.CInt -> IO RIP.CInt))
hs_bindgen_4d522e3ec1eb0b9f =
  RIP.fromFFIType hs_bindgen_4d522e3ec1eb0b9f_base

{-# NOINLINE validateInput #-}
{-| __C declaration:__ @validateInput@

    __defined at:__ @functions\/callbacks.h 16:5@

    __exported by:__ @functions\/callbacks.h@
-}
validateInput :: RIP.FunPtr (DataValidator -> RIP.CInt -> IO RIP.CInt)
validateInput =
  RIP.unsafePerformIO hs_bindgen_4d522e3ec1eb0b9f

-- __unique:__ @test_functionscallbacks_Example_get_onNewMeasurement@
foreign import ccall unsafe "hs_bindgen_a26d6914af82ade0" hs_bindgen_a26d6914af82ade0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_onNewMeasurement@
hs_bindgen_a26d6914af82ade0 :: IO (RIP.FunPtr (MeasurementReceived -> IO ()))
hs_bindgen_a26d6914af82ade0 =
  RIP.fromFFIType hs_bindgen_a26d6914af82ade0_base

{-# NOINLINE onNewMeasurement #-}
{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @functions\/callbacks.h 27:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement :: RIP.FunPtr (MeasurementReceived -> IO ())
onNewMeasurement =
  RIP.unsafePerformIO hs_bindgen_a26d6914af82ade0

-- __unique:__ @test_functionscallbacks_Example_get_onNewMeasurement2@
foreign import ccall unsafe "hs_bindgen_2c377b597bd80e50" hs_bindgen_2c377b597bd80e50_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_onNewMeasurement2@
hs_bindgen_2c377b597bd80e50 :: IO (RIP.FunPtr (MeasurementReceived2 -> IO ()))
hs_bindgen_2c377b597bd80e50 =
  RIP.fromFFIType hs_bindgen_2c377b597bd80e50_base

{-# NOINLINE onNewMeasurement2 #-}
{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @functions\/callbacks.h 30:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement2 :: RIP.FunPtr (MeasurementReceived2 -> IO ())
onNewMeasurement2 =
  RIP.unsafePerformIO hs_bindgen_2c377b597bd80e50

-- __unique:__ @test_functionscallbacks_Example_get_onBufferReady@
foreign import ccall unsafe "hs_bindgen_d1ce142d0c667e22" hs_bindgen_d1ce142d0c667e22_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_onBufferReady@
hs_bindgen_d1ce142d0c667e22 :: IO (RIP.FunPtr (SampleBufferFull -> IO ()))
hs_bindgen_d1ce142d0c667e22 =
  RIP.fromFFIType hs_bindgen_d1ce142d0c667e22_base

{-# NOINLINE onBufferReady #-}
{-| __C declaration:__ @onBufferReady@

    __defined at:__ @functions\/callbacks.h 33:6@

    __exported by:__ @functions\/callbacks.h@
-}
onBufferReady :: RIP.FunPtr (SampleBufferFull -> IO ())
onBufferReady =
  RIP.unsafePerformIO hs_bindgen_d1ce142d0c667e22

-- __unique:__ @test_functionscallbacks_Example_get_transformMeasurement@
foreign import ccall unsafe "hs_bindgen_2e8d6b201521b14a" hs_bindgen_2e8d6b201521b14a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_transformMeasurement@
hs_bindgen_2e8d6b201521b14a :: IO (RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ())) -> IO ()))
hs_bindgen_2e8d6b201521b14a =
  RIP.fromFFIType hs_bindgen_2e8d6b201521b14a_base

{-# NOINLINE transformMeasurement #-}
{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @functions\/callbacks.h 38:6@

    __exported by:__ @functions\/callbacks.h@
-}
transformMeasurement :: RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr (RIP.CDouble -> RIP.CInt -> IO RIP.CDouble)) -> RIP.CInt -> IO ())) -> IO ())
transformMeasurement =
  RIP.unsafePerformIO hs_bindgen_2e8d6b201521b14a

-- __unique:__ @test_functionscallbacks_Example_get_processWithCallbacks@
foreign import ccall unsafe "hs_bindgen_7f4acb7260277d86" hs_bindgen_7f4acb7260277d86_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_processWithCallbacks@
hs_bindgen_7f4acb7260277d86 :: IO (RIP.FunPtr ((RIP.FunPtr ((RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ())) -> IO ()))
hs_bindgen_7f4acb7260277d86 =
  RIP.fromFFIType hs_bindgen_7f4acb7260277d86_base

{-# NOINLINE processWithCallbacks #-}
{-| __C declaration:__ @processWithCallbacks@

    __defined at:__ @functions\/callbacks.h 43:6@

    __exported by:__ @functions\/callbacks.h@
-}
processWithCallbacks :: RIP.FunPtr ((RIP.FunPtr ((RIP.Ptr Measurement) -> FileOpenedNotification -> RIP.CInt -> IO ())) -> IO ())
processWithCallbacks =
  RIP.unsafePerformIO hs_bindgen_7f4acb7260277d86

-- __unique:__ @test_functionscallbacks_Example_get_registerHandler@
foreign import ccall unsafe "hs_bindgen_80684ecfa6d982b7" hs_bindgen_80684ecfa6d982b7_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_registerHandler@
hs_bindgen_80684ecfa6d982b7 :: IO (RIP.FunPtr ((RIP.Ptr MeasurementHandler) -> IO ()))
hs_bindgen_80684ecfa6d982b7 =
  RIP.fromFFIType hs_bindgen_80684ecfa6d982b7_base

{-# NOINLINE registerHandler #-}
{-| __C declaration:__ @registerHandler@

    __defined at:__ @functions\/callbacks.h 56:6@

    __exported by:__ @functions\/callbacks.h@
-}
registerHandler :: RIP.FunPtr ((RIP.Ptr MeasurementHandler) -> IO ())
registerHandler =
  RIP.unsafePerformIO hs_bindgen_80684ecfa6d982b7

-- __unique:__ @test_functionscallbacks_Example_get_executePipeline@
foreign import ccall unsafe "hs_bindgen_1714a267564c9f6f" hs_bindgen_1714a267564c9f6f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_executePipeline@
hs_bindgen_1714a267564c9f6f :: IO (RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.Ptr DataPipeline) -> IO ()))
hs_bindgen_1714a267564c9f6f =
  RIP.fromFFIType hs_bindgen_1714a267564c9f6f_base

{-# NOINLINE executePipeline #-}
{-| __C declaration:__ @executePipeline@

    __defined at:__ @functions\/callbacks.h 64:6@

    __exported by:__ @functions\/callbacks.h@
-}
executePipeline :: RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.Ptr DataPipeline) -> IO ())
executePipeline =
  RIP.unsafePerformIO hs_bindgen_1714a267564c9f6f

-- __unique:__ @test_functionscallbacks_Example_get_runProcessor@
foreign import ccall unsafe "hs_bindgen_fedf80fe8e91dc7a" hs_bindgen_fedf80fe8e91dc7a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_runProcessor@
hs_bindgen_fedf80fe8e91dc7a :: IO (RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.Ptr Processor) -> IO ()))
hs_bindgen_fedf80fe8e91dc7a =
  RIP.fromFFIType hs_bindgen_fedf80fe8e91dc7a_base

{-# NOINLINE runProcessor #-}
{-| __C declaration:__ @runProcessor@

    __defined at:__ @functions\/callbacks.h 80:6@

    __exported by:__ @functions\/callbacks.h@
-}
runProcessor :: RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.Ptr Processor) -> IO ())
runProcessor =
  RIP.unsafePerformIO hs_bindgen_fedf80fe8e91dc7a

-- __unique:__ @test_functionscallbacks_Example_get_processMeasurementWithValidation@
foreign import ccall unsafe "hs_bindgen_443cb57c681c2cab" hs_bindgen_443cb57c681c2cab_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_processMeasurementWithValidation@
hs_bindgen_443cb57c681c2cab :: IO (RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ())) -> IO ()))
hs_bindgen_443cb57c681c2cab =
  RIP.fromFFIType hs_bindgen_443cb57c681c2cab_base

{-# NOINLINE processMeasurementWithValidation #-}
{-| __C declaration:__ @processMeasurementWithValidation@

    __defined at:__ @functions\/callbacks.h 85:6@

    __exported by:__ @functions\/callbacks.h@
-}
processMeasurementWithValidation :: RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> (RIP.FunPtr ((RIP.Ptr Measurement) -> DataValidator -> RIP.CInt -> IO ())) -> DataValidator -> IO ())) -> IO ())
processMeasurementWithValidation =
  RIP.unsafePerformIO hs_bindgen_443cb57c681c2cab

-- __unique:__ @test_functionscallbacks_Example_get_f@
foreign import ccall unsafe "hs_bindgen_7cf6cffd19682c36" hs_bindgen_7cf6cffd19682c36_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_f@
hs_bindgen_7cf6cffd19682c36 :: IO (RIP.FunPtr ((RIP.FunPtr (Foo -> IO ())) -> IO ()))
hs_bindgen_7cf6cffd19682c36 =
  RIP.fromFFIType hs_bindgen_7cf6cffd19682c36_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @functions\/callbacks.h 96:6@

    __exported by:__ @functions\/callbacks.h@
-}
f :: RIP.FunPtr ((RIP.FunPtr (Foo -> IO ())) -> IO ())
f = RIP.unsafePerformIO hs_bindgen_7cf6cffd19682c36

-- __unique:__ @test_functionscallbacks_Example_get_f2@
foreign import ccall unsafe "hs_bindgen_81fde2984e04c791" hs_bindgen_81fde2984e04c791_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionscallbacks_Example_get_f2@
hs_bindgen_81fde2984e04c791 :: IO (RIP.FunPtr ((RIP.FunPtr (Foo2 -> IO ())) -> IO ()))
hs_bindgen_81fde2984e04c791 =
  RIP.fromFFIType hs_bindgen_81fde2984e04c791_base

{-# NOINLINE f2 #-}
{-| __C declaration:__ @f2@

    __defined at:__ @functions\/callbacks.h 97:6@

    __exported by:__ @functions\/callbacks.h@
-}
f2 :: RIP.FunPtr ((RIP.FunPtr (Foo2 -> IO ())) -> IO ())
f2 = RIP.unsafePerformIO hs_bindgen_81fde2984e04c791
