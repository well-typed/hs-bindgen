{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/callbacks.h>"
  , "/* test_functionscallbacks_Example_get_readFileWithProcessor_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_11c3318ecc076134 (void)) ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &readFileWithProcessor;"
  , "}"
  , "/* test_functionscallbacks_Example_get_watchTemperature_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_812229d77f36833a (void)) ("
  , "  void (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &watchTemperature;"
  , "}"
  , "/* test_functionscallbacks_Example_get_onFileOpened_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cd162abdd104aa42 (void)) ("
  , "  FileOpenedNotification arg1"
  , ")"
  , "{"
  , "  return &onFileOpened;"
  , "}"
  , "/* test_functionscallbacks_Example_get_onProgressChanged_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b66e61e98e8145a4 (void)) ("
  , "  ProgressUpdate arg1"
  , ")"
  , "{"
  , "  return &onProgressChanged;"
  , "}"
  , "/* test_functionscallbacks_Example_get_validateInput_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4c20e93be5c3b5bb (void)) ("
  , "  DataValidator arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &validateInput;"
  , "}"
  , "/* test_functionscallbacks_Example_get_onNewMeasurement_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f0fa88e6072c2d7a (void)) ("
  , "  MeasurementReceived arg1"
  , ")"
  , "{"
  , "  return &onNewMeasurement;"
  , "}"
  , "/* test_functionscallbacks_Example_get_onNewMeasurement2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c72d8638d47aae13 (void)) ("
  , "  MeasurementReceived2 arg1"
  , ")"
  , "{"
  , "  return &onNewMeasurement2;"
  , "}"
  , "/* test_functionscallbacks_Example_get_onBufferReady_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7b54895b95bee198 (void)) ("
  , "  SampleBufferFull arg1"
  , ")"
  , "{"
  , "  return &onBufferReady;"
  , "}"
  , "/* test_functionscallbacks_Example_get_transformMeasurement_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4215bdb12daf9024 (void)) ("
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
  , "/* test_functionscallbacks_Example_get_processWithCallbacks_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_701c0161802d878b (void)) ("
  , "  void (*arg1) ("
  , "  struct Measurement *arg1,"
  , "  FileOpenedNotification arg2,"
  , "  signed int arg3"
  , ")"
  , ")"
  , "{"
  , "  return &processWithCallbacks;"
  , "}"
  , "/* test_functionscallbacks_Example_get_registerHandler_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_90c9d96723cea577 (void)) ("
  , "  struct MeasurementHandler *arg1"
  , ")"
  , "{"
  , "  return &registerHandler;"
  , "}"
  , "/* test_functionscallbacks_Example_get_executePipeline_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bc33471040d45469 (void)) ("
  , "  struct Measurement *arg1,"
  , "  struct DataPipeline *arg2"
  , ")"
  , "{"
  , "  return &executePipeline;"
  , "}"
  , "/* test_functionscallbacks_Example_get_runProcessor_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_86a8e8897172172b (void)) ("
  , "  struct Measurement *arg1,"
  , "  struct Processor *arg2"
  , ")"
  , "{"
  , "  return &runProcessor;"
  , "}"
  , "/* test_functionscallbacks_Example_get_processMeasurementWithValidation_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f4667aed4d51fd75 (void)) ("
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
  , "/* test_functionscallbacks_Example_get_f_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_77b468218b567b37 (void)) ("
  , "  void (*arg1) ("
  , "  foo arg1"
  , ")"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  , "/* test_functionscallbacks_Example_get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d5a4de10d670d97d (void)) ("
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
foreign import ccall unsafe "hs_bindgen_11c3318ecc076134" hs_bindgen_11c3318ecc076134_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_readFileWithProcessor_ptr@
hs_bindgen_11c3318ecc076134 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt))
hs_bindgen_11c3318ecc076134 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_11c3318ecc076134_base

{-# NOINLINE readFileWithProcessor_ptr #-}

{-| __C declaration:__ @readFileWithProcessor@

    __defined at:__ @functions\/callbacks.h:4:5@

    __exported by:__ @functions\/callbacks.h@
-}
readFileWithProcessor_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO FC.CInt)
readFileWithProcessor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_11c3318ecc076134

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_812229d77f36833a" hs_bindgen_812229d77f36833a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_watchTemperature_ptr@
hs_bindgen_812229d77f36833a ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ()))
hs_bindgen_812229d77f36833a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_812229d77f36833a_base

{-# NOINLINE watchTemperature_ptr #-}

{-| __C declaration:__ @watchTemperature@

    __defined at:__ @functions\/callbacks.h:5:6@

    __exported by:__ @functions\/callbacks.h@
-}
watchTemperature_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO ())) -> FC.CInt -> IO ())
watchTemperature_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_812229d77f36833a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_cd162abdd104aa42" hs_bindgen_cd162abdd104aa42_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_onFileOpened_ptr@
hs_bindgen_cd162abdd104aa42 ::
     IO (Ptr.FunPtr (FileOpenedNotification -> IO ()))
hs_bindgen_cd162abdd104aa42 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_cd162abdd104aa42_base

{-# NOINLINE onFileOpened_ptr #-}

{-| __C declaration:__ @onFileOpened@

    __defined at:__ @functions\/callbacks.h:14:6@

    __exported by:__ @functions\/callbacks.h@
-}
onFileOpened_ptr :: Ptr.FunPtr (FileOpenedNotification -> IO ())
onFileOpened_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cd162abdd104aa42

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_b66e61e98e8145a4" hs_bindgen_b66e61e98e8145a4_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_onProgressChanged_ptr@
hs_bindgen_b66e61e98e8145a4 ::
     IO (Ptr.FunPtr (ProgressUpdate -> IO ()))
hs_bindgen_b66e61e98e8145a4 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_b66e61e98e8145a4_base

{-# NOINLINE onProgressChanged_ptr #-}

{-| __C declaration:__ @onProgressChanged@

    __defined at:__ @functions\/callbacks.h:15:6@

    __exported by:__ @functions\/callbacks.h@
-}
onProgressChanged_ptr :: Ptr.FunPtr (ProgressUpdate -> IO ())
onProgressChanged_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b66e61e98e8145a4

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4c20e93be5c3b5bb" hs_bindgen_4c20e93be5c3b5bb_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_validateInput_ptr@
hs_bindgen_4c20e93be5c3b5bb ::
     IO (Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt))
hs_bindgen_4c20e93be5c3b5bb =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4c20e93be5c3b5bb_base

{-# NOINLINE validateInput_ptr #-}

{-| __C declaration:__ @validateInput@

    __defined at:__ @functions\/callbacks.h:16:5@

    __exported by:__ @functions\/callbacks.h@
-}
validateInput_ptr :: Ptr.FunPtr (DataValidator -> FC.CInt -> IO FC.CInt)
validateInput_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4c20e93be5c3b5bb

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f0fa88e6072c2d7a" hs_bindgen_f0fa88e6072c2d7a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_onNewMeasurement_ptr@
hs_bindgen_f0fa88e6072c2d7a ::
     IO (Ptr.FunPtr (MeasurementReceived -> IO ()))
hs_bindgen_f0fa88e6072c2d7a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f0fa88e6072c2d7a_base

{-# NOINLINE onNewMeasurement_ptr #-}

{-| __C declaration:__ @onNewMeasurement@

    __defined at:__ @functions\/callbacks.h:27:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement_ptr :: Ptr.FunPtr (MeasurementReceived -> IO ())
onNewMeasurement_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f0fa88e6072c2d7a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c72d8638d47aae13" hs_bindgen_c72d8638d47aae13_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_onNewMeasurement2_ptr@
hs_bindgen_c72d8638d47aae13 ::
     IO (Ptr.FunPtr (MeasurementReceived2 -> IO ()))
hs_bindgen_c72d8638d47aae13 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c72d8638d47aae13_base

{-# NOINLINE onNewMeasurement2_ptr #-}

{-| __C declaration:__ @onNewMeasurement2@

    __defined at:__ @functions\/callbacks.h:30:6@

    __exported by:__ @functions\/callbacks.h@
-}
onNewMeasurement2_ptr :: Ptr.FunPtr (MeasurementReceived2 -> IO ())
onNewMeasurement2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c72d8638d47aae13

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_7b54895b95bee198" hs_bindgen_7b54895b95bee198_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_onBufferReady_ptr@
hs_bindgen_7b54895b95bee198 ::
     IO (Ptr.FunPtr (SampleBufferFull -> IO ()))
hs_bindgen_7b54895b95bee198 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_7b54895b95bee198_base

{-# NOINLINE onBufferReady_ptr #-}

{-| __C declaration:__ @onBufferReady@

    __defined at:__ @functions\/callbacks.h:33:6@

    __exported by:__ @functions\/callbacks.h@
-}
onBufferReady_ptr :: Ptr.FunPtr (SampleBufferFull -> IO ())
onBufferReady_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7b54895b95bee198

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4215bdb12daf9024" hs_bindgen_4215bdb12daf9024_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_transformMeasurement_ptr@
hs_bindgen_4215bdb12daf9024 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())) -> IO ()))
hs_bindgen_4215bdb12daf9024 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4215bdb12daf9024_base

{-# NOINLINE transformMeasurement_ptr #-}

{-| __C declaration:__ @transformMeasurement@

    __defined at:__ @functions\/callbacks.h:38:6@

    __exported by:__ @functions\/callbacks.h@
-}
transformMeasurement_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr (FC.CDouble -> FC.CInt -> IO FC.CDouble)) -> FC.CInt -> IO ())) -> IO ())
transformMeasurement_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4215bdb12daf9024

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_701c0161802d878b" hs_bindgen_701c0161802d878b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_processWithCallbacks_ptr@
hs_bindgen_701c0161802d878b ::
     IO (Ptr.FunPtr ((Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())) -> IO ()))
hs_bindgen_701c0161802d878b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_701c0161802d878b_base

{-# NOINLINE processWithCallbacks_ptr #-}

{-| __C declaration:__ @processWithCallbacks@

    __defined at:__ @functions\/callbacks.h:43:6@

    __exported by:__ @functions\/callbacks.h@
-}
processWithCallbacks_ptr :: Ptr.FunPtr ((Ptr.FunPtr ((Ptr.Ptr Measurement) -> FileOpenedNotification -> FC.CInt -> IO ())) -> IO ())
processWithCallbacks_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_701c0161802d878b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_90c9d96723cea577" hs_bindgen_90c9d96723cea577_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_registerHandler_ptr@
hs_bindgen_90c9d96723cea577 ::
     IO (Ptr.FunPtr ((Ptr.Ptr MeasurementHandler) -> IO ()))
hs_bindgen_90c9d96723cea577 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_90c9d96723cea577_base

{-# NOINLINE registerHandler_ptr #-}

{-| __C declaration:__ @registerHandler@

    __defined at:__ @functions\/callbacks.h:56:6@

    __exported by:__ @functions\/callbacks.h@
-}
registerHandler_ptr :: Ptr.FunPtr ((Ptr.Ptr MeasurementHandler) -> IO ())
registerHandler_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_90c9d96723cea577

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_bc33471040d45469" hs_bindgen_bc33471040d45469_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_executePipeline_ptr@
hs_bindgen_bc33471040d45469 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr DataPipeline) -> IO ()))
hs_bindgen_bc33471040d45469 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_bc33471040d45469_base

{-# NOINLINE executePipeline_ptr #-}

{-| __C declaration:__ @executePipeline@

    __defined at:__ @functions\/callbacks.h:64:6@

    __exported by:__ @functions\/callbacks.h@
-}
executePipeline_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr DataPipeline) -> IO ())
executePipeline_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bc33471040d45469

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_86a8e8897172172b" hs_bindgen_86a8e8897172172b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_runProcessor_ptr@
hs_bindgen_86a8e8897172172b ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr Processor) -> IO ()))
hs_bindgen_86a8e8897172172b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_86a8e8897172172b_base

{-# NOINLINE runProcessor_ptr #-}

{-| __C declaration:__ @runProcessor@

    __defined at:__ @functions\/callbacks.h:80:6@

    __exported by:__ @functions\/callbacks.h@
-}
runProcessor_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.Ptr Processor) -> IO ())
runProcessor_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_86a8e8897172172b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f4667aed4d51fd75" hs_bindgen_f4667aed4d51fd75_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_processMeasurementWithValidation_ptr@
hs_bindgen_f4667aed4d51fd75 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())) -> IO ()))
hs_bindgen_f4667aed4d51fd75 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f4667aed4d51fd75_base

{-# NOINLINE processMeasurementWithValidation_ptr #-}

{-| __C declaration:__ @processMeasurementWithValidation@

    __defined at:__ @functions\/callbacks.h:85:6@

    __exported by:__ @functions\/callbacks.h@
-}
processMeasurementWithValidation_ptr :: Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> (Ptr.FunPtr ((Ptr.Ptr Measurement) -> DataValidator -> FC.CInt -> IO ())) -> DataValidator -> IO ())) -> IO ())
processMeasurementWithValidation_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f4667aed4d51fd75

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_77b468218b567b37" hs_bindgen_77b468218b567b37_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_f_ptr@
hs_bindgen_77b468218b567b37 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (Foo -> IO ())) -> IO ()))
hs_bindgen_77b468218b567b37 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_77b468218b567b37_base

{-# NOINLINE f_ptr #-}

{-| __C declaration:__ @f@

    __defined at:__ @functions\/callbacks.h:96:6@

    __exported by:__ @functions\/callbacks.h@
-}
f_ptr :: Ptr.FunPtr ((Ptr.FunPtr (Foo -> IO ())) -> IO ())
f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_77b468218b567b37

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d5a4de10d670d97d" hs_bindgen_d5a4de10d670d97d_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionscallbacks_Example_get_f2_ptr@
hs_bindgen_d5a4de10d670d97d ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (Foo2 -> IO ())) -> IO ()))
hs_bindgen_d5a4de10d670d97d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_d5a4de10d670d97d_base

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/callbacks.h:97:6@

    __exported by:__ @functions\/callbacks.h@
-}
f2_ptr :: Ptr.FunPtr ((Ptr.FunPtr (Foo2 -> IO ())) -> IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d5a4de10d670d97d
