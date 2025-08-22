{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-foreign-imports #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Block
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <iterator.h>\nToggle hs_bindgen_test_iterator_4f34fce61cc68c9f (_Bool arg1) { return makeToggle(arg1); }\n/* get_makeToggle_ptr */ __attribute__ ((const)) Toggle (*hs_bindgen_test_iterator_03950e0c09bdb508 (void)) (_Bool arg1) { return &makeToggle; } \n_Bool hs_bindgen_test_iterator_bfb4e32e3a824c7e (Toggle arg1) { return toggleNext(arg1); }\n/* get_toggleNext_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_iterator_9c2755ef750f5d45 (void)) (Toggle arg1) { return &toggleNext; } \nvoid hs_bindgen_test_iterator_8d23fba933ba9584 (Toggle arg1) { releaseToggle(arg1); }\n/* get_releaseToggle_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_2f7023ef559c7cdc (void)) (Toggle arg1) { return &releaseToggle; } \nCounter hs_bindgen_test_iterator_5b455070cb6127b9 (signed int arg1, signed int arg2) { return makeCounter(arg1, arg2); }\n/* get_makeCounter_ptr */ __attribute__ ((const)) Counter (*hs_bindgen_test_iterator_216174b924f641ef (void)) (signed int arg1, signed int arg2) { return &makeCounter; } \nsigned int hs_bindgen_test_iterator_1eb9473844c466c6 (Counter arg1) { return counterNext(arg1); }\n/* get_counterNext_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_iterator_9d967a23215cebaa (void)) (Counter arg1) { return &counterNext; } \nvoid hs_bindgen_test_iterator_4bd3562b992f2f1c (Counter arg1) { releaseCounter(arg1); }\n/* get_releaseCounter_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_315c7ff0ed90e2c8 (void)) (Counter arg1) { return &releaseCounter; } \nVarCounter hs_bindgen_test_iterator_0fc005ef62990438 (signed int arg1) { return makeVarCounter(arg1); }\n/* get_makeVarCounter_ptr */ __attribute__ ((const)) VarCounter (*hs_bindgen_test_iterator_a29c0a830311b22a (void)) (signed int arg1) { return &makeVarCounter; } \nsigned int hs_bindgen_test_iterator_a88cd5c9559b5d52 (VarCounter arg1, signed int arg2) { return varCounterNext(arg1, arg2); }\n/* get_varCounterNext_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_iterator_cd9433fb0fa76d19 (void)) (VarCounter arg1, signed int arg2) { return &varCounterNext; } \nvoid hs_bindgen_test_iterator_2d2d26e60eea04a8 (VarCounter arg1) { releaseVarCounter(arg1); }\n/* get_releaseVarCounter_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_bad5305a8cb077b0 (void)) (VarCounter arg1) { return &releaseVarCounter; } \n")

newtype Toggle = Toggle
  { un_Toggle :: HsBindgen.Runtime.Block.Block (IO FC.CBool)
  }

foreign import ccall safe "hs_bindgen_test_iterator_4f34fce61cc68c9f" makeToggle
  :: FC.CBool
     {- ^ __from C:__ @start@ -}
  -> IO Toggle

foreign import ccall safe "hs_bindgen_test_iterator_03950e0c09bdb508" makeToggle_ptr
  :: F.FunPtr (FC.CBool -> IO Toggle)

foreign import ccall safe "hs_bindgen_test_iterator_bfb4e32e3a824c7e" toggleNext
  :: Toggle
     {- ^ __from C:__ @block@ -}
  -> IO FC.CBool

foreign import ccall safe "hs_bindgen_test_iterator_9c2755ef750f5d45" toggleNext_ptr
  :: F.FunPtr (Toggle -> IO FC.CBool)

foreign import ccall safe "hs_bindgen_test_iterator_8d23fba933ba9584" releaseToggle
  :: Toggle
     {- ^ __from C:__ @block@ -}
  -> IO ()

foreign import ccall safe "hs_bindgen_test_iterator_2f7023ef559c7cdc" releaseToggle_ptr
  :: F.FunPtr (Toggle -> IO ())

newtype Counter = Counter
  { un_Counter :: HsBindgen.Runtime.Block.Block (IO FC.CInt)
  }

foreign import ccall safe "hs_bindgen_test_iterator_5b455070cb6127b9" makeCounter
  :: FC.CInt
     {- ^ __from C:__ @start@ -}
  -> FC.CInt
     {- ^ __from C:__ @increment@ -}
  -> IO Counter

foreign import ccall safe "hs_bindgen_test_iterator_216174b924f641ef" makeCounter_ptr
  :: F.FunPtr (FC.CInt -> FC.CInt -> IO Counter)

foreign import ccall safe "hs_bindgen_test_iterator_1eb9473844c466c6" counterNext
  :: Counter
     {- ^ __from C:__ @block@ -}
  -> IO FC.CInt

foreign import ccall safe "hs_bindgen_test_iterator_9d967a23215cebaa" counterNext_ptr
  :: F.FunPtr (Counter -> IO FC.CInt)

foreign import ccall safe "hs_bindgen_test_iterator_4bd3562b992f2f1c" releaseCounter
  :: Counter
     {- ^ __from C:__ @block@ -}
  -> IO ()

foreign import ccall safe "hs_bindgen_test_iterator_315c7ff0ed90e2c8" releaseCounter_ptr
  :: F.FunPtr (Counter -> IO ())

newtype VarCounter = VarCounter
  { un_VarCounter :: HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt)
  }

foreign import ccall safe "hs_bindgen_test_iterator_0fc005ef62990438" makeVarCounter
  :: FC.CInt
     {- ^ __from C:__ @start@ -}
  -> IO VarCounter

foreign import ccall safe "hs_bindgen_test_iterator_a29c0a830311b22a" makeVarCounter_ptr
  :: F.FunPtr (FC.CInt -> IO VarCounter)

foreign import ccall safe "hs_bindgen_test_iterator_a88cd5c9559b5d52" varCounterNext
  :: VarCounter
     {- ^ __from C:__ @block@ -}
  -> FC.CInt
     {- ^ __from C:__ @increment@ -}
  -> IO FC.CInt

foreign import ccall safe "hs_bindgen_test_iterator_cd9433fb0fa76d19" varCounterNext_ptr
  :: F.FunPtr (VarCounter -> FC.CInt -> IO FC.CInt)

foreign import ccall safe "hs_bindgen_test_iterator_2d2d26e60eea04a8" releaseVarCounter
  :: VarCounter
     {- ^ __from C:__ @block@ -}
  -> IO ()

foreign import ccall safe "hs_bindgen_test_iterator_bad5305a8cb077b0" releaseVarCounter_ptr
  :: F.FunPtr (VarCounter -> IO ())
