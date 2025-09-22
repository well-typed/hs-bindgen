{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Block
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource "#include <iterator.h>\nToggle hs_bindgen_test_iterator_900530c0457bf5ee (_Bool arg1) { return makeToggle(arg1); }\n_Bool hs_bindgen_test_iterator_0584fdb88b39872d (Toggle arg1) { return toggleNext(arg1); }\nvoid hs_bindgen_test_iterator_6d013ab8b38fc1d9 (Toggle arg1) { releaseToggle(arg1); }\nCounter hs_bindgen_test_iterator_10d749b6b17037ba (signed int arg1, signed int arg2) { return makeCounter(arg1, arg2); }\nsigned int hs_bindgen_test_iterator_9695aacc59a66573 (Counter arg1) { return counterNext(arg1); }\nvoid hs_bindgen_test_iterator_49a436aa15c7ea70 (Counter arg1) { releaseCounter(arg1); }\nVarCounter hs_bindgen_test_iterator_f12a50c4468834b5 (signed int arg1) { return makeVarCounter(arg1); }\nsigned int hs_bindgen_test_iterator_2c8a2667de9b1ffb (VarCounter arg1, signed int arg2) { return varCounterNext(arg1, arg2); }\nvoid hs_bindgen_test_iterator_023ae1f9abf46556 (VarCounter arg1) { releaseVarCounter(arg1); }\n/* get_makeToggle_ptr */ __attribute__ ((const)) Toggle (*hs_bindgen_test_iterator_504a6a44ef649697 (void)) (_Bool arg1) { return &makeToggle; } \n/* get_toggleNext_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_iterator_ee784d0363e34151 (void)) (Toggle arg1) { return &toggleNext; } \n/* get_releaseToggle_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_864850832eaf96b9 (void)) (Toggle arg1) { return &releaseToggle; } \n/* get_makeCounter_ptr */ __attribute__ ((const)) Counter (*hs_bindgen_test_iterator_48b98d306e2a8d53 (void)) (signed int arg1, signed int arg2) { return &makeCounter; } \n/* get_counterNext_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_iterator_aeb21db5034e4d66 (void)) (Counter arg1) { return &counterNext; } \n/* get_releaseCounter_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_8e1661e238f6f451 (void)) (Counter arg1) { return &releaseCounter; } \n/* get_makeVarCounter_ptr */ __attribute__ ((const)) VarCounter (*hs_bindgen_test_iterator_b14e88e9cf7a56b8 (void)) (signed int arg1) { return &makeVarCounter; } \n/* get_varCounterNext_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_iterator_4d10204c4166188d (void)) (VarCounter arg1, signed int arg2) { return &varCounterNext; } \n/* get_releaseVarCounter_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_iterator_bde04ef01335be42 (void)) (VarCounter arg1) { return &releaseVarCounter; } \n")

{-| __C declaration:__ @Toggle@

    __defined at:__ @iterator.h:3:16@

    __exported by:__ @iterator.h@
-}
newtype Toggle = Toggle
  { un_Toggle :: HsBindgen.Runtime.Block.Block (IO FC.CBool)
  }

{-| __C declaration:__ @Counter@

    __defined at:__ @iterator.h:10:14@

    __exported by:__ @iterator.h@
-}
newtype Counter = Counter
  { un_Counter :: HsBindgen.Runtime.Block.Block (IO FC.CInt)
  }

{-| __C declaration:__ @VarCounter@

    __defined at:__ @iterator.h:17:14@

    __exported by:__ @iterator.h@
-}
newtype VarCounter = VarCounter
  { un_VarCounter :: HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt)
  }

{-| __C declaration:__ @makeToggle@

    __defined at:__ @iterator.h:4:8@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_900530c0457bf5ee" makeToggle
  :: FC.CBool
     {- ^ __C declaration:__ @start@
     -}
  -> IO Toggle

{-| __C declaration:__ @toggleNext@

    __defined at:__ @iterator.h:5:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_0584fdb88b39872d" toggleNext
  :: Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @iterator.h:6:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_6d013ab8b38fc1d9" releaseToggle
  :: Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()

{-| __C declaration:__ @makeCounter@

    __defined at:__ @iterator.h:11:9@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_10d749b6b17037ba" makeCounter
  :: FC.CInt
     {- ^ __C declaration:__ @start@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @increment@
     -}
  -> IO Counter

{-| __C declaration:__ @counterNext@

    __defined at:__ @iterator.h:12:5@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_9695aacc59a66573" counterNext
  :: Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @iterator.h:13:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_49a436aa15c7ea70" releaseCounter
  :: Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @iterator.h:18:12@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_f12a50c4468834b5" makeVarCounter
  :: FC.CInt
     {- ^ __C declaration:__ @start@
     -}
  -> IO VarCounter

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @iterator.h:19:5@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_2c8a2667de9b1ffb" varCounterNext
  :: VarCounter
     {- ^ __C declaration:__ @block@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @increment@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @iterator.h:20:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_023ae1f9abf46556" releaseVarCounter
  :: VarCounter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()

{-| __C declaration:__ @makeToggle@

    __defined at:__ @iterator.h:4:8@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_504a6a44ef649697" hs_bindgen_test_iterator_504a6a44ef649697
  :: IO (Ptr.FunPtr (FC.CBool -> IO Toggle))

{-# NOINLINE makeToggle_ptr #-}

makeToggle_ptr :: Ptr.FunPtr (FC.CBool -> IO Toggle)
makeToggle_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_iterator_504a6a44ef649697

{-| __C declaration:__ @toggleNext@

    __defined at:__ @iterator.h:5:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_ee784d0363e34151" hs_bindgen_test_iterator_ee784d0363e34151
  :: IO (Ptr.FunPtr (Toggle -> IO FC.CBool))

{-# NOINLINE toggleNext_ptr #-}

toggleNext_ptr :: Ptr.FunPtr (Toggle -> IO FC.CBool)
toggleNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_iterator_ee784d0363e34151

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @iterator.h:6:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_864850832eaf96b9" hs_bindgen_test_iterator_864850832eaf96b9
  :: IO (Ptr.FunPtr (Toggle -> IO ()))

{-# NOINLINE releaseToggle_ptr #-}

releaseToggle_ptr :: Ptr.FunPtr (Toggle -> IO ())
releaseToggle_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_iterator_864850832eaf96b9

{-| __C declaration:__ @makeCounter@

    __defined at:__ @iterator.h:11:9@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_48b98d306e2a8d53" hs_bindgen_test_iterator_48b98d306e2a8d53
  :: IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO Counter))

{-# NOINLINE makeCounter_ptr #-}

makeCounter_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO Counter)
makeCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_iterator_48b98d306e2a8d53

{-| __C declaration:__ @counterNext@

    __defined at:__ @iterator.h:12:5@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_aeb21db5034e4d66" hs_bindgen_test_iterator_aeb21db5034e4d66
  :: IO (Ptr.FunPtr (Counter -> IO FC.CInt))

{-# NOINLINE counterNext_ptr #-}

counterNext_ptr :: Ptr.FunPtr (Counter -> IO FC.CInt)
counterNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_iterator_aeb21db5034e4d66

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @iterator.h:13:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_8e1661e238f6f451" hs_bindgen_test_iterator_8e1661e238f6f451
  :: IO (Ptr.FunPtr (Counter -> IO ()))

{-# NOINLINE releaseCounter_ptr #-}

releaseCounter_ptr :: Ptr.FunPtr (Counter -> IO ())
releaseCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_iterator_8e1661e238f6f451

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @iterator.h:18:12@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_b14e88e9cf7a56b8" hs_bindgen_test_iterator_b14e88e9cf7a56b8
  :: IO (Ptr.FunPtr (FC.CInt -> IO VarCounter))

{-# NOINLINE makeVarCounter_ptr #-}

makeVarCounter_ptr :: Ptr.FunPtr (FC.CInt -> IO VarCounter)
makeVarCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_iterator_b14e88e9cf7a56b8

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @iterator.h:19:5@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_4d10204c4166188d" hs_bindgen_test_iterator_4d10204c4166188d
  :: IO (Ptr.FunPtr (VarCounter -> FC.CInt -> IO FC.CInt))

{-# NOINLINE varCounterNext_ptr #-}

varCounterNext_ptr :: Ptr.FunPtr (VarCounter -> FC.CInt -> IO FC.CInt)
varCounterNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_iterator_4d10204c4166188d

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @iterator.h:20:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_bde04ef01335be42" hs_bindgen_test_iterator_bde04ef01335be42
  :: IO (Ptr.FunPtr (VarCounter -> IO ()))

{-# NOINLINE releaseVarCounter_ptr #-}

releaseVarCounter_ptr :: Ptr.FunPtr (VarCounter -> IO ())
releaseVarCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_iterator_bde04ef01335be42
