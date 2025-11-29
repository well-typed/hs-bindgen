{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/iterator.h>"
  , "/* ExampleNothingget_makeToggle_ptr */"
  , "__attribute__ ((const))"
  , "Toggle (*hs_bindgen_test_edgecasesiterator_504a6a44ef649697 (void)) ("
  , "  _Bool arg1"
  , ")"
  , "{"
  , "  return &makeToggle;"
  , "}"
  , "/* ExampleNothingget_toggleNext_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_edgecasesiterator_ee784d0363e34151 (void)) ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return &toggleNext;"
  , "}"
  , "/* ExampleNothingget_releaseToggle_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesiterator_864850832eaf96b9 (void)) ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return &releaseToggle;"
  , "}"
  , "/* ExampleNothingget_makeCounter_ptr */"
  , "__attribute__ ((const))"
  , "Counter (*hs_bindgen_test_edgecasesiterator_48b98d306e2a8d53 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &makeCounter;"
  , "}"
  , "/* ExampleNothingget_counterNext_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_edgecasesiterator_aeb21db5034e4d66 (void)) ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return &counterNext;"
  , "}"
  , "/* ExampleNothingget_releaseCounter_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesiterator_8e1661e238f6f451 (void)) ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return &releaseCounter;"
  , "}"
  , "/* ExampleNothingget_makeVarCounter_ptr */"
  , "__attribute__ ((const))"
  , "VarCounter (*hs_bindgen_test_edgecasesiterator_b14e88e9cf7a56b8 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &makeVarCounter;"
  , "}"
  , "/* ExampleNothingget_varCounterNext_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_edgecasesiterator_4d10204c4166188d (void)) ("
  , "  VarCounter arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &varCounterNext;"
  , "}"
  , "/* ExampleNothingget_releaseVarCounter_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesiterator_bde04ef01335be42 (void)) ("
  , "  VarCounter arg1"
  , ")"
  , "{"
  , "  return &releaseVarCounter;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_makeToggle_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_504a6a44ef649697" hs_bindgen_test_edgecasesiterator_504a6a44ef649697 ::
     IO (Ptr.FunPtr (FC.CBool -> IO Toggle))

{-# NOINLINE makeToggle_ptr #-}

{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h:4:8@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeToggle_ptr :: Ptr.FunPtr (FC.CBool -> IO Toggle)
makeToggle_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_504a6a44ef649697

{-| __unique:__ @ExampleNothingget_toggleNext_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_ee784d0363e34151" hs_bindgen_test_edgecasesiterator_ee784d0363e34151 ::
     IO (Ptr.FunPtr (Toggle -> IO FC.CBool))

{-# NOINLINE toggleNext_ptr #-}

{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h:5:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
toggleNext_ptr :: Ptr.FunPtr (Toggle -> IO FC.CBool)
toggleNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_ee784d0363e34151

{-| __unique:__ @ExampleNothingget_releaseToggle_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_864850832eaf96b9" hs_bindgen_test_edgecasesiterator_864850832eaf96b9 ::
     IO (Ptr.FunPtr (Toggle -> IO ()))

{-# NOINLINE releaseToggle_ptr #-}

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @edge-cases\/iterator.h:6:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseToggle_ptr :: Ptr.FunPtr (Toggle -> IO ())
releaseToggle_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_864850832eaf96b9

{-| __unique:__ @ExampleNothingget_makeCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_48b98d306e2a8d53" hs_bindgen_test_edgecasesiterator_48b98d306e2a8d53 ::
     IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO Counter))

{-# NOINLINE makeCounter_ptr #-}

{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h:11:9@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeCounter_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO Counter)
makeCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_48b98d306e2a8d53

{-| __unique:__ @ExampleNothingget_counterNext_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_aeb21db5034e4d66" hs_bindgen_test_edgecasesiterator_aeb21db5034e4d66 ::
     IO (Ptr.FunPtr (Counter -> IO FC.CInt))

{-# NOINLINE counterNext_ptr #-}

{-| __C declaration:__ @counterNext@

    __defined at:__ @edge-cases\/iterator.h:12:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
counterNext_ptr :: Ptr.FunPtr (Counter -> IO FC.CInt)
counterNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_aeb21db5034e4d66

{-| __unique:__ @ExampleNothingget_releaseCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_8e1661e238f6f451" hs_bindgen_test_edgecasesiterator_8e1661e238f6f451 ::
     IO (Ptr.FunPtr (Counter -> IO ()))

{-# NOINLINE releaseCounter_ptr #-}

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @edge-cases\/iterator.h:13:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseCounter_ptr :: Ptr.FunPtr (Counter -> IO ())
releaseCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_8e1661e238f6f451

{-| __unique:__ @ExampleNothingget_makeVarCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_b14e88e9cf7a56b8" hs_bindgen_test_edgecasesiterator_b14e88e9cf7a56b8 ::
     IO (Ptr.FunPtr (FC.CInt -> IO VarCounter))

{-# NOINLINE makeVarCounter_ptr #-}

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h:18:12@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeVarCounter_ptr :: Ptr.FunPtr (FC.CInt -> IO VarCounter)
makeVarCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_b14e88e9cf7a56b8

{-| __unique:__ @ExampleNothingget_varCounterNext_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_4d10204c4166188d" hs_bindgen_test_edgecasesiterator_4d10204c4166188d ::
     IO (Ptr.FunPtr (VarCounter -> FC.CInt -> IO FC.CInt))

{-# NOINLINE varCounterNext_ptr #-}

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h:19:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
varCounterNext_ptr :: Ptr.FunPtr (VarCounter -> FC.CInt -> IO FC.CInt)
varCounterNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_4d10204c4166188d

{-| __unique:__ @ExampleNothingget_releaseVarCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_bde04ef01335be42" hs_bindgen_test_edgecasesiterator_bde04ef01335be42 ::
     IO (Ptr.FunPtr (VarCounter -> IO ()))

{-# NOINLINE releaseVarCounter_ptr #-}

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @edge-cases\/iterator.h:20:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseVarCounter_ptr :: Ptr.FunPtr (VarCounter -> IO ())
releaseVarCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_bde04ef01335be42
