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
  , "/* Example_get_makeToggle_ptr */"
  , "__attribute__ ((const))"
  , "Toggle (*hs_bindgen_test_edgecasesiterator_e47ab40cc44ba40c (void)) ("
  , "  _Bool arg1"
  , ")"
  , "{"
  , "  return &makeToggle;"
  , "}"
  , "/* Example_get_toggleNext_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_edgecasesiterator_9b9bd4761bf90823 (void)) ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return &toggleNext;"
  , "}"
  , "/* Example_get_releaseToggle_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesiterator_e844e0fabbfdaa6f (void)) ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return &releaseToggle;"
  , "}"
  , "/* Example_get_makeCounter_ptr */"
  , "__attribute__ ((const))"
  , "Counter (*hs_bindgen_test_edgecasesiterator_1f78798c907de064 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &makeCounter;"
  , "}"
  , "/* Example_get_counterNext_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_edgecasesiterator_a0e390bd34a86f16 (void)) ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return &counterNext;"
  , "}"
  , "/* Example_get_releaseCounter_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesiterator_99ac4983c28b3141 (void)) ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return &releaseCounter;"
  , "}"
  , "/* Example_get_makeVarCounter_ptr */"
  , "__attribute__ ((const))"
  , "VarCounter (*hs_bindgen_test_edgecasesiterator_840426e6a0f31bb9 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &makeVarCounter;"
  , "}"
  , "/* Example_get_varCounterNext_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_edgecasesiterator_7bd9280f90a7cdb0 (void)) ("
  , "  VarCounter arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &varCounterNext;"
  , "}"
  , "/* Example_get_releaseVarCounter_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesiterator_772a82dba2c79bee (void)) ("
  , "  VarCounter arg1"
  , ")"
  , "{"
  , "  return &releaseVarCounter;"
  , "}"
  ]))

{-| __unique:__ @Example_get_makeToggle_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_e47ab40cc44ba40c" hs_bindgen_test_edgecasesiterator_e47ab40cc44ba40c ::
     IO (Ptr.FunPtr (FC.CBool -> IO Toggle))

{-# NOINLINE makeToggle_ptr #-}

{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h:4:8@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeToggle_ptr :: Ptr.FunPtr (FC.CBool -> IO Toggle)
makeToggle_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_e47ab40cc44ba40c

{-| __unique:__ @Example_get_toggleNext_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_9b9bd4761bf90823" hs_bindgen_test_edgecasesiterator_9b9bd4761bf90823 ::
     IO (Ptr.FunPtr (Toggle -> IO FC.CBool))

{-# NOINLINE toggleNext_ptr #-}

{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h:5:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
toggleNext_ptr :: Ptr.FunPtr (Toggle -> IO FC.CBool)
toggleNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_9b9bd4761bf90823

{-| __unique:__ @Example_get_releaseToggle_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_e844e0fabbfdaa6f" hs_bindgen_test_edgecasesiterator_e844e0fabbfdaa6f ::
     IO (Ptr.FunPtr (Toggle -> IO ()))

{-# NOINLINE releaseToggle_ptr #-}

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @edge-cases\/iterator.h:6:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseToggle_ptr :: Ptr.FunPtr (Toggle -> IO ())
releaseToggle_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_e844e0fabbfdaa6f

{-| __unique:__ @Example_get_makeCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_1f78798c907de064" hs_bindgen_test_edgecasesiterator_1f78798c907de064 ::
     IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO Counter))

{-# NOINLINE makeCounter_ptr #-}

{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h:11:9@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeCounter_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO Counter)
makeCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_1f78798c907de064

{-| __unique:__ @Example_get_counterNext_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_a0e390bd34a86f16" hs_bindgen_test_edgecasesiterator_a0e390bd34a86f16 ::
     IO (Ptr.FunPtr (Counter -> IO FC.CInt))

{-# NOINLINE counterNext_ptr #-}

{-| __C declaration:__ @counterNext@

    __defined at:__ @edge-cases\/iterator.h:12:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
counterNext_ptr :: Ptr.FunPtr (Counter -> IO FC.CInt)
counterNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_a0e390bd34a86f16

{-| __unique:__ @Example_get_releaseCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_99ac4983c28b3141" hs_bindgen_test_edgecasesiterator_99ac4983c28b3141 ::
     IO (Ptr.FunPtr (Counter -> IO ()))

{-# NOINLINE releaseCounter_ptr #-}

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @edge-cases\/iterator.h:13:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseCounter_ptr :: Ptr.FunPtr (Counter -> IO ())
releaseCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_99ac4983c28b3141

{-| __unique:__ @Example_get_makeVarCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_840426e6a0f31bb9" hs_bindgen_test_edgecasesiterator_840426e6a0f31bb9 ::
     IO (Ptr.FunPtr (FC.CInt -> IO VarCounter))

{-# NOINLINE makeVarCounter_ptr #-}

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h:18:12@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeVarCounter_ptr :: Ptr.FunPtr (FC.CInt -> IO VarCounter)
makeVarCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_840426e6a0f31bb9

{-| __unique:__ @Example_get_varCounterNext_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_7bd9280f90a7cdb0" hs_bindgen_test_edgecasesiterator_7bd9280f90a7cdb0 ::
     IO (Ptr.FunPtr (VarCounter -> FC.CInt -> IO FC.CInt))

{-# NOINLINE varCounterNext_ptr #-}

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h:19:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
varCounterNext_ptr :: Ptr.FunPtr (VarCounter -> FC.CInt -> IO FC.CInt)
varCounterNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_7bd9280f90a7cdb0

{-| __unique:__ @Example_get_releaseVarCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_772a82dba2c79bee" hs_bindgen_test_edgecasesiterator_772a82dba2c79bee ::
     IO (Ptr.FunPtr (VarCounter -> IO ()))

{-# NOINLINE releaseVarCounter_ptr #-}

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @edge-cases\/iterator.h:20:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseVarCounter_ptr :: Ptr.FunPtr (VarCounter -> IO ())
releaseVarCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesiterator_772a82dba2c79bee
