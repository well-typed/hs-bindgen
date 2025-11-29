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
  , "/* test_edgecasesiterator_Example_get_makeToggle_ptr */"
  , "__attribute__ ((const))"
  , "Toggle (*hs_bindgen_7ac156a1e5f0a7d8 (void)) ("
  , "  _Bool arg1"
  , ")"
  , "{"
  , "  return &makeToggle;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_toggleNext_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_ea9e16aea1caff87 (void)) ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return &toggleNext;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_releaseToggle_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1a2eb91b4ecd58ef (void)) ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return &releaseToggle;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_makeCounter_ptr */"
  , "__attribute__ ((const))"
  , "Counter (*hs_bindgen_8254625fbaf5d305 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &makeCounter;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_counterNext_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_aaec448c4bfb8541 (void)) ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return &counterNext;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_releaseCounter_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7046cfbf9189e5b5 (void)) ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return &releaseCounter;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_makeVarCounter_ptr */"
  , "__attribute__ ((const))"
  , "VarCounter (*hs_bindgen_c17e617bab3c2003 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &makeVarCounter;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_varCounterNext_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_429fd2c32d78e77a (void)) ("
  , "  VarCounter arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &varCounterNext;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_releaseVarCounter_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0059f6a0749bda00 (void)) ("
  , "  VarCounter arg1"
  , ")"
  , "{"
  , "  return &releaseVarCounter;"
  , "}"
  ]))

{-| __unique:__ @test_edgecasesiterator_Example_get_makeToggle_ptr@
-}
foreign import ccall unsafe "hs_bindgen_7ac156a1e5f0a7d8" hs_bindgen_7ac156a1e5f0a7d8 ::
     IO (Ptr.FunPtr (FC.CBool -> IO Toggle))

{-# NOINLINE makeToggle_ptr #-}

{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h:4:8@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeToggle_ptr :: Ptr.FunPtr (FC.CBool -> IO Toggle)
makeToggle_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7ac156a1e5f0a7d8

{-| __unique:__ @test_edgecasesiterator_Example_get_toggleNext_ptr@
-}
foreign import ccall unsafe "hs_bindgen_ea9e16aea1caff87" hs_bindgen_ea9e16aea1caff87 ::
     IO (Ptr.FunPtr (Toggle -> IO FC.CBool))

{-# NOINLINE toggleNext_ptr #-}

{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h:5:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
toggleNext_ptr :: Ptr.FunPtr (Toggle -> IO FC.CBool)
toggleNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ea9e16aea1caff87

{-| __unique:__ @test_edgecasesiterator_Example_get_releaseToggle_ptr@
-}
foreign import ccall unsafe "hs_bindgen_1a2eb91b4ecd58ef" hs_bindgen_1a2eb91b4ecd58ef ::
     IO (Ptr.FunPtr (Toggle -> IO ()))

{-# NOINLINE releaseToggle_ptr #-}

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @edge-cases\/iterator.h:6:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseToggle_ptr :: Ptr.FunPtr (Toggle -> IO ())
releaseToggle_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1a2eb91b4ecd58ef

{-| __unique:__ @test_edgecasesiterator_Example_get_makeCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_8254625fbaf5d305" hs_bindgen_8254625fbaf5d305 ::
     IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO Counter))

{-# NOINLINE makeCounter_ptr #-}

{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h:11:9@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeCounter_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO Counter)
makeCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8254625fbaf5d305

{-| __unique:__ @test_edgecasesiterator_Example_get_counterNext_ptr@
-}
foreign import ccall unsafe "hs_bindgen_aaec448c4bfb8541" hs_bindgen_aaec448c4bfb8541 ::
     IO (Ptr.FunPtr (Counter -> IO FC.CInt))

{-# NOINLINE counterNext_ptr #-}

{-| __C declaration:__ @counterNext@

    __defined at:__ @edge-cases\/iterator.h:12:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
counterNext_ptr :: Ptr.FunPtr (Counter -> IO FC.CInt)
counterNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aaec448c4bfb8541

{-| __unique:__ @test_edgecasesiterator_Example_get_releaseCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_7046cfbf9189e5b5" hs_bindgen_7046cfbf9189e5b5 ::
     IO (Ptr.FunPtr (Counter -> IO ()))

{-# NOINLINE releaseCounter_ptr #-}

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @edge-cases\/iterator.h:13:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseCounter_ptr :: Ptr.FunPtr (Counter -> IO ())
releaseCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7046cfbf9189e5b5

{-| __unique:__ @test_edgecasesiterator_Example_get_makeVarCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_c17e617bab3c2003" hs_bindgen_c17e617bab3c2003 ::
     IO (Ptr.FunPtr (FC.CInt -> IO VarCounter))

{-# NOINLINE makeVarCounter_ptr #-}

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h:18:12@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeVarCounter_ptr :: Ptr.FunPtr (FC.CInt -> IO VarCounter)
makeVarCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c17e617bab3c2003

{-| __unique:__ @test_edgecasesiterator_Example_get_varCounterNext_ptr@
-}
foreign import ccall unsafe "hs_bindgen_429fd2c32d78e77a" hs_bindgen_429fd2c32d78e77a ::
     IO (Ptr.FunPtr (VarCounter -> FC.CInt -> IO FC.CInt))

{-# NOINLINE varCounterNext_ptr #-}

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h:19:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
varCounterNext_ptr :: Ptr.FunPtr (VarCounter -> FC.CInt -> IO FC.CInt)
varCounterNext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_429fd2c32d78e77a

{-| __unique:__ @test_edgecasesiterator_Example_get_releaseVarCounter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_0059f6a0749bda00" hs_bindgen_0059f6a0749bda00 ::
     IO (Ptr.FunPtr (VarCounter -> IO ()))

{-# NOINLINE releaseVarCounter_ptr #-}

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @edge-cases\/iterator.h:20:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseVarCounter_ptr :: Ptr.FunPtr (VarCounter -> IO ())
releaseVarCounter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0059f6a0749bda00
