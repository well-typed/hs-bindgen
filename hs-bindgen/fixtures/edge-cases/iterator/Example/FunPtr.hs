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
  , "/* test_edgecasesiterator_Example_get_makeToggle */"
  , "__attribute__ ((const))"
  , "Toggle (*hs_bindgen_ccdad25a057f8efd (void)) ("
  , "  _Bool arg1"
  , ")"
  , "{"
  , "  return &makeToggle;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_toggleNext */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_fd8e19ba2d78baa9 (void)) ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return &toggleNext;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_releaseToggle */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_780e490698de9df1 (void)) ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return &releaseToggle;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_makeCounter */"
  , "__attribute__ ((const))"
  , "Counter (*hs_bindgen_517dd14dbdb5e3ba (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &makeCounter;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_counterNext */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_28d81dd8ce6aca30 (void)) ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return &counterNext;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_releaseCounter */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_849de94baad0430a (void)) ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return &releaseCounter;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_makeVarCounter */"
  , "__attribute__ ((const))"
  , "VarCounter (*hs_bindgen_8eea20d99febeef3 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &makeVarCounter;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_varCounterNext */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3ae982266c863a3d (void)) ("
  , "  VarCounter arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &varCounterNext;"
  , "}"
  , "/* test_edgecasesiterator_Example_get_releaseVarCounter */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_802a723805cb041f (void)) ("
  , "  VarCounter arg1"
  , ")"
  , "{"
  , "  return &releaseVarCounter;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesiterator_Example_get_makeToggle@
foreign import ccall unsafe "hs_bindgen_ccdad25a057f8efd" hs_bindgen_ccdad25a057f8efd ::
     IO (Ptr.FunPtr (FC.CBool -> IO Toggle))

{-# NOINLINE makeToggle #-}
{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h:4:8@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeToggle :: Ptr.FunPtr (FC.CBool -> IO Toggle)
makeToggle =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ccdad25a057f8efd

-- __unique:__ @test_edgecasesiterator_Example_get_toggleNext@
foreign import ccall unsafe "hs_bindgen_fd8e19ba2d78baa9" hs_bindgen_fd8e19ba2d78baa9 ::
     IO (Ptr.FunPtr (Toggle -> IO FC.CBool))

{-# NOINLINE toggleNext #-}
{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h:5:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
toggleNext :: Ptr.FunPtr (Toggle -> IO FC.CBool)
toggleNext =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fd8e19ba2d78baa9

-- __unique:__ @test_edgecasesiterator_Example_get_releaseToggle@
foreign import ccall unsafe "hs_bindgen_780e490698de9df1" hs_bindgen_780e490698de9df1 ::
     IO (Ptr.FunPtr (Toggle -> IO ()))

{-# NOINLINE releaseToggle #-}
{-| __C declaration:__ @releaseToggle@

    __defined at:__ @edge-cases\/iterator.h:6:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseToggle :: Ptr.FunPtr (Toggle -> IO ())
releaseToggle =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_780e490698de9df1

-- __unique:__ @test_edgecasesiterator_Example_get_makeCounter@
foreign import ccall unsafe "hs_bindgen_517dd14dbdb5e3ba" hs_bindgen_517dd14dbdb5e3ba ::
     IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO Counter))

{-# NOINLINE makeCounter #-}
{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h:11:9@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeCounter :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO Counter)
makeCounter =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_517dd14dbdb5e3ba

-- __unique:__ @test_edgecasesiterator_Example_get_counterNext@
foreign import ccall unsafe "hs_bindgen_28d81dd8ce6aca30" hs_bindgen_28d81dd8ce6aca30 ::
     IO (Ptr.FunPtr (Counter -> IO FC.CInt))

{-# NOINLINE counterNext #-}
{-| __C declaration:__ @counterNext@

    __defined at:__ @edge-cases\/iterator.h:12:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
counterNext :: Ptr.FunPtr (Counter -> IO FC.CInt)
counterNext =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_28d81dd8ce6aca30

-- __unique:__ @test_edgecasesiterator_Example_get_releaseCounter@
foreign import ccall unsafe "hs_bindgen_849de94baad0430a" hs_bindgen_849de94baad0430a ::
     IO (Ptr.FunPtr (Counter -> IO ()))

{-# NOINLINE releaseCounter #-}
{-| __C declaration:__ @releaseCounter@

    __defined at:__ @edge-cases\/iterator.h:13:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseCounter :: Ptr.FunPtr (Counter -> IO ())
releaseCounter =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_849de94baad0430a

-- __unique:__ @test_edgecasesiterator_Example_get_makeVarCounter@
foreign import ccall unsafe "hs_bindgen_8eea20d99febeef3" hs_bindgen_8eea20d99febeef3 ::
     IO (Ptr.FunPtr (FC.CInt -> IO VarCounter))

{-# NOINLINE makeVarCounter #-}
{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h:18:12@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeVarCounter :: Ptr.FunPtr (FC.CInt -> IO VarCounter)
makeVarCounter =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8eea20d99febeef3

-- __unique:__ @test_edgecasesiterator_Example_get_varCounterNext@
foreign import ccall unsafe "hs_bindgen_3ae982266c863a3d" hs_bindgen_3ae982266c863a3d ::
     IO (Ptr.FunPtr (VarCounter -> FC.CInt -> IO FC.CInt))

{-# NOINLINE varCounterNext #-}
{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h:19:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
varCounterNext :: Ptr.FunPtr (VarCounter -> FC.CInt -> IO FC.CInt)
varCounterNext =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3ae982266c863a3d

-- __unique:__ @test_edgecasesiterator_Example_get_releaseVarCounter@
foreign import ccall unsafe "hs_bindgen_802a723805cb041f" hs_bindgen_802a723805cb041f ::
     IO (Ptr.FunPtr (VarCounter -> IO ()))

{-# NOINLINE releaseVarCounter #-}
{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @edge-cases\/iterator.h:20:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseVarCounter :: Ptr.FunPtr (VarCounter -> IO ())
releaseVarCounter =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_802a723805cb041f
