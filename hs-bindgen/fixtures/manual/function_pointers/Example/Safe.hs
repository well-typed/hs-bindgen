{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <manual/function_pointers.h>"
  , "signed int hs_bindgen_8c6beff641297a13 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square(arg1);"
  , "}"
  , "signed int hs_bindgen_3dfb239ac098f471 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return plus(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_983beb37938c4d96 ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_8a62074f5475563b ("
  , "  signed int (*arg1) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , "),"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return apply2(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_229d4041a92cd6b6 ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1_pointer_arg(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_de9f1109e03648e4 ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1_nopointer_arg(arg1, arg2);"
  , "}"
  , "signed int (*const hs_bindgen_8bea6b2106c55d5b (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return apply1_nopointer_res();"
  , "}"
  ]))

-- __unique:__ @test_manualfunction_pointers_Example_Safe_square@
foreign import ccall safe "hs_bindgen_8c6beff641297a13" hs_bindgen_8c6beff641297a13_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_square@
hs_bindgen_8c6beff641297a13 ::
     FC.CInt
  -> IO FC.CInt
hs_bindgen_8c6beff641297a13 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8c6beff641297a13_base

{-| __C declaration:__ @square@

    __defined at:__ @manual\/function_pointers.h 5:12@

    __exported by:__ @manual\/function_pointers.h@
-}
square ::
     FC.CInt
  -> IO FC.CInt
square = hs_bindgen_8c6beff641297a13

-- __unique:__ @test_manualfunction_pointers_Example_Safe_plus@
foreign import ccall safe "hs_bindgen_3dfb239ac098f471" hs_bindgen_3dfb239ac098f471_base ::
     GHC.Int.Int32
  -> GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_plus@
hs_bindgen_3dfb239ac098f471 ::
     FC.CInt
  -> FC.CInt
  -> IO FC.CInt
hs_bindgen_3dfb239ac098f471 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3dfb239ac098f471_base

{-| __C declaration:__ @plus@

    __defined at:__ @manual\/function_pointers.h 7:12@

    __exported by:__ @manual\/function_pointers.h@
-}
plus ::
     FC.CInt
  -> FC.CInt
  -> IO FC.CInt
plus = hs_bindgen_3dfb239ac098f471

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1@
foreign import ccall safe "hs_bindgen_983beb37938c4d96" hs_bindgen_983beb37938c4d96_base ::
     Ptr.FunPtr Void
  -> GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1@
hs_bindgen_983beb37938c4d96 ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> FC.CInt
  -> IO FC.CInt
hs_bindgen_983beb37938c4d96 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_983beb37938c4d96_base

{-| __C declaration:__ @apply1@

    __defined at:__ @manual\/function_pointers.h 9:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply1 ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @f@
  -> FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO FC.CInt
apply1 = hs_bindgen_983beb37938c4d96

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply2@
foreign import ccall safe "hs_bindgen_8a62074f5475563b" hs_bindgen_8a62074f5475563b_base ::
     Ptr.FunPtr Void
  -> GHC.Int.Int32
  -> GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply2@
hs_bindgen_8a62074f5475563b ::
     Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
  -> FC.CInt
  -> FC.CInt
  -> IO FC.CInt
hs_bindgen_8a62074f5475563b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8a62074f5475563b_base

{-| __C declaration:__ @apply2@

    __defined at:__ @manual\/function_pointers.h 11:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply2 ::
     Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @f@
  -> FC.CInt
     -- ^ __C declaration:__ @x@
  -> FC.CInt
     -- ^ __C declaration:__ @y@
  -> IO FC.CInt
apply2 = hs_bindgen_8a62074f5475563b

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_pointer_arg@
foreign import ccall safe "hs_bindgen_229d4041a92cd6b6" hs_bindgen_229d4041a92cd6b6_base ::
     Ptr.FunPtr Void
  -> GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_pointer_arg@
hs_bindgen_229d4041a92cd6b6 ::
     Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt
hs_bindgen_229d4041a92cd6b6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_229d4041a92cd6b6_base

{-| Basically the same as apply1(), but here for illustratory purposes.

__C declaration:__ @apply1_pointer_arg@

__defined at:__ @manual\/function_pointers.h 22:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_pointer_arg ::
     Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt
apply1_pointer_arg = hs_bindgen_229d4041a92cd6b6

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_nopointer_arg@
foreign import ccall safe "hs_bindgen_de9f1109e03648e4" hs_bindgen_de9f1109e03648e4_base ::
     Ptr.FunPtr Void
  -> GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_nopointer_arg@
hs_bindgen_de9f1109e03648e4 ::
     Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt
hs_bindgen_de9f1109e03648e4 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_de9f1109e03648e4_base

{-| A version of apply1_pointer_arg() that declares to take a argument of function type, rather than a pointer-to-function type.

__C declaration:__ @apply1_nopointer_arg@

__defined at:__ @manual\/function_pointers.h 26:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_arg ::
     Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt
apply1_nopointer_arg = hs_bindgen_de9f1109e03648e4

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_nopointer_res@
foreign import ccall safe "hs_bindgen_8bea6b2106c55d5b" hs_bindgen_8bea6b2106c55d5b_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_nopointer_res@
hs_bindgen_8bea6b2106c55d5b :: IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
hs_bindgen_8bea6b2106c55d5b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8bea6b2106c55d5b_base

{-| A function returning a pointer to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_res@

__defined at:__ @manual\/function_pointers.h 31:21@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_res :: IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
apply1_nopointer_res = hs_bindgen_8bea6b2106c55d5b
