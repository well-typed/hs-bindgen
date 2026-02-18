{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_square@
hs_bindgen_8c6beff641297a13 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_8c6beff641297a13 =
  RIP.fromFFIType hs_bindgen_8c6beff641297a13_base

{-| __C declaration:__ @square@

    __defined at:__ @manual\/function_pointers.h 5:12@

    __exported by:__ @manual\/function_pointers.h@
-}
square ::
     RIP.CInt
  -> IO RIP.CInt
square = hs_bindgen_8c6beff641297a13

-- __unique:__ @test_manualfunction_pointers_Example_Safe_plus@
foreign import ccall safe "hs_bindgen_3dfb239ac098f471" hs_bindgen_3dfb239ac098f471_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_plus@
hs_bindgen_3dfb239ac098f471 ::
     RIP.CInt
  -> RIP.CInt
  -> IO RIP.CInt
hs_bindgen_3dfb239ac098f471 =
  RIP.fromFFIType hs_bindgen_3dfb239ac098f471_base

{-| __C declaration:__ @plus@

    __defined at:__ @manual\/function_pointers.h 7:12@

    __exported by:__ @manual\/function_pointers.h@
-}
plus ::
     RIP.CInt
  -> RIP.CInt
  -> IO RIP.CInt
plus = hs_bindgen_3dfb239ac098f471

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1@
foreign import ccall safe "hs_bindgen_983beb37938c4d96" hs_bindgen_983beb37938c4d96_base ::
     RIP.FunPtr RIP.Void
  -> RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1@
hs_bindgen_983beb37938c4d96 ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
  -> RIP.CInt
  -> IO RIP.CInt
hs_bindgen_983beb37938c4d96 =
  RIP.fromFFIType hs_bindgen_983beb37938c4d96_base

{-| __C declaration:__ @apply1@

    __defined at:__ @manual\/function_pointers.h 9:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply1 ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
     -- ^ __C declaration:__ @f@
  -> RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
apply1 = hs_bindgen_983beb37938c4d96

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply2@
foreign import ccall safe "hs_bindgen_8a62074f5475563b" hs_bindgen_8a62074f5475563b_base ::
     RIP.FunPtr RIP.Void
  -> RIP.Int32
  -> RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply2@
hs_bindgen_8a62074f5475563b ::
     RIP.FunPtr (RIP.CInt -> RIP.CInt -> IO RIP.CInt)
  -> RIP.CInt
  -> RIP.CInt
  -> IO RIP.CInt
hs_bindgen_8a62074f5475563b =
  RIP.fromFFIType hs_bindgen_8a62074f5475563b_base

{-| __C declaration:__ @apply2@

    __defined at:__ @manual\/function_pointers.h 11:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply2 ::
     RIP.FunPtr (RIP.CInt -> RIP.CInt -> IO RIP.CInt)
     -- ^ __C declaration:__ @f@
  -> RIP.CInt
     -- ^ __C declaration:__ @x@
  -> RIP.CInt
     -- ^ __C declaration:__ @y@
  -> IO RIP.CInt
apply2 = hs_bindgen_8a62074f5475563b

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_pointer_arg@
foreign import ccall safe "hs_bindgen_229d4041a92cd6b6" hs_bindgen_229d4041a92cd6b6_base ::
     RIP.FunPtr RIP.Void
  -> RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_pointer_arg@
hs_bindgen_229d4041a92cd6b6 ::
     RIP.FunPtr Int2int
  -> RIP.CInt
  -> IO RIP.CInt
hs_bindgen_229d4041a92cd6b6 =
  RIP.fromFFIType hs_bindgen_229d4041a92cd6b6_base

{-| Basically the same as apply1(), but here for illustratory purposes.

__C declaration:__ @apply1_pointer_arg@

__defined at:__ @manual\/function_pointers.h 22:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_pointer_arg ::
     RIP.FunPtr Int2int
  -> RIP.CInt
  -> IO RIP.CInt
apply1_pointer_arg = hs_bindgen_229d4041a92cd6b6

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_nopointer_arg@
foreign import ccall safe "hs_bindgen_de9f1109e03648e4" hs_bindgen_de9f1109e03648e4_base ::
     RIP.FunPtr RIP.Void
  -> RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_nopointer_arg@
hs_bindgen_de9f1109e03648e4 ::
     RIP.FunPtr Int2int
  -> RIP.CInt
  -> IO RIP.CInt
hs_bindgen_de9f1109e03648e4 =
  RIP.fromFFIType hs_bindgen_de9f1109e03648e4_base

{-| A version of apply1_pointer_arg() that declares to take a argument of function type, rather than a pointer-to-function type.

__C declaration:__ @apply1_nopointer_arg@

__defined at:__ @manual\/function_pointers.h 26:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_arg ::
     RIP.FunPtr Int2int
  -> RIP.CInt
  -> IO RIP.CInt
apply1_nopointer_arg = hs_bindgen_de9f1109e03648e4

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_nopointer_res@
foreign import ccall safe "hs_bindgen_8bea6b2106c55d5b" hs_bindgen_8bea6b2106c55d5b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_manualfunction_pointers_Example_Safe_apply1_nopointer_res@
hs_bindgen_8bea6b2106c55d5b :: IO (RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt))
hs_bindgen_8bea6b2106c55d5b =
  RIP.fromFFIType hs_bindgen_8bea6b2106c55d5b_base

{-| A function returning a pointer to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_res@

__defined at:__ @manual\/function_pointers.h 31:21@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_res :: IO (RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt))
apply1_nopointer_res = hs_bindgen_8bea6b2106c55d5b
