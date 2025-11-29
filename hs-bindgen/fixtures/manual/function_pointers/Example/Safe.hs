{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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

{-| __C declaration:__ @square@

    __defined at:__ @manual\/function_pointers.h:5:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_Safe_square@
-}
foreign import ccall safe "hs_bindgen_8c6beff641297a13" square ::
     FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @plus@

    __defined at:__ @manual\/function_pointers.h:7:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_Safe_plus@
-}
foreign import ccall safe "hs_bindgen_3dfb239ac098f471" plus ::
     FC.CInt
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @apply1@

    __defined at:__ @manual\/function_pointers.h:9:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_Safe_apply1@
-}
foreign import ccall safe "hs_bindgen_983beb37938c4d96" apply1 ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @f@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @apply2@

    __defined at:__ @manual\/function_pointers.h:11:12@

    __exported by:__ @manual\/function_pointers.h@

    __unique:__ @test_manualfunction_pointers_Example_Safe_apply2@
-}
foreign import ccall safe "hs_bindgen_8a62074f5475563b" apply2 ::
     Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @f@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CInt

{-| Basically the same as apply1(), but here for illustratory purposes.

__C declaration:__ @apply1_pointer_arg@

__defined at:__ @manual\/function_pointers.h:22:12@

__exported by:__ @manual\/function_pointers.h@

__unique:__ @test_manualfunction_pointers_Example_Safe_apply1_pointer_arg@
-}
foreign import ccall safe "hs_bindgen_229d4041a92cd6b6" apply1_pointer_arg ::
     Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt

{-| A version of apply1_pointer_arg() that declares to take a argument of function type, rather than a pointer-to-function type.

__C declaration:__ @apply1_nopointer_arg@

__defined at:__ @manual\/function_pointers.h:26:12@

__exported by:__ @manual\/function_pointers.h@

__unique:__ @test_manualfunction_pointers_Example_Safe_apply1_nopointer_arg@
-}
foreign import ccall safe "hs_bindgen_de9f1109e03648e4" apply1_nopointer_arg ::
     Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt

{-| A function returning a pointer to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_res@

__defined at:__ @manual\/function_pointers.h:31:21@

__exported by:__ @manual\/function_pointers.h@

__unique:__ @test_manualfunction_pointers_Example_Safe_apply1_nopointer_res@
-}
foreign import ccall safe "hs_bindgen_8bea6b2106c55d5b" apply1_nopointer_res ::
     IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
