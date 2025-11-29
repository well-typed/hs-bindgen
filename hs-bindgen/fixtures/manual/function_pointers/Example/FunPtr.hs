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
  [ "#include <manual/function_pointers.h>"
  , "/* Example_get_square_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_61cbe351b243e6fe (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square;"
  , "}"
  , "/* Example_get_plus_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_52103e09983c1de1 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &plus;"
  , "}"
  , "/* Example_get_apply1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_41ce08a83047be18 (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1;"
  , "}"
  , "/* Example_get_apply2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_409070a4b835a069 (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , "),"
  , "  signed int arg2,"
  , "  signed int arg3"
  , ")"
  , "{"
  , "  return &apply2;"
  , "}"
  , "/* Example_get_apply1_pointer_arg_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_ae8f5df5f683d6e0 (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_pointer_arg;"
  , "}"
  , "/* Example_get_apply1_nopointer_arg_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_480ef37d71c488fe (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_nopointer_arg;"
  , "}"
  , "/* Example_get_apply1_nopointer_res_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*const (*hs_bindgen_test_manualfunction_pointers_d42459b98597bf28 (void)) (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_nopointer_res;"
  , "}"
  ]))

{-| __unique:__ @Example_get_square_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_61cbe351b243e6fe" hs_bindgen_test_manualfunction_pointers_61cbe351b243e6fe ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_ptr #-}

{-| __C declaration:__ @square@

    __defined at:__ @manual\/function_pointers.h:5:12@

    __exported by:__ @manual\/function_pointers.h@
-}
square_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_61cbe351b243e6fe

{-| __unique:__ @Example_get_plus_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_52103e09983c1de1" hs_bindgen_test_manualfunction_pointers_52103e09983c1de1 ::
     IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt))

{-# NOINLINE plus_ptr #-}

{-| __C declaration:__ @plus@

    __defined at:__ @manual\/function_pointers.h:7:12@

    __exported by:__ @manual\/function_pointers.h@
-}
plus_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
plus_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_52103e09983c1de1

{-| __unique:__ @Example_get_apply1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_41ce08a83047be18" hs_bindgen_test_manualfunction_pointers_41ce08a83047be18 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply1_ptr #-}

{-| __C declaration:__ @apply1@

    __defined at:__ @manual\/function_pointers.h:9:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply1_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt)
apply1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_41ce08a83047be18

{-| __unique:__ @Example_get_apply2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_409070a4b835a069" hs_bindgen_test_manualfunction_pointers_409070a4b835a069 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)) -> FC.CInt -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply2_ptr #-}

{-| __C declaration:__ @apply2@

    __defined at:__ @manual\/function_pointers.h:11:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply2_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)) -> FC.CInt -> FC.CInt -> IO FC.CInt)
apply2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_409070a4b835a069

{-| __unique:__ @Example_get_apply1_pointer_arg_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_ae8f5df5f683d6e0" hs_bindgen_test_manualfunction_pointers_ae8f5df5f683d6e0 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply1_pointer_arg_ptr #-}

{-| Basically the same as apply1(), but here for illustratory purposes.

__C declaration:__ @apply1_pointer_arg@

__defined at:__ @manual\/function_pointers.h:22:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_pointer_arg_ptr :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_pointer_arg_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_ae8f5df5f683d6e0

{-| __unique:__ @Example_get_apply1_nopointer_arg_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_480ef37d71c488fe" hs_bindgen_test_manualfunction_pointers_480ef37d71c488fe ::
     IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply1_nopointer_arg_ptr #-}

{-| A version of apply1_pointer_arg() that declares to take a argument of function type, rather than a pointer-to-function type.

__C declaration:__ @apply1_nopointer_arg@

__defined at:__ @manual\/function_pointers.h:26:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_arg_ptr :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_nopointer_arg_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_480ef37d71c488fe

{-| __unique:__ @Example_get_apply1_nopointer_res_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_d42459b98597bf28" hs_bindgen_test_manualfunction_pointers_d42459b98597bf28 ::
     IO (Ptr.FunPtr (IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))))

{-# NOINLINE apply1_nopointer_res_ptr #-}

{-| A function returning a pointer to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_res@

__defined at:__ @manual\/function_pointers.h:31:21@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_res_ptr :: Ptr.FunPtr (IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)))
apply1_nopointer_res_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_d42459b98597bf28
