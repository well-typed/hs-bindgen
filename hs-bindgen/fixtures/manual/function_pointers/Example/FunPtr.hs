{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.CAPI
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/function_pointers.h>"
  , "/* test_manualfunction_pointers_Example_get_square */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f0adbe322df05825 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_plus */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_ab2a2131b5f9f197 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &plus;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_b3bb0146641acd39 (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_78245c4946e6da00 (void)) ("
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
  , "/* test_manualfunction_pointers_Example_get_apply1_pointer_arg */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_00276448b1e8af8a (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_pointer_arg;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply1_nopointer_arg */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_00f9010b80cdee19 (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_nopointer_arg;"
  , "}"
  , "/* test_manualfunction_pointers_Example_get_apply1_nopointer_res */"
  , "__attribute__ ((const))"
  , "signed int (*const (*hs_bindgen_16929b4528263721 (void)) (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_nopointer_res;"
  , "}"
  ]))

-- __unique:__ @test_manualfunction_pointers_Example_get_square@
foreign import ccall unsafe "hs_bindgen_f0adbe322df05825" hs_bindgen_f0adbe322df05825_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_square@
hs_bindgen_f0adbe322df05825 :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_f0adbe322df05825 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f0adbe322df05825_base

{-# NOINLINE square #-}
{-| __C declaration:__ @square@

    __defined at:__ @manual\/function_pointers.h 5:12@

    __exported by:__ @manual\/function_pointers.h@
-}
square :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f0adbe322df05825

-- __unique:__ @test_manualfunction_pointers_Example_get_plus@
foreign import ccall unsafe "hs_bindgen_ab2a2131b5f9f197" hs_bindgen_ab2a2131b5f9f197_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_plus@
hs_bindgen_ab2a2131b5f9f197 :: IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt))
hs_bindgen_ab2a2131b5f9f197 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ab2a2131b5f9f197_base

{-# NOINLINE plus #-}
{-| __C declaration:__ @plus@

    __defined at:__ @manual\/function_pointers.h 7:12@

    __exported by:__ @manual\/function_pointers.h@
-}
plus :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
plus =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ab2a2131b5f9f197

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1@
foreign import ccall unsafe "hs_bindgen_b3bb0146641acd39" hs_bindgen_b3bb0146641acd39_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1@
hs_bindgen_b3bb0146641acd39 :: IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt))
hs_bindgen_b3bb0146641acd39 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_b3bb0146641acd39_base

{-# NOINLINE apply1 #-}
{-| __C declaration:__ @apply1@

    __defined at:__ @manual\/function_pointers.h 9:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply1 :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt)
apply1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b3bb0146641acd39

-- __unique:__ @test_manualfunction_pointers_Example_get_apply2@
foreign import ccall unsafe "hs_bindgen_78245c4946e6da00" hs_bindgen_78245c4946e6da00_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply2@
hs_bindgen_78245c4946e6da00 :: IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)) -> FC.CInt -> FC.CInt -> IO FC.CInt))
hs_bindgen_78245c4946e6da00 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_78245c4946e6da00_base

{-# NOINLINE apply2 #-}
{-| __C declaration:__ @apply2@

    __defined at:__ @manual\/function_pointers.h 11:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply2 :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)) -> FC.CInt -> FC.CInt -> IO FC.CInt)
apply2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_78245c4946e6da00

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_pointer_arg@
foreign import ccall unsafe "hs_bindgen_00276448b1e8af8a" hs_bindgen_00276448b1e8af8a_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_pointer_arg@
hs_bindgen_00276448b1e8af8a :: IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
hs_bindgen_00276448b1e8af8a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_00276448b1e8af8a_base

{-# NOINLINE apply1_pointer_arg #-}
{-| Basically the same as apply1(), but here for illustratory purposes.

__C declaration:__ @apply1_pointer_arg@

__defined at:__ @manual\/function_pointers.h 22:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_pointer_arg :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_pointer_arg =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_00276448b1e8af8a

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_nopointer_arg@
foreign import ccall unsafe "hs_bindgen_00f9010b80cdee19" hs_bindgen_00f9010b80cdee19_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_nopointer_arg@
hs_bindgen_00f9010b80cdee19 :: IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
hs_bindgen_00f9010b80cdee19 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_00f9010b80cdee19_base

{-# NOINLINE apply1_nopointer_arg #-}
{-| A version of apply1_pointer_arg() that declares to take a argument of function type, rather than a pointer-to-function type.

__C declaration:__ @apply1_nopointer_arg@

__defined at:__ @manual\/function_pointers.h 26:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_arg :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_nopointer_arg =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_00f9010b80cdee19

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_nopointer_res@
foreign import ccall unsafe "hs_bindgen_16929b4528263721" hs_bindgen_16929b4528263721_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualfunction_pointers_Example_get_apply1_nopointer_res@
hs_bindgen_16929b4528263721 :: IO (Ptr.FunPtr (IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))))
hs_bindgen_16929b4528263721 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_16929b4528263721_base

{-# NOINLINE apply1_nopointer_res #-}
{-| A function returning a pointer to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_res@

__defined at:__ @manual\/function_pointers.h 31:21@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_res :: Ptr.FunPtr (IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)))
apply1_nopointer_res =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_16929b4528263721
