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
  , "/* get_square_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square;"
  , "}"
  , "/* get_plus_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_bf838c747898dc42 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &plus;"
  , "}"
  , "/* get_apply1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070 (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , "),"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1;"
  , "}"
  , "/* get_apply2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a (void)) ("
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
  , "/* get_apply1_pointer_arg_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_pointer_arg;"
  , "}"
  , "/* get_apply1_nopointer_arg_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81 (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_nopointer_arg;"
  , "}"
  , "/* get_apply1_nopointer_res_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*const (*hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f (void)) (void)) ("
  , "  int2int *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &apply1_nopointer_res;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_square_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9" hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_ptr #-}

{-| __C declaration:__ @square@

    __defined at:__ @manual\/function_pointers.h:5:12@

    __exported by:__ @manual\/function_pointers.h@
-}
square_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9

{-| __unique:__ @ExampleNothingget_plus_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_bf838c747898dc42" hs_bindgen_test_manualfunction_pointers_bf838c747898dc42 ::
     IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt))

{-# NOINLINE plus_ptr #-}

{-| __C declaration:__ @plus@

    __defined at:__ @manual\/function_pointers.h:7:12@

    __exported by:__ @manual\/function_pointers.h@
-}
plus_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
plus_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_bf838c747898dc42

{-| __unique:__ @ExampleNothingget_apply1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070" hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply1_ptr #-}

{-| __C declaration:__ @apply1@

    __defined at:__ @manual\/function_pointers.h:9:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply1_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt)
apply1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070

{-| __unique:__ @ExampleNothingget_apply2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a" hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)) -> FC.CInt -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply2_ptr #-}

{-| __C declaration:__ @apply2@

    __defined at:__ @manual\/function_pointers.h:11:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply2_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)) -> FC.CInt -> FC.CInt -> IO FC.CInt)
apply2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a

{-| __unique:__ @ExampleNothingget_apply1_pointer_arg_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca" hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca ::
     IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply1_pointer_arg_ptr #-}

{-| Basically the same as apply1(), but here for illustratory purposes.

__C declaration:__ @apply1_pointer_arg@

__defined at:__ @manual\/function_pointers.h:22:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_pointer_arg_ptr :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_pointer_arg_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca

{-| __unique:__ @ExampleNothingget_apply1_nopointer_arg_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81" hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply1_nopointer_arg_ptr #-}

{-| A version of apply1_pointer_arg() that declares to take a argument of function type, rather than a pointer-to-function type.

__C declaration:__ @apply1_nopointer_arg@

__defined at:__ @manual\/function_pointers.h:26:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_arg_ptr :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_nopointer_arg_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81

{-| __unique:__ @ExampleNothingget_apply1_nopointer_res_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f" hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f ::
     IO (Ptr.FunPtr (IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))))

{-# NOINLINE apply1_nopointer_res_ptr #-}

{-| A function returning a pointer to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_res@

__defined at:__ @manual\/function_pointers.h:31:21@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_res_ptr :: Ptr.FunPtr (IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)))
apply1_nopointer_res_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f
