{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/macro_in_fundecl.h>"
  , "/* Example_get_quux_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_test_macrosmacro_in_fundecl_d8f5ece302797670 (void)) ("
  , "  F arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &quux;"
  , "}"
  , "/* Example_get_wam_ptr */"
  , "__attribute__ ((const))"
  , "C *(*hs_bindgen_test_macrosmacro_in_fundecl_6eddc7cae32c87a9 (void)) ("
  , "  float arg1,"
  , "  C *arg2"
  , ")"
  , "{"
  , "  return &wam;"
  , "}"
  , "/* Example_get_foo1_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_test_macrosmacro_in_fundecl_06131a9037617445 (void)) ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo1;"
  , "}"
  , "/* Example_get_foo2_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_test_macrosmacro_in_fundecl_68d5128a477a55ab (void)) ("
  , "  F arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo2;"
  , "}"
  , "/* Example_get_foo3_ptr */"
  , "__attribute__ ((const))"
  , "C *(*hs_bindgen_test_macrosmacro_in_fundecl_da476d2762343643 (void)) ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo3;"
  , "}"
  , "/* Example_get_bar1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosmacro_in_fundecl_2a02d3414f09b0bb (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar1;"
  , "}"
  , "/* Example_get_bar2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosmacro_in_fundecl_e7f3e52afd3430bd (void)) ("
  , "  L arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar2;"
  , "}"
  , "/* Example_get_bar3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosmacro_in_fundecl_7d9ba98465d2c7b7 (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  S arg1"
  , ")"
  , "{"
  , "  return &bar3;"
  , "}"
  , "/* Example_get_bar4_ptr */"
  , "__attribute__ ((const))"
  , "I (*(*hs_bindgen_test_macrosmacro_in_fundecl_51a5dccf21628be1 (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar4;"
  , "}"
  , "/* Example_get_baz1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosmacro_in_fundecl_971f9c00d0ce5eae (void)) ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz1;"
  , "}"
  , "/* Example_get_baz2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosmacro_in_fundecl_2aac4344645e949e (void)) ("
  , "  I const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz2;"
  , "}"
  , "/* Example_get_baz3_ptr */"
  , "__attribute__ ((const))"
  , "I (*(*hs_bindgen_test_macrosmacro_in_fundecl_df109ce3836adf2e (void)) ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz3;"
  , "}"
  , "/* Example_get_no_args_no_void_ptr */"
  , "__attribute__ ((const))"
  , "I (*hs_bindgen_test_macrosmacro_in_fundecl_6594699f2f8bc19e (void)) (void)"
  , "{"
  , "  return &no_args_no_void;"
  , "}"
  ]))

{-| __unique:__ @Example_get_quux_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_d8f5ece302797670" hs_bindgen_test_macrosmacro_in_fundecl_d8f5ece302797670 ::
     IO (Ptr.FunPtr (F -> FC.CChar -> IO FC.CChar))

{-# NOINLINE quux_ptr #-}

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h:12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
quux_ptr :: Ptr.FunPtr (F -> FC.CChar -> IO FC.CChar)
quux_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_d8f5ece302797670

{-| __unique:__ @Example_get_wam_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_6eddc7cae32c87a9" hs_bindgen_test_macrosmacro_in_fundecl_6eddc7cae32c87a9 ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr C) -> IO (Ptr.Ptr C)))

{-# NOINLINE wam_ptr #-}

{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h:13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
wam_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr C) -> IO (Ptr.Ptr C))
wam_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_6eddc7cae32c87a9

{-| __unique:__ @Example_get_foo1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_06131a9037617445" hs_bindgen_test_macrosmacro_in_fundecl_06131a9037617445 ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE foo1_ptr #-}

{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h:16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo1_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar))
foo1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_06131a9037617445

{-| __unique:__ @Example_get_foo2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_68d5128a477a55ab" hs_bindgen_test_macrosmacro_in_fundecl_68d5128a477a55ab ::
     IO (Ptr.FunPtr (F -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE foo2_ptr #-}

{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h:17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo2_ptr :: Ptr.FunPtr (F -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar))
foo2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_68d5128a477a55ab

{-| __unique:__ @Example_get_foo3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_da476d2762343643" hs_bindgen_test_macrosmacro_in_fundecl_da476d2762343643 ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr C)))

{-# NOINLINE foo3_ptr #-}

{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h:18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo3_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr C))
foo3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_da476d2762343643

{-| __unique:__ @Example_get_bar1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_2a02d3414f09b0bb" hs_bindgen_test_macrosmacro_in_fundecl_2a02d3414f09b0bb ::
     IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))))

{-# NOINLINE bar1_ptr #-}

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h:21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt)))
bar1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_2a02d3414f09b0bb

{-| __unique:__ @Example_get_bar2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_e7f3e52afd3430bd" hs_bindgen_test_macrosmacro_in_fundecl_e7f3e52afd3430bd ::
     IO (Ptr.FunPtr (L -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))))

{-# NOINLINE bar2_ptr #-}

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h:22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2_ptr :: Ptr.FunPtr (L -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt)))
bar2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_e7f3e52afd3430bd

{-| __unique:__ @Example_get_bar3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_7d9ba98465d2c7b7" hs_bindgen_test_macrosmacro_in_fundecl_7d9ba98465d2c7b7 ::
     IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (S -> IO FC.CInt))))

{-# NOINLINE bar3_ptr #-}

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h:23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (S -> IO FC.CInt)))
bar3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_7d9ba98465d2c7b7

{-| __unique:__ @Example_get_bar4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_51a5dccf21628be1" hs_bindgen_test_macrosmacro_in_fundecl_51a5dccf21628be1 ::
     IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO I))))

{-# NOINLINE bar4_ptr #-}

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h:24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO I)))
bar4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_51a5dccf21628be1

{-| __unique:__ @Example_get_baz1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_971f9c00d0ce5eae" hs_bindgen_test_macrosmacro_in_fundecl_971f9c00d0ce5eae ::
     IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE baz1_ptr #-}

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h:27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
baz1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_971f9c00d0ce5eae

{-| __unique:__ @Example_get_baz2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_2aac4344645e949e" hs_bindgen_test_macrosmacro_in_fundecl_2aac4344645e949e ::
     IO (Ptr.FunPtr (I -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE baz2_ptr #-}

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h:35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2_ptr :: Ptr.FunPtr (I -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
baz2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_2aac4344645e949e

{-| __unique:__ @Example_get_baz3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_df109ce3836adf2e" hs_bindgen_test_macrosmacro_in_fundecl_df109ce3836adf2e ::
     IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I)))))

{-# NOINLINE baz3_ptr #-}

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h:43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I))))
baz3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_df109ce3836adf2e

{-| __unique:__ @Example_get_no_args_no_void_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_6594699f2f8bc19e" hs_bindgen_test_macrosmacro_in_fundecl_6594699f2f8bc19e ::
     IO (Ptr.FunPtr (IO I))

{-# NOINLINE no_args_no_void_ptr #-}

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h:53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void_ptr :: Ptr.FunPtr (IO I)
no_args_no_void_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_6594699f2f8bc19e
