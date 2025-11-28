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
  , "/* get_quux_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_test_macrosmacro_in_fundecl_75296b863af23367 (void)) ("
  , "  F arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &quux;"
  , "}"
  , "/* get_wam_ptr */"
  , "__attribute__ ((const))"
  , "C *(*hs_bindgen_test_macrosmacro_in_fundecl_fd1ccca5616729da (void)) ("
  , "  float arg1,"
  , "  C *arg2"
  , ")"
  , "{"
  , "  return &wam;"
  , "}"
  , "/* get_foo1_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_test_macrosmacro_in_fundecl_786c8d7bfea481fd (void)) ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo1;"
  , "}"
  , "/* get_foo2_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_test_macrosmacro_in_fundecl_42a47aecc35f5bda (void)) ("
  , "  F arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo2;"
  , "}"
  , "/* get_foo3_ptr */"
  , "__attribute__ ((const))"
  , "C *(*hs_bindgen_test_macrosmacro_in_fundecl_17760ec60140242e (void)) ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo3;"
  , "}"
  , "/* get_bar1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosmacro_in_fundecl_13fa512840072e8d (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar1;"
  , "}"
  , "/* get_bar2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosmacro_in_fundecl_0d63f3c4f98f04a3 (void)) ("
  , "  L arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar2;"
  , "}"
  , "/* get_bar3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosmacro_in_fundecl_8bd44eebdbce7f71 (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  S arg1"
  , ")"
  , "{"
  , "  return &bar3;"
  , "}"
  , "/* get_bar4_ptr */"
  , "__attribute__ ((const))"
  , "I (*(*hs_bindgen_test_macrosmacro_in_fundecl_0515cdde3c6f0f19 (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar4;"
  , "}"
  , "/* get_baz1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosmacro_in_fundecl_8edeef2444de2cee (void)) ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz1;"
  , "}"
  , "/* get_baz2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_macrosmacro_in_fundecl_61853d26cc39ced6 (void)) ("
  , "  I const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz2;"
  , "}"
  , "/* get_baz3_ptr */"
  , "__attribute__ ((const))"
  , "I (*(*hs_bindgen_test_macrosmacro_in_fundecl_b465262d2f67a146 (void)) ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz3;"
  , "}"
  , "/* get_no_args_no_void_ptr */"
  , "__attribute__ ((const))"
  , "I (*hs_bindgen_test_macrosmacro_in_fundecl_452280b5085b4ccd (void)) (void)"
  , "{"
  , "  return &no_args_no_void;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_75296b863af23367" hs_bindgen_test_macrosmacro_in_fundecl_75296b863af23367 ::
     IO (Ptr.FunPtr (F -> FC.CChar -> IO FC.CChar))

{-# NOINLINE quux_ptr #-}

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h:12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
quux_ptr :: Ptr.FunPtr (F -> FC.CChar -> IO FC.CChar)
quux_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_75296b863af23367

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_fd1ccca5616729da" hs_bindgen_test_macrosmacro_in_fundecl_fd1ccca5616729da ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr C) -> IO (Ptr.Ptr C)))

{-# NOINLINE wam_ptr #-}

{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h:13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
wam_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr C) -> IO (Ptr.Ptr C))
wam_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_fd1ccca5616729da

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_786c8d7bfea481fd" hs_bindgen_test_macrosmacro_in_fundecl_786c8d7bfea481fd ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE foo1_ptr #-}

{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h:16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo1_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar))
foo1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_786c8d7bfea481fd

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_42a47aecc35f5bda" hs_bindgen_test_macrosmacro_in_fundecl_42a47aecc35f5bda ::
     IO (Ptr.FunPtr (F -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE foo2_ptr #-}

{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h:17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo2_ptr :: Ptr.FunPtr (F -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar))
foo2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_42a47aecc35f5bda

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_17760ec60140242e" hs_bindgen_test_macrosmacro_in_fundecl_17760ec60140242e ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr C)))

{-# NOINLINE foo3_ptr #-}

{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h:18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo3_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr C))
foo3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_17760ec60140242e

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_13fa512840072e8d" hs_bindgen_test_macrosmacro_in_fundecl_13fa512840072e8d ::
     IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))))

{-# NOINLINE bar1_ptr #-}

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h:21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt)))
bar1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_13fa512840072e8d

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_0d63f3c4f98f04a3" hs_bindgen_test_macrosmacro_in_fundecl_0d63f3c4f98f04a3 ::
     IO (Ptr.FunPtr (L -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))))

{-# NOINLINE bar2_ptr #-}

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h:22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2_ptr :: Ptr.FunPtr (L -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt)))
bar2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_0d63f3c4f98f04a3

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_8bd44eebdbce7f71" hs_bindgen_test_macrosmacro_in_fundecl_8bd44eebdbce7f71 ::
     IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (S -> IO FC.CInt))))

{-# NOINLINE bar3_ptr #-}

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h:23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (S -> IO FC.CInt)))
bar3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_8bd44eebdbce7f71

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_0515cdde3c6f0f19" hs_bindgen_test_macrosmacro_in_fundecl_0515cdde3c6f0f19 ::
     IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO I))))

{-# NOINLINE bar4_ptr #-}

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h:24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO I)))
bar4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_0515cdde3c6f0f19

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_8edeef2444de2cee" hs_bindgen_test_macrosmacro_in_fundecl_8edeef2444de2cee ::
     IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE baz1_ptr #-}

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h:27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
baz1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_8edeef2444de2cee

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_61853d26cc39ced6" hs_bindgen_test_macrosmacro_in_fundecl_61853d26cc39ced6 ::
     IO (Ptr.FunPtr (I -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE baz2_ptr #-}

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h:35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2_ptr :: Ptr.FunPtr (I -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
baz2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_61853d26cc39ced6

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_b465262d2f67a146" hs_bindgen_test_macrosmacro_in_fundecl_b465262d2f67a146 ::
     IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I)))))

{-# NOINLINE baz3_ptr #-}

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h:43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I))))
baz3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_b465262d2f67a146

foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_452280b5085b4ccd" hs_bindgen_test_macrosmacro_in_fundecl_452280b5085b4ccd ::
     IO (Ptr.FunPtr (IO I))

{-# NOINLINE no_args_no_void_ptr #-}

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h:53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void_ptr :: Ptr.FunPtr (IO I)
no_args_no_void_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macrosmacro_in_fundecl_452280b5085b4ccd
