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
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/macro_in_fundecl.h>"
  , "/* test_macrosmacro_in_fundecl_Example_get_quux_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_9ba032a8ddf22326 (void)) ("
  , "  F arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &quux;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_wam_ptr */"
  , "__attribute__ ((const))"
  , "C *(*hs_bindgen_dafcba2967781c8d (void)) ("
  , "  float arg1,"
  , "  C *arg2"
  , ")"
  , "{"
  , "  return &wam;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_foo1_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_27ded2f560eadb5b (void)) ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_foo2_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_2f92fb3aace15650 (void)) ("
  , "  F arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_foo3_ptr */"
  , "__attribute__ ((const))"
  , "C *(*hs_bindgen_06568a4cca591e6c (void)) ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo3;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_bar1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_d9248136916656f7 (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_bar2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_2638a77b200d9571 (void)) ("
  , "  L arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_bar3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_ebbd5d09631f1f45 (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  S arg1"
  , ")"
  , "{"
  , "  return &bar3;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_bar4_ptr */"
  , "__attribute__ ((const))"
  , "I (*(*hs_bindgen_7082943a6d3dd96f (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar4;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_baz1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_d13a2e48d313bb66 (void)) ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_baz2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_2e558de67f4f715d (void)) ("
  , "  I const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_baz3_ptr */"
  , "__attribute__ ((const))"
  , "I (*(*hs_bindgen_7aba9db18419135a (void)) ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz3;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_no_args_no_void_ptr */"
  , "__attribute__ ((const))"
  , "I (*hs_bindgen_35d1920cc9d86b63 (void)) (void)"
  , "{"
  , "  return &no_args_no_void;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_9ba032a8ddf22326" hs_bindgen_9ba032a8ddf22326_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_quux_ptr@
hs_bindgen_9ba032a8ddf22326 ::
     IO (Ptr.FunPtr (F -> FC.CChar -> IO FC.CChar))
hs_bindgen_9ba032a8ddf22326 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9ba032a8ddf22326_base

{-# NOINLINE quux_ptr #-}

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h:12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
quux_ptr :: Ptr.FunPtr (F -> FC.CChar -> IO FC.CChar)
quux_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9ba032a8ddf22326

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_dafcba2967781c8d" hs_bindgen_dafcba2967781c8d_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_wam_ptr@
hs_bindgen_dafcba2967781c8d ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr C) -> IO (Ptr.Ptr C)))
hs_bindgen_dafcba2967781c8d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_dafcba2967781c8d_base

{-# NOINLINE wam_ptr #-}

{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h:13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
wam_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr C) -> IO (Ptr.Ptr C))
wam_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dafcba2967781c8d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_27ded2f560eadb5b" hs_bindgen_27ded2f560eadb5b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo1_ptr@
hs_bindgen_27ded2f560eadb5b ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar)))
hs_bindgen_27ded2f560eadb5b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_27ded2f560eadb5b_base

{-# NOINLINE foo1_ptr #-}

{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h:16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo1_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar))
foo1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_27ded2f560eadb5b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2f92fb3aace15650" hs_bindgen_2f92fb3aace15650_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo2_ptr@
hs_bindgen_2f92fb3aace15650 ::
     IO (Ptr.FunPtr (F -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar)))
hs_bindgen_2f92fb3aace15650 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2f92fb3aace15650_base

{-# NOINLINE foo2_ptr #-}

{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h:17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo2_ptr :: Ptr.FunPtr (F -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar))
foo2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2f92fb3aace15650

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_06568a4cca591e6c" hs_bindgen_06568a4cca591e6c_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo3_ptr@
hs_bindgen_06568a4cca591e6c ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr C)))
hs_bindgen_06568a4cca591e6c =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_06568a4cca591e6c_base

{-# NOINLINE foo3_ptr #-}

{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h:18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo3_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr C))
foo3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_06568a4cca591e6c

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d9248136916656f7" hs_bindgen_d9248136916656f7_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar1_ptr@
hs_bindgen_d9248136916656f7 ::
     IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))))
hs_bindgen_d9248136916656f7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_d9248136916656f7_base

{-# NOINLINE bar1_ptr #-}

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h:21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt)))
bar1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d9248136916656f7

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2638a77b200d9571" hs_bindgen_2638a77b200d9571_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar2_ptr@
hs_bindgen_2638a77b200d9571 ::
     IO (Ptr.FunPtr (L -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))))
hs_bindgen_2638a77b200d9571 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2638a77b200d9571_base

{-# NOINLINE bar2_ptr #-}

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h:22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2_ptr :: Ptr.FunPtr (L -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt)))
bar2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2638a77b200d9571

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ebbd5d09631f1f45" hs_bindgen_ebbd5d09631f1f45_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar3_ptr@
hs_bindgen_ebbd5d09631f1f45 ::
     IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (S -> IO FC.CInt))))
hs_bindgen_ebbd5d09631f1f45 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ebbd5d09631f1f45_base

{-# NOINLINE bar3_ptr #-}

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h:23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (S -> IO FC.CInt)))
bar3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ebbd5d09631f1f45

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_7082943a6d3dd96f" hs_bindgen_7082943a6d3dd96f_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar4_ptr@
hs_bindgen_7082943a6d3dd96f ::
     IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO I))))
hs_bindgen_7082943a6d3dd96f =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_7082943a6d3dd96f_base

{-# NOINLINE bar4_ptr #-}

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h:24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4_ptr :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO I)))
bar4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7082943a6d3dd96f

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d13a2e48d313bb66" hs_bindgen_d13a2e48d313bb66_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz1_ptr@
hs_bindgen_d13a2e48d313bb66 ::
     IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))
hs_bindgen_d13a2e48d313bb66 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_d13a2e48d313bb66_base

{-# NOINLINE baz1_ptr #-}

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h:27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
baz1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d13a2e48d313bb66

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2e558de67f4f715d" hs_bindgen_2e558de67f4f715d_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz2_ptr@
hs_bindgen_2e558de67f4f715d ::
     IO (Ptr.FunPtr (I -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))
hs_bindgen_2e558de67f4f715d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2e558de67f4f715d_base

{-# NOINLINE baz2_ptr #-}

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h:35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2_ptr :: Ptr.FunPtr (I -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
baz2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2e558de67f4f715d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_7aba9db18419135a" hs_bindgen_7aba9db18419135a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz3_ptr@
hs_bindgen_7aba9db18419135a ::
     IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I)))))
hs_bindgen_7aba9db18419135a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_7aba9db18419135a_base

{-# NOINLINE baz3_ptr #-}

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h:43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3_ptr :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I))))
baz3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7aba9db18419135a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_35d1920cc9d86b63" hs_bindgen_35d1920cc9d86b63_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_macrosmacro_in_fundecl_Example_get_no_args_no_void_ptr@
hs_bindgen_35d1920cc9d86b63 ::
     IO (Ptr.FunPtr (IO I))
hs_bindgen_35d1920cc9d86b63 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_35d1920cc9d86b63_base

{-# NOINLINE no_args_no_void_ptr #-}

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h:53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void_ptr :: Ptr.FunPtr (IO I)
no_args_no_void_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_35d1920cc9d86b63
