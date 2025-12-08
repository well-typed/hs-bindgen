{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/macro_in_fundecl.h>"
  , "char hs_bindgen_ab9081efcd629826 ("
  , "  F arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return quux(arg1, arg2);"
  , "}"
  , "C *hs_bindgen_7db4d5f10d9904d8 ("
  , "  float arg1,"
  , "  C *arg2"
  , ")"
  , "{"
  , "  return wam(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_18401e906d384fd5 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return foo1(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_1e16ebe63a290ff6 ("
  , "  F arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return foo2(arg1, arg2);"
  , "}"
  , "C *hs_bindgen_091043692da958ac ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return foo3(arg1, arg2);"
  , "}"
  , "signed int (*hs_bindgen_cf4fa39c5b4ef431 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return bar1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_9092ebfb46f7f31b ("
  , "  L arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return bar2(arg1);"
  , "}"
  , "signed int (*hs_bindgen_a5e6607b472003eb ("
  , "  signed long arg1"
  , ")) ("
  , "  S arg1"
  , ")"
  , "{"
  , "  return bar3(arg1);"
  , "}"
  , "I (*hs_bindgen_050bd8903c7b13dd ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return bar4(arg1);"
  , "}"
  , "signed int (*hs_bindgen_f378b374e8c8c095 ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return baz1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_27cf571d08ac8c04 ("
  , "  I const arg1"
  , "))[2][3]"
  , "{"
  , "  return baz2(arg1);"
  , "}"
  , "I (*hs_bindgen_c4035ef23b908e27 ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return baz3(arg1);"
  , "}"
  , "I hs_bindgen_77a9149f03b2767f (void)"
  , "{"
  , "  return no_args_no_void();"
  , "}"
  ]))

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h:12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_quux@
-}
foreign import ccall unsafe "hs_bindgen_ab9081efcd629826" quux ::
     F
     -- ^ __C declaration:__ @x@
  -> FC.CChar
     -- ^ __C declaration:__ @y@
  -> IO FC.CChar

{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h:13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_wam@
-}
foreign import ccall unsafe "hs_bindgen_7db4d5f10d9904d8" wam ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr C
     -- ^ __C declaration:__ @y@
  -> IO (Ptr.Ptr C)

{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h:16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_foo1@
-}
foreign import ccall unsafe "hs_bindgen_18401e906d384fd5" foo1 ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h:17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_foo2@
-}
foreign import ccall unsafe "hs_bindgen_1e16ebe63a290ff6" foo2 ::
     F
     -- ^ __C declaration:__ @x@
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h:18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_foo3@
-}
foreign import ccall unsafe "hs_bindgen_091043692da958ac" foo3 ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (Ptr.Ptr C)

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h:21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar1@
-}
foreign import ccall unsafe "hs_bindgen_cf4fa39c5b4ef431" bar1 ::
     FC.CLong
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h:22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar2@
-}
foreign import ccall unsafe "hs_bindgen_9092ebfb46f7f31b" bar2 ::
     L
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h:23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar3@
-}
foreign import ccall unsafe "hs_bindgen_a5e6607b472003eb" bar3 ::
     FC.CLong
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (S -> IO FC.CInt))

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h:24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar4@
-}
foreign import ccall unsafe "hs_bindgen_050bd8903c7b13dd" bar4 ::
     FC.CLong
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (FC.CShort -> IO I))

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h:27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_baz1@
-}
foreign import ccall unsafe "hs_bindgen_f378b374e8c8c095" baz1 ::
     FC.CInt
     -- ^ __C declaration:__ @i@
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h:35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_baz2@
-}
foreign import ccall unsafe "hs_bindgen_27cf571d08ac8c04" baz2 ::
     I
     -- ^ __C declaration:__ @i@
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h:43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_baz3@
-}
foreign import ccall unsafe "hs_bindgen_c4035ef23b908e27" baz3 ::
     FC.CInt
     -- ^ __C declaration:__ @i@
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I)))

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h:53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_no_args_no_void@
-}
foreign import ccall unsafe "hs_bindgen_77a9149f03b2767f" no_args_no_void ::
     IO I
