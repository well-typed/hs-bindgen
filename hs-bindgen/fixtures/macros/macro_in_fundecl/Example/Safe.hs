{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/macro_in_fundecl.h>"
  , "char hs_bindgen_d345c332b6547629 ("
  , "  F arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return quux(arg1, arg2);"
  , "}"
  , "C *hs_bindgen_195036c94aad554b ("
  , "  float arg1,"
  , "  C *arg2"
  , ")"
  , "{"
  , "  return wam(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_a40b504a8f7c1d11 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return foo1(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_83392129a2035c99 ("
  , "  F arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return foo2(arg1, arg2);"
  , "}"
  , "C *hs_bindgen_0c7f4bce7905d355 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return foo3(arg1, arg2);"
  , "}"
  , "signed int (*hs_bindgen_3471ca0525deb2c0 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return bar1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_d5a4af88f772ff72 ("
  , "  L arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return bar2(arg1);"
  , "}"
  , "signed int (*hs_bindgen_b289d62136acab77 ("
  , "  signed long arg1"
  , ")) ("
  , "  S arg1"
  , ")"
  , "{"
  , "  return bar3(arg1);"
  , "}"
  , "I (*hs_bindgen_2b5b36cf49f0e40e ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return bar4(arg1);"
  , "}"
  , "signed int (*hs_bindgen_b56f5f3515f3cc33 ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return baz1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_0b9b2e4d1699b6f3 ("
  , "  I const arg1"
  , "))[2][3]"
  , "{"
  , "  return baz2(arg1);"
  , "}"
  , "I (*hs_bindgen_459eabcbd019687c ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return baz3(arg1);"
  , "}"
  , "I hs_bindgen_7ae4ab0ad4fb8cad (void)"
  , "{"
  , "  return no_args_no_void();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_d345c332b6547629" quux_base ::
     FC.CFloat
  -> FC.CChar
  -> IO FC.CChar

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h:12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_quux@
-}
quux ::
     F
     -- ^ __C declaration:__ @x@
  -> FC.CChar
     -- ^ __C declaration:__ @y@
  -> IO FC.CChar
quux =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType quux_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_195036c94aad554b" wam_base ::
     FC.CFloat
  -> Ptr.Ptr Void
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h:13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_wam@
-}
wam ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr C
     -- ^ __C declaration:__ @y@
  -> IO (Ptr.Ptr C)
wam =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType wam_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_a40b504a8f7c1d11" foo1_base ::
     FC.CFloat
  -> Ptr.FunPtr Void
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h:16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_foo1@
-}
foo1 ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (Ptr.Ptr FC.CChar)
foo1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType foo1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_83392129a2035c99" foo2_base ::
     FC.CFloat
  -> Ptr.FunPtr Void
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h:17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_foo2@
-}
foo2 ::
     F
     -- ^ __C declaration:__ @x@
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (Ptr.Ptr FC.CChar)
foo2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType foo2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_0c7f4bce7905d355" foo3_base ::
     FC.CFloat
  -> Ptr.FunPtr Void
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h:18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_foo3@
-}
foo3 ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (Ptr.Ptr C)
foo3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType foo3_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_3471ca0525deb2c0" bar1_base ::
     FC.CLong
  -> IO (Ptr.FunPtr Void)

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h:21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar1@
-}
bar1 ::
     FC.CLong
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))
bar1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType bar1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_d5a4af88f772ff72" bar2_base ::
     FC.CLong
  -> IO (Ptr.FunPtr Void)

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h:22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar2@
-}
bar2 ::
     L
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))
bar2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType bar2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_b289d62136acab77" bar3_base ::
     FC.CLong
  -> IO (Ptr.FunPtr Void)

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h:23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar3@
-}
bar3 ::
     FC.CLong
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (S -> IO FC.CInt))
bar3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType bar3_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_2b5b36cf49f0e40e" bar4_base ::
     FC.CLong
  -> IO (Ptr.FunPtr Void)

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h:24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar4@
-}
bar4 ::
     FC.CLong
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (FC.CShort -> IO I))
bar4 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType bar4_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_b56f5f3515f3cc33" baz1_base ::
     FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h:27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_baz1@
-}
baz1 ::
     FC.CInt
     -- ^ __C declaration:__ @i@
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
baz1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType baz1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_0b9b2e4d1699b6f3" baz2_base ::
     FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h:35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_baz2@
-}
baz2 ::
     I
     -- ^ __C declaration:__ @i@
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
baz2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType baz2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_459eabcbd019687c" baz3_base ::
     FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h:43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_baz3@
-}
baz3 ::
     FC.CInt
     -- ^ __C declaration:__ @i@
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I)))
baz3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType baz3_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_7ae4ab0ad4fb8cad" no_args_no_void_base ::
     IO FC.CInt

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h:53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@

    __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_no_args_no_void@
-}
no_args_no_void ::
     IO I
no_args_no_void =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType no_args_no_void_base
