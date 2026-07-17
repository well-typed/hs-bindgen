{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.quux
    , Example.Safe.wam
    , Example.Safe.foo1
    , Example.Safe.foo2
    , Example.Safe.foo3
    , Example.Safe.bar1
    , Example.Safe.bar2
    , Example.Safe.bar3
    , Example.Safe.bar4
    , Example.Safe.baz1
    , Example.Safe.baz2
    , Example.Safe.baz3
    , Example.Safe.no_args_no_void
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_in_fundecl.h>"
  , "char hs_bindgen_d345c332b6547629 ("
  , "  F arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return (quux)(arg1, arg2);"
  , "}"
  , "C *hs_bindgen_195036c94aad554b ("
  , "  float arg1,"
  , "  C *arg2"
  , ")"
  , "{"
  , "  return (wam)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_a40b504a8f7c1d11 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo1)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_83392129a2035c99 ("
  , "  F arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo2)(arg1, arg2);"
  , "}"
  , "C *hs_bindgen_0c7f4bce7905d355 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo3)(arg1, arg2);"
  , "}"
  , "signed int (*hs_bindgen_3471ca0525deb2c0 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_d5a4af88f772ff72 ("
  , "  L arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar2)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_b289d62136acab77 ("
  , "  signed long arg1"
  , ")) ("
  , "  S arg1"
  , ")"
  , "{"
  , "  return (bar3)(arg1);"
  , "}"
  , "I (*hs_bindgen_2b5b36cf49f0e40e ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar4)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_b56f5f3515f3cc33 ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_0b9b2e4d1699b6f3 ("
  , "  I const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz2)(arg1);"
  , "}"
  , "I (*hs_bindgen_459eabcbd019687c ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz3)(arg1);"
  , "}"
  , "I hs_bindgen_7ae4ab0ad4fb8cad (void)"
  , "{"
  , "  return (no_args_no_void)();"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_quux@
foreign import ccall safe "hs_bindgen_d345c332b6547629" hs_bindgen_d345c332b6547629_base ::
     Float
  -> BG.Int8
  -> IO BG.Int8

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_quux@
hs_bindgen_d345c332b6547629 ::
     F
  -> BG.CChar
  -> IO BG.CChar
hs_bindgen_d345c332b6547629 =
  BG.fromFFIType hs_bindgen_d345c332b6547629_base

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h 12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
quux ::
     F
     -- ^ __C declaration:__ @x@
  -> BG.CChar
     -- ^ __C declaration:__ @y@
  -> IO BG.CChar
quux = hs_bindgen_d345c332b6547629

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_wam@
foreign import ccall safe "hs_bindgen_195036c94aad554b" hs_bindgen_195036c94aad554b_base ::
     Float
  -> BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_wam@
hs_bindgen_195036c94aad554b ::
     BG.CFloat
  -> BG.Ptr C
  -> IO (BG.Ptr C)
hs_bindgen_195036c94aad554b =
  BG.fromFFIType hs_bindgen_195036c94aad554b_base

{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h 13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
wam ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.Ptr C
     -- ^ __C declaration:__ @y@
  -> IO (BG.Ptr C)
wam = hs_bindgen_195036c94aad554b

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_foo1@
foreign import ccall safe "hs_bindgen_a40b504a8f7c1d11" hs_bindgen_a40b504a8f7c1d11_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_foo1@
hs_bindgen_a40b504a8f7c1d11 ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_a40b504a8f7c1d11 =
  BG.fromFFIType hs_bindgen_a40b504a8f7c1d11_base

{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h 16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo1 ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (BG.Ptr BG.CChar)
foo1 = hs_bindgen_a40b504a8f7c1d11

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_foo2@
foreign import ccall safe "hs_bindgen_83392129a2035c99" hs_bindgen_83392129a2035c99_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_foo2@
hs_bindgen_83392129a2035c99 ::
     F
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_83392129a2035c99 =
  BG.fromFFIType hs_bindgen_83392129a2035c99_base

{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h 17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo2 ::
     F
     -- ^ __C declaration:__ @x@
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (BG.Ptr BG.CChar)
foo2 = hs_bindgen_83392129a2035c99

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_foo3@
foreign import ccall safe "hs_bindgen_0c7f4bce7905d355" hs_bindgen_0c7f4bce7905d355_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_foo3@
hs_bindgen_0c7f4bce7905d355 ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr C)
hs_bindgen_0c7f4bce7905d355 =
  BG.fromFFIType hs_bindgen_0c7f4bce7905d355_base

{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h 18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo3 ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (BG.Ptr C)
foo3 = hs_bindgen_0c7f4bce7905d355

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar1@
foreign import ccall safe "hs_bindgen_3471ca0525deb2c0" hs_bindgen_3471ca0525deb2c0_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar1@
hs_bindgen_3471ca0525deb2c0 ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_3471ca0525deb2c0 =
  BG.fromFFIType hs_bindgen_3471ca0525deb2c0_base

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h 21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar1 = hs_bindgen_3471ca0525deb2c0

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar2@
foreign import ccall safe "hs_bindgen_d5a4af88f772ff72" hs_bindgen_d5a4af88f772ff72_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar2@
hs_bindgen_d5a4af88f772ff72 ::
     L
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_d5a4af88f772ff72 =
  BG.fromFFIType hs_bindgen_d5a4af88f772ff72_base

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h 22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2 ::
     L
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar2 = hs_bindgen_d5a4af88f772ff72

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar3@
foreign import ccall safe "hs_bindgen_b289d62136acab77" hs_bindgen_b289d62136acab77_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar3@
hs_bindgen_b289d62136acab77 ::
     BG.CLong
  -> IO (BG.FunPtr (S -> IO BG.CInt))
hs_bindgen_b289d62136acab77 =
  BG.fromFFIType hs_bindgen_b289d62136acab77_base

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h 23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (S -> IO BG.CInt))
bar3 = hs_bindgen_b289d62136acab77

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar4@
foreign import ccall safe "hs_bindgen_2b5b36cf49f0e40e" hs_bindgen_2b5b36cf49f0e40e_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_bar4@
hs_bindgen_2b5b36cf49f0e40e ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO I))
hs_bindgen_2b5b36cf49f0e40e =
  BG.fromFFIType hs_bindgen_2b5b36cf49f0e40e_base

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h 24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO I))
bar4 = hs_bindgen_2b5b36cf49f0e40e

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_baz1@
foreign import ccall safe "hs_bindgen_b56f5f3515f3cc33" hs_bindgen_b56f5f3515f3cc33_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_baz1@
hs_bindgen_b56f5f3515f3cc33 ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_b56f5f3515f3cc33 =
  BG.fromFFIType hs_bindgen_b56f5f3515f3cc33_base

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h 27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz1 = hs_bindgen_b56f5f3515f3cc33

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_baz2@
foreign import ccall safe "hs_bindgen_0b9b2e4d1699b6f3" hs_bindgen_0b9b2e4d1699b6f3_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_baz2@
hs_bindgen_0b9b2e4d1699b6f3 ::
     I
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_0b9b2e4d1699b6f3 =
  BG.fromFFIType hs_bindgen_0b9b2e4d1699b6f3_base

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h 35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2 ::
     I
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz2 = hs_bindgen_0b9b2e4d1699b6f3

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_baz3@
foreign import ccall safe "hs_bindgen_459eabcbd019687c" hs_bindgen_459eabcbd019687c_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_baz3@
hs_bindgen_459eabcbd019687c ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 I)))
hs_bindgen_459eabcbd019687c =
  BG.fromFFIType hs_bindgen_459eabcbd019687c_base

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h 43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 I)))
baz3 = hs_bindgen_459eabcbd019687c

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_no_args_no_void@
foreign import ccall safe "hs_bindgen_7ae4ab0ad4fb8cad" hs_bindgen_7ae4ab0ad4fb8cad_base ::
     IO BG.Int32

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Safe_no_args_no_void@
hs_bindgen_7ae4ab0ad4fb8cad :: IO I
hs_bindgen_7ae4ab0ad4fb8cad =
  BG.fromFFIType hs_bindgen_7ae4ab0ad4fb8cad_base

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h 53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void :: IO I
no_args_no_void = hs_bindgen_7ae4ab0ad4fb8cad
