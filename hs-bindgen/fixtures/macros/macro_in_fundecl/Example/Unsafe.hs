{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Array.KnownSize.Mutable
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (Float, IO)

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

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_quux@
foreign import ccall unsafe "hs_bindgen_ab9081efcd629826" hs_bindgen_ab9081efcd629826_base ::
     Float
  -> GHC.Int.Int8
  -> IO GHC.Int.Int8

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_quux@
hs_bindgen_ab9081efcd629826 ::
     F
  -> FC.CChar
  -> IO FC.CChar
hs_bindgen_ab9081efcd629826 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ab9081efcd629826_base

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h 12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
quux ::
     F
     -- ^ __C declaration:__ @x@
  -> FC.CChar
     -- ^ __C declaration:__ @y@
  -> IO FC.CChar
quux = hs_bindgen_ab9081efcd629826

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_wam@
foreign import ccall unsafe "hs_bindgen_7db4d5f10d9904d8" hs_bindgen_7db4d5f10d9904d8_base ::
     Float
  -> Ptr.Ptr Void
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_wam@
hs_bindgen_7db4d5f10d9904d8 ::
     FC.CFloat
  -> Ptr.Ptr C
  -> IO (Ptr.Ptr C)
hs_bindgen_7db4d5f10d9904d8 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_7db4d5f10d9904d8_base

{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h 13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
wam ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr C
     -- ^ __C declaration:__ @y@
  -> IO (Ptr.Ptr C)
wam = hs_bindgen_7db4d5f10d9904d8

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_foo1@
foreign import ccall unsafe "hs_bindgen_18401e906d384fd5" hs_bindgen_18401e906d384fd5_base ::
     Float
  -> Ptr.FunPtr Void
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_foo1@
hs_bindgen_18401e906d384fd5 ::
     FC.CFloat
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> IO (Ptr.Ptr FC.CChar)
hs_bindgen_18401e906d384fd5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_18401e906d384fd5_base

{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h 16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo1 ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (Ptr.Ptr FC.CChar)
foo1 = hs_bindgen_18401e906d384fd5

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_foo2@
foreign import ccall unsafe "hs_bindgen_1e16ebe63a290ff6" hs_bindgen_1e16ebe63a290ff6_base ::
     Float
  -> Ptr.FunPtr Void
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_foo2@
hs_bindgen_1e16ebe63a290ff6 ::
     F
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> IO (Ptr.Ptr FC.CChar)
hs_bindgen_1e16ebe63a290ff6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1e16ebe63a290ff6_base

{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h 17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo2 ::
     F
     -- ^ __C declaration:__ @x@
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (Ptr.Ptr FC.CChar)
foo2 = hs_bindgen_1e16ebe63a290ff6

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_foo3@
foreign import ccall unsafe "hs_bindgen_091043692da958ac" hs_bindgen_091043692da958ac_base ::
     Float
  -> Ptr.FunPtr Void
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_foo3@
hs_bindgen_091043692da958ac ::
     FC.CFloat
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> IO (Ptr.Ptr C)
hs_bindgen_091043692da958ac =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_091043692da958ac_base

{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h 18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo3 ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (Ptr.Ptr C)
foo3 = hs_bindgen_091043692da958ac

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar1@
foreign import ccall unsafe "hs_bindgen_cf4fa39c5b4ef431" hs_bindgen_cf4fa39c5b4ef431_base ::
     GHC.Int.Int64
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar1@
hs_bindgen_cf4fa39c5b4ef431 ::
     FC.CLong
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))
hs_bindgen_cf4fa39c5b4ef431 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_cf4fa39c5b4ef431_base

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h 21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1 ::
     FC.CLong
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))
bar1 = hs_bindgen_cf4fa39c5b4ef431

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar2@
foreign import ccall unsafe "hs_bindgen_9092ebfb46f7f31b" hs_bindgen_9092ebfb46f7f31b_base ::
     GHC.Int.Int64
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar2@
hs_bindgen_9092ebfb46f7f31b ::
     L
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))
hs_bindgen_9092ebfb46f7f31b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_9092ebfb46f7f31b_base

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h 22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2 ::
     L
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))
bar2 = hs_bindgen_9092ebfb46f7f31b

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar3@
foreign import ccall unsafe "hs_bindgen_a5e6607b472003eb" hs_bindgen_a5e6607b472003eb_base ::
     GHC.Int.Int64
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar3@
hs_bindgen_a5e6607b472003eb ::
     FC.CLong
  -> IO (Ptr.FunPtr (S -> IO FC.CInt))
hs_bindgen_a5e6607b472003eb =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a5e6607b472003eb_base

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h 23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3 ::
     FC.CLong
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (S -> IO FC.CInt))
bar3 = hs_bindgen_a5e6607b472003eb

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar4@
foreign import ccall unsafe "hs_bindgen_050bd8903c7b13dd" hs_bindgen_050bd8903c7b13dd_base ::
     GHC.Int.Int64
  -> IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_bar4@
hs_bindgen_050bd8903c7b13dd ::
     FC.CLong
  -> IO (Ptr.FunPtr (FC.CShort -> IO I))
hs_bindgen_050bd8903c7b13dd =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_050bd8903c7b13dd_base

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h 24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4 ::
     FC.CLong
     -- ^ __C declaration:__ @x@
  -> IO (Ptr.FunPtr (FC.CShort -> IO I))
bar4 = hs_bindgen_050bd8903c7b13dd

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_baz1@
foreign import ccall unsafe "hs_bindgen_f378b374e8c8c095" hs_bindgen_f378b374e8c8c095_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_baz1@
hs_bindgen_f378b374e8c8c095 ::
     FC.CInt
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 2) ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 3) FC.CInt)))
hs_bindgen_f378b374e8c8c095 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f378b374e8c8c095_base

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h 27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1 ::
     FC.CInt
     -- ^ __C declaration:__ @i@
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 2) ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 3) FC.CInt)))
baz1 = hs_bindgen_f378b374e8c8c095

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_baz2@
foreign import ccall unsafe "hs_bindgen_27cf571d08ac8c04" hs_bindgen_27cf571d08ac8c04_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_baz2@
hs_bindgen_27cf571d08ac8c04 ::
     I
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 2) ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 3) FC.CInt)))
hs_bindgen_27cf571d08ac8c04 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_27cf571d08ac8c04_base

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h 35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2 ::
     I
     -- ^ __C declaration:__ @i@
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 2) ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 3) FC.CInt)))
baz2 = hs_bindgen_27cf571d08ac8c04

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_baz3@
foreign import ccall unsafe "hs_bindgen_c4035ef23b908e27" hs_bindgen_c4035ef23b908e27_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_baz3@
hs_bindgen_c4035ef23b908e27 ::
     FC.CInt
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 2) ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 3) I)))
hs_bindgen_c4035ef23b908e27 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c4035ef23b908e27_base

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h 43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3 ::
     FC.CInt
     -- ^ __C declaration:__ @i@
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 2) ((HsBindgen.Runtime.Array.KnownSize.Mutable.Array 3) I)))
baz3 = hs_bindgen_c4035ef23b908e27

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_77a9149f03b2767f" hs_bindgen_77a9149f03b2767f_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_macrosmacro_in_fundecl_Example_Unsafe_no_args_no_void@
hs_bindgen_77a9149f03b2767f :: IO I
hs_bindgen_77a9149f03b2767f =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_77a9149f03b2767f_base

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h 53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void :: IO I
no_args_no_void = hs_bindgen_77a9149f03b2767f
