{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/macro_in_fundecl.h>"
  , "char hs_bindgen_test_macrosmacro_in_fundecl_9c091e7a5fbe00eb ("
  , "  F arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return quux(arg1, arg2);"
  , "}"
  , "C *hs_bindgen_test_macrosmacro_in_fundecl_ca29786771bf115c ("
  , "  float arg1,"
  , "  C *arg2"
  , ")"
  , "{"
  , "  return wam(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_test_macrosmacro_in_fundecl_a1ddb0ab90dd90ae ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return foo1(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_test_macrosmacro_in_fundecl_39158ea36a52c749 ("
  , "  F arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return foo2(arg1, arg2);"
  , "}"
  , "C *hs_bindgen_test_macrosmacro_in_fundecl_30c473506139927c ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return foo3(arg1, arg2);"
  , "}"
  , "signed int (*hs_bindgen_test_macrosmacro_in_fundecl_ef6d9a2254300a4a ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return bar1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_test_macrosmacro_in_fundecl_6570ce6435b60197 ("
  , "  L arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return bar2(arg1);"
  , "}"
  , "signed int (*hs_bindgen_test_macrosmacro_in_fundecl_ac35ce1ad86420e1 ("
  , "  signed long arg1"
  , ")) ("
  , "  S arg1"
  , ")"
  , "{"
  , "  return bar3(arg1);"
  , "}"
  , "I (*hs_bindgen_test_macrosmacro_in_fundecl_c5e59f203c41c1c2 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return bar4(arg1);"
  , "}"
  , "signed int (*hs_bindgen_test_macrosmacro_in_fundecl_f2b917ad9122f0e2 ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return baz1(arg1);"
  , "}"
  , "signed int (*hs_bindgen_test_macrosmacro_in_fundecl_d27cd45b344a00d3 ("
  , "  I const arg1"
  , "))[2][3]"
  , "{"
  , "  return baz2(arg1);"
  , "}"
  , "I (*hs_bindgen_test_macrosmacro_in_fundecl_c4ed14d761bc89ba ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return baz3(arg1);"
  , "}"
  , "I hs_bindgen_test_macrosmacro_in_fundecl_8d4283a1963012db (void)"
  , "{"
  , "  return no_args_no_void();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_9c091e7a5fbe00eb" quux_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       F
    -> FC.CChar
    -> IO FC.CChar
    )

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h:12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
quux ::
     F
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CChar
quux =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType quux_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_ca29786771bf115c" wam_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CFloat
    -> Ptr.Ptr C
    -> IO (Ptr.Ptr C)
    )

{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h:13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
wam ::
     FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr C
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr C)
wam =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType wam_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_a1ddb0ab90dd90ae" foo1_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CFloat
    -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
    -> IO (Ptr.Ptr FC.CChar)
    )

{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h:16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo1 ::
     FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @g@
     -}
  -> IO (Ptr.Ptr FC.CChar)
foo1 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType foo1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_39158ea36a52c749" foo2_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       F
    -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
    -> IO (Ptr.Ptr FC.CChar)
    )

{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h:17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo2 ::
     F
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @g@
     -}
  -> IO (Ptr.Ptr FC.CChar)
foo2 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType foo2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_30c473506139927c" foo3_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CFloat
    -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
    -> IO (Ptr.Ptr C)
    )

{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h:18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo3 ::
     FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @g@
     -}
  -> IO (Ptr.Ptr C)
foo3 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType foo3_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_ef6d9a2254300a4a" bar1_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CLong
    -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))
    )

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h:21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1 ::
     FC.CLong
     {- ^ __C declaration:__ @x@
     -}
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))
bar1 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType bar1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_6570ce6435b60197" bar2_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       L
    -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))
    )

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h:22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2 ::
     L
     {- ^ __C declaration:__ @x@
     -}
  -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))
bar2 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType bar2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_ac35ce1ad86420e1" bar3_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CLong
    -> IO (Ptr.FunPtr (S -> IO FC.CInt))
    )

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h:23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3 ::
     FC.CLong
     {- ^ __C declaration:__ @x@
     -}
  -> IO (Ptr.FunPtr (S -> IO FC.CInt))
bar3 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType bar3_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_c5e59f203c41c1c2" bar4_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CLong
    -> IO (Ptr.FunPtr (FC.CShort -> IO I))
    )

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h:24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4 ::
     FC.CLong
     {- ^ __C declaration:__ @x@
     -}
  -> IO (Ptr.FunPtr (FC.CShort -> IO I))
bar4 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType bar4_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_f2b917ad9122f0e2" baz1_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
    )

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h:27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1 ::
     FC.CInt
     {- ^ __C declaration:__ @i@
     -}
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
baz1 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType baz1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_d27cd45b344a00d3" baz2_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       I
    -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
    )

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h:35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2 ::
     I
     {- ^ __C declaration:__ @i@
     -}
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
baz2 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType baz2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_c4ed14d761bc89ba" baz3_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I)))
    )

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h:43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3 ::
     FC.CInt
     {- ^ __C declaration:__ @i@
     -}
  -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I)))
baz3 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType baz3_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_8d4283a1963012db" no_args_no_void_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO I
    )

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h:53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void ::
     IO I
no_args_no_void =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType no_args_no_void_base
