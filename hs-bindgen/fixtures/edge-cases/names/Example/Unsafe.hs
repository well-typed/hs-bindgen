{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/names.h>"
  , "void hs_bindgen_28b998af1f39a743 (void)"
  , "{"
  , "  by();"
  , "}"
  , "void hs_bindgen_5d7ea7c4d11a5fc8 (void)"
  , "{"
  , "  forall();"
  , "}"
  , "void hs_bindgen_2d65448c684c09d5 (void)"
  , "{"
  , "  mdo();"
  , "}"
  , "void hs_bindgen_13fe653d670d3712 (void)"
  , "{"
  , "  pattern();"
  , "}"
  , "void hs_bindgen_e9cc2037d33041aa (void)"
  , "{"
  , "  proc();"
  , "}"
  , "void hs_bindgen_4a1e741f9ef596ff (void)"
  , "{"
  , "  rec();"
  , "}"
  , "void hs_bindgen_ef6f3f22c615db58 (void)"
  , "{"
  , "  using();"
  , "}"
  , "void hs_bindgen_3c7afeaaf3ff040b (void)"
  , "{"
  , "  anyclass();"
  , "}"
  , "void hs_bindgen_0518740d4c3caa1d (void)"
  , "{"
  , "  capi();"
  , "}"
  , "void hs_bindgen_61f14ad7bb2e3d54 (void)"
  , "{"
  , "  cases();"
  , "}"
  , "void hs_bindgen_ace8c96ed6673c3b (void)"
  , "{"
  , "  ccall();"
  , "}"
  , "void hs_bindgen_8865833b99552d03 (void)"
  , "{"
  , "  dynamic();"
  , "}"
  , "void hs_bindgen_15729ba251f5ec57 (void)"
  , "{"
  , "  export();"
  , "}"
  , "void hs_bindgen_e6a4f7e833da2687 (void)"
  , "{"
  , "  family();"
  , "}"
  , "void hs_bindgen_d4dd1bb5e95de858 (void)"
  , "{"
  , "  group();"
  , "}"
  , "void hs_bindgen_516f1ad5aba6de29 (void)"
  , "{"
  , "  interruptible();"
  , "}"
  , "void hs_bindgen_214230db174dc3e6 (void)"
  , "{"
  , "  javascript();"
  , "}"
  , "void hs_bindgen_88f1f0cf9c0f080e (void)"
  , "{"
  , "  label();"
  , "}"
  , "void hs_bindgen_93a4c73f587dcf3c (void)"
  , "{"
  , "  prim();"
  , "}"
  , "void hs_bindgen_a267fe5585862ecc (void)"
  , "{"
  , "  role();"
  , "}"
  , "void hs_bindgen_ddac4cdf91c756a8 (void)"
  , "{"
  , "  safe();"
  , "}"
  , "void hs_bindgen_8dd57b02f322a7ae (void)"
  , "{"
  , "  stdcall();"
  , "}"
  , "void hs_bindgen_3b69e1860d72507c (void)"
  , "{"
  , "  stock();"
  , "}"
  , "void hs_bindgen_b9d80fa39d7ebb06 (void)"
  , "{"
  , "  unsafe();"
  , "}"
  , "void hs_bindgen_708f6397f5e5ac73 (void)"
  , "{"
  , "  via();"
  , "}"
  ]))

-- __unique:__ @test_edgecasesnames_Example_Unsafe_by@
foreign import ccall unsafe "hs_bindgen_28b998af1f39a743" hs_bindgen_28b998af1f39a743_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_by@
hs_bindgen_28b998af1f39a743 :: IO ()
hs_bindgen_28b998af1f39a743 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_28b998af1f39a743_base

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h 3:6@

    __exported by:__ @edge-cases\/names.h@
-}
by' :: IO ()
by' = hs_bindgen_28b998af1f39a743

-- __unique:__ @test_edgecasesnames_Example_Unsafe_forall@
foreign import ccall unsafe "hs_bindgen_5d7ea7c4d11a5fc8" hs_bindgen_5d7ea7c4d11a5fc8_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_forall@
hs_bindgen_5d7ea7c4d11a5fc8 :: IO ()
hs_bindgen_5d7ea7c4d11a5fc8 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_5d7ea7c4d11a5fc8_base

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h 4:6@

    __exported by:__ @edge-cases\/names.h@
-}
forall' :: IO ()
forall' = hs_bindgen_5d7ea7c4d11a5fc8

-- __unique:__ @test_edgecasesnames_Example_Unsafe_mdo@
foreign import ccall unsafe "hs_bindgen_2d65448c684c09d5" hs_bindgen_2d65448c684c09d5_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_mdo@
hs_bindgen_2d65448c684c09d5 :: IO ()
hs_bindgen_2d65448c684c09d5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_2d65448c684c09d5_base

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h 5:6@

    __exported by:__ @edge-cases\/names.h@
-}
mdo' :: IO ()
mdo' = hs_bindgen_2d65448c684c09d5

-- __unique:__ @test_edgecasesnames_Example_Unsafe_pattern@
foreign import ccall unsafe "hs_bindgen_13fe653d670d3712" hs_bindgen_13fe653d670d3712_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_pattern@
hs_bindgen_13fe653d670d3712 :: IO ()
hs_bindgen_13fe653d670d3712 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_13fe653d670d3712_base

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h 6:6@

    __exported by:__ @edge-cases\/names.h@
-}
pattern' :: IO ()
pattern' = hs_bindgen_13fe653d670d3712

-- __unique:__ @test_edgecasesnames_Example_Unsafe_proc@
foreign import ccall unsafe "hs_bindgen_e9cc2037d33041aa" hs_bindgen_e9cc2037d33041aa_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_proc@
hs_bindgen_e9cc2037d33041aa :: IO ()
hs_bindgen_e9cc2037d33041aa =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e9cc2037d33041aa_base

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h 7:6@

    __exported by:__ @edge-cases\/names.h@
-}
proc' :: IO ()
proc' = hs_bindgen_e9cc2037d33041aa

-- __unique:__ @test_edgecasesnames_Example_Unsafe_rec@
foreign import ccall unsafe "hs_bindgen_4a1e741f9ef596ff" hs_bindgen_4a1e741f9ef596ff_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_rec@
hs_bindgen_4a1e741f9ef596ff :: IO ()
hs_bindgen_4a1e741f9ef596ff =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4a1e741f9ef596ff_base

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h 8:6@

    __exported by:__ @edge-cases\/names.h@
-}
rec' :: IO ()
rec' = hs_bindgen_4a1e741f9ef596ff

-- __unique:__ @test_edgecasesnames_Example_Unsafe_using@
foreign import ccall unsafe "hs_bindgen_ef6f3f22c615db58" hs_bindgen_ef6f3f22c615db58_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_using@
hs_bindgen_ef6f3f22c615db58 :: IO ()
hs_bindgen_ef6f3f22c615db58 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ef6f3f22c615db58_base

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h 9:6@

    __exported by:__ @edge-cases\/names.h@
-}
using' :: IO ()
using' = hs_bindgen_ef6f3f22c615db58

-- __unique:__ @test_edgecasesnames_Example_Unsafe_anyclass@
foreign import ccall unsafe "hs_bindgen_3c7afeaaf3ff040b" hs_bindgen_3c7afeaaf3ff040b_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_anyclass@
hs_bindgen_3c7afeaaf3ff040b :: IO ()
hs_bindgen_3c7afeaaf3ff040b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3c7afeaaf3ff040b_base

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h 12:6@

    __exported by:__ @edge-cases\/names.h@
-}
anyclass :: IO ()
anyclass = hs_bindgen_3c7afeaaf3ff040b

-- __unique:__ @test_edgecasesnames_Example_Unsafe_capi@
foreign import ccall unsafe "hs_bindgen_0518740d4c3caa1d" hs_bindgen_0518740d4c3caa1d_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_capi@
hs_bindgen_0518740d4c3caa1d :: IO ()
hs_bindgen_0518740d4c3caa1d =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0518740d4c3caa1d_base

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h 13:6@

    __exported by:__ @edge-cases\/names.h@
-}
capi :: IO ()
capi = hs_bindgen_0518740d4c3caa1d

-- __unique:__ @test_edgecasesnames_Example_Unsafe_cases@
foreign import ccall unsafe "hs_bindgen_61f14ad7bb2e3d54" hs_bindgen_61f14ad7bb2e3d54_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_cases@
hs_bindgen_61f14ad7bb2e3d54 :: IO ()
hs_bindgen_61f14ad7bb2e3d54 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_61f14ad7bb2e3d54_base

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h 14:6@

    __exported by:__ @edge-cases\/names.h@
-}
cases :: IO ()
cases = hs_bindgen_61f14ad7bb2e3d54

-- __unique:__ @test_edgecasesnames_Example_Unsafe_ccall@
foreign import ccall unsafe "hs_bindgen_ace8c96ed6673c3b" hs_bindgen_ace8c96ed6673c3b_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_ccall@
hs_bindgen_ace8c96ed6673c3b :: IO ()
hs_bindgen_ace8c96ed6673c3b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ace8c96ed6673c3b_base

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h 15:6@

    __exported by:__ @edge-cases\/names.h@
-}
ccall :: IO ()
ccall = hs_bindgen_ace8c96ed6673c3b

-- __unique:__ @test_edgecasesnames_Example_Unsafe_dynamic@
foreign import ccall unsafe "hs_bindgen_8865833b99552d03" hs_bindgen_8865833b99552d03_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_dynamic@
hs_bindgen_8865833b99552d03 :: IO ()
hs_bindgen_8865833b99552d03 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8865833b99552d03_base

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h 16:6@

    __exported by:__ @edge-cases\/names.h@
-}
dynamic :: IO ()
dynamic = hs_bindgen_8865833b99552d03

-- __unique:__ @test_edgecasesnames_Example_Unsafe_export@
foreign import ccall unsafe "hs_bindgen_15729ba251f5ec57" hs_bindgen_15729ba251f5ec57_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_export@
hs_bindgen_15729ba251f5ec57 :: IO ()
hs_bindgen_15729ba251f5ec57 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_15729ba251f5ec57_base

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h 17:6@

    __exported by:__ @edge-cases\/names.h@
-}
export :: IO ()
export = hs_bindgen_15729ba251f5ec57

-- __unique:__ @test_edgecasesnames_Example_Unsafe_family@
foreign import ccall unsafe "hs_bindgen_e6a4f7e833da2687" hs_bindgen_e6a4f7e833da2687_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_family@
hs_bindgen_e6a4f7e833da2687 :: IO ()
hs_bindgen_e6a4f7e833da2687 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e6a4f7e833da2687_base

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h 18:6@

    __exported by:__ @edge-cases\/names.h@
-}
family :: IO ()
family = hs_bindgen_e6a4f7e833da2687

-- __unique:__ @test_edgecasesnames_Example_Unsafe_group@
foreign import ccall unsafe "hs_bindgen_d4dd1bb5e95de858" hs_bindgen_d4dd1bb5e95de858_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_group@
hs_bindgen_d4dd1bb5e95de858 :: IO ()
hs_bindgen_d4dd1bb5e95de858 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d4dd1bb5e95de858_base

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h 19:6@

    __exported by:__ @edge-cases\/names.h@
-}
group :: IO ()
group = hs_bindgen_d4dd1bb5e95de858

-- __unique:__ @test_edgecasesnames_Example_Unsafe_interruptible@
foreign import ccall unsafe "hs_bindgen_516f1ad5aba6de29" hs_bindgen_516f1ad5aba6de29_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_interruptible@
hs_bindgen_516f1ad5aba6de29 :: IO ()
hs_bindgen_516f1ad5aba6de29 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_516f1ad5aba6de29_base

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h 20:6@

    __exported by:__ @edge-cases\/names.h@
-}
interruptible :: IO ()
interruptible = hs_bindgen_516f1ad5aba6de29

-- __unique:__ @test_edgecasesnames_Example_Unsafe_javascript@
foreign import ccall unsafe "hs_bindgen_214230db174dc3e6" hs_bindgen_214230db174dc3e6_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_javascript@
hs_bindgen_214230db174dc3e6 :: IO ()
hs_bindgen_214230db174dc3e6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_214230db174dc3e6_base

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h 21:6@

    __exported by:__ @edge-cases\/names.h@
-}
javascript :: IO ()
javascript = hs_bindgen_214230db174dc3e6

-- __unique:__ @test_edgecasesnames_Example_Unsafe_label@
foreign import ccall unsafe "hs_bindgen_88f1f0cf9c0f080e" hs_bindgen_88f1f0cf9c0f080e_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_label@
hs_bindgen_88f1f0cf9c0f080e :: IO ()
hs_bindgen_88f1f0cf9c0f080e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_88f1f0cf9c0f080e_base

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h 22:6@

    __exported by:__ @edge-cases\/names.h@
-}
label :: IO ()
label = hs_bindgen_88f1f0cf9c0f080e

-- __unique:__ @test_edgecasesnames_Example_Unsafe_prim@
foreign import ccall unsafe "hs_bindgen_93a4c73f587dcf3c" hs_bindgen_93a4c73f587dcf3c_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_prim@
hs_bindgen_93a4c73f587dcf3c :: IO ()
hs_bindgen_93a4c73f587dcf3c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_93a4c73f587dcf3c_base

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h 23:6@

    __exported by:__ @edge-cases\/names.h@
-}
prim :: IO ()
prim = hs_bindgen_93a4c73f587dcf3c

-- __unique:__ @test_edgecasesnames_Example_Unsafe_role@
foreign import ccall unsafe "hs_bindgen_a267fe5585862ecc" hs_bindgen_a267fe5585862ecc_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_role@
hs_bindgen_a267fe5585862ecc :: IO ()
hs_bindgen_a267fe5585862ecc =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a267fe5585862ecc_base

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h 24:6@

    __exported by:__ @edge-cases\/names.h@
-}
role :: IO ()
role = hs_bindgen_a267fe5585862ecc

-- __unique:__ @test_edgecasesnames_Example_Unsafe_safe@
foreign import ccall unsafe "hs_bindgen_ddac4cdf91c756a8" hs_bindgen_ddac4cdf91c756a8_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_safe@
hs_bindgen_ddac4cdf91c756a8 :: IO ()
hs_bindgen_ddac4cdf91c756a8 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ddac4cdf91c756a8_base

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h 25:6@

    __exported by:__ @edge-cases\/names.h@
-}
safe :: IO ()
safe = hs_bindgen_ddac4cdf91c756a8

-- __unique:__ @test_edgecasesnames_Example_Unsafe_stdcall@
foreign import ccall unsafe "hs_bindgen_8dd57b02f322a7ae" hs_bindgen_8dd57b02f322a7ae_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_stdcall@
hs_bindgen_8dd57b02f322a7ae :: IO ()
hs_bindgen_8dd57b02f322a7ae =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8dd57b02f322a7ae_base

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h 26:6@

    __exported by:__ @edge-cases\/names.h@
-}
stdcall :: IO ()
stdcall = hs_bindgen_8dd57b02f322a7ae

-- __unique:__ @test_edgecasesnames_Example_Unsafe_stock@
foreign import ccall unsafe "hs_bindgen_3b69e1860d72507c" hs_bindgen_3b69e1860d72507c_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_stock@
hs_bindgen_3b69e1860d72507c :: IO ()
hs_bindgen_3b69e1860d72507c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3b69e1860d72507c_base

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h 27:6@

    __exported by:__ @edge-cases\/names.h@
-}
stock :: IO ()
stock = hs_bindgen_3b69e1860d72507c

-- __unique:__ @test_edgecasesnames_Example_Unsafe_unsafe@
foreign import ccall unsafe "hs_bindgen_b9d80fa39d7ebb06" hs_bindgen_b9d80fa39d7ebb06_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_unsafe@
hs_bindgen_b9d80fa39d7ebb06 :: IO ()
hs_bindgen_b9d80fa39d7ebb06 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_b9d80fa39d7ebb06_base

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h 28:6@

    __exported by:__ @edge-cases\/names.h@
-}
unsafe :: IO ()
unsafe = hs_bindgen_b9d80fa39d7ebb06

-- __unique:__ @test_edgecasesnames_Example_Unsafe_via@
foreign import ccall unsafe "hs_bindgen_708f6397f5e5ac73" hs_bindgen_708f6397f5e5ac73_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Unsafe_via@
hs_bindgen_708f6397f5e5ac73 :: IO ()
hs_bindgen_708f6397f5e5ac73 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_708f6397f5e5ac73_base

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h 29:6@

    __exported by:__ @edge-cases\/names.h@
-}
via :: IO ()
via = hs_bindgen_708f6397f5e5ac73
