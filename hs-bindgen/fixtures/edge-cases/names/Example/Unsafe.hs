{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.HasBaseForeignType
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

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_28b998af1f39a743" by'_base ::
     IO ()

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h:3:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_by@
-}
by' ::
     IO ()
by' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType by'_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_5d7ea7c4d11a5fc8" forall'_base ::
     IO ()

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h:4:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_forall@
-}
forall' ::
     IO ()
forall' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType forall'_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2d65448c684c09d5" mdo'_base ::
     IO ()

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h:5:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_mdo@
-}
mdo' ::
     IO ()
mdo' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType mdo'_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_13fe653d670d3712" pattern'_base ::
     IO ()

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h:6:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_pattern@
-}
pattern' ::
     IO ()
pattern' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType pattern'_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e9cc2037d33041aa" proc'_base ::
     IO ()

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h:7:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_proc@
-}
proc' ::
     IO ()
proc' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType proc'_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4a1e741f9ef596ff" rec'_base ::
     IO ()

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h:8:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_rec@
-}
rec' ::
     IO ()
rec' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType rec'_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ef6f3f22c615db58" using'_base ::
     IO ()

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h:9:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_using@
-}
using' ::
     IO ()
using' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType using'_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3c7afeaaf3ff040b" anyclass_base ::
     IO ()

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h:12:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_anyclass@
-}
anyclass ::
     IO ()
anyclass =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType anyclass_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_0518740d4c3caa1d" capi_base ::
     IO ()

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h:13:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_capi@
-}
capi ::
     IO ()
capi =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType capi_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_61f14ad7bb2e3d54" cases_base ::
     IO ()

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h:14:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_cases@
-}
cases ::
     IO ()
cases =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType cases_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ace8c96ed6673c3b" ccall_base ::
     IO ()

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h:15:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_ccall@
-}
ccall ::
     IO ()
ccall =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType ccall_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8865833b99552d03" dynamic_base ::
     IO ()

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h:16:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_dynamic@
-}
dynamic ::
     IO ()
dynamic =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType dynamic_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_15729ba251f5ec57" export_base ::
     IO ()

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h:17:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_export@
-}
export ::
     IO ()
export =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType export_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e6a4f7e833da2687" family_base ::
     IO ()

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h:18:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_family@
-}
family ::
     IO ()
family =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType family_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d4dd1bb5e95de858" group_base ::
     IO ()

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h:19:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_group@
-}
group ::
     IO ()
group =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType group_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_516f1ad5aba6de29" interruptible_base ::
     IO ()

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h:20:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_interruptible@
-}
interruptible ::
     IO ()
interruptible =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType interruptible_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_214230db174dc3e6" javascript_base ::
     IO ()

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h:21:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_javascript@
-}
javascript ::
     IO ()
javascript =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType javascript_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_88f1f0cf9c0f080e" label_base ::
     IO ()

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h:22:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_label@
-}
label ::
     IO ()
label =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType label_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_93a4c73f587dcf3c" prim_base ::
     IO ()

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h:23:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_prim@
-}
prim ::
     IO ()
prim =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType prim_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a267fe5585862ecc" role_base ::
     IO ()

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h:24:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_role@
-}
role ::
     IO ()
role =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType role_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ddac4cdf91c756a8" safe_base ::
     IO ()

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h:25:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_safe@
-}
safe ::
     IO ()
safe =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType safe_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8dd57b02f322a7ae" stdcall_base ::
     IO ()

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h:26:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_stdcall@
-}
stdcall ::
     IO ()
stdcall =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType stdcall_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3b69e1860d72507c" stock_base ::
     IO ()

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h:27:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_stock@
-}
stock ::
     IO ()
stock =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType stock_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_b9d80fa39d7ebb06" unsafe_base ::
     IO ()

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h:28:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_unsafe@
-}
unsafe ::
     IO ()
unsafe =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType unsafe_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_708f6397f5e5ac73" via_base ::
     IO ()

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h:29:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_via@
-}
via ::
     IO ()
via =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType via_base
