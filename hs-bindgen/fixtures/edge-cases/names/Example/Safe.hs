{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/names.h>"
  , "void hs_bindgen_601290db9e101424 (void)"
  , "{"
  , "  by();"
  , "}"
  , "void hs_bindgen_f03dbed5eebb711a (void)"
  , "{"
  , "  forall();"
  , "}"
  , "void hs_bindgen_d1e3196c869f9fa1 (void)"
  , "{"
  , "  mdo();"
  , "}"
  , "void hs_bindgen_d3dcd898c88fb2e0 (void)"
  , "{"
  , "  pattern();"
  , "}"
  , "void hs_bindgen_7f08456473f564e3 (void)"
  , "{"
  , "  proc();"
  , "}"
  , "void hs_bindgen_5df814c22f546599 (void)"
  , "{"
  , "  rec();"
  , "}"
  , "void hs_bindgen_3877b6deb653b5a4 (void)"
  , "{"
  , "  using();"
  , "}"
  , "void hs_bindgen_7b8790d04357731b (void)"
  , "{"
  , "  anyclass();"
  , "}"
  , "void hs_bindgen_0bf4ab515f3279b9 (void)"
  , "{"
  , "  capi();"
  , "}"
  , "void hs_bindgen_fa8166b2793e4236 (void)"
  , "{"
  , "  cases();"
  , "}"
  , "void hs_bindgen_decc2d43a62d063d (void)"
  , "{"
  , "  ccall();"
  , "}"
  , "void hs_bindgen_b5a75e2b6434134b (void)"
  , "{"
  , "  dynamic();"
  , "}"
  , "void hs_bindgen_406f7b014573b3d3 (void)"
  , "{"
  , "  export();"
  , "}"
  , "void hs_bindgen_14aab2af04efc222 (void)"
  , "{"
  , "  family();"
  , "}"
  , "void hs_bindgen_ee9285b26b11b393 (void)"
  , "{"
  , "  group();"
  , "}"
  , "void hs_bindgen_b2463d5c1d51883e (void)"
  , "{"
  , "  interruptible();"
  , "}"
  , "void hs_bindgen_55a55462d9cd296c (void)"
  , "{"
  , "  javascript();"
  , "}"
  , "void hs_bindgen_227f34efb176d1fb (void)"
  , "{"
  , "  label();"
  , "}"
  , "void hs_bindgen_51c6e0d18dce403a (void)"
  , "{"
  , "  prim();"
  , "}"
  , "void hs_bindgen_f85f2418d208e6a0 (void)"
  , "{"
  , "  role();"
  , "}"
  , "void hs_bindgen_0855ecbc4b53ebbb (void)"
  , "{"
  , "  safe();"
  , "}"
  , "void hs_bindgen_e5238d13788a6df9 (void)"
  , "{"
  , "  stdcall();"
  , "}"
  , "void hs_bindgen_cf38d8bd096a7a42 (void)"
  , "{"
  , "  stock();"
  , "}"
  , "void hs_bindgen_fda9b083b24404f0 (void)"
  , "{"
  , "  unsafe();"
  , "}"
  , "void hs_bindgen_1d2a76a3a595be25 (void)"
  , "{"
  , "  via();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_601290db9e101424" by'_base ::
     IO ()

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h:3:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_by@
-}
by' ::
     IO ()
by' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType by'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_f03dbed5eebb711a" forall'_base ::
     IO ()

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h:4:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_forall@
-}
forall' ::
     IO ()
forall' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType forall'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_d1e3196c869f9fa1" mdo'_base ::
     IO ()

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h:5:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_mdo@
-}
mdo' ::
     IO ()
mdo' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType mdo'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_d3dcd898c88fb2e0" pattern'_base ::
     IO ()

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h:6:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_pattern@
-}
pattern' ::
     IO ()
pattern' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType pattern'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_7f08456473f564e3" proc'_base ::
     IO ()

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h:7:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_proc@
-}
proc' ::
     IO ()
proc' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType proc'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_5df814c22f546599" rec'_base ::
     IO ()

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h:8:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_rec@
-}
rec' ::
     IO ()
rec' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType rec'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_3877b6deb653b5a4" using'_base ::
     IO ()

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h:9:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_using@
-}
using' ::
     IO ()
using' =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType using'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_7b8790d04357731b" anyclass_base ::
     IO ()

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h:12:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_anyclass@
-}
anyclass ::
     IO ()
anyclass =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType anyclass_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_0bf4ab515f3279b9" capi_base ::
     IO ()

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h:13:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_capi@
-}
capi ::
     IO ()
capi =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType capi_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_fa8166b2793e4236" cases_base ::
     IO ()

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h:14:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_cases@
-}
cases ::
     IO ()
cases =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType cases_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_decc2d43a62d063d" ccall_base ::
     IO ()

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h:15:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_ccall@
-}
ccall ::
     IO ()
ccall =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType ccall_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_b5a75e2b6434134b" dynamic_base ::
     IO ()

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h:16:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_dynamic@
-}
dynamic ::
     IO ()
dynamic =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType dynamic_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_406f7b014573b3d3" export_base ::
     IO ()

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h:17:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_export@
-}
export ::
     IO ()
export =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType export_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_14aab2af04efc222" family_base ::
     IO ()

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h:18:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_family@
-}
family ::
     IO ()
family =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType family_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_ee9285b26b11b393" group_base ::
     IO ()

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h:19:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_group@
-}
group ::
     IO ()
group =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType group_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_b2463d5c1d51883e" interruptible_base ::
     IO ()

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h:20:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_interruptible@
-}
interruptible ::
     IO ()
interruptible =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType interruptible_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_55a55462d9cd296c" javascript_base ::
     IO ()

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h:21:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_javascript@
-}
javascript ::
     IO ()
javascript =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType javascript_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_227f34efb176d1fb" label_base ::
     IO ()

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h:22:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_label@
-}
label ::
     IO ()
label =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType label_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_51c6e0d18dce403a" prim_base ::
     IO ()

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h:23:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_prim@
-}
prim ::
     IO ()
prim =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType prim_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_f85f2418d208e6a0" role_base ::
     IO ()

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h:24:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_role@
-}
role ::
     IO ()
role =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType role_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_0855ecbc4b53ebbb" safe_base ::
     IO ()

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h:25:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_safe@
-}
safe ::
     IO ()
safe =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType safe_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_e5238d13788a6df9" stdcall_base ::
     IO ()

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h:26:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_stdcall@
-}
stdcall ::
     IO ()
stdcall =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType stdcall_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_cf38d8bd096a7a42" stock_base ::
     IO ()

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h:27:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_stock@
-}
stock ::
     IO ()
stock =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType stock_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_fda9b083b24404f0" unsafe_base ::
     IO ()

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h:28:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_unsafe@
-}
unsafe ::
     IO ()
unsafe =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType unsafe_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_1d2a76a3a595be25" via_base ::
     IO ()

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h:29:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_via@
-}
via ::
     IO ()
via =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType via_base
