{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

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

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h:3:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_by@
-}
foreign import ccall safe "hs_bindgen_601290db9e101424" by' ::
     IO ()

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h:4:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_forall@
-}
foreign import ccall safe "hs_bindgen_f03dbed5eebb711a" forall' ::
     IO ()

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h:5:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_mdo@
-}
foreign import ccall safe "hs_bindgen_d1e3196c869f9fa1" mdo' ::
     IO ()

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h:6:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_pattern@
-}
foreign import ccall safe "hs_bindgen_d3dcd898c88fb2e0" pattern' ::
     IO ()

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h:7:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_proc@
-}
foreign import ccall safe "hs_bindgen_7f08456473f564e3" proc' ::
     IO ()

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h:8:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_rec@
-}
foreign import ccall safe "hs_bindgen_5df814c22f546599" rec' ::
     IO ()

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h:9:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_using@
-}
foreign import ccall safe "hs_bindgen_3877b6deb653b5a4" using' ::
     IO ()

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h:12:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_anyclass@
-}
foreign import ccall safe "hs_bindgen_7b8790d04357731b" anyclass ::
     IO ()

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h:13:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_capi@
-}
foreign import ccall safe "hs_bindgen_0bf4ab515f3279b9" capi ::
     IO ()

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h:14:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_cases@
-}
foreign import ccall safe "hs_bindgen_fa8166b2793e4236" cases ::
     IO ()

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h:15:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_ccall@
-}
foreign import ccall safe "hs_bindgen_decc2d43a62d063d" ccall ::
     IO ()

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h:16:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_dynamic@
-}
foreign import ccall safe "hs_bindgen_b5a75e2b6434134b" dynamic ::
     IO ()

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h:17:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_export@
-}
foreign import ccall safe "hs_bindgen_406f7b014573b3d3" export ::
     IO ()

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h:18:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_family@
-}
foreign import ccall safe "hs_bindgen_14aab2af04efc222" family ::
     IO ()

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h:19:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_group@
-}
foreign import ccall safe "hs_bindgen_ee9285b26b11b393" group ::
     IO ()

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h:20:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_interruptible@
-}
foreign import ccall safe "hs_bindgen_b2463d5c1d51883e" interruptible ::
     IO ()

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h:21:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_javascript@
-}
foreign import ccall safe "hs_bindgen_55a55462d9cd296c" javascript ::
     IO ()

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h:22:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_label@
-}
foreign import ccall safe "hs_bindgen_227f34efb176d1fb" label ::
     IO ()

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h:23:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_prim@
-}
foreign import ccall safe "hs_bindgen_51c6e0d18dce403a" prim ::
     IO ()

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h:24:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_role@
-}
foreign import ccall safe "hs_bindgen_f85f2418d208e6a0" role ::
     IO ()

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h:25:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_safe@
-}
foreign import ccall safe "hs_bindgen_0855ecbc4b53ebbb" safe ::
     IO ()

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h:26:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_stdcall@
-}
foreign import ccall safe "hs_bindgen_e5238d13788a6df9" stdcall ::
     IO ()

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h:27:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_stock@
-}
foreign import ccall safe "hs_bindgen_cf38d8bd096a7a42" stock ::
     IO ()

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h:28:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_unsafe@
-}
foreign import ccall safe "hs_bindgen_fda9b083b24404f0" unsafe ::
     IO ()

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h:29:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Safe_via@
-}
foreign import ccall safe "hs_bindgen_1d2a76a3a595be25" via ::
     IO ()
