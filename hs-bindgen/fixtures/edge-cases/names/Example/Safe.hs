{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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

-- __unique:__ @test_edgecasesnames_Example_Safe_by@
foreign import ccall safe "hs_bindgen_601290db9e101424" hs_bindgen_601290db9e101424_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_by@
hs_bindgen_601290db9e101424 :: IO ()
hs_bindgen_601290db9e101424 =
  RIP.fromFFIType hs_bindgen_601290db9e101424_base

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h 3:6@

    __exported by:__ @edge-cases\/names.h@
-}
by' :: IO ()
by' = hs_bindgen_601290db9e101424

-- __unique:__ @test_edgecasesnames_Example_Safe_forall@
foreign import ccall safe "hs_bindgen_f03dbed5eebb711a" hs_bindgen_f03dbed5eebb711a_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_forall@
hs_bindgen_f03dbed5eebb711a :: IO ()
hs_bindgen_f03dbed5eebb711a =
  RIP.fromFFIType hs_bindgen_f03dbed5eebb711a_base

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h 4:6@

    __exported by:__ @edge-cases\/names.h@
-}
forall' :: IO ()
forall' = hs_bindgen_f03dbed5eebb711a

-- __unique:__ @test_edgecasesnames_Example_Safe_mdo@
foreign import ccall safe "hs_bindgen_d1e3196c869f9fa1" hs_bindgen_d1e3196c869f9fa1_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_mdo@
hs_bindgen_d1e3196c869f9fa1 :: IO ()
hs_bindgen_d1e3196c869f9fa1 =
  RIP.fromFFIType hs_bindgen_d1e3196c869f9fa1_base

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h 5:6@

    __exported by:__ @edge-cases\/names.h@
-}
mdo' :: IO ()
mdo' = hs_bindgen_d1e3196c869f9fa1

-- __unique:__ @test_edgecasesnames_Example_Safe_pattern@
foreign import ccall safe "hs_bindgen_d3dcd898c88fb2e0" hs_bindgen_d3dcd898c88fb2e0_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_pattern@
hs_bindgen_d3dcd898c88fb2e0 :: IO ()
hs_bindgen_d3dcd898c88fb2e0 =
  RIP.fromFFIType hs_bindgen_d3dcd898c88fb2e0_base

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h 6:6@

    __exported by:__ @edge-cases\/names.h@
-}
pattern' :: IO ()
pattern' = hs_bindgen_d3dcd898c88fb2e0

-- __unique:__ @test_edgecasesnames_Example_Safe_proc@
foreign import ccall safe "hs_bindgen_7f08456473f564e3" hs_bindgen_7f08456473f564e3_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_proc@
hs_bindgen_7f08456473f564e3 :: IO ()
hs_bindgen_7f08456473f564e3 =
  RIP.fromFFIType hs_bindgen_7f08456473f564e3_base

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h 7:6@

    __exported by:__ @edge-cases\/names.h@
-}
proc' :: IO ()
proc' = hs_bindgen_7f08456473f564e3

-- __unique:__ @test_edgecasesnames_Example_Safe_rec@
foreign import ccall safe "hs_bindgen_5df814c22f546599" hs_bindgen_5df814c22f546599_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_rec@
hs_bindgen_5df814c22f546599 :: IO ()
hs_bindgen_5df814c22f546599 =
  RIP.fromFFIType hs_bindgen_5df814c22f546599_base

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h 8:6@

    __exported by:__ @edge-cases\/names.h@
-}
rec' :: IO ()
rec' = hs_bindgen_5df814c22f546599

-- __unique:__ @test_edgecasesnames_Example_Safe_using@
foreign import ccall safe "hs_bindgen_3877b6deb653b5a4" hs_bindgen_3877b6deb653b5a4_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_using@
hs_bindgen_3877b6deb653b5a4 :: IO ()
hs_bindgen_3877b6deb653b5a4 =
  RIP.fromFFIType hs_bindgen_3877b6deb653b5a4_base

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h 9:6@

    __exported by:__ @edge-cases\/names.h@
-}
using' :: IO ()
using' = hs_bindgen_3877b6deb653b5a4

-- __unique:__ @test_edgecasesnames_Example_Safe_anyclass@
foreign import ccall safe "hs_bindgen_7b8790d04357731b" hs_bindgen_7b8790d04357731b_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_anyclass@
hs_bindgen_7b8790d04357731b :: IO ()
hs_bindgen_7b8790d04357731b =
  RIP.fromFFIType hs_bindgen_7b8790d04357731b_base

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h 12:6@

    __exported by:__ @edge-cases\/names.h@
-}
anyclass :: IO ()
anyclass = hs_bindgen_7b8790d04357731b

-- __unique:__ @test_edgecasesnames_Example_Safe_capi@
foreign import ccall safe "hs_bindgen_0bf4ab515f3279b9" hs_bindgen_0bf4ab515f3279b9_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_capi@
hs_bindgen_0bf4ab515f3279b9 :: IO ()
hs_bindgen_0bf4ab515f3279b9 =
  RIP.fromFFIType hs_bindgen_0bf4ab515f3279b9_base

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h 13:6@

    __exported by:__ @edge-cases\/names.h@
-}
capi :: IO ()
capi = hs_bindgen_0bf4ab515f3279b9

-- __unique:__ @test_edgecasesnames_Example_Safe_cases@
foreign import ccall safe "hs_bindgen_fa8166b2793e4236" hs_bindgen_fa8166b2793e4236_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_cases@
hs_bindgen_fa8166b2793e4236 :: IO ()
hs_bindgen_fa8166b2793e4236 =
  RIP.fromFFIType hs_bindgen_fa8166b2793e4236_base

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h 14:6@

    __exported by:__ @edge-cases\/names.h@
-}
cases :: IO ()
cases = hs_bindgen_fa8166b2793e4236

-- __unique:__ @test_edgecasesnames_Example_Safe_ccall@
foreign import ccall safe "hs_bindgen_decc2d43a62d063d" hs_bindgen_decc2d43a62d063d_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_ccall@
hs_bindgen_decc2d43a62d063d :: IO ()
hs_bindgen_decc2d43a62d063d =
  RIP.fromFFIType hs_bindgen_decc2d43a62d063d_base

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h 15:6@

    __exported by:__ @edge-cases\/names.h@
-}
ccall :: IO ()
ccall = hs_bindgen_decc2d43a62d063d

-- __unique:__ @test_edgecasesnames_Example_Safe_dynamic@
foreign import ccall safe "hs_bindgen_b5a75e2b6434134b" hs_bindgen_b5a75e2b6434134b_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_dynamic@
hs_bindgen_b5a75e2b6434134b :: IO ()
hs_bindgen_b5a75e2b6434134b =
  RIP.fromFFIType hs_bindgen_b5a75e2b6434134b_base

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h 16:6@

    __exported by:__ @edge-cases\/names.h@
-}
dynamic :: IO ()
dynamic = hs_bindgen_b5a75e2b6434134b

-- __unique:__ @test_edgecasesnames_Example_Safe_export@
foreign import ccall safe "hs_bindgen_406f7b014573b3d3" hs_bindgen_406f7b014573b3d3_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_export@
hs_bindgen_406f7b014573b3d3 :: IO ()
hs_bindgen_406f7b014573b3d3 =
  RIP.fromFFIType hs_bindgen_406f7b014573b3d3_base

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h 17:6@

    __exported by:__ @edge-cases\/names.h@
-}
export :: IO ()
export = hs_bindgen_406f7b014573b3d3

-- __unique:__ @test_edgecasesnames_Example_Safe_family@
foreign import ccall safe "hs_bindgen_14aab2af04efc222" hs_bindgen_14aab2af04efc222_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_family@
hs_bindgen_14aab2af04efc222 :: IO ()
hs_bindgen_14aab2af04efc222 =
  RIP.fromFFIType hs_bindgen_14aab2af04efc222_base

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h 18:6@

    __exported by:__ @edge-cases\/names.h@
-}
family :: IO ()
family = hs_bindgen_14aab2af04efc222

-- __unique:__ @test_edgecasesnames_Example_Safe_group@
foreign import ccall safe "hs_bindgen_ee9285b26b11b393" hs_bindgen_ee9285b26b11b393_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_group@
hs_bindgen_ee9285b26b11b393 :: IO ()
hs_bindgen_ee9285b26b11b393 =
  RIP.fromFFIType hs_bindgen_ee9285b26b11b393_base

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h 19:6@

    __exported by:__ @edge-cases\/names.h@
-}
group :: IO ()
group = hs_bindgen_ee9285b26b11b393

-- __unique:__ @test_edgecasesnames_Example_Safe_interruptible@
foreign import ccall safe "hs_bindgen_b2463d5c1d51883e" hs_bindgen_b2463d5c1d51883e_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_interruptible@
hs_bindgen_b2463d5c1d51883e :: IO ()
hs_bindgen_b2463d5c1d51883e =
  RIP.fromFFIType hs_bindgen_b2463d5c1d51883e_base

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h 20:6@

    __exported by:__ @edge-cases\/names.h@
-}
interruptible :: IO ()
interruptible = hs_bindgen_b2463d5c1d51883e

-- __unique:__ @test_edgecasesnames_Example_Safe_javascript@
foreign import ccall safe "hs_bindgen_55a55462d9cd296c" hs_bindgen_55a55462d9cd296c_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_javascript@
hs_bindgen_55a55462d9cd296c :: IO ()
hs_bindgen_55a55462d9cd296c =
  RIP.fromFFIType hs_bindgen_55a55462d9cd296c_base

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h 21:6@

    __exported by:__ @edge-cases\/names.h@
-}
javascript :: IO ()
javascript = hs_bindgen_55a55462d9cd296c

-- __unique:__ @test_edgecasesnames_Example_Safe_label@
foreign import ccall safe "hs_bindgen_227f34efb176d1fb" hs_bindgen_227f34efb176d1fb_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_label@
hs_bindgen_227f34efb176d1fb :: IO ()
hs_bindgen_227f34efb176d1fb =
  RIP.fromFFIType hs_bindgen_227f34efb176d1fb_base

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h 22:6@

    __exported by:__ @edge-cases\/names.h@
-}
label :: IO ()
label = hs_bindgen_227f34efb176d1fb

-- __unique:__ @test_edgecasesnames_Example_Safe_prim@
foreign import ccall safe "hs_bindgen_51c6e0d18dce403a" hs_bindgen_51c6e0d18dce403a_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_prim@
hs_bindgen_51c6e0d18dce403a :: IO ()
hs_bindgen_51c6e0d18dce403a =
  RIP.fromFFIType hs_bindgen_51c6e0d18dce403a_base

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h 23:6@

    __exported by:__ @edge-cases\/names.h@
-}
prim :: IO ()
prim = hs_bindgen_51c6e0d18dce403a

-- __unique:__ @test_edgecasesnames_Example_Safe_role@
foreign import ccall safe "hs_bindgen_f85f2418d208e6a0" hs_bindgen_f85f2418d208e6a0_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_role@
hs_bindgen_f85f2418d208e6a0 :: IO ()
hs_bindgen_f85f2418d208e6a0 =
  RIP.fromFFIType hs_bindgen_f85f2418d208e6a0_base

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h 24:6@

    __exported by:__ @edge-cases\/names.h@
-}
role :: IO ()
role = hs_bindgen_f85f2418d208e6a0

-- __unique:__ @test_edgecasesnames_Example_Safe_safe@
foreign import ccall safe "hs_bindgen_0855ecbc4b53ebbb" hs_bindgen_0855ecbc4b53ebbb_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_safe@
hs_bindgen_0855ecbc4b53ebbb :: IO ()
hs_bindgen_0855ecbc4b53ebbb =
  RIP.fromFFIType hs_bindgen_0855ecbc4b53ebbb_base

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h 25:6@

    __exported by:__ @edge-cases\/names.h@
-}
safe :: IO ()
safe = hs_bindgen_0855ecbc4b53ebbb

-- __unique:__ @test_edgecasesnames_Example_Safe_stdcall@
foreign import ccall safe "hs_bindgen_e5238d13788a6df9" hs_bindgen_e5238d13788a6df9_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_stdcall@
hs_bindgen_e5238d13788a6df9 :: IO ()
hs_bindgen_e5238d13788a6df9 =
  RIP.fromFFIType hs_bindgen_e5238d13788a6df9_base

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h 26:6@

    __exported by:__ @edge-cases\/names.h@
-}
stdcall :: IO ()
stdcall = hs_bindgen_e5238d13788a6df9

-- __unique:__ @test_edgecasesnames_Example_Safe_stock@
foreign import ccall safe "hs_bindgen_cf38d8bd096a7a42" hs_bindgen_cf38d8bd096a7a42_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_stock@
hs_bindgen_cf38d8bd096a7a42 :: IO ()
hs_bindgen_cf38d8bd096a7a42 =
  RIP.fromFFIType hs_bindgen_cf38d8bd096a7a42_base

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h 27:6@

    __exported by:__ @edge-cases\/names.h@
-}
stock :: IO ()
stock = hs_bindgen_cf38d8bd096a7a42

-- __unique:__ @test_edgecasesnames_Example_Safe_unsafe@
foreign import ccall safe "hs_bindgen_fda9b083b24404f0" hs_bindgen_fda9b083b24404f0_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_unsafe@
hs_bindgen_fda9b083b24404f0 :: IO ()
hs_bindgen_fda9b083b24404f0 =
  RIP.fromFFIType hs_bindgen_fda9b083b24404f0_base

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h 28:6@

    __exported by:__ @edge-cases\/names.h@
-}
unsafe :: IO ()
unsafe = hs_bindgen_fda9b083b24404f0

-- __unique:__ @test_edgecasesnames_Example_Safe_via@
foreign import ccall safe "hs_bindgen_1d2a76a3a595be25" hs_bindgen_1d2a76a3a595be25_base ::
     IO ()

-- __unique:__ @test_edgecasesnames_Example_Safe_via@
hs_bindgen_1d2a76a3a595be25 :: IO ()
hs_bindgen_1d2a76a3a595be25 =
  RIP.fromFFIType hs_bindgen_1d2a76a3a595be25_base

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h 29:6@

    __exported by:__ @edge-cases\/names.h@
-}
via :: IO ()
via = hs_bindgen_1d2a76a3a595be25
