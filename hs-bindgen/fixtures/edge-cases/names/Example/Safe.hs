{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/names.h>"
  , "void hs_bindgen_test_edgecasesnames_6b1683eab27dd041 (void)"
  , "{"
  , "  by();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_76e56e69d7bd6504 (void)"
  , "{"
  , "  forall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_f3eb907265b16dff (void)"
  , "{"
  , "  mdo();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_cbf9098e12568a15 (void)"
  , "{"
  , "  pattern();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_d0994563528bf3d7 (void)"
  , "{"
  , "  proc();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_f77195db1f5abc2a (void)"
  , "{"
  , "  rec();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_3925bb0a7ff69a5a (void)"
  , "{"
  , "  using();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_c8a44b3f295df2e6 (void)"
  , "{"
  , "  anyclass();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_83655361fd26da45 (void)"
  , "{"
  , "  capi();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_b2b0213b89941cb8 (void)"
  , "{"
  , "  cases();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_7acec9ea3af1ce41 (void)"
  , "{"
  , "  ccall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_525ab64a73fd6954 (void)"
  , "{"
  , "  dynamic();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_f334bbd66ff4ea9e (void)"
  , "{"
  , "  export();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_b902a9b1963f0dd5 (void)"
  , "{"
  , "  family();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_8096173b0135b06a (void)"
  , "{"
  , "  group();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_bcd7b1b3b04a4a4b (void)"
  , "{"
  , "  interruptible();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_160cfd915f9c110b (void)"
  , "{"
  , "  javascript();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_a004a476bf49f442 (void)"
  , "{"
  , "  label();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_a7a302dacb9687dd (void)"
  , "{"
  , "  prim();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_f2f5d60ec656e031 (void)"
  , "{"
  , "  role();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_4c4ff28358b74c7a (void)"
  , "{"
  , "  safe();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_af83e769e04ddc19 (void)"
  , "{"
  , "  stdcall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_31665b564093da49 (void)"
  , "{"
  , "  stock();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_64c248daa510f48e (void)"
  , "{"
  , "  unsafe();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_9f3a6f774a6f0f30 (void)"
  , "{"
  , "  via();"
  , "}"
  ]))

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h:3:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_by@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_6b1683eab27dd041" by' ::
     IO ()

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h:4:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_forall@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_76e56e69d7bd6504" forall' ::
     IO ()

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h:5:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_mdo@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_f3eb907265b16dff" mdo' ::
     IO ()

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h:6:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_pattern@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_cbf9098e12568a15" pattern' ::
     IO ()

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h:7:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_proc@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_d0994563528bf3d7" proc' ::
     IO ()

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h:8:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_rec@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_f77195db1f5abc2a" rec' ::
     IO ()

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h:9:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_using@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_3925bb0a7ff69a5a" using' ::
     IO ()

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h:12:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_anyclass@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_c8a44b3f295df2e6" anyclass ::
     IO ()

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h:13:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_capi@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_83655361fd26da45" capi ::
     IO ()

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h:14:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_cases@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_b2b0213b89941cb8" cases ::
     IO ()

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h:15:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_ccall@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_7acec9ea3af1ce41" ccall ::
     IO ()

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h:16:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_dynamic@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_525ab64a73fd6954" dynamic ::
     IO ()

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h:17:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_export@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_f334bbd66ff4ea9e" export ::
     IO ()

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h:18:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_family@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_b902a9b1963f0dd5" family ::
     IO ()

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h:19:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_group@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_8096173b0135b06a" group ::
     IO ()

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h:20:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_interruptible@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_bcd7b1b3b04a4a4b" interruptible ::
     IO ()

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h:21:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_javascript@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_160cfd915f9c110b" javascript ::
     IO ()

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h:22:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_label@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_a004a476bf49f442" label ::
     IO ()

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h:23:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_prim@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_a7a302dacb9687dd" prim ::
     IO ()

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h:24:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_role@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_f2f5d60ec656e031" role ::
     IO ()

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h:25:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_safe@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_4c4ff28358b74c7a" safe ::
     IO ()

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h:26:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_stdcall@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_af83e769e04ddc19" stdcall ::
     IO ()

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h:27:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_stock@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_31665b564093da49" stock ::
     IO ()

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h:28:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_unsafe@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_64c248daa510f48e" unsafe ::
     IO ()

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h:29:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Safe_via@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_9f3a6f774a6f0f30" via ::
     IO ()
