{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/names.h>"
  , "void hs_bindgen_test_edgecasesnames_77b774d3e1fe681c (void)"
  , "{"
  , "  by();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_5be6182ce9524fbc (void)"
  , "{"
  , "  forall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_e08c482e0a81c17b (void)"
  , "{"
  , "  mdo();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_0399be1dee5140e1 (void)"
  , "{"
  , "  pattern();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_94a56da41db69d94 (void)"
  , "{"
  , "  proc();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_a00df748e87b00e1 (void)"
  , "{"
  , "  rec();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_88c7636004fad38f (void)"
  , "{"
  , "  using();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_ff3ce64178a0cbd6 (void)"
  , "{"
  , "  anyclass();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_0849bbffba9b5577 (void)"
  , "{"
  , "  capi();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_3ef285f011098609 (void)"
  , "{"
  , "  cases();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_0f61df1a46a35919 (void)"
  , "{"
  , "  ccall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_3413829455250a72 (void)"
  , "{"
  , "  dynamic();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_eae216d323a1a759 (void)"
  , "{"
  , "  export();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_915da0024b64f741 (void)"
  , "{"
  , "  family();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_2e2287f6825c3747 (void)"
  , "{"
  , "  group();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_3fe62d3b9ca87190 (void)"
  , "{"
  , "  interruptible();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_6783575b15866c9e (void)"
  , "{"
  , "  javascript();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_6195918dc5ce6135 (void)"
  , "{"
  , "  label();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_f0977fcecb721c51 (void)"
  , "{"
  , "  prim();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_3c2ca7cd36cfbeb6 (void)"
  , "{"
  , "  role();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_acb7881a4364f913 (void)"
  , "{"
  , "  safe();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_7b1ddbc7401672da (void)"
  , "{"
  , "  stdcall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_e8769a2b297c5b53 (void)"
  , "{"
  , "  stock();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_cc8aa36a59d54411 (void)"
  , "{"
  , "  unsafe();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_fb82461452130ee6 (void)"
  , "{"
  , "  via();"
  , "}"
  ]))

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h:3:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_by@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_77b774d3e1fe681c" by' ::
     IO ()

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h:4:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_forall@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_5be6182ce9524fbc" forall' ::
     IO ()

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h:5:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_mdo@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_e08c482e0a81c17b" mdo' ::
     IO ()

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h:6:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_pattern@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_0399be1dee5140e1" pattern' ::
     IO ()

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h:7:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_proc@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_94a56da41db69d94" proc' ::
     IO ()

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h:8:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_rec@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_a00df748e87b00e1" rec' ::
     IO ()

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h:9:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_using@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_88c7636004fad38f" using' ::
     IO ()

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h:12:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_anyclass@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_ff3ce64178a0cbd6" anyclass ::
     IO ()

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h:13:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_capi@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_0849bbffba9b5577" capi ::
     IO ()

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h:14:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_cases@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_3ef285f011098609" cases ::
     IO ()

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h:15:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_ccall@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_0f61df1a46a35919" ccall ::
     IO ()

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h:16:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_dynamic@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_3413829455250a72" dynamic ::
     IO ()

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h:17:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_export@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_eae216d323a1a759" export ::
     IO ()

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h:18:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_family@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_915da0024b64f741" family ::
     IO ()

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h:19:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_group@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_2e2287f6825c3747" group ::
     IO ()

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h:20:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_interruptible@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_3fe62d3b9ca87190" interruptible ::
     IO ()

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h:21:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_javascript@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_6783575b15866c9e" javascript ::
     IO ()

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h:22:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_label@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_6195918dc5ce6135" label ::
     IO ()

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h:23:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_prim@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_f0977fcecb721c51" prim ::
     IO ()

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h:24:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_role@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_3c2ca7cd36cfbeb6" role ::
     IO ()

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h:25:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_safe@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_acb7881a4364f913" safe ::
     IO ()

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h:26:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_stdcall@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_7b1ddbc7401672da" stdcall ::
     IO ()

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h:27:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_stock@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_e8769a2b297c5b53" stock ::
     IO ()

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h:28:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_unsafe@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_cc8aa36a59d54411" unsafe ::
     IO ()

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h:29:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @Example_Unsafe_via@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_fb82461452130ee6" via ::
     IO ()
