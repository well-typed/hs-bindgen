{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

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

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h:3:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_by@
-}
foreign import ccall unsafe "hs_bindgen_28b998af1f39a743" by' ::
     IO ()

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h:4:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_forall@
-}
foreign import ccall unsafe "hs_bindgen_5d7ea7c4d11a5fc8" forall' ::
     IO ()

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h:5:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_mdo@
-}
foreign import ccall unsafe "hs_bindgen_2d65448c684c09d5" mdo' ::
     IO ()

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h:6:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_pattern@
-}
foreign import ccall unsafe "hs_bindgen_13fe653d670d3712" pattern' ::
     IO ()

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h:7:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_proc@
-}
foreign import ccall unsafe "hs_bindgen_e9cc2037d33041aa" proc' ::
     IO ()

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h:8:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_rec@
-}
foreign import ccall unsafe "hs_bindgen_4a1e741f9ef596ff" rec' ::
     IO ()

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h:9:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_using@
-}
foreign import ccall unsafe "hs_bindgen_ef6f3f22c615db58" using' ::
     IO ()

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h:12:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_anyclass@
-}
foreign import ccall unsafe "hs_bindgen_3c7afeaaf3ff040b" anyclass ::
     IO ()

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h:13:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_capi@
-}
foreign import ccall unsafe "hs_bindgen_0518740d4c3caa1d" capi ::
     IO ()

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h:14:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_cases@
-}
foreign import ccall unsafe "hs_bindgen_61f14ad7bb2e3d54" cases ::
     IO ()

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h:15:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_ccall@
-}
foreign import ccall unsafe "hs_bindgen_ace8c96ed6673c3b" ccall ::
     IO ()

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h:16:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_dynamic@
-}
foreign import ccall unsafe "hs_bindgen_8865833b99552d03" dynamic ::
     IO ()

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h:17:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_export@
-}
foreign import ccall unsafe "hs_bindgen_15729ba251f5ec57" export ::
     IO ()

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h:18:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_family@
-}
foreign import ccall unsafe "hs_bindgen_e6a4f7e833da2687" family ::
     IO ()

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h:19:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_group@
-}
foreign import ccall unsafe "hs_bindgen_d4dd1bb5e95de858" group ::
     IO ()

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h:20:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_interruptible@
-}
foreign import ccall unsafe "hs_bindgen_516f1ad5aba6de29" interruptible ::
     IO ()

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h:21:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_javascript@
-}
foreign import ccall unsafe "hs_bindgen_214230db174dc3e6" javascript ::
     IO ()

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h:22:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_label@
-}
foreign import ccall unsafe "hs_bindgen_88f1f0cf9c0f080e" label ::
     IO ()

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h:23:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_prim@
-}
foreign import ccall unsafe "hs_bindgen_93a4c73f587dcf3c" prim ::
     IO ()

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h:24:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_role@
-}
foreign import ccall unsafe "hs_bindgen_a267fe5585862ecc" role ::
     IO ()

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h:25:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_safe@
-}
foreign import ccall unsafe "hs_bindgen_ddac4cdf91c756a8" safe ::
     IO ()

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h:26:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_stdcall@
-}
foreign import ccall unsafe "hs_bindgen_8dd57b02f322a7ae" stdcall ::
     IO ()

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h:27:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_stock@
-}
foreign import ccall unsafe "hs_bindgen_3b69e1860d72507c" stock ::
     IO ()

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h:28:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_unsafe@
-}
foreign import ccall unsafe "hs_bindgen_b9d80fa39d7ebb06" unsafe ::
     IO ()

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h:29:6@

    __exported by:__ @edge-cases\/names.h@

    __unique:__ @test_edgecasesnames_Example_Unsafe_via@
-}
foreign import ccall unsafe "hs_bindgen_708f6397f5e5ac73" via ::
     IO ()
