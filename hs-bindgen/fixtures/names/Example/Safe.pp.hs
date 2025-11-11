{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <names.h>"
  , "void hs_bindgen_test_names_0e7f535b7085fc20 (void)"
  , "{"
  , "  by();"
  , "}"
  , "void hs_bindgen_test_names_fc72c248490e573d (void)"
  , "{"
  , "  forall();"
  , "}"
  , "void hs_bindgen_test_names_f21dd25a8b27374a (void)"
  , "{"
  , "  mdo();"
  , "}"
  , "void hs_bindgen_test_names_0ab5fb5a8105f789 (void)"
  , "{"
  , "  pattern();"
  , "}"
  , "void hs_bindgen_test_names_c14da8252137427c (void)"
  , "{"
  , "  proc();"
  , "}"
  , "void hs_bindgen_test_names_1706dcb069f095bc (void)"
  , "{"
  , "  rec();"
  , "}"
  , "void hs_bindgen_test_names_9cbe86d126f1d8ee (void)"
  , "{"
  , "  using();"
  , "}"
  , "void hs_bindgen_test_names_84e650893f3a92e0 (void)"
  , "{"
  , "  anyclass();"
  , "}"
  , "void hs_bindgen_test_names_a931e0b3ecf47315 (void)"
  , "{"
  , "  capi();"
  , "}"
  , "void hs_bindgen_test_names_8a21f58b60402b4c (void)"
  , "{"
  , "  cases();"
  , "}"
  , "void hs_bindgen_test_names_e72f0a6b1520490f (void)"
  , "{"
  , "  ccall();"
  , "}"
  , "void hs_bindgen_test_names_87e085120fafde48 (void)"
  , "{"
  , "  dynamic();"
  , "}"
  , "void hs_bindgen_test_names_0e6d6a351c0f2d6b (void)"
  , "{"
  , "  export();"
  , "}"
  , "void hs_bindgen_test_names_bd4d89d0e474ae7a (void)"
  , "{"
  , "  family();"
  , "}"
  , "void hs_bindgen_test_names_d33488d9cb279feb (void)"
  , "{"
  , "  group();"
  , "}"
  , "void hs_bindgen_test_names_9a3851f5a39a9e5d (void)"
  , "{"
  , "  interruptible();"
  , "}"
  , "void hs_bindgen_test_names_fa9100d6d4da2bd4 (void)"
  , "{"
  , "  javascript();"
  , "}"
  , "void hs_bindgen_test_names_264e9aa826f1d8fd (void)"
  , "{"
  , "  label();"
  , "}"
  , "void hs_bindgen_test_names_06aa327310da5120 (void)"
  , "{"
  , "  prim();"
  , "}"
  , "void hs_bindgen_test_names_0c2d88e0ce229900 (void)"
  , "{"
  , "  role();"
  , "}"
  , "void hs_bindgen_test_names_a703e7f54db3712e (void)"
  , "{"
  , "  safe();"
  , "}"
  , "void hs_bindgen_test_names_001ca64746b969f3 (void)"
  , "{"
  , "  stdcall();"
  , "}"
  , "void hs_bindgen_test_names_44ab69657e70c20b (void)"
  , "{"
  , "  stock();"
  , "}"
  , "void hs_bindgen_test_names_cf74a7d7f3568406 (void)"
  , "{"
  , "  unsafe();"
  , "}"
  , "void hs_bindgen_test_names_f24fb703d4751a60 (void)"
  , "{"
  , "  via();"
  , "}"
  ]))

{-| __C declaration:__ @by@

    __defined at:__ @names.h:3:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0e7f535b7085fc20" by' ::
     IO ()

{-| __C declaration:__ @forall@

    __defined at:__ @names.h:4:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_fc72c248490e573d" forall' ::
     IO ()

{-| __C declaration:__ @mdo@

    __defined at:__ @names.h:5:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_f21dd25a8b27374a" mdo' ::
     IO ()

{-| __C declaration:__ @pattern@

    __defined at:__ @names.h:6:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0ab5fb5a8105f789" pattern' ::
     IO ()

{-| __C declaration:__ @proc@

    __defined at:__ @names.h:7:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_c14da8252137427c" proc' ::
     IO ()

{-| __C declaration:__ @rec@

    __defined at:__ @names.h:8:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_1706dcb069f095bc" rec' ::
     IO ()

{-| __C declaration:__ @using@

    __defined at:__ @names.h:9:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_9cbe86d126f1d8ee" using' ::
     IO ()

{-| __C declaration:__ @anyclass@

    __defined at:__ @names.h:12:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_84e650893f3a92e0" anyclass ::
     IO ()

{-| __C declaration:__ @capi@

    __defined at:__ @names.h:13:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_a931e0b3ecf47315" capi ::
     IO ()

{-| __C declaration:__ @cases@

    __defined at:__ @names.h:14:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_8a21f58b60402b4c" cases ::
     IO ()

{-| __C declaration:__ @ccall@

    __defined at:__ @names.h:15:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_e72f0a6b1520490f" ccall ::
     IO ()

{-| __C declaration:__ @dynamic@

    __defined at:__ @names.h:16:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_87e085120fafde48" dynamic ::
     IO ()

{-| __C declaration:__ @export@

    __defined at:__ @names.h:17:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0e6d6a351c0f2d6b" export ::
     IO ()

{-| __C declaration:__ @family@

    __defined at:__ @names.h:18:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_bd4d89d0e474ae7a" family ::
     IO ()

{-| __C declaration:__ @group@

    __defined at:__ @names.h:19:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_d33488d9cb279feb" group ::
     IO ()

{-| __C declaration:__ @interruptible@

    __defined at:__ @names.h:20:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_9a3851f5a39a9e5d" interruptible ::
     IO ()

{-| __C declaration:__ @javascript@

    __defined at:__ @names.h:21:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_fa9100d6d4da2bd4" javascript ::
     IO ()

{-| __C declaration:__ @label@

    __defined at:__ @names.h:22:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_264e9aa826f1d8fd" label ::
     IO ()

{-| __C declaration:__ @prim@

    __defined at:__ @names.h:23:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_06aa327310da5120" prim ::
     IO ()

{-| __C declaration:__ @role@

    __defined at:__ @names.h:24:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0c2d88e0ce229900" role ::
     IO ()

{-| __C declaration:__ @safe@

    __defined at:__ @names.h:25:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_a703e7f54db3712e" safe ::
     IO ()

{-| __C declaration:__ @stdcall@

    __defined at:__ @names.h:26:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_001ca64746b969f3" stdcall ::
     IO ()

{-| __C declaration:__ @stock@

    __defined at:__ @names.h:27:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_44ab69657e70c20b" stock ::
     IO ()

{-| __C declaration:__ @unsafe@

    __defined at:__ @names.h:28:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_cf74a7d7f3568406" unsafe ::
     IO ()

{-| __C declaration:__ @via@

    __defined at:__ @names.h:29:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_f24fb703d4751a60" via ::
     IO ()
