{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/names.h>"
  , "void hs_bindgen_test_edgecasesnames_0e7f535b7085fc20 (void)"
  , "{"
  , "  by();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_fc72c248490e573d (void)"
  , "{"
  , "  forall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_f21dd25a8b27374a (void)"
  , "{"
  , "  mdo();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_0ab5fb5a8105f789 (void)"
  , "{"
  , "  pattern();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_c14da8252137427c (void)"
  , "{"
  , "  proc();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_1706dcb069f095bc (void)"
  , "{"
  , "  rec();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_9cbe86d126f1d8ee (void)"
  , "{"
  , "  using();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_84e650893f3a92e0 (void)"
  , "{"
  , "  anyclass();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_a931e0b3ecf47315 (void)"
  , "{"
  , "  capi();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_8a21f58b60402b4c (void)"
  , "{"
  , "  cases();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_e72f0a6b1520490f (void)"
  , "{"
  , "  ccall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_87e085120fafde48 (void)"
  , "{"
  , "  dynamic();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_0e6d6a351c0f2d6b (void)"
  , "{"
  , "  export();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_bd4d89d0e474ae7a (void)"
  , "{"
  , "  family();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_d33488d9cb279feb (void)"
  , "{"
  , "  group();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_9a3851f5a39a9e5d (void)"
  , "{"
  , "  interruptible();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_fa9100d6d4da2bd4 (void)"
  , "{"
  , "  javascript();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_264e9aa826f1d8fd (void)"
  , "{"
  , "  label();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_06aa327310da5120 (void)"
  , "{"
  , "  prim();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_0c2d88e0ce229900 (void)"
  , "{"
  , "  role();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_a703e7f54db3712e (void)"
  , "{"
  , "  safe();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_001ca64746b969f3 (void)"
  , "{"
  , "  stdcall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_44ab69657e70c20b (void)"
  , "{"
  , "  stock();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_cf74a7d7f3568406 (void)"
  , "{"
  , "  unsafe();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_f24fb703d4751a60 (void)"
  , "{"
  , "  via();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_0e7f535b7085fc20" by'_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h:3:6@

    __exported by:__ @edge-cases\/names.h@
-}
by' ::
     IO ()
by' =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType by'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_fc72c248490e573d" forall'_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h:4:6@

    __exported by:__ @edge-cases\/names.h@
-}
forall' ::
     IO ()
forall' =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType forall'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_f21dd25a8b27374a" mdo'_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h:5:6@

    __exported by:__ @edge-cases\/names.h@
-}
mdo' ::
     IO ()
mdo' =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType mdo'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_0ab5fb5a8105f789" pattern'_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h:6:6@

    __exported by:__ @edge-cases\/names.h@
-}
pattern' ::
     IO ()
pattern' =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType pattern'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_c14da8252137427c" proc'_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h:7:6@

    __exported by:__ @edge-cases\/names.h@
-}
proc' ::
     IO ()
proc' =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType proc'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_1706dcb069f095bc" rec'_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h:8:6@

    __exported by:__ @edge-cases\/names.h@
-}
rec' ::
     IO ()
rec' =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType rec'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_9cbe86d126f1d8ee" using'_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h:9:6@

    __exported by:__ @edge-cases\/names.h@
-}
using' ::
     IO ()
using' =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType using'_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_84e650893f3a92e0" anyclass_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h:12:6@

    __exported by:__ @edge-cases\/names.h@
-}
anyclass ::
     IO ()
anyclass =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType anyclass_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_a931e0b3ecf47315" capi_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h:13:6@

    __exported by:__ @edge-cases\/names.h@
-}
capi ::
     IO ()
capi =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType capi_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_8a21f58b60402b4c" cases_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h:14:6@

    __exported by:__ @edge-cases\/names.h@
-}
cases ::
     IO ()
cases =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType cases_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_e72f0a6b1520490f" ccall_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h:15:6@

    __exported by:__ @edge-cases\/names.h@
-}
ccall ::
     IO ()
ccall =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType ccall_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_87e085120fafde48" dynamic_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h:16:6@

    __exported by:__ @edge-cases\/names.h@
-}
dynamic ::
     IO ()
dynamic =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType dynamic_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_0e6d6a351c0f2d6b" export_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h:17:6@

    __exported by:__ @edge-cases\/names.h@
-}
export ::
     IO ()
export =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType export_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_bd4d89d0e474ae7a" family_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h:18:6@

    __exported by:__ @edge-cases\/names.h@
-}
family ::
     IO ()
family =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType family_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_d33488d9cb279feb" group_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h:19:6@

    __exported by:__ @edge-cases\/names.h@
-}
group ::
     IO ()
group =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType group_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_9a3851f5a39a9e5d" interruptible_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h:20:6@

    __exported by:__ @edge-cases\/names.h@
-}
interruptible ::
     IO ()
interruptible =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType interruptible_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_fa9100d6d4da2bd4" javascript_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h:21:6@

    __exported by:__ @edge-cases\/names.h@
-}
javascript ::
     IO ()
javascript =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType javascript_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_264e9aa826f1d8fd" label_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h:22:6@

    __exported by:__ @edge-cases\/names.h@
-}
label ::
     IO ()
label =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType label_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_06aa327310da5120" prim_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h:23:6@

    __exported by:__ @edge-cases\/names.h@
-}
prim ::
     IO ()
prim =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType prim_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_0c2d88e0ce229900" role_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h:24:6@

    __exported by:__ @edge-cases\/names.h@
-}
role ::
     IO ()
role =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType role_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_a703e7f54db3712e" safe_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h:25:6@

    __exported by:__ @edge-cases\/names.h@
-}
safe ::
     IO ()
safe =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType safe_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_001ca64746b969f3" stdcall_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h:26:6@

    __exported by:__ @edge-cases\/names.h@
-}
stdcall ::
     IO ()
stdcall =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType stdcall_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_44ab69657e70c20b" stock_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h:27:6@

    __exported by:__ @edge-cases\/names.h@
-}
stock ::
     IO ()
stock =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType stock_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_cf74a7d7f3568406" unsafe_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h:28:6@

    __exported by:__ @edge-cases\/names.h@
-}
unsafe ::
     IO ()
unsafe =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType unsafe_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesnames_f24fb703d4751a60" via_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h:29:6@

    __exported by:__ @edge-cases\/names.h@
-}
via ::
     IO ()
via =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType via_base
