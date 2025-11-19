{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/names.h>"
  , "void hs_bindgen_test_edgecasesnames_757d799fd708dfd3 (void)"
  , "{"
  , "  by();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_56a86e542f270644 (void)"
  , "{"
  , "  forall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_66ccc318fc33f3db (void)"
  , "{"
  , "  mdo();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_59e8310c2fbfd7a5 (void)"
  , "{"
  , "  pattern();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_0e3a2ce0789fb7e6 (void)"
  , "{"
  , "  proc();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_a8bd1db6a6708dae (void)"
  , "{"
  , "  rec();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_4f84a89a7d96b0e6 (void)"
  , "{"
  , "  using();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_e4c0e5d7430611d1 (void)"
  , "{"
  , "  anyclass();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_0b2f0d5537de919f (void)"
  , "{"
  , "  capi();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_1ffa6d7a4b3beb7c (void)"
  , "{"
  , "  cases();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_ebc1b326e523e58f (void)"
  , "{"
  , "  ccall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_ab433e9e3ef36d2f (void)"
  , "{"
  , "  dynamic();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_df679baa272472fa (void)"
  , "{"
  , "  export();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_9a60b7b7be9ca341 (void)"
  , "{"
  , "  family();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_3923700cc828a4dd (void)"
  , "{"
  , "  group();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_ba3f97d9b80e552c (void)"
  , "{"
  , "  interruptible();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_f348c51860201a12 (void)"
  , "{"
  , "  javascript();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_464d035f44366a5b (void)"
  , "{"
  , "  label();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_1a1c4d5a866be872 (void)"
  , "{"
  , "  prim();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_2b805a841a0f220d (void)"
  , "{"
  , "  role();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_efbfe85dc9b68bc4 (void)"
  , "{"
  , "  safe();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_27ef4256cbaf6815 (void)"
  , "{"
  , "  stdcall();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_6e4ae46773b9e016 (void)"
  , "{"
  , "  stock();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_03cf69812528c410 (void)"
  , "{"
  , "  unsafe();"
  , "}"
  , "void hs_bindgen_test_edgecasesnames_e611a3f38f7a3eb9 (void)"
  , "{"
  , "  via();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_757d799fd708dfd3" by'_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_56a86e542f270644" forall'_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_66ccc318fc33f3db" mdo'_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_59e8310c2fbfd7a5" pattern'_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_0e3a2ce0789fb7e6" proc'_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_a8bd1db6a6708dae" rec'_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_4f84a89a7d96b0e6" using'_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_e4c0e5d7430611d1" anyclass_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_0b2f0d5537de919f" capi_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_1ffa6d7a4b3beb7c" cases_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_ebc1b326e523e58f" ccall_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_ab433e9e3ef36d2f" dynamic_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_df679baa272472fa" export_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_9a60b7b7be9ca341" family_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_3923700cc828a4dd" group_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_ba3f97d9b80e552c" interruptible_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_f348c51860201a12" javascript_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_464d035f44366a5b" label_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_1a1c4d5a866be872" prim_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_2b805a841a0f220d" role_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_efbfe85dc9b68bc4" safe_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_27ef4256cbaf6815" stdcall_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_6e4ae46773b9e016" stock_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_03cf69812528c410" unsafe_base ::
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
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_e611a3f38f7a3eb9" via_base ::
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
