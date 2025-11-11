{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <names.h>"
  , "void hs_bindgen_test_names_757d799fd708dfd3 (void)"
  , "{"
  , "  by();"
  , "}"
  , "void hs_bindgen_test_names_56a86e542f270644 (void)"
  , "{"
  , "  forall();"
  , "}"
  , "void hs_bindgen_test_names_66ccc318fc33f3db (void)"
  , "{"
  , "  mdo();"
  , "}"
  , "void hs_bindgen_test_names_59e8310c2fbfd7a5 (void)"
  , "{"
  , "  pattern();"
  , "}"
  , "void hs_bindgen_test_names_0e3a2ce0789fb7e6 (void)"
  , "{"
  , "  proc();"
  , "}"
  , "void hs_bindgen_test_names_a8bd1db6a6708dae (void)"
  , "{"
  , "  rec();"
  , "}"
  , "void hs_bindgen_test_names_4f84a89a7d96b0e6 (void)"
  , "{"
  , "  using();"
  , "}"
  , "void hs_bindgen_test_names_e4c0e5d7430611d1 (void)"
  , "{"
  , "  anyclass();"
  , "}"
  , "void hs_bindgen_test_names_0b2f0d5537de919f (void)"
  , "{"
  , "  capi();"
  , "}"
  , "void hs_bindgen_test_names_1ffa6d7a4b3beb7c (void)"
  , "{"
  , "  cases();"
  , "}"
  , "void hs_bindgen_test_names_ebc1b326e523e58f (void)"
  , "{"
  , "  ccall();"
  , "}"
  , "void hs_bindgen_test_names_ab433e9e3ef36d2f (void)"
  , "{"
  , "  dynamic();"
  , "}"
  , "void hs_bindgen_test_names_df679baa272472fa (void)"
  , "{"
  , "  export();"
  , "}"
  , "void hs_bindgen_test_names_9a60b7b7be9ca341 (void)"
  , "{"
  , "  family();"
  , "}"
  , "void hs_bindgen_test_names_3923700cc828a4dd (void)"
  , "{"
  , "  group();"
  , "}"
  , "void hs_bindgen_test_names_ba3f97d9b80e552c (void)"
  , "{"
  , "  interruptible();"
  , "}"
  , "void hs_bindgen_test_names_f348c51860201a12 (void)"
  , "{"
  , "  javascript();"
  , "}"
  , "void hs_bindgen_test_names_464d035f44366a5b (void)"
  , "{"
  , "  label();"
  , "}"
  , "void hs_bindgen_test_names_1a1c4d5a866be872 (void)"
  , "{"
  , "  prim();"
  , "}"
  , "void hs_bindgen_test_names_2b805a841a0f220d (void)"
  , "{"
  , "  role();"
  , "}"
  , "void hs_bindgen_test_names_efbfe85dc9b68bc4 (void)"
  , "{"
  , "  safe();"
  , "}"
  , "void hs_bindgen_test_names_27ef4256cbaf6815 (void)"
  , "{"
  , "  stdcall();"
  , "}"
  , "void hs_bindgen_test_names_6e4ae46773b9e016 (void)"
  , "{"
  , "  stock();"
  , "}"
  , "void hs_bindgen_test_names_03cf69812528c410 (void)"
  , "{"
  , "  unsafe();"
  , "}"
  , "void hs_bindgen_test_names_e611a3f38f7a3eb9 (void)"
  , "{"
  , "  via();"
  , "}"
  ]))

{-| __C declaration:__ @by@

    __defined at:__ @names.h:3:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_757d799fd708dfd3" by' ::
     IO ()

{-| __C declaration:__ @forall@

    __defined at:__ @names.h:4:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_56a86e542f270644" forall' ::
     IO ()

{-| __C declaration:__ @mdo@

    __defined at:__ @names.h:5:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_66ccc318fc33f3db" mdo' ::
     IO ()

{-| __C declaration:__ @pattern@

    __defined at:__ @names.h:6:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_59e8310c2fbfd7a5" pattern' ::
     IO ()

{-| __C declaration:__ @proc@

    __defined at:__ @names.h:7:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_0e3a2ce0789fb7e6" proc' ::
     IO ()

{-| __C declaration:__ @rec@

    __defined at:__ @names.h:8:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_a8bd1db6a6708dae" rec' ::
     IO ()

{-| __C declaration:__ @using@

    __defined at:__ @names.h:9:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_4f84a89a7d96b0e6" using' ::
     IO ()

{-| __C declaration:__ @anyclass@

    __defined at:__ @names.h:12:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_e4c0e5d7430611d1" anyclass ::
     IO ()

{-| __C declaration:__ @capi@

    __defined at:__ @names.h:13:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_0b2f0d5537de919f" capi ::
     IO ()

{-| __C declaration:__ @cases@

    __defined at:__ @names.h:14:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_1ffa6d7a4b3beb7c" cases ::
     IO ()

{-| __C declaration:__ @ccall@

    __defined at:__ @names.h:15:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_ebc1b326e523e58f" ccall ::
     IO ()

{-| __C declaration:__ @dynamic@

    __defined at:__ @names.h:16:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_ab433e9e3ef36d2f" dynamic ::
     IO ()

{-| __C declaration:__ @export@

    __defined at:__ @names.h:17:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_df679baa272472fa" export ::
     IO ()

{-| __C declaration:__ @family@

    __defined at:__ @names.h:18:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_9a60b7b7be9ca341" family ::
     IO ()

{-| __C declaration:__ @group@

    __defined at:__ @names.h:19:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_3923700cc828a4dd" group ::
     IO ()

{-| __C declaration:__ @interruptible@

    __defined at:__ @names.h:20:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_ba3f97d9b80e552c" interruptible ::
     IO ()

{-| __C declaration:__ @javascript@

    __defined at:__ @names.h:21:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_f348c51860201a12" javascript ::
     IO ()

{-| __C declaration:__ @label@

    __defined at:__ @names.h:22:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_464d035f44366a5b" label ::
     IO ()

{-| __C declaration:__ @prim@

    __defined at:__ @names.h:23:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_1a1c4d5a866be872" prim ::
     IO ()

{-| __C declaration:__ @role@

    __defined at:__ @names.h:24:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_2b805a841a0f220d" role ::
     IO ()

{-| __C declaration:__ @safe@

    __defined at:__ @names.h:25:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_efbfe85dc9b68bc4" safe ::
     IO ()

{-| __C declaration:__ @stdcall@

    __defined at:__ @names.h:26:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_27ef4256cbaf6815" stdcall ::
     IO ()

{-| __C declaration:__ @stock@

    __defined at:__ @names.h:27:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_6e4ae46773b9e016" stock ::
     IO ()

{-| __C declaration:__ @unsafe@

    __defined at:__ @names.h:28:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_03cf69812528c410" unsafe ::
     IO ()

{-| __C declaration:__ @via@

    __defined at:__ @names.h:29:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_e611a3f38f7a3eb9" via ::
     IO ()
