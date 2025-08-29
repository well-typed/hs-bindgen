{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <names.h>\nvoid hs_bindgen_test_names_a08e966b63669ba8 (void) { by(); }\nvoid hs_bindgen_test_names_946148697abf4575 (void) { forall(); }\nvoid hs_bindgen_test_names_a1ece6a6fa8e9763 (void) { mdo(); }\nvoid hs_bindgen_test_names_1e48788938305e48 (void) { pattern(); }\nvoid hs_bindgen_test_names_9bb16600c50998ed (void) { proc(); }\nvoid hs_bindgen_test_names_8dfd7fa26e360d8e (void) { rec(); }\nvoid hs_bindgen_test_names_57c5446244bcece1 (void) { using(); }\nvoid hs_bindgen_test_names_a2a878b7e49c1a71 (void) { anyclass(); }\nvoid hs_bindgen_test_names_2f73a4347f70a468 (void) { capi(); }\nvoid hs_bindgen_test_names_cb2993ce15743783 (void) { cases(); }\nvoid hs_bindgen_test_names_954c564a2bd129a5 (void) { ccall(); }\nvoid hs_bindgen_test_names_3a96dcbee4c024e0 (void) { dynamic(); }\nvoid hs_bindgen_test_names_2e78d006337ea551 (void) { export(); }\nvoid hs_bindgen_test_names_10c8e433951b50a8 (void) { family(); }\nvoid hs_bindgen_test_names_6217d85466fed7df (void) { group(); }\nvoid hs_bindgen_test_names_00164685ff44cb75 (void) { interruptible(); }\nvoid hs_bindgen_test_names_c810b13a75ea93bc (void) { javascript(); }\nvoid hs_bindgen_test_names_41d6d93a2614dff5 (void) { label(); }\nvoid hs_bindgen_test_names_bdfec7f41298a418 (void) { prim(); }\nvoid hs_bindgen_test_names_0d10aaa9ecbf0f2f (void) { role(); }\nvoid hs_bindgen_test_names_0ebc368c3818b18b (void) { safe(); }\nvoid hs_bindgen_test_names_764bfbe5fbb4fac9 (void) { stdcall(); }\nvoid hs_bindgen_test_names_6597a6eedc28f577 (void) { stock(); }\nvoid hs_bindgen_test_names_1aef1bdf2d1ba544 (void) { unsafe(); }\nvoid hs_bindgen_test_names_d428a3f008917f79 (void) { via(); }\n")

{-| __/Automatically generated from C/__

    __C declaration:__ @by@

    __defined at:__ @names.h:3:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_a08e966b63669ba8" by'
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @forall@

    __defined at:__ @names.h:4:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_946148697abf4575" forall'
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @mdo@

    __defined at:__ @names.h:5:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_a1ece6a6fa8e9763" mdo'
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @pattern@

    __defined at:__ @names.h:6:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_1e48788938305e48" pattern'
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @proc@

    __defined at:__ @names.h:7:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_9bb16600c50998ed" proc'
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @rec@

    __defined at:__ @names.h:8:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_8dfd7fa26e360d8e" rec'
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @using@

    __defined at:__ @names.h:9:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_57c5446244bcece1" using'
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @anyclass@

    __defined at:__ @names.h:12:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_a2a878b7e49c1a71" anyclass
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @capi@

    __defined at:__ @names.h:13:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_2f73a4347f70a468" capi
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @cases@

    __defined at:__ @names.h:14:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_cb2993ce15743783" cases
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @ccall@

    __defined at:__ @names.h:15:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_954c564a2bd129a5" ccall
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @dynamic@

    __defined at:__ @names.h:16:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_3a96dcbee4c024e0" dynamic
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @export@

    __defined at:__ @names.h:17:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_2e78d006337ea551" export
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @family@

    __defined at:__ @names.h:18:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_10c8e433951b50a8" family
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @group@

    __defined at:__ @names.h:19:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_6217d85466fed7df" group
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @interruptible@

    __defined at:__ @names.h:20:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_00164685ff44cb75" interruptible
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @javascript@

    __defined at:__ @names.h:21:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_c810b13a75ea93bc" javascript
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @label@

    __defined at:__ @names.h:22:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_41d6d93a2614dff5" label
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @prim@

    __defined at:__ @names.h:23:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_bdfec7f41298a418" prim
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @role@

    __defined at:__ @names.h:24:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0d10aaa9ecbf0f2f" role
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @safe@

    __defined at:__ @names.h:25:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0ebc368c3818b18b" safe
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @stdcall@

    __defined at:__ @names.h:26:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_764bfbe5fbb4fac9" stdcall
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @stock@

    __defined at:__ @names.h:27:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_6597a6eedc28f577" stock
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @unsafe@

    __defined at:__ @names.h:28:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_1aef1bdf2d1ba544" unsafe
  :: IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @via@

    __defined at:__ @names.h:29:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_d428a3f008917f79" via
  :: IO ()
