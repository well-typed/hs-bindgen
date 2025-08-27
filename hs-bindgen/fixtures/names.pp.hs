{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <names.h>\nvoid hs_bindgen_test_names_a08e966b63669ba8 (void) { by(); }\nvoid hs_bindgen_test_names_946148697abf4575 (void) { forall(); }\nvoid hs_bindgen_test_names_a1ece6a6fa8e9763 (void) { mdo(); }\nvoid hs_bindgen_test_names_1e48788938305e48 (void) { pattern(); }\nvoid hs_bindgen_test_names_9bb16600c50998ed (void) { proc(); }\nvoid hs_bindgen_test_names_8dfd7fa26e360d8e (void) { rec(); }\nvoid hs_bindgen_test_names_57c5446244bcece1 (void) { using(); }\nvoid hs_bindgen_test_names_a2a878b7e49c1a71 (void) { anyclass(); }\nvoid hs_bindgen_test_names_2f73a4347f70a468 (void) { capi(); }\nvoid hs_bindgen_test_names_cb2993ce15743783 (void) { cases(); }\nvoid hs_bindgen_test_names_954c564a2bd129a5 (void) { ccall(); }\nvoid hs_bindgen_test_names_3a96dcbee4c024e0 (void) { dynamic(); }\nvoid hs_bindgen_test_names_2e78d006337ea551 (void) { export(); }\nvoid hs_bindgen_test_names_10c8e433951b50a8 (void) { family(); }\nvoid hs_bindgen_test_names_6217d85466fed7df (void) { group(); }\nvoid hs_bindgen_test_names_00164685ff44cb75 (void) { interruptible(); }\nvoid hs_bindgen_test_names_c810b13a75ea93bc (void) { javascript(); }\nvoid hs_bindgen_test_names_41d6d93a2614dff5 (void) { label(); }\nvoid hs_bindgen_test_names_bdfec7f41298a418 (void) { prim(); }\nvoid hs_bindgen_test_names_0d10aaa9ecbf0f2f (void) { role(); }\nvoid hs_bindgen_test_names_0ebc368c3818b18b (void) { safe(); }\nvoid hs_bindgen_test_names_764bfbe5fbb4fac9 (void) { stdcall(); }\nvoid hs_bindgen_test_names_6597a6eedc28f577 (void) { stock(); }\nvoid hs_bindgen_test_names_1aef1bdf2d1ba544 (void) { unsafe(); }\nvoid hs_bindgen_test_names_d428a3f008917f79 (void) { via(); }\n")

{-| __from C:__ @by@ -}
foreign import ccall safe "hs_bindgen_test_names_a08e966b63669ba8" by'
  :: IO ()

{-| __from C:__ @forall@ -}
foreign import ccall safe "hs_bindgen_test_names_946148697abf4575" forall'
  :: IO ()

{-| __from C:__ @mdo@ -}
foreign import ccall safe "hs_bindgen_test_names_a1ece6a6fa8e9763" mdo'
  :: IO ()

{-| __from C:__ @pattern@ -}
foreign import ccall safe "hs_bindgen_test_names_1e48788938305e48" pattern'
  :: IO ()

{-| __from C:__ @proc@ -}
foreign import ccall safe "hs_bindgen_test_names_9bb16600c50998ed" proc'
  :: IO ()

{-| __from C:__ @rec@ -}
foreign import ccall safe "hs_bindgen_test_names_8dfd7fa26e360d8e" rec'
  :: IO ()

{-| __from C:__ @using@ -}
foreign import ccall safe "hs_bindgen_test_names_57c5446244bcece1" using'
  :: IO ()

{-| __from C:__ @anyclass@ -}
foreign import ccall safe "hs_bindgen_test_names_a2a878b7e49c1a71" anyclass
  :: IO ()

{-| __from C:__ @capi@ -}
foreign import ccall safe "hs_bindgen_test_names_2f73a4347f70a468" capi
  :: IO ()

{-| __from C:__ @cases@ -}
foreign import ccall safe "hs_bindgen_test_names_cb2993ce15743783" cases
  :: IO ()

{-| __from C:__ @ccall@ -}
foreign import ccall safe "hs_bindgen_test_names_954c564a2bd129a5" ccall
  :: IO ()

{-| __from C:__ @dynamic@ -}
foreign import ccall safe "hs_bindgen_test_names_3a96dcbee4c024e0" dynamic
  :: IO ()

{-| __from C:__ @export@ -}
foreign import ccall safe "hs_bindgen_test_names_2e78d006337ea551" export
  :: IO ()

{-| __from C:__ @family@ -}
foreign import ccall safe "hs_bindgen_test_names_10c8e433951b50a8" family
  :: IO ()

{-| __from C:__ @group@ -}
foreign import ccall safe "hs_bindgen_test_names_6217d85466fed7df" group
  :: IO ()

{-| __from C:__ @interruptible@ -}
foreign import ccall safe "hs_bindgen_test_names_00164685ff44cb75" interruptible
  :: IO ()

{-| __from C:__ @javascript@ -}
foreign import ccall safe "hs_bindgen_test_names_c810b13a75ea93bc" javascript
  :: IO ()

{-| __from C:__ @label@ -}
foreign import ccall safe "hs_bindgen_test_names_41d6d93a2614dff5" label
  :: IO ()

{-| __from C:__ @prim@ -}
foreign import ccall safe "hs_bindgen_test_names_bdfec7f41298a418" prim
  :: IO ()

{-| __from C:__ @role@ -}
foreign import ccall safe "hs_bindgen_test_names_0d10aaa9ecbf0f2f" role
  :: IO ()

{-| __from C:__ @safe@ -}
foreign import ccall safe "hs_bindgen_test_names_0ebc368c3818b18b" safe
  :: IO ()

{-| __from C:__ @stdcall@ -}
foreign import ccall safe "hs_bindgen_test_names_764bfbe5fbb4fac9" stdcall
  :: IO ()

{-| __from C:__ @stock@ -}
foreign import ccall safe "hs_bindgen_test_names_6597a6eedc28f577" stock
  :: IO ()

{-| __from C:__ @unsafe@ -}
foreign import ccall safe "hs_bindgen_test_names_1aef1bdf2d1ba544" unsafe
  :: IO ()

{-| __from C:__ @via@ -}
foreign import ccall safe "hs_bindgen_test_names_d428a3f008917f79" via
  :: IO ()
