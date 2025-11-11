{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <visibility_attributes.h>"
  , "void hs_bindgen_test_visibility_attributes_e8fda12159f2be9f (void)"
  , "{"
  , "  f0();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_a2f84d2570ef3892 (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_1d043de05a457e90 (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_e23eff1955ebb459 (void)"
  , "{"
  , "  f3();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_ce219a1a5351d14e (void)"
  , "{"
  , "  f4();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_caebbc1a0babf9c3 (void)"
  , "{"
  , "  f5();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_0e94ab16fe1245e4 (void)"
  , "{"
  , "  f6();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_595393c65507c7b2 (void)"
  , "{"
  , "  f7();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_2e6297cc5a3e79e0 (void)"
  , "{"
  , "  f8();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_7d6b88eb048c2261 (void)"
  , "{"
  , "  f9();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_4ef53db381225865 (void)"
  , "{"
  , "  f10();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_c626c8c382be7e47 (void)"
  , "{"
  , "  f11();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_09bc3cf816a85839 (void)"
  , "{"
  , "  f12();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_eadf3eb9d39365cf (void)"
  , "{"
  , "  f13();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_24313656b5162754 (void)"
  , "{"
  , "  f14();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_db09067813df28c1 (void)"
  , "{"
  , "  f15();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_4ce3176c4406cf10 (void)"
  , "{"
  , "  f16();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_2354b7b245be3629 (void)"
  , "{"
  , "  f17();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_a24f6e8ea1a74456 (void)"
  , "{"
  , "  f18();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_e14cdae313c9647d (void)"
  , "{"
  , "  f19();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_d973493c824fdf05 (void)"
  , "{"
  , "  f20();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_cc1b3614f810260c (void)"
  , "{"
  , "  f21();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_cb764aa14ed3e34c (void)"
  , "{"
  , "  f22();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_dc225cc74f4331bf (void)"
  , "{"
  , "  f23();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_88449e6c03902cdf (void)"
  , "{"
  , "  f24();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_7370ca6009a58826 (void)"
  , "{"
  , "  f25();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_a67323b70e59146d (void)"
  , "{"
  , "  f26();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_968a7a3827f17839 (void)"
  , "{"
  , "  f27();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_fbb18ffa92c2c5be (void)"
  , "{"
  , "  f28();"
  , "}"
  , "void hs_bindgen_test_visibility_attributes_60afc98eb89b8a2d (void)"
  , "{"
  , "  f29();"
  , "}"
  ]))

{-| __C declaration:__ @f0@

    __defined at:__ @visibility_attributes.h:17:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_e8fda12159f2be9f" f0 ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @visibility_attributes.h:18:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_a2f84d2570ef3892" f1 ::
     IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @visibility_attributes.h:19:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_1d043de05a457e90" f2 ::
     IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @visibility_attributes.h:20:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_e23eff1955ebb459" f3 ::
     IO ()

{-| __C declaration:__ @f4@

    __defined at:__ @visibility_attributes.h:21:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_ce219a1a5351d14e" f4 ::
     IO ()

{-| __C declaration:__ @f5@

    __defined at:__ @visibility_attributes.h:24:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_caebbc1a0babf9c3" f5 ::
     IO ()

{-| __C declaration:__ @f6@

    __defined at:__ @visibility_attributes.h:25:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_0e94ab16fe1245e4" f6 ::
     IO ()

{-| __C declaration:__ @f7@

    __defined at:__ @visibility_attributes.h:26:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_595393c65507c7b2" f7 ::
     IO ()

{-| __C declaration:__ @f8@

    __defined at:__ @visibility_attributes.h:27:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_2e6297cc5a3e79e0" f8 ::
     IO ()

{-| __C declaration:__ @f9@

    __defined at:__ @visibility_attributes.h:28:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_7d6b88eb048c2261" f9 ::
     IO ()

{-| __C declaration:__ @f10@

    __defined at:__ @visibility_attributes.h:31:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_4ef53db381225865" f10 ::
     IO ()

{-| __C declaration:__ @f11@

    __defined at:__ @visibility_attributes.h:32:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_c626c8c382be7e47" f11 ::
     IO ()

{-| __C declaration:__ @f12@

    __defined at:__ @visibility_attributes.h:33:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_09bc3cf816a85839" f12 ::
     IO ()

{-| __C declaration:__ @f13@

    __defined at:__ @visibility_attributes.h:34:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_eadf3eb9d39365cf" f13 ::
     IO ()

{-| __C declaration:__ @f14@

    __defined at:__ @visibility_attributes.h:35:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_24313656b5162754" f14 ::
     IO ()

{-| __C declaration:__ @f15@

    __defined at:__ @visibility_attributes.h:38:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_db09067813df28c1" f15 ::
     IO ()

{-| __C declaration:__ @f16@

    __defined at:__ @visibility_attributes.h:39:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_4ce3176c4406cf10" f16 ::
     IO ()

{-| __C declaration:__ @f17@

    __defined at:__ @visibility_attributes.h:40:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_2354b7b245be3629" f17 ::
     IO ()

{-| __C declaration:__ @f18@

    __defined at:__ @visibility_attributes.h:41:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_a24f6e8ea1a74456" f18 ::
     IO ()

{-| __C declaration:__ @f19@

    __defined at:__ @visibility_attributes.h:42:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_e14cdae313c9647d" f19 ::
     IO ()

{-| __C declaration:__ @f20@

    __defined at:__ @visibility_attributes.h:45:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_d973493c824fdf05" f20 ::
     IO ()

{-| __C declaration:__ @f21@

    __defined at:__ @visibility_attributes.h:46:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_cc1b3614f810260c" f21 ::
     IO ()

{-| __C declaration:__ @f22@

    __defined at:__ @visibility_attributes.h:47:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_cb764aa14ed3e34c" f22 ::
     IO ()

{-| __C declaration:__ @f23@

    __defined at:__ @visibility_attributes.h:48:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_dc225cc74f4331bf" f23 ::
     IO ()

{-| __C declaration:__ @f24@

    __defined at:__ @visibility_attributes.h:49:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_88449e6c03902cdf" f24 ::
     IO ()

{-| __C declaration:__ @f25@

    __defined at:__ @visibility_attributes.h:52:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_7370ca6009a58826" f25 ::
     IO ()

{-| __C declaration:__ @f26@

    __defined at:__ @visibility_attributes.h:53:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_a67323b70e59146d" f26 ::
     IO ()

{-| __C declaration:__ @f27@

    __defined at:__ @visibility_attributes.h:54:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_968a7a3827f17839" f27 ::
     IO ()

{-| __C declaration:__ @f28@

    __defined at:__ @visibility_attributes.h:55:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_fbb18ffa92c2c5be" f28 ::
     IO ()

{-| __C declaration:__ @f29@

    __defined at:__ @visibility_attributes.h:56:56@

    __exported by:__ @visibility_attributes.h@
-}
foreign import ccall safe "hs_bindgen_test_visibility_attributes_60afc98eb89b8a2d" f29 ::
     IO ()
