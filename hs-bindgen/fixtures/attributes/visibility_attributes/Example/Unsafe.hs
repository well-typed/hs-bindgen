{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/visibility_attributes.h>"
  , "void hs_bindgen_test_attributesvisibility_attribut_b7fea22cf48785ab (void)"
  , "{"
  , "  f0();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_cc22dfb87e5838fe (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_661cac524e84b1f7 (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_46d6d2725c8f934c (void)"
  , "{"
  , "  f3();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_043141ed3d2a1e74 (void)"
  , "{"
  , "  f4();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_dff83ef67c97454a (void)"
  , "{"
  , "  f5();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_2936bb74acad834a (void)"
  , "{"
  , "  f6();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_84e85f6df1bc85e1 (void)"
  , "{"
  , "  f7();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_2371e91d8a06cba5 (void)"
  , "{"
  , "  f8();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_5edd38e479d29fa1 (void)"
  , "{"
  , "  f9();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_8ee11c47a640cccc (void)"
  , "{"
  , "  f10();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_36385f623acf9e69 (void)"
  , "{"
  , "  f11();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_2feba1c626b1d941 (void)"
  , "{"
  , "  f12();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_31573e1afc8cf7e9 (void)"
  , "{"
  , "  f13();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_57cfde0d20b9bca8 (void)"
  , "{"
  , "  f14();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_37dbbb30b7b99264 (void)"
  , "{"
  , "  f15();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_c2f3ed13d53ceb4b (void)"
  , "{"
  , "  f16();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_27188012a1bb637e (void)"
  , "{"
  , "  f17();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_07705bf38b9dbb21 (void)"
  , "{"
  , "  f18();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_6e4aede31f1a2b4e (void)"
  , "{"
  , "  f19();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_290ebe994bdfacb3 (void)"
  , "{"
  , "  f20();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_ebc7846de13f332d (void)"
  , "{"
  , "  f21();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_5b855f0636e631e8 (void)"
  , "{"
  , "  f22();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_023acd489abdd6c8 (void)"
  , "{"
  , "  f23();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_064baf360f731ec3 (void)"
  , "{"
  , "  f24();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_f972dc3a9906b2c2 (void)"
  , "{"
  , "  f25();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_ff08eba573c6d6b3 (void)"
  , "{"
  , "  f26();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_db77cb86c895b957 (void)"
  , "{"
  , "  f27();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_dcf44a01b30717b0 (void)"
  , "{"
  , "  f28();"
  , "}"
  , "void hs_bindgen_test_attributesvisibility_attribut_4e91b08445b2ab7d (void)"
  , "{"
  , "  f29();"
  , "}"
  ]))

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h:17:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f0@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b7fea22cf48785ab" f0 ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h:18:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f1@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_cc22dfb87e5838fe" f1 ::
     IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h:19:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f2@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_661cac524e84b1f7" f2 ::
     IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h:20:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f3@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_46d6d2725c8f934c" f3 ::
     IO ()

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h:21:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f4@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_043141ed3d2a1e74" f4 ::
     IO ()

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h:24:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f5@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_dff83ef67c97454a" f5 ::
     IO ()

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h:25:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f6@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_2936bb74acad834a" f6 ::
     IO ()

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h:26:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f7@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_84e85f6df1bc85e1" f7 ::
     IO ()

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h:27:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f8@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_2371e91d8a06cba5" f8 ::
     IO ()

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h:28:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f9@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_5edd38e479d29fa1" f9 ::
     IO ()

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h:31:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f10@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_8ee11c47a640cccc" f10 ::
     IO ()

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h:32:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f11@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_36385f623acf9e69" f11 ::
     IO ()

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h:33:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f12@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_2feba1c626b1d941" f12 ::
     IO ()

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h:34:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f13@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_31573e1afc8cf7e9" f13 ::
     IO ()

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h:35:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f14@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_57cfde0d20b9bca8" f14 ::
     IO ()

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h:38:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f15@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_37dbbb30b7b99264" f15 ::
     IO ()

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h:39:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f16@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_c2f3ed13d53ceb4b" f16 ::
     IO ()

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h:40:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f17@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_27188012a1bb637e" f17 ::
     IO ()

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h:41:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f18@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_07705bf38b9dbb21" f18 ::
     IO ()

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h:42:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f19@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_6e4aede31f1a2b4e" f19 ::
     IO ()

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h:45:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f20@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_290ebe994bdfacb3" f20 ::
     IO ()

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h:46:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f21@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_ebc7846de13f332d" f21 ::
     IO ()

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h:47:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f22@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_5b855f0636e631e8" f22 ::
     IO ()

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h:48:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f23@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_023acd489abdd6c8" f23 ::
     IO ()

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h:49:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f24@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_064baf360f731ec3" f24 ::
     IO ()

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h:52:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f25@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_f972dc3a9906b2c2" f25 ::
     IO ()

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h:53:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f26@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_ff08eba573c6d6b3" f26 ::
     IO ()

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h:54:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f27@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_db77cb86c895b957" f27 ::
     IO ()

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h:55:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f28@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_dcf44a01b30717b0" f28 ::
     IO ()

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h:56:56@

    __exported by:__ @attributes\/visibility_attributes.h@

    __unique:__ @Example_Unsafe_f29@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_4e91b08445b2ab7d" f29 ::
     IO ()
