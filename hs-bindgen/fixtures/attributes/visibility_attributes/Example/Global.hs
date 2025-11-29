{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/visibility_attributes.h>"
  , "/* Example_get_i0_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_3f991a677094d64b (void)"
  , "{"
  , "  return &i0;"
  , "}"
  , "/* Example_get_i1_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_983e8b54f954ac01 (void)"
  , "{"
  , "  return &i1;"
  , "}"
  , "/* Example_get_i2_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_78a629807e8d262a (void)"
  , "{"
  , "  return &i2;"
  , "}"
  , "/* Example_get_i3_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_1c9b71f1e6ae9346 (void)"
  , "{"
  , "  return &i3;"
  , "}"
  , "/* Example_get_i4_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_3cf6be4fd8f63be5 (void)"
  , "{"
  , "  return &i4;"
  , "}"
  , "/* Example_get_i5_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_4971cfdd29304e9f (void)"
  , "{"
  , "  return &i5;"
  , "}"
  , "/* Example_get_i6_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_6bb9a13a42de98ac (void)"
  , "{"
  , "  return &i6;"
  , "}"
  , "/* Example_get_i7_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_f18649a673bf6122 (void)"
  , "{"
  , "  return &i7;"
  , "}"
  , "/* Example_get_i8_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_b7c12252ce8b67c9 (void)"
  , "{"
  , "  return &i8;"
  , "}"
  , "/* Example_get_i9_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_7dbc52fabde2c490 (void)"
  , "{"
  , "  return &i9;"
  , "}"
  , "/* Example_get_i10_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_8ef53a945d6bdaff (void)"
  , "{"
  , "  return &i10;"
  , "}"
  , "/* Example_get_i11_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_26053e253cedbbf4 (void)"
  , "{"
  , "  return &i11;"
  , "}"
  , "/* Example_get_i12_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_80484ade97642bf3 (void)"
  , "{"
  , "  return &i12;"
  , "}"
  , "/* Example_get_i13_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_fa542b067e40db07 (void)"
  , "{"
  , "  return &i13;"
  , "}"
  , "/* Example_get_i14_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_b75ecacfa98f1861 (void)"
  , "{"
  , "  return &i14;"
  , "}"
  , "/* Example_get_i15_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_3e08d6f9c4819dda (void)"
  , "{"
  , "  return &i15;"
  , "}"
  , "/* Example_get_i16_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_f4946ad21fe6750b (void)"
  , "{"
  , "  return &i16;"
  , "}"
  , "/* Example_get_i17_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_04d033ab3eb97d4b (void)"
  , "{"
  , "  return &i17;"
  , "}"
  , "/* Example_get_i18_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_1dceb01c3642e61c (void)"
  , "{"
  , "  return &i18;"
  , "}"
  , "/* Example_get_i19_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_8c799a5f7a6f1aa5 (void)"
  , "{"
  , "  return &i19;"
  , "}"
  ]))

{-| __unique:__ @Example_get_i0_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_3f991a677094d64b" hs_bindgen_test_attributesvisibility_attribut_3f991a677094d64b ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i0_ptr #-}

{-| __C declaration:__ @i0@

    __defined at:__ @attributes\/visibility_attributes.h:61:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i0_ptr :: Ptr.Ptr FC.CInt
i0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_3f991a677094d64b

{-| __unique:__ @Example_get_i1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_983e8b54f954ac01" hs_bindgen_test_attributesvisibility_attribut_983e8b54f954ac01 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i1_ptr #-}

{-| __C declaration:__ @i1@

    __defined at:__ @attributes\/visibility_attributes.h:62:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i1_ptr :: Ptr.Ptr FC.CInt
i1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_983e8b54f954ac01

{-| __unique:__ @Example_get_i2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_78a629807e8d262a" hs_bindgen_test_attributesvisibility_attribut_78a629807e8d262a ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i2_ptr #-}

{-| __C declaration:__ @i2@

    __defined at:__ @attributes\/visibility_attributes.h:63:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i2_ptr :: Ptr.Ptr FC.CInt
i2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_78a629807e8d262a

{-| __unique:__ @Example_get_i3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_1c9b71f1e6ae9346" hs_bindgen_test_attributesvisibility_attribut_1c9b71f1e6ae9346 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i3_ptr #-}

{-| __C declaration:__ @i3@

    __defined at:__ @attributes\/visibility_attributes.h:64:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i3_ptr :: Ptr.Ptr FC.CInt
i3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_1c9b71f1e6ae9346

{-| __unique:__ @Example_get_i4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_3cf6be4fd8f63be5" hs_bindgen_test_attributesvisibility_attribut_3cf6be4fd8f63be5 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i4_ptr #-}

{-| __C declaration:__ @i4@

    __defined at:__ @attributes\/visibility_attributes.h:65:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i4_ptr :: Ptr.Ptr FC.CInt
i4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_3cf6be4fd8f63be5

{-| __unique:__ @Example_get_i5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_4971cfdd29304e9f" hs_bindgen_test_attributesvisibility_attribut_4971cfdd29304e9f ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i5_ptr #-}

{-| __C declaration:__ @i5@

    __defined at:__ @attributes\/visibility_attributes.h:68:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i5_ptr :: Ptr.Ptr FC.CInt
i5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_4971cfdd29304e9f

{-| __unique:__ @Example_get_i6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_6bb9a13a42de98ac" hs_bindgen_test_attributesvisibility_attribut_6bb9a13a42de98ac ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i6_ptr #-}

{-| __C declaration:__ @i6@

    __defined at:__ @attributes\/visibility_attributes.h:69:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i6_ptr :: Ptr.Ptr FC.CInt
i6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_6bb9a13a42de98ac

{-| __unique:__ @Example_get_i7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_f18649a673bf6122" hs_bindgen_test_attributesvisibility_attribut_f18649a673bf6122 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i7_ptr #-}

{-| __C declaration:__ @i7@

    __defined at:__ @attributes\/visibility_attributes.h:70:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i7_ptr :: Ptr.Ptr FC.CInt
i7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_f18649a673bf6122

{-| __unique:__ @Example_get_i8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b7c12252ce8b67c9" hs_bindgen_test_attributesvisibility_attribut_b7c12252ce8b67c9 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i8_ptr #-}

{-| __C declaration:__ @i8@

    __defined at:__ @attributes\/visibility_attributes.h:71:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i8_ptr :: Ptr.Ptr FC.CInt
i8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_b7c12252ce8b67c9

{-| __unique:__ @Example_get_i9_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_7dbc52fabde2c490" hs_bindgen_test_attributesvisibility_attribut_7dbc52fabde2c490 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i9_ptr #-}

{-| __C declaration:__ @i9@

    __defined at:__ @attributes\/visibility_attributes.h:72:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i9_ptr :: Ptr.Ptr FC.CInt
i9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_7dbc52fabde2c490

{-| __unique:__ @Example_get_i10_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_8ef53a945d6bdaff" hs_bindgen_test_attributesvisibility_attribut_8ef53a945d6bdaff ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i10_ptr #-}

{-| __C declaration:__ @i10@

    __defined at:__ @attributes\/visibility_attributes.h:75:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i10_ptr :: Ptr.Ptr FC.CInt
i10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_8ef53a945d6bdaff

{-| __unique:__ @Example_get_i11_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_26053e253cedbbf4" hs_bindgen_test_attributesvisibility_attribut_26053e253cedbbf4 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i11_ptr #-}

{-| __C declaration:__ @i11@

    __defined at:__ @attributes\/visibility_attributes.h:76:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i11_ptr :: Ptr.Ptr FC.CInt
i11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_26053e253cedbbf4

{-| __unique:__ @Example_get_i12_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_80484ade97642bf3" hs_bindgen_test_attributesvisibility_attribut_80484ade97642bf3 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i12_ptr #-}

{-| __C declaration:__ @i12@

    __defined at:__ @attributes\/visibility_attributes.h:77:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i12_ptr :: Ptr.Ptr FC.CInt
i12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_80484ade97642bf3

{-| __unique:__ @Example_get_i13_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_fa542b067e40db07" hs_bindgen_test_attributesvisibility_attribut_fa542b067e40db07 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i13_ptr #-}

{-| __C declaration:__ @i13@

    __defined at:__ @attributes\/visibility_attributes.h:78:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i13_ptr :: Ptr.Ptr FC.CInt
i13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_fa542b067e40db07

{-| __unique:__ @Example_get_i14_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b75ecacfa98f1861" hs_bindgen_test_attributesvisibility_attribut_b75ecacfa98f1861 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i14_ptr #-}

{-| __C declaration:__ @i14@

    __defined at:__ @attributes\/visibility_attributes.h:79:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i14_ptr :: Ptr.Ptr FC.CInt
i14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_b75ecacfa98f1861

{-| __unique:__ @Example_get_i15_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_3e08d6f9c4819dda" hs_bindgen_test_attributesvisibility_attribut_3e08d6f9c4819dda ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i15_ptr #-}

{-| __C declaration:__ @i15@

    __defined at:__ @attributes\/visibility_attributes.h:82:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i15_ptr :: Ptr.Ptr FC.CInt
i15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_3e08d6f9c4819dda

{-| __unique:__ @Example_get_i16_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_f4946ad21fe6750b" hs_bindgen_test_attributesvisibility_attribut_f4946ad21fe6750b ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i16_ptr #-}

{-| __C declaration:__ @i16@

    __defined at:__ @attributes\/visibility_attributes.h:83:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i16_ptr :: Ptr.Ptr FC.CInt
i16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_f4946ad21fe6750b

{-| __unique:__ @Example_get_i17_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_04d033ab3eb97d4b" hs_bindgen_test_attributesvisibility_attribut_04d033ab3eb97d4b ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i17_ptr #-}

{-| __C declaration:__ @i17@

    __defined at:__ @attributes\/visibility_attributes.h:84:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i17_ptr :: Ptr.Ptr FC.CInt
i17_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_04d033ab3eb97d4b

{-| __unique:__ @Example_get_i18_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_1dceb01c3642e61c" hs_bindgen_test_attributesvisibility_attribut_1dceb01c3642e61c ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i18_ptr #-}

{-| __C declaration:__ @i18@

    __defined at:__ @attributes\/visibility_attributes.h:85:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i18_ptr :: Ptr.Ptr FC.CInt
i18_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_1dceb01c3642e61c

{-| __unique:__ @Example_get_i19_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_8c799a5f7a6f1aa5" hs_bindgen_test_attributesvisibility_attribut_8c799a5f7a6f1aa5 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i19_ptr #-}

{-| __C declaration:__ @i19@

    __defined at:__ @attributes\/visibility_attributes.h:86:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i19_ptr :: Ptr.Ptr FC.CInt
i19_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_8c799a5f7a6f1aa5
