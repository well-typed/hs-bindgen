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
  , "/* get_i0_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_216496b15d8f3143 (void)"
  , "{"
  , "  return &i0;"
  , "}"
  , "/* get_i1_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_8a4a155fb4b3e983 (void)"
  , "{"
  , "  return &i1;"
  , "}"
  , "/* get_i2_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_8a341976b53c3159 (void)"
  , "{"
  , "  return &i2;"
  , "}"
  , "/* get_i3_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_8a18e8a325536dc5 (void)"
  , "{"
  , "  return &i3;"
  , "}"
  , "/* get_i4_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_8a083f6803595ed2 (void)"
  , "{"
  , "  return &i4;"
  , "}"
  , "/* get_i5_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_b9d322a4c171d6fa (void)"
  , "{"
  , "  return &i5;"
  , "}"
  , "/* get_i6_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_2c4836056a76ae78 (void)"
  , "{"
  , "  return &i6;"
  , "}"
  , "/* get_i7_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_b9d40a2f9eb7062e (void)"
  , "{"
  , "  return &i7;"
  , "}"
  , "/* get_i8_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_3fd4c67173dc11ce (void)"
  , "{"
  , "  return &i8;"
  , "}"
  , "/* get_i9_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_b9ff2784975e7295 (void)"
  , "{"
  , "  return &i9;"
  , "}"
  , "/* get_i10_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_17a29f1f8c101878 (void)"
  , "{"
  , "  return &i10;"
  , "}"
  , "/* get_i11_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_4e591df5ac4216c9 (void)"
  , "{"
  , "  return &i11;"
  , "}"
  , "/* get_i12_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_7a8621a15e9246c4 (void)"
  , "{"
  , "  return &i12;"
  , "}"
  , "/* get_i13_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_96cb39c8a898775e (void)"
  , "{"
  , "  return &i13;"
  , "}"
  , "/* get_i14_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_9f817ee25723fbcd (void)"
  , "{"
  , "  return &i14;"
  , "}"
  , "/* get_i15_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_0f8755cf60822f2e (void)"
  , "{"
  , "  return &i15;"
  , "}"
  , "/* get_i16_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_3023a5c63e753d58 (void)"
  , "{"
  , "  return &i16;"
  , "}"
  , "/* get_i17_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_b5610fa653ca61e6 (void)"
  , "{"
  , "  return &i17;"
  , "}"
  , "/* get_i18_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_17edf053c36e012c (void)"
  , "{"
  , "  return &i18;"
  , "}"
  , "/* get_i19_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_attributesvisibility_attribut_df1c0751f896e6b8 (void)"
  , "{"
  , "  return &i19;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_i0_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_216496b15d8f3143" hs_bindgen_test_attributesvisibility_attribut_216496b15d8f3143 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i0_ptr #-}

{-| __C declaration:__ @i0@

    __defined at:__ @attributes\/visibility_attributes.h:61:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i0_ptr :: Ptr.Ptr FC.CInt
i0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_216496b15d8f3143

{-| __unique:__ @ExampleNothingget_i1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_8a4a155fb4b3e983" hs_bindgen_test_attributesvisibility_attribut_8a4a155fb4b3e983 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i1_ptr #-}

{-| __C declaration:__ @i1@

    __defined at:__ @attributes\/visibility_attributes.h:62:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i1_ptr :: Ptr.Ptr FC.CInt
i1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_8a4a155fb4b3e983

{-| __unique:__ @ExampleNothingget_i2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_8a341976b53c3159" hs_bindgen_test_attributesvisibility_attribut_8a341976b53c3159 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i2_ptr #-}

{-| __C declaration:__ @i2@

    __defined at:__ @attributes\/visibility_attributes.h:63:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i2_ptr :: Ptr.Ptr FC.CInt
i2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_8a341976b53c3159

{-| __unique:__ @ExampleNothingget_i3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_8a18e8a325536dc5" hs_bindgen_test_attributesvisibility_attribut_8a18e8a325536dc5 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i3_ptr #-}

{-| __C declaration:__ @i3@

    __defined at:__ @attributes\/visibility_attributes.h:64:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i3_ptr :: Ptr.Ptr FC.CInt
i3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_8a18e8a325536dc5

{-| __unique:__ @ExampleNothingget_i4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_8a083f6803595ed2" hs_bindgen_test_attributesvisibility_attribut_8a083f6803595ed2 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i4_ptr #-}

{-| __C declaration:__ @i4@

    __defined at:__ @attributes\/visibility_attributes.h:65:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i4_ptr :: Ptr.Ptr FC.CInt
i4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_8a083f6803595ed2

{-| __unique:__ @ExampleNothingget_i5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b9d322a4c171d6fa" hs_bindgen_test_attributesvisibility_attribut_b9d322a4c171d6fa ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i5_ptr #-}

{-| __C declaration:__ @i5@

    __defined at:__ @attributes\/visibility_attributes.h:68:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i5_ptr :: Ptr.Ptr FC.CInt
i5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_b9d322a4c171d6fa

{-| __unique:__ @ExampleNothingget_i6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_2c4836056a76ae78" hs_bindgen_test_attributesvisibility_attribut_2c4836056a76ae78 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i6_ptr #-}

{-| __C declaration:__ @i6@

    __defined at:__ @attributes\/visibility_attributes.h:69:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i6_ptr :: Ptr.Ptr FC.CInt
i6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_2c4836056a76ae78

{-| __unique:__ @ExampleNothingget_i7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b9d40a2f9eb7062e" hs_bindgen_test_attributesvisibility_attribut_b9d40a2f9eb7062e ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i7_ptr #-}

{-| __C declaration:__ @i7@

    __defined at:__ @attributes\/visibility_attributes.h:70:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i7_ptr :: Ptr.Ptr FC.CInt
i7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_b9d40a2f9eb7062e

{-| __unique:__ @ExampleNothingget_i8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_3fd4c67173dc11ce" hs_bindgen_test_attributesvisibility_attribut_3fd4c67173dc11ce ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i8_ptr #-}

{-| __C declaration:__ @i8@

    __defined at:__ @attributes\/visibility_attributes.h:71:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i8_ptr :: Ptr.Ptr FC.CInt
i8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_3fd4c67173dc11ce

{-| __unique:__ @ExampleNothingget_i9_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b9ff2784975e7295" hs_bindgen_test_attributesvisibility_attribut_b9ff2784975e7295 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i9_ptr #-}

{-| __C declaration:__ @i9@

    __defined at:__ @attributes\/visibility_attributes.h:72:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i9_ptr :: Ptr.Ptr FC.CInt
i9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_b9ff2784975e7295

{-| __unique:__ @ExampleNothingget_i10_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_17a29f1f8c101878" hs_bindgen_test_attributesvisibility_attribut_17a29f1f8c101878 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i10_ptr #-}

{-| __C declaration:__ @i10@

    __defined at:__ @attributes\/visibility_attributes.h:75:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i10_ptr :: Ptr.Ptr FC.CInt
i10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_17a29f1f8c101878

{-| __unique:__ @ExampleNothingget_i11_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_4e591df5ac4216c9" hs_bindgen_test_attributesvisibility_attribut_4e591df5ac4216c9 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i11_ptr #-}

{-| __C declaration:__ @i11@

    __defined at:__ @attributes\/visibility_attributes.h:76:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i11_ptr :: Ptr.Ptr FC.CInt
i11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_4e591df5ac4216c9

{-| __unique:__ @ExampleNothingget_i12_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_7a8621a15e9246c4" hs_bindgen_test_attributesvisibility_attribut_7a8621a15e9246c4 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i12_ptr #-}

{-| __C declaration:__ @i12@

    __defined at:__ @attributes\/visibility_attributes.h:77:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i12_ptr :: Ptr.Ptr FC.CInt
i12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_7a8621a15e9246c4

{-| __unique:__ @ExampleNothingget_i13_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_96cb39c8a898775e" hs_bindgen_test_attributesvisibility_attribut_96cb39c8a898775e ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i13_ptr #-}

{-| __C declaration:__ @i13@

    __defined at:__ @attributes\/visibility_attributes.h:78:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i13_ptr :: Ptr.Ptr FC.CInt
i13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_96cb39c8a898775e

{-| __unique:__ @ExampleNothingget_i14_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_9f817ee25723fbcd" hs_bindgen_test_attributesvisibility_attribut_9f817ee25723fbcd ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i14_ptr #-}

{-| __C declaration:__ @i14@

    __defined at:__ @attributes\/visibility_attributes.h:79:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i14_ptr :: Ptr.Ptr FC.CInt
i14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_9f817ee25723fbcd

{-| __unique:__ @ExampleNothingget_i15_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_0f8755cf60822f2e" hs_bindgen_test_attributesvisibility_attribut_0f8755cf60822f2e ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i15_ptr #-}

{-| __C declaration:__ @i15@

    __defined at:__ @attributes\/visibility_attributes.h:82:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i15_ptr :: Ptr.Ptr FC.CInt
i15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_0f8755cf60822f2e

{-| __unique:__ @ExampleNothingget_i16_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_3023a5c63e753d58" hs_bindgen_test_attributesvisibility_attribut_3023a5c63e753d58 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i16_ptr #-}

{-| __C declaration:__ @i16@

    __defined at:__ @attributes\/visibility_attributes.h:83:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i16_ptr :: Ptr.Ptr FC.CInt
i16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_3023a5c63e753d58

{-| __unique:__ @ExampleNothingget_i17_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b5610fa653ca61e6" hs_bindgen_test_attributesvisibility_attribut_b5610fa653ca61e6 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i17_ptr #-}

{-| __C declaration:__ @i17@

    __defined at:__ @attributes\/visibility_attributes.h:84:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i17_ptr :: Ptr.Ptr FC.CInt
i17_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_b5610fa653ca61e6

{-| __unique:__ @ExampleNothingget_i18_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_17edf053c36e012c" hs_bindgen_test_attributesvisibility_attribut_17edf053c36e012c ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i18_ptr #-}

{-| __C declaration:__ @i18@

    __defined at:__ @attributes\/visibility_attributes.h:85:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i18_ptr :: Ptr.Ptr FC.CInt
i18_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_17edf053c36e012c

{-| __unique:__ @ExampleNothingget_i19_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_df1c0751f896e6b8" hs_bindgen_test_attributesvisibility_attribut_df1c0751f896e6b8 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i19_ptr #-}

{-| __C declaration:__ @i19@

    __defined at:__ @attributes\/visibility_attributes.h:86:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i19_ptr :: Ptr.Ptr FC.CInt
i19_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_df1c0751f896e6b8
