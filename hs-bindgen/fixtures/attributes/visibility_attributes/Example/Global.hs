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
  , "/* test_attributesvisibility_attribut_Example_get_i0_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_882dcb40c6ad1461 (void)"
  , "{"
  , "  return &i0;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i1_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_67e8ccdb1d25e3ae (void)"
  , "{"
  , "  return &i1;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i2_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_1ce9eb133565b90a (void)"
  , "{"
  , "  return &i2;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i3_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_00fce981bcb56c1a (void)"
  , "{"
  , "  return &i3;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i4_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_c4b37069d8e025e5 (void)"
  , "{"
  , "  return &i4;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i5_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_bd0f182728abf16f (void)"
  , "{"
  , "  return &i5;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i6_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_c53b7cfddc89a6b9 (void)"
  , "{"
  , "  return &i6;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i7_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_72edacaf16ab0c81 (void)"
  , "{"
  , "  return &i7;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i8_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_1b48c1380972701f (void)"
  , "{"
  , "  return &i8;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i9_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_63a2d96d25b60025 (void)"
  , "{"
  , "  return &i9;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i10_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_181bf3398f5fd2d3 (void)"
  , "{"
  , "  return &i10;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i11_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_46ba7aba6f2491ca (void)"
  , "{"
  , "  return &i11;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i12_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_d9e0a613cbcc9f3e (void)"
  , "{"
  , "  return &i12;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i13_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_d02e91d8b8f37508 (void)"
  , "{"
  , "  return &i13;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i14_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_fc03a1c74eda2944 (void)"
  , "{"
  , "  return &i14;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i15_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_1d303eaadfd446c8 (void)"
  , "{"
  , "  return &i15;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i16_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_625545a81d12a4a3 (void)"
  , "{"
  , "  return &i16;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i17_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_c8e2d4272fd70085 (void)"
  , "{"
  , "  return &i17;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i18_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_50f4901f7ed0ca1d (void)"
  , "{"
  , "  return &i18;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i19_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_e70a0a7f087993cf (void)"
  , "{"
  , "  return &i19;"
  , "}"
  ]))

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i0_ptr@
foreign import ccall unsafe "hs_bindgen_882dcb40c6ad1461" hs_bindgen_882dcb40c6ad1461 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i0_ptr #-}

{-| __C declaration:__ @i0@

    __defined at:__ @attributes\/visibility_attributes.h:61:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i0_ptr :: Ptr.Ptr FC.CInt
i0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_882dcb40c6ad1461

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i1_ptr@
foreign import ccall unsafe "hs_bindgen_67e8ccdb1d25e3ae" hs_bindgen_67e8ccdb1d25e3ae ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i1_ptr #-}

{-| __C declaration:__ @i1@

    __defined at:__ @attributes\/visibility_attributes.h:62:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i1_ptr :: Ptr.Ptr FC.CInt
i1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_67e8ccdb1d25e3ae

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i2_ptr@
foreign import ccall unsafe "hs_bindgen_1ce9eb133565b90a" hs_bindgen_1ce9eb133565b90a ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i2_ptr #-}

{-| __C declaration:__ @i2@

    __defined at:__ @attributes\/visibility_attributes.h:63:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i2_ptr :: Ptr.Ptr FC.CInt
i2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1ce9eb133565b90a

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i3_ptr@
foreign import ccall unsafe "hs_bindgen_00fce981bcb56c1a" hs_bindgen_00fce981bcb56c1a ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i3_ptr #-}

{-| __C declaration:__ @i3@

    __defined at:__ @attributes\/visibility_attributes.h:64:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i3_ptr :: Ptr.Ptr FC.CInt
i3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_00fce981bcb56c1a

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i4_ptr@
foreign import ccall unsafe "hs_bindgen_c4b37069d8e025e5" hs_bindgen_c4b37069d8e025e5 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i4_ptr #-}

{-| __C declaration:__ @i4@

    __defined at:__ @attributes\/visibility_attributes.h:65:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i4_ptr :: Ptr.Ptr FC.CInt
i4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c4b37069d8e025e5

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i5_ptr@
foreign import ccall unsafe "hs_bindgen_bd0f182728abf16f" hs_bindgen_bd0f182728abf16f ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i5_ptr #-}

{-| __C declaration:__ @i5@

    __defined at:__ @attributes\/visibility_attributes.h:68:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i5_ptr :: Ptr.Ptr FC.CInt
i5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bd0f182728abf16f

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i6_ptr@
foreign import ccall unsafe "hs_bindgen_c53b7cfddc89a6b9" hs_bindgen_c53b7cfddc89a6b9 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i6_ptr #-}

{-| __C declaration:__ @i6@

    __defined at:__ @attributes\/visibility_attributes.h:69:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i6_ptr :: Ptr.Ptr FC.CInt
i6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c53b7cfddc89a6b9

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i7_ptr@
foreign import ccall unsafe "hs_bindgen_72edacaf16ab0c81" hs_bindgen_72edacaf16ab0c81 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i7_ptr #-}

{-| __C declaration:__ @i7@

    __defined at:__ @attributes\/visibility_attributes.h:70:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i7_ptr :: Ptr.Ptr FC.CInt
i7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_72edacaf16ab0c81

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i8_ptr@
foreign import ccall unsafe "hs_bindgen_1b48c1380972701f" hs_bindgen_1b48c1380972701f ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i8_ptr #-}

{-| __C declaration:__ @i8@

    __defined at:__ @attributes\/visibility_attributes.h:71:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i8_ptr :: Ptr.Ptr FC.CInt
i8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1b48c1380972701f

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i9_ptr@
foreign import ccall unsafe "hs_bindgen_63a2d96d25b60025" hs_bindgen_63a2d96d25b60025 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i9_ptr #-}

{-| __C declaration:__ @i9@

    __defined at:__ @attributes\/visibility_attributes.h:72:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i9_ptr :: Ptr.Ptr FC.CInt
i9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_63a2d96d25b60025

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i10_ptr@
foreign import ccall unsafe "hs_bindgen_181bf3398f5fd2d3" hs_bindgen_181bf3398f5fd2d3 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i10_ptr #-}

{-| __C declaration:__ @i10@

    __defined at:__ @attributes\/visibility_attributes.h:75:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i10_ptr :: Ptr.Ptr FC.CInt
i10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_181bf3398f5fd2d3

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i11_ptr@
foreign import ccall unsafe "hs_bindgen_46ba7aba6f2491ca" hs_bindgen_46ba7aba6f2491ca ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i11_ptr #-}

{-| __C declaration:__ @i11@

    __defined at:__ @attributes\/visibility_attributes.h:76:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i11_ptr :: Ptr.Ptr FC.CInt
i11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_46ba7aba6f2491ca

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i12_ptr@
foreign import ccall unsafe "hs_bindgen_d9e0a613cbcc9f3e" hs_bindgen_d9e0a613cbcc9f3e ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i12_ptr #-}

{-| __C declaration:__ @i12@

    __defined at:__ @attributes\/visibility_attributes.h:77:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i12_ptr :: Ptr.Ptr FC.CInt
i12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d9e0a613cbcc9f3e

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i13_ptr@
foreign import ccall unsafe "hs_bindgen_d02e91d8b8f37508" hs_bindgen_d02e91d8b8f37508 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i13_ptr #-}

{-| __C declaration:__ @i13@

    __defined at:__ @attributes\/visibility_attributes.h:78:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i13_ptr :: Ptr.Ptr FC.CInt
i13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d02e91d8b8f37508

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i14_ptr@
foreign import ccall unsafe "hs_bindgen_fc03a1c74eda2944" hs_bindgen_fc03a1c74eda2944 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i14_ptr #-}

{-| __C declaration:__ @i14@

    __defined at:__ @attributes\/visibility_attributes.h:79:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i14_ptr :: Ptr.Ptr FC.CInt
i14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fc03a1c74eda2944

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i15_ptr@
foreign import ccall unsafe "hs_bindgen_1d303eaadfd446c8" hs_bindgen_1d303eaadfd446c8 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i15_ptr #-}

{-| __C declaration:__ @i15@

    __defined at:__ @attributes\/visibility_attributes.h:82:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i15_ptr :: Ptr.Ptr FC.CInt
i15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1d303eaadfd446c8

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i16_ptr@
foreign import ccall unsafe "hs_bindgen_625545a81d12a4a3" hs_bindgen_625545a81d12a4a3 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i16_ptr #-}

{-| __C declaration:__ @i16@

    __defined at:__ @attributes\/visibility_attributes.h:83:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i16_ptr :: Ptr.Ptr FC.CInt
i16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_625545a81d12a4a3

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i17_ptr@
foreign import ccall unsafe "hs_bindgen_c8e2d4272fd70085" hs_bindgen_c8e2d4272fd70085 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i17_ptr #-}

{-| __C declaration:__ @i17@

    __defined at:__ @attributes\/visibility_attributes.h:84:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i17_ptr :: Ptr.Ptr FC.CInt
i17_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c8e2d4272fd70085

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i18_ptr@
foreign import ccall unsafe "hs_bindgen_50f4901f7ed0ca1d" hs_bindgen_50f4901f7ed0ca1d ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i18_ptr #-}

{-| __C declaration:__ @i18@

    __defined at:__ @attributes\/visibility_attributes.h:85:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i18_ptr :: Ptr.Ptr FC.CInt
i18_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_50f4901f7ed0ca1d

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i19_ptr@
foreign import ccall unsafe "hs_bindgen_e70a0a7f087993cf" hs_bindgen_e70a0a7f087993cf ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i19_ptr #-}

{-| __C declaration:__ @i19@

    __defined at:__ @attributes\/visibility_attributes.h:86:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i19_ptr :: Ptr.Ptr FC.CInt
i19_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e70a0a7f087993cf
