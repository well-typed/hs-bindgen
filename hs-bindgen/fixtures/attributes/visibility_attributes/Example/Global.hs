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
  , "/* test_attributesvisibility_attribut_Example_get_i0 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_70b2711233a0b0ce (void)"
  , "{"
  , "  return &i0;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i1 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_8e3cae6c0337359f (void)"
  , "{"
  , "  return &i1;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i2 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_e9d06c8375760e65 (void)"
  , "{"
  , "  return &i2;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i3 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_de2fe93c2259b4c5 (void)"
  , "{"
  , "  return &i3;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i4 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_dd416cc3f6d465a3 (void)"
  , "{"
  , "  return &i4;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i5 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_71a58d536a57d502 (void)"
  , "{"
  , "  return &i5;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i6 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_64a79cbcb421682d (void)"
  , "{"
  , "  return &i6;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i7 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_1dab5bd455fe1cd2 (void)"
  , "{"
  , "  return &i7;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i8 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_95f3d7edf9ffbfec (void)"
  , "{"
  , "  return &i8;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i9 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_726a1f70db74e014 (void)"
  , "{"
  , "  return &i9;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i10 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_8bf7d37b407412c2 (void)"
  , "{"
  , "  return &i10;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i11 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_9d4d113b052a7d1c (void)"
  , "{"
  , "  return &i11;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i12 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_ce56442ac026dd87 (void)"
  , "{"
  , "  return &i12;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i13 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_5822d4dd4d1c00b5 (void)"
  , "{"
  , "  return &i13;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i14 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_1f5ecb9a293e4e85 (void)"
  , "{"
  , "  return &i14;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i15 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_f8cafe2a4b2f0171 (void)"
  , "{"
  , "  return &i15;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i16 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_9f351d568129877e (void)"
  , "{"
  , "  return &i16;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i17 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_4219dc8cc178afd0 (void)"
  , "{"
  , "  return &i17;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i18 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_60dd1e2f74f319bb (void)"
  , "{"
  , "  return &i18;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_i19 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_795a8bc9f1790423 (void)"
  , "{"
  , "  return &i19;"
  , "}"
  ]))

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i0@
foreign import ccall unsafe "hs_bindgen_70b2711233a0b0ce" hs_bindgen_70b2711233a0b0ce ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i0 #-}

{-| __C declaration:__ @i0@

    __defined at:__ @attributes\/visibility_attributes.h:61:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i0 :: Ptr.Ptr FC.CInt
i0 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_70b2711233a0b0ce

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i1@
foreign import ccall unsafe "hs_bindgen_8e3cae6c0337359f" hs_bindgen_8e3cae6c0337359f ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i1 #-}

{-| __C declaration:__ @i1@

    __defined at:__ @attributes\/visibility_attributes.h:62:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i1 :: Ptr.Ptr FC.CInt
i1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8e3cae6c0337359f

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i2@
foreign import ccall unsafe "hs_bindgen_e9d06c8375760e65" hs_bindgen_e9d06c8375760e65 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i2 #-}

{-| __C declaration:__ @i2@

    __defined at:__ @attributes\/visibility_attributes.h:63:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i2 :: Ptr.Ptr FC.CInt
i2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e9d06c8375760e65

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i3@
foreign import ccall unsafe "hs_bindgen_de2fe93c2259b4c5" hs_bindgen_de2fe93c2259b4c5 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i3 #-}

{-| __C declaration:__ @i3@

    __defined at:__ @attributes\/visibility_attributes.h:64:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i3 :: Ptr.Ptr FC.CInt
i3 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_de2fe93c2259b4c5

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i4@
foreign import ccall unsafe "hs_bindgen_dd416cc3f6d465a3" hs_bindgen_dd416cc3f6d465a3 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i4 #-}

{-| __C declaration:__ @i4@

    __defined at:__ @attributes\/visibility_attributes.h:65:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i4 :: Ptr.Ptr FC.CInt
i4 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dd416cc3f6d465a3

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i5@
foreign import ccall unsafe "hs_bindgen_71a58d536a57d502" hs_bindgen_71a58d536a57d502 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i5 #-}

{-| __C declaration:__ @i5@

    __defined at:__ @attributes\/visibility_attributes.h:68:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i5 :: Ptr.Ptr FC.CInt
i5 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_71a58d536a57d502

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i6@
foreign import ccall unsafe "hs_bindgen_64a79cbcb421682d" hs_bindgen_64a79cbcb421682d ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i6 #-}

{-| __C declaration:__ @i6@

    __defined at:__ @attributes\/visibility_attributes.h:69:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i6 :: Ptr.Ptr FC.CInt
i6 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_64a79cbcb421682d

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i7@
foreign import ccall unsafe "hs_bindgen_1dab5bd455fe1cd2" hs_bindgen_1dab5bd455fe1cd2 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i7 #-}

{-| __C declaration:__ @i7@

    __defined at:__ @attributes\/visibility_attributes.h:70:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i7 :: Ptr.Ptr FC.CInt
i7 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1dab5bd455fe1cd2

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i8@
foreign import ccall unsafe "hs_bindgen_95f3d7edf9ffbfec" hs_bindgen_95f3d7edf9ffbfec ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i8 #-}

{-| __C declaration:__ @i8@

    __defined at:__ @attributes\/visibility_attributes.h:71:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i8 :: Ptr.Ptr FC.CInt
i8 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_95f3d7edf9ffbfec

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i9@
foreign import ccall unsafe "hs_bindgen_726a1f70db74e014" hs_bindgen_726a1f70db74e014 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i9 #-}

{-| __C declaration:__ @i9@

    __defined at:__ @attributes\/visibility_attributes.h:72:48@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i9 :: Ptr.Ptr FC.CInt
i9 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_726a1f70db74e014

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i10@
foreign import ccall unsafe "hs_bindgen_8bf7d37b407412c2" hs_bindgen_8bf7d37b407412c2 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i10 #-}

{-| __C declaration:__ @i10@

    __defined at:__ @attributes\/visibility_attributes.h:75:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i10 :: Ptr.Ptr FC.CInt
i10 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8bf7d37b407412c2

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i11@
foreign import ccall unsafe "hs_bindgen_9d4d113b052a7d1c" hs_bindgen_9d4d113b052a7d1c ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i11 #-}

{-| __C declaration:__ @i11@

    __defined at:__ @attributes\/visibility_attributes.h:76:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i11 :: Ptr.Ptr FC.CInt
i11 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9d4d113b052a7d1c

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i12@
foreign import ccall unsafe "hs_bindgen_ce56442ac026dd87" hs_bindgen_ce56442ac026dd87 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i12 #-}

{-| __C declaration:__ @i12@

    __defined at:__ @attributes\/visibility_attributes.h:77:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i12 :: Ptr.Ptr FC.CInt
i12 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ce56442ac026dd87

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i13@
foreign import ccall unsafe "hs_bindgen_5822d4dd4d1c00b5" hs_bindgen_5822d4dd4d1c00b5 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i13 #-}

{-| __C declaration:__ @i13@

    __defined at:__ @attributes\/visibility_attributes.h:78:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i13 :: Ptr.Ptr FC.CInt
i13 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5822d4dd4d1c00b5

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i14@
foreign import ccall unsafe "hs_bindgen_1f5ecb9a293e4e85" hs_bindgen_1f5ecb9a293e4e85 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i14 #-}

{-| __C declaration:__ @i14@

    __defined at:__ @attributes\/visibility_attributes.h:79:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i14 :: Ptr.Ptr FC.CInt
i14 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1f5ecb9a293e4e85

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i15@
foreign import ccall unsafe "hs_bindgen_f8cafe2a4b2f0171" hs_bindgen_f8cafe2a4b2f0171 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i15 #-}

{-| __C declaration:__ @i15@

    __defined at:__ @attributes\/visibility_attributes.h:82:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i15 :: Ptr.Ptr FC.CInt
i15 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f8cafe2a4b2f0171

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i16@
foreign import ccall unsafe "hs_bindgen_9f351d568129877e" hs_bindgen_9f351d568129877e ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i16 #-}

{-| __C declaration:__ @i16@

    __defined at:__ @attributes\/visibility_attributes.h:83:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i16 :: Ptr.Ptr FC.CInt
i16 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9f351d568129877e

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i17@
foreign import ccall unsafe "hs_bindgen_4219dc8cc178afd0" hs_bindgen_4219dc8cc178afd0 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i17 #-}

{-| __C declaration:__ @i17@

    __defined at:__ @attributes\/visibility_attributes.h:84:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i17 :: Ptr.Ptr FC.CInt
i17 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4219dc8cc178afd0

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i18@
foreign import ccall unsafe "hs_bindgen_60dd1e2f74f319bb" hs_bindgen_60dd1e2f74f319bb ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i18 #-}

{-| __C declaration:__ @i18@

    __defined at:__ @attributes\/visibility_attributes.h:85:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i18 :: Ptr.Ptr FC.CInt
i18 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_60dd1e2f74f319bb

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_i19@
foreign import ccall unsafe "hs_bindgen_795a8bc9f1790423" hs_bindgen_795a8bc9f1790423 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE i19 #-}

{-| __C declaration:__ @i19@

    __defined at:__ @attributes\/visibility_attributes.h:86:55@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
i19 :: Ptr.Ptr FC.CInt
i19 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_795a8bc9f1790423
