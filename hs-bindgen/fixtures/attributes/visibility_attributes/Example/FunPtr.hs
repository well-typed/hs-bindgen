{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/visibility_attributes.h>"
  , "/* Example_get_f0_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_39900d6f1c702f6f (void)) (void)"
  , "{"
  , "  return &f0;"
  , "}"
  , "/* Example_get_f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_b864c2909d300a7a (void)) (void)"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* Example_get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_66763a4fad90fe22 (void)) (void)"
  , "{"
  , "  return &f2;"
  , "}"
  , "/* Example_get_f3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_10ff0632953957cf (void)) (void)"
  , "{"
  , "  return &f3;"
  , "}"
  , "/* Example_get_f4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_8f5f028209f340b7 (void)) (void)"
  , "{"
  , "  return &f4;"
  , "}"
  , "/* Example_get_f5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_13d1031dc1246967 (void)) (void)"
  , "{"
  , "  return &f5;"
  , "}"
  , "/* Example_get_f6_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_4e4b9a5d15dcda07 (void)) (void)"
  , "{"
  , "  return &f6;"
  , "}"
  , "/* Example_get_f7_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_ef1cd251a8589ebf (void)) (void)"
  , "{"
  , "  return &f7;"
  , "}"
  , "/* Example_get_f8_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_9855d59676f03eb1 (void)) (void)"
  , "{"
  , "  return &f8;"
  , "}"
  , "/* Example_get_f9_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_dfaf76443486eb4c (void)) (void)"
  , "{"
  , "  return &f9;"
  , "}"
  , "/* Example_get_f10_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_71db365abe24d9cf (void)) (void)"
  , "{"
  , "  return &f10;"
  , "}"
  , "/* Example_get_f11_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_10b8d6eb89f13d97 (void)) (void)"
  , "{"
  , "  return &f11;"
  , "}"
  , "/* Example_get_f12_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_5a3f8dff4a4fde9e (void)) (void)"
  , "{"
  , "  return &f12;"
  , "}"
  , "/* Example_get_f13_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_8463e55ccd90deca (void)) (void)"
  , "{"
  , "  return &f13;"
  , "}"
  , "/* Example_get_f14_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_67c37e7924c16de6 (void)) (void)"
  , "{"
  , "  return &f14;"
  , "}"
  , "/* Example_get_f15_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_06cfd6bf4b542e1b (void)) (void)"
  , "{"
  , "  return &f15;"
  , "}"
  , "/* Example_get_f16_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_ba078945080874d8 (void)) (void)"
  , "{"
  , "  return &f16;"
  , "}"
  , "/* Example_get_f17_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_91be0de755482a8f (void)) (void)"
  , "{"
  , "  return &f17;"
  , "}"
  , "/* Example_get_f18_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_adf127f9fe5d13e2 (void)) (void)"
  , "{"
  , "  return &f18;"
  , "}"
  , "/* Example_get_f19_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_4aa5ecbf37cb0882 (void)) (void)"
  , "{"
  , "  return &f19;"
  , "}"
  , "/* Example_get_f20_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_710495fd8a04c45d (void)) (void)"
  , "{"
  , "  return &f20;"
  , "}"
  , "/* Example_get_f21_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_c1945ae77f583160 (void)) (void)"
  , "{"
  , "  return &f21;"
  , "}"
  , "/* Example_get_f22_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_065c9398694bdb73 (void)) (void)"
  , "{"
  , "  return &f22;"
  , "}"
  , "/* Example_get_f23_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_439bf295fef545be (void)) (void)"
  , "{"
  , "  return &f23;"
  , "}"
  , "/* Example_get_f24_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_eb2afa3b4df44d0d (void)) (void)"
  , "{"
  , "  return &f24;"
  , "}"
  , "/* Example_get_f25_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_30a3d7d340423e8e (void)) (void)"
  , "{"
  , "  return &f25;"
  , "}"
  , "/* Example_get_f26_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_48cfd38359610120 (void)) (void)"
  , "{"
  , "  return &f26;"
  , "}"
  , "/* Example_get_f27_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_3f834643c5a4a67f (void)) (void)"
  , "{"
  , "  return &f27;"
  , "}"
  , "/* Example_get_f28_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_d75fe2c2a9a0ac9e (void)) (void)"
  , "{"
  , "  return &f28;"
  , "}"
  , "/* Example_get_f29_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_bb58990bd67b2ce2 (void)) (void)"
  , "{"
  , "  return &f29;"
  , "}"
  ]))

{-| __unique:__ @Example_get_f0_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_39900d6f1c702f6f" hs_bindgen_test_attributesvisibility_attribut_39900d6f1c702f6f ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f0_ptr #-}

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h:17:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f0_ptr :: Ptr.FunPtr (IO ())
f0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_39900d6f1c702f6f

{-| __unique:__ @Example_get_f1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b864c2909d300a7a" hs_bindgen_test_attributesvisibility_attribut_b864c2909d300a7a ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f1_ptr #-}

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h:18:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f1_ptr :: Ptr.FunPtr (IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_b864c2909d300a7a

{-| __unique:__ @Example_get_f2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_66763a4fad90fe22" hs_bindgen_test_attributesvisibility_attribut_66763a4fad90fe22 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h:19:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f2_ptr :: Ptr.FunPtr (IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_66763a4fad90fe22

{-| __unique:__ @Example_get_f3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_10ff0632953957cf" hs_bindgen_test_attributesvisibility_attribut_10ff0632953957cf ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f3_ptr #-}

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h:20:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f3_ptr :: Ptr.FunPtr (IO ())
f3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_10ff0632953957cf

{-| __unique:__ @Example_get_f4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_8f5f028209f340b7" hs_bindgen_test_attributesvisibility_attribut_8f5f028209f340b7 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f4_ptr #-}

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h:21:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f4_ptr :: Ptr.FunPtr (IO ())
f4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_8f5f028209f340b7

{-| __unique:__ @Example_get_f5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_13d1031dc1246967" hs_bindgen_test_attributesvisibility_attribut_13d1031dc1246967 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f5_ptr #-}

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h:24:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f5_ptr :: Ptr.FunPtr (IO ())
f5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_13d1031dc1246967

{-| __unique:__ @Example_get_f6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_4e4b9a5d15dcda07" hs_bindgen_test_attributesvisibility_attribut_4e4b9a5d15dcda07 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f6_ptr #-}

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h:25:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f6_ptr :: Ptr.FunPtr (IO ())
f6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_4e4b9a5d15dcda07

{-| __unique:__ @Example_get_f7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_ef1cd251a8589ebf" hs_bindgen_test_attributesvisibility_attribut_ef1cd251a8589ebf ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f7_ptr #-}

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h:26:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f7_ptr :: Ptr.FunPtr (IO ())
f7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_ef1cd251a8589ebf

{-| __unique:__ @Example_get_f8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_9855d59676f03eb1" hs_bindgen_test_attributesvisibility_attribut_9855d59676f03eb1 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f8_ptr #-}

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h:27:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f8_ptr :: Ptr.FunPtr (IO ())
f8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_9855d59676f03eb1

{-| __unique:__ @Example_get_f9_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_dfaf76443486eb4c" hs_bindgen_test_attributesvisibility_attribut_dfaf76443486eb4c ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f9_ptr #-}

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h:28:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f9_ptr :: Ptr.FunPtr (IO ())
f9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_dfaf76443486eb4c

{-| __unique:__ @Example_get_f10_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_71db365abe24d9cf" hs_bindgen_test_attributesvisibility_attribut_71db365abe24d9cf ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f10_ptr #-}

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h:31:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f10_ptr :: Ptr.FunPtr (IO ())
f10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_71db365abe24d9cf

{-| __unique:__ @Example_get_f11_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_10b8d6eb89f13d97" hs_bindgen_test_attributesvisibility_attribut_10b8d6eb89f13d97 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f11_ptr #-}

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h:32:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f11_ptr :: Ptr.FunPtr (IO ())
f11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_10b8d6eb89f13d97

{-| __unique:__ @Example_get_f12_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_5a3f8dff4a4fde9e" hs_bindgen_test_attributesvisibility_attribut_5a3f8dff4a4fde9e ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f12_ptr #-}

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h:33:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f12_ptr :: Ptr.FunPtr (IO ())
f12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_5a3f8dff4a4fde9e

{-| __unique:__ @Example_get_f13_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_8463e55ccd90deca" hs_bindgen_test_attributesvisibility_attribut_8463e55ccd90deca ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f13_ptr #-}

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h:34:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f13_ptr :: Ptr.FunPtr (IO ())
f13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_8463e55ccd90deca

{-| __unique:__ @Example_get_f14_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_67c37e7924c16de6" hs_bindgen_test_attributesvisibility_attribut_67c37e7924c16de6 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f14_ptr #-}

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h:35:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f14_ptr :: Ptr.FunPtr (IO ())
f14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_67c37e7924c16de6

{-| __unique:__ @Example_get_f15_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_06cfd6bf4b542e1b" hs_bindgen_test_attributesvisibility_attribut_06cfd6bf4b542e1b ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f15_ptr #-}

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h:38:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f15_ptr :: Ptr.FunPtr (IO ())
f15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_06cfd6bf4b542e1b

{-| __unique:__ @Example_get_f16_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_ba078945080874d8" hs_bindgen_test_attributesvisibility_attribut_ba078945080874d8 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f16_ptr #-}

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h:39:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f16_ptr :: Ptr.FunPtr (IO ())
f16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_ba078945080874d8

{-| __unique:__ @Example_get_f17_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_91be0de755482a8f" hs_bindgen_test_attributesvisibility_attribut_91be0de755482a8f ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f17_ptr #-}

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h:40:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f17_ptr :: Ptr.FunPtr (IO ())
f17_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_91be0de755482a8f

{-| __unique:__ @Example_get_f18_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_adf127f9fe5d13e2" hs_bindgen_test_attributesvisibility_attribut_adf127f9fe5d13e2 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f18_ptr #-}

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h:41:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f18_ptr :: Ptr.FunPtr (IO ())
f18_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_adf127f9fe5d13e2

{-| __unique:__ @Example_get_f19_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_4aa5ecbf37cb0882" hs_bindgen_test_attributesvisibility_attribut_4aa5ecbf37cb0882 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f19_ptr #-}

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h:42:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f19_ptr :: Ptr.FunPtr (IO ())
f19_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_4aa5ecbf37cb0882

{-| __unique:__ @Example_get_f20_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_710495fd8a04c45d" hs_bindgen_test_attributesvisibility_attribut_710495fd8a04c45d ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f20_ptr #-}

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h:45:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f20_ptr :: Ptr.FunPtr (IO ())
f20_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_710495fd8a04c45d

{-| __unique:__ @Example_get_f21_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_c1945ae77f583160" hs_bindgen_test_attributesvisibility_attribut_c1945ae77f583160 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f21_ptr #-}

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h:46:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f21_ptr :: Ptr.FunPtr (IO ())
f21_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_c1945ae77f583160

{-| __unique:__ @Example_get_f22_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_065c9398694bdb73" hs_bindgen_test_attributesvisibility_attribut_065c9398694bdb73 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f22_ptr #-}

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h:47:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f22_ptr :: Ptr.FunPtr (IO ())
f22_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_065c9398694bdb73

{-| __unique:__ @Example_get_f23_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_439bf295fef545be" hs_bindgen_test_attributesvisibility_attribut_439bf295fef545be ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f23_ptr #-}

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h:48:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f23_ptr :: Ptr.FunPtr (IO ())
f23_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_439bf295fef545be

{-| __unique:__ @Example_get_f24_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_eb2afa3b4df44d0d" hs_bindgen_test_attributesvisibility_attribut_eb2afa3b4df44d0d ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f24_ptr #-}

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h:49:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f24_ptr :: Ptr.FunPtr (IO ())
f24_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_eb2afa3b4df44d0d

{-| __unique:__ @Example_get_f25_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_30a3d7d340423e8e" hs_bindgen_test_attributesvisibility_attribut_30a3d7d340423e8e ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f25_ptr #-}

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h:52:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f25_ptr :: Ptr.FunPtr (IO ())
f25_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_30a3d7d340423e8e

{-| __unique:__ @Example_get_f26_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_48cfd38359610120" hs_bindgen_test_attributesvisibility_attribut_48cfd38359610120 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f26_ptr #-}

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h:53:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f26_ptr :: Ptr.FunPtr (IO ())
f26_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_48cfd38359610120

{-| __unique:__ @Example_get_f27_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_3f834643c5a4a67f" hs_bindgen_test_attributesvisibility_attribut_3f834643c5a4a67f ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f27_ptr #-}

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h:54:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f27_ptr :: Ptr.FunPtr (IO ())
f27_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_3f834643c5a4a67f

{-| __unique:__ @Example_get_f28_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_d75fe2c2a9a0ac9e" hs_bindgen_test_attributesvisibility_attribut_d75fe2c2a9a0ac9e ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f28_ptr #-}

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h:55:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f28_ptr :: Ptr.FunPtr (IO ())
f28_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_d75fe2c2a9a0ac9e

{-| __unique:__ @Example_get_f29_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_bb58990bd67b2ce2" hs_bindgen_test_attributesvisibility_attribut_bb58990bd67b2ce2 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f29_ptr #-}

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h:56:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f29_ptr :: Ptr.FunPtr (IO ())
f29_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_bb58990bd67b2ce2
