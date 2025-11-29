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
  , "/* get_f0_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_a03f2cbeac50b3d3 (void)) (void)"
  , "{"
  , "  return &f0;"
  , "}"
  , "/* get_f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_5469bdc0395f86c1 (void)) (void)"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_490ca7e8c8282a69 (void)) (void)"
  , "{"
  , "  return &f2;"
  , "}"
  , "/* get_f3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_38506a9ac5626bf2 (void)) (void)"
  , "{"
  , "  return &f3;"
  , "}"
  , "/* get_f4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_10e5fac8fefa811b (void)) (void)"
  , "{"
  , "  return &f4;"
  , "}"
  , "/* get_f5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_3f137e2ee71fd73b (void)) (void)"
  , "{"
  , "  return &f5;"
  , "}"
  , "/* get_f6_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_b69720e01b3b7ccd (void)) (void)"
  , "{"
  , "  return &f6;"
  , "}"
  , "/* get_f7_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_97be5f53b506f3b5 (void)) (void)"
  , "{"
  , "  return &f7;"
  , "}"
  , "/* get_f8_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_ae7ef3d579d77d0b (void)) (void)"
  , "{"
  , "  return &f8;"
  , "}"
  , "/* get_f9_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_ef6d611329a20b40 (void)) (void)"
  , "{"
  , "  return &f9;"
  , "}"
  , "/* get_f10_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_45797238134784ac (void)) (void)"
  , "{"
  , "  return &f10;"
  , "}"
  , "/* get_f11_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_f09c80553786e039 (void)) (void)"
  , "{"
  , "  return &f11;"
  , "}"
  , "/* get_f12_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_2e9999eac1cab3da (void)) (void)"
  , "{"
  , "  return &f12;"
  , "}"
  , "/* get_f13_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_32e5be8a3f3ac037 (void)) (void)"
  , "{"
  , "  return &f13;"
  , "}"
  , "/* get_f14_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_0b00a23924c6dc70 (void)) (void)"
  , "{"
  , "  return &f14;"
  , "}"
  , "/* get_f15_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_0d2891732562e5ef (void)) (void)"
  , "{"
  , "  return &f15;"
  , "}"
  , "/* get_f16_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_f25227febbe8db15 (void)) (void)"
  , "{"
  , "  return &f16;"
  , "}"
  , "/* get_f17_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_b90b1824c2839fd2 (void)) (void)"
  , "{"
  , "  return &f17;"
  , "}"
  , "/* get_f18_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_24ba8b98fe453a5c (void)) (void)"
  , "{"
  , "  return &f18;"
  , "}"
  , "/* get_f19_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_b857c57c0cf79909 (void)) (void)"
  , "{"
  , "  return &f19;"
  , "}"
  , "/* get_f20_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_d695cc521dd39753 (void)) (void)"
  , "{"
  , "  return &f20;"
  , "}"
  , "/* get_f21_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_7311dbbdd00abedc (void)) (void)"
  , "{"
  , "  return &f21;"
  , "}"
  , "/* get_f22_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_4246b9344ad4db0d (void)) (void)"
  , "{"
  , "  return &f22;"
  , "}"
  , "/* get_f23_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_dcef056ccb5953f9 (void)) (void)"
  , "{"
  , "  return &f23;"
  , "}"
  , "/* get_f24_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_9ab6df359be6d370 (void)) (void)"
  , "{"
  , "  return &f24;"
  , "}"
  , "/* get_f25_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_80cad6f0afd3f1fc (void)) (void)"
  , "{"
  , "  return &f25;"
  , "}"
  , "/* get_f26_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_b1580cdccf30f552 (void)) (void)"
  , "{"
  , "  return &f26;"
  , "}"
  , "/* get_f27_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_2e4891a5e2afe0df (void)) (void)"
  , "{"
  , "  return &f27;"
  , "}"
  , "/* get_f28_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_e2949a9b7b7cbfc0 (void)) (void)"
  , "{"
  , "  return &f28;"
  , "}"
  , "/* get_f29_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_attributesvisibility_attribut_1224b39f0e8e72cd (void)) (void)"
  , "{"
  , "  return &f29;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_f0_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_a03f2cbeac50b3d3" hs_bindgen_test_attributesvisibility_attribut_a03f2cbeac50b3d3 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f0_ptr #-}

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h:17:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f0_ptr :: Ptr.FunPtr (IO ())
f0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_a03f2cbeac50b3d3

{-| __unique:__ @ExampleNothingget_f1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_5469bdc0395f86c1" hs_bindgen_test_attributesvisibility_attribut_5469bdc0395f86c1 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f1_ptr #-}

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h:18:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f1_ptr :: Ptr.FunPtr (IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_5469bdc0395f86c1

{-| __unique:__ @ExampleNothingget_f2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_490ca7e8c8282a69" hs_bindgen_test_attributesvisibility_attribut_490ca7e8c8282a69 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h:19:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f2_ptr :: Ptr.FunPtr (IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_490ca7e8c8282a69

{-| __unique:__ @ExampleNothingget_f3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_38506a9ac5626bf2" hs_bindgen_test_attributesvisibility_attribut_38506a9ac5626bf2 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f3_ptr #-}

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h:20:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f3_ptr :: Ptr.FunPtr (IO ())
f3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_38506a9ac5626bf2

{-| __unique:__ @ExampleNothingget_f4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_10e5fac8fefa811b" hs_bindgen_test_attributesvisibility_attribut_10e5fac8fefa811b ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f4_ptr #-}

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h:21:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f4_ptr :: Ptr.FunPtr (IO ())
f4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_10e5fac8fefa811b

{-| __unique:__ @ExampleNothingget_f5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_3f137e2ee71fd73b" hs_bindgen_test_attributesvisibility_attribut_3f137e2ee71fd73b ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f5_ptr #-}

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h:24:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f5_ptr :: Ptr.FunPtr (IO ())
f5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_3f137e2ee71fd73b

{-| __unique:__ @ExampleNothingget_f6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b69720e01b3b7ccd" hs_bindgen_test_attributesvisibility_attribut_b69720e01b3b7ccd ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f6_ptr #-}

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h:25:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f6_ptr :: Ptr.FunPtr (IO ())
f6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_b69720e01b3b7ccd

{-| __unique:__ @ExampleNothingget_f7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_97be5f53b506f3b5" hs_bindgen_test_attributesvisibility_attribut_97be5f53b506f3b5 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f7_ptr #-}

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h:26:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f7_ptr :: Ptr.FunPtr (IO ())
f7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_97be5f53b506f3b5

{-| __unique:__ @ExampleNothingget_f8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_ae7ef3d579d77d0b" hs_bindgen_test_attributesvisibility_attribut_ae7ef3d579d77d0b ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f8_ptr #-}

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h:27:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f8_ptr :: Ptr.FunPtr (IO ())
f8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_ae7ef3d579d77d0b

{-| __unique:__ @ExampleNothingget_f9_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_ef6d611329a20b40" hs_bindgen_test_attributesvisibility_attribut_ef6d611329a20b40 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f9_ptr #-}

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h:28:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f9_ptr :: Ptr.FunPtr (IO ())
f9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_ef6d611329a20b40

{-| __unique:__ @ExampleNothingget_f10_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_45797238134784ac" hs_bindgen_test_attributesvisibility_attribut_45797238134784ac ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f10_ptr #-}

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h:31:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f10_ptr :: Ptr.FunPtr (IO ())
f10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_45797238134784ac

{-| __unique:__ @ExampleNothingget_f11_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_f09c80553786e039" hs_bindgen_test_attributesvisibility_attribut_f09c80553786e039 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f11_ptr #-}

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h:32:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f11_ptr :: Ptr.FunPtr (IO ())
f11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_f09c80553786e039

{-| __unique:__ @ExampleNothingget_f12_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_2e9999eac1cab3da" hs_bindgen_test_attributesvisibility_attribut_2e9999eac1cab3da ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f12_ptr #-}

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h:33:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f12_ptr :: Ptr.FunPtr (IO ())
f12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_2e9999eac1cab3da

{-| __unique:__ @ExampleNothingget_f13_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_32e5be8a3f3ac037" hs_bindgen_test_attributesvisibility_attribut_32e5be8a3f3ac037 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f13_ptr #-}

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h:34:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f13_ptr :: Ptr.FunPtr (IO ())
f13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_32e5be8a3f3ac037

{-| __unique:__ @ExampleNothingget_f14_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_0b00a23924c6dc70" hs_bindgen_test_attributesvisibility_attribut_0b00a23924c6dc70 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f14_ptr #-}

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h:35:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f14_ptr :: Ptr.FunPtr (IO ())
f14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_0b00a23924c6dc70

{-| __unique:__ @ExampleNothingget_f15_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_0d2891732562e5ef" hs_bindgen_test_attributesvisibility_attribut_0d2891732562e5ef ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f15_ptr #-}

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h:38:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f15_ptr :: Ptr.FunPtr (IO ())
f15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_0d2891732562e5ef

{-| __unique:__ @ExampleNothingget_f16_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_f25227febbe8db15" hs_bindgen_test_attributesvisibility_attribut_f25227febbe8db15 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f16_ptr #-}

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h:39:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f16_ptr :: Ptr.FunPtr (IO ())
f16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_f25227febbe8db15

{-| __unique:__ @ExampleNothingget_f17_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b90b1824c2839fd2" hs_bindgen_test_attributesvisibility_attribut_b90b1824c2839fd2 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f17_ptr #-}

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h:40:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f17_ptr :: Ptr.FunPtr (IO ())
f17_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_b90b1824c2839fd2

{-| __unique:__ @ExampleNothingget_f18_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_24ba8b98fe453a5c" hs_bindgen_test_attributesvisibility_attribut_24ba8b98fe453a5c ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f18_ptr #-}

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h:41:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f18_ptr :: Ptr.FunPtr (IO ())
f18_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_24ba8b98fe453a5c

{-| __unique:__ @ExampleNothingget_f19_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b857c57c0cf79909" hs_bindgen_test_attributesvisibility_attribut_b857c57c0cf79909 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f19_ptr #-}

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h:42:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f19_ptr :: Ptr.FunPtr (IO ())
f19_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_b857c57c0cf79909

{-| __unique:__ @ExampleNothingget_f20_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_d695cc521dd39753" hs_bindgen_test_attributesvisibility_attribut_d695cc521dd39753 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f20_ptr #-}

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h:45:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f20_ptr :: Ptr.FunPtr (IO ())
f20_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_d695cc521dd39753

{-| __unique:__ @ExampleNothingget_f21_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_7311dbbdd00abedc" hs_bindgen_test_attributesvisibility_attribut_7311dbbdd00abedc ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f21_ptr #-}

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h:46:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f21_ptr :: Ptr.FunPtr (IO ())
f21_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_7311dbbdd00abedc

{-| __unique:__ @ExampleNothingget_f22_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_4246b9344ad4db0d" hs_bindgen_test_attributesvisibility_attribut_4246b9344ad4db0d ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f22_ptr #-}

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h:47:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f22_ptr :: Ptr.FunPtr (IO ())
f22_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_4246b9344ad4db0d

{-| __unique:__ @ExampleNothingget_f23_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_dcef056ccb5953f9" hs_bindgen_test_attributesvisibility_attribut_dcef056ccb5953f9 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f23_ptr #-}

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h:48:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f23_ptr :: Ptr.FunPtr (IO ())
f23_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_dcef056ccb5953f9

{-| __unique:__ @ExampleNothingget_f24_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_9ab6df359be6d370" hs_bindgen_test_attributesvisibility_attribut_9ab6df359be6d370 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f24_ptr #-}

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h:49:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f24_ptr :: Ptr.FunPtr (IO ())
f24_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_9ab6df359be6d370

{-| __unique:__ @ExampleNothingget_f25_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_80cad6f0afd3f1fc" hs_bindgen_test_attributesvisibility_attribut_80cad6f0afd3f1fc ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f25_ptr #-}

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h:52:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f25_ptr :: Ptr.FunPtr (IO ())
f25_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_80cad6f0afd3f1fc

{-| __unique:__ @ExampleNothingget_f26_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_b1580cdccf30f552" hs_bindgen_test_attributesvisibility_attribut_b1580cdccf30f552 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f26_ptr #-}

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h:53:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f26_ptr :: Ptr.FunPtr (IO ())
f26_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_b1580cdccf30f552

{-| __unique:__ @ExampleNothingget_f27_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_2e4891a5e2afe0df" hs_bindgen_test_attributesvisibility_attribut_2e4891a5e2afe0df ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f27_ptr #-}

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h:54:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f27_ptr :: Ptr.FunPtr (IO ())
f27_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_2e4891a5e2afe0df

{-| __unique:__ @ExampleNothingget_f28_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_e2949a9b7b7cbfc0" hs_bindgen_test_attributesvisibility_attribut_e2949a9b7b7cbfc0 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f28_ptr #-}

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h:55:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f28_ptr :: Ptr.FunPtr (IO ())
f28_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_e2949a9b7b7cbfc0

{-| __unique:__ @ExampleNothingget_f29_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesvisibility_attribut_1224b39f0e8e72cd" hs_bindgen_test_attributesvisibility_attribut_1224b39f0e8e72cd ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f29_ptr #-}

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h:56:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f29_ptr :: Ptr.FunPtr (IO ())
f29_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesvisibility_attribut_1224b39f0e8e72cd
