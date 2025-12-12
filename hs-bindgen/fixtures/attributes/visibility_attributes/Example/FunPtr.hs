{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/visibility_attributes.h>"
  , "/* test_attributesvisibility_attribut_Example_get_f0_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4fa50edab5785792 (void)) (void)"
  , "{"
  , "  return &f0;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c13821592f55652c (void)) (void)"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_eeb8c07b1c7d4892 (void)) (void)"
  , "{"
  , "  return &f2;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_22da6befd7cfebfe (void)) (void)"
  , "{"
  , "  return &f3;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f4_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_50c8df797d6f5c39 (void)) (void)"
  , "{"
  , "  return &f4;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f5_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_495ed8345db40ba2 (void)) (void)"
  , "{"
  , "  return &f5;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f6_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2443b169338ac3f7 (void)) (void)"
  , "{"
  , "  return &f6;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f7_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_949fd6e2edb95316 (void)) (void)"
  , "{"
  , "  return &f7;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f8_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f474c8449f3cc4f7 (void)) (void)"
  , "{"
  , "  return &f8;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f9_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3646a569205d32fd (void)) (void)"
  , "{"
  , "  return &f9;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f10_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3538a19bdbcce7dd (void)) (void)"
  , "{"
  , "  return &f10;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f11_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_212757456c565a4f (void)) (void)"
  , "{"
  , "  return &f11;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f12_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d1984175b52d2a8a (void)) (void)"
  , "{"
  , "  return &f12;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f13_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_318302468a1f1e5b (void)) (void)"
  , "{"
  , "  return &f13;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f14_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ad80c7d6dbd5cae9 (void)) (void)"
  , "{"
  , "  return &f14;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f15_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4cfe90744e725641 (void)) (void)"
  , "{"
  , "  return &f15;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f16_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6d14737fe874b3cb (void)) (void)"
  , "{"
  , "  return &f16;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f17_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_09e8fbff66923029 (void)) (void)"
  , "{"
  , "  return &f17;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f18_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_42a26b5e01e5cd71 (void)) (void)"
  , "{"
  , "  return &f18;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f19_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5176a5601da0207c (void)) (void)"
  , "{"
  , "  return &f19;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f20_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4eaefbdc60946d59 (void)) (void)"
  , "{"
  , "  return &f20;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f21_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_41dfc77185a5f202 (void)) (void)"
  , "{"
  , "  return &f21;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f22_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a5c1f65da28b559c (void)) (void)"
  , "{"
  , "  return &f22;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f23_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6ea6382845ca7a26 (void)) (void)"
  , "{"
  , "  return &f23;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f24_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8f9343a5bdbbe418 (void)) (void)"
  , "{"
  , "  return &f24;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f25_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8185701609035828 (void)) (void)"
  , "{"
  , "  return &f25;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f26_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cba990b34ea47f08 (void)) (void)"
  , "{"
  , "  return &f26;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f27_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d20da035780e1286 (void)) (void)"
  , "{"
  , "  return &f27;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f28_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_53755375b7f63b0d (void)) (void)"
  , "{"
  , "  return &f28;"
  , "}"
  , "/* test_attributesvisibility_attribut_Example_get_f29_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cba78220552029e8 (void)) (void)"
  , "{"
  , "  return &f29;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4fa50edab5785792" hs_bindgen_4fa50edab5785792_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f0_ptr@
hs_bindgen_4fa50edab5785792 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_4fa50edab5785792 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4fa50edab5785792_base

{-# NOINLINE f0_ptr #-}

{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility_attributes.h:17:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f0_ptr :: Ptr.FunPtr (IO ())
f0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4fa50edab5785792

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c13821592f55652c" hs_bindgen_c13821592f55652c_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f1_ptr@
hs_bindgen_c13821592f55652c ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_c13821592f55652c =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c13821592f55652c_base

{-# NOINLINE f1_ptr #-}

{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility_attributes.h:18:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f1_ptr :: Ptr.FunPtr (IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c13821592f55652c

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_eeb8c07b1c7d4892" hs_bindgen_eeb8c07b1c7d4892_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f2_ptr@
hs_bindgen_eeb8c07b1c7d4892 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_eeb8c07b1c7d4892 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_eeb8c07b1c7d4892_base

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility_attributes.h:19:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f2_ptr :: Ptr.FunPtr (IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_eeb8c07b1c7d4892

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_22da6befd7cfebfe" hs_bindgen_22da6befd7cfebfe_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f3_ptr@
hs_bindgen_22da6befd7cfebfe ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_22da6befd7cfebfe =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_22da6befd7cfebfe_base

{-# NOINLINE f3_ptr #-}

{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility_attributes.h:20:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f3_ptr :: Ptr.FunPtr (IO ())
f3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_22da6befd7cfebfe

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_50c8df797d6f5c39" hs_bindgen_50c8df797d6f5c39_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f4_ptr@
hs_bindgen_50c8df797d6f5c39 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_50c8df797d6f5c39 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_50c8df797d6f5c39_base

{-# NOINLINE f4_ptr #-}

{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility_attributes.h:21:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f4_ptr :: Ptr.FunPtr (IO ())
f4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_50c8df797d6f5c39

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_495ed8345db40ba2" hs_bindgen_495ed8345db40ba2_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f5_ptr@
hs_bindgen_495ed8345db40ba2 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_495ed8345db40ba2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_495ed8345db40ba2_base

{-# NOINLINE f5_ptr #-}

{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility_attributes.h:24:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f5_ptr :: Ptr.FunPtr (IO ())
f5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_495ed8345db40ba2

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2443b169338ac3f7" hs_bindgen_2443b169338ac3f7_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f6_ptr@
hs_bindgen_2443b169338ac3f7 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_2443b169338ac3f7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2443b169338ac3f7_base

{-# NOINLINE f6_ptr #-}

{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility_attributes.h:25:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f6_ptr :: Ptr.FunPtr (IO ())
f6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2443b169338ac3f7

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_949fd6e2edb95316" hs_bindgen_949fd6e2edb95316_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f7_ptr@
hs_bindgen_949fd6e2edb95316 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_949fd6e2edb95316 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_949fd6e2edb95316_base

{-# NOINLINE f7_ptr #-}

{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility_attributes.h:26:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f7_ptr :: Ptr.FunPtr (IO ())
f7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_949fd6e2edb95316

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f474c8449f3cc4f7" hs_bindgen_f474c8449f3cc4f7_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f8_ptr@
hs_bindgen_f474c8449f3cc4f7 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_f474c8449f3cc4f7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f474c8449f3cc4f7_base

{-# NOINLINE f8_ptr #-}

{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility_attributes.h:27:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f8_ptr :: Ptr.FunPtr (IO ())
f8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f474c8449f3cc4f7

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3646a569205d32fd" hs_bindgen_3646a569205d32fd_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f9_ptr@
hs_bindgen_3646a569205d32fd ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_3646a569205d32fd =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3646a569205d32fd_base

{-# NOINLINE f9_ptr #-}

{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility_attributes.h:28:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f9_ptr :: Ptr.FunPtr (IO ())
f9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3646a569205d32fd

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3538a19bdbcce7dd" hs_bindgen_3538a19bdbcce7dd_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f10_ptr@
hs_bindgen_3538a19bdbcce7dd ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_3538a19bdbcce7dd =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3538a19bdbcce7dd_base

{-# NOINLINE f10_ptr #-}

{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility_attributes.h:31:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f10_ptr :: Ptr.FunPtr (IO ())
f10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3538a19bdbcce7dd

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_212757456c565a4f" hs_bindgen_212757456c565a4f_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f11_ptr@
hs_bindgen_212757456c565a4f ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_212757456c565a4f =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_212757456c565a4f_base

{-# NOINLINE f11_ptr #-}

{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility_attributes.h:32:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f11_ptr :: Ptr.FunPtr (IO ())
f11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_212757456c565a4f

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d1984175b52d2a8a" hs_bindgen_d1984175b52d2a8a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f12_ptr@
hs_bindgen_d1984175b52d2a8a ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_d1984175b52d2a8a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_d1984175b52d2a8a_base

{-# NOINLINE f12_ptr #-}

{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility_attributes.h:33:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f12_ptr :: Ptr.FunPtr (IO ())
f12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d1984175b52d2a8a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_318302468a1f1e5b" hs_bindgen_318302468a1f1e5b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f13_ptr@
hs_bindgen_318302468a1f1e5b ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_318302468a1f1e5b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_318302468a1f1e5b_base

{-# NOINLINE f13_ptr #-}

{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility_attributes.h:34:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f13_ptr :: Ptr.FunPtr (IO ())
f13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_318302468a1f1e5b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ad80c7d6dbd5cae9" hs_bindgen_ad80c7d6dbd5cae9_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f14_ptr@
hs_bindgen_ad80c7d6dbd5cae9 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_ad80c7d6dbd5cae9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ad80c7d6dbd5cae9_base

{-# NOINLINE f14_ptr #-}

{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility_attributes.h:35:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f14_ptr :: Ptr.FunPtr (IO ())
f14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ad80c7d6dbd5cae9

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4cfe90744e725641" hs_bindgen_4cfe90744e725641_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f15_ptr@
hs_bindgen_4cfe90744e725641 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_4cfe90744e725641 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4cfe90744e725641_base

{-# NOINLINE f15_ptr #-}

{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility_attributes.h:38:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f15_ptr :: Ptr.FunPtr (IO ())
f15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4cfe90744e725641

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_6d14737fe874b3cb" hs_bindgen_6d14737fe874b3cb_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f16_ptr@
hs_bindgen_6d14737fe874b3cb ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_6d14737fe874b3cb =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_6d14737fe874b3cb_base

{-# NOINLINE f16_ptr #-}

{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility_attributes.h:39:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f16_ptr :: Ptr.FunPtr (IO ())
f16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6d14737fe874b3cb

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_09e8fbff66923029" hs_bindgen_09e8fbff66923029_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f17_ptr@
hs_bindgen_09e8fbff66923029 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_09e8fbff66923029 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_09e8fbff66923029_base

{-# NOINLINE f17_ptr #-}

{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility_attributes.h:40:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f17_ptr :: Ptr.FunPtr (IO ())
f17_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_09e8fbff66923029

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_42a26b5e01e5cd71" hs_bindgen_42a26b5e01e5cd71_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f18_ptr@
hs_bindgen_42a26b5e01e5cd71 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_42a26b5e01e5cd71 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_42a26b5e01e5cd71_base

{-# NOINLINE f18_ptr #-}

{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility_attributes.h:41:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f18_ptr :: Ptr.FunPtr (IO ())
f18_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_42a26b5e01e5cd71

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_5176a5601da0207c" hs_bindgen_5176a5601da0207c_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f19_ptr@
hs_bindgen_5176a5601da0207c ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_5176a5601da0207c =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_5176a5601da0207c_base

{-# NOINLINE f19_ptr #-}

{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility_attributes.h:42:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f19_ptr :: Ptr.FunPtr (IO ())
f19_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5176a5601da0207c

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4eaefbdc60946d59" hs_bindgen_4eaefbdc60946d59_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f20_ptr@
hs_bindgen_4eaefbdc60946d59 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_4eaefbdc60946d59 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4eaefbdc60946d59_base

{-# NOINLINE f20_ptr #-}

{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility_attributes.h:45:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f20_ptr :: Ptr.FunPtr (IO ())
f20_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4eaefbdc60946d59

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_41dfc77185a5f202" hs_bindgen_41dfc77185a5f202_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f21_ptr@
hs_bindgen_41dfc77185a5f202 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_41dfc77185a5f202 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_41dfc77185a5f202_base

{-# NOINLINE f21_ptr #-}

{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility_attributes.h:46:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f21_ptr :: Ptr.FunPtr (IO ())
f21_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_41dfc77185a5f202

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a5c1f65da28b559c" hs_bindgen_a5c1f65da28b559c_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f22_ptr@
hs_bindgen_a5c1f65da28b559c ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_a5c1f65da28b559c =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a5c1f65da28b559c_base

{-# NOINLINE f22_ptr #-}

{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility_attributes.h:47:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f22_ptr :: Ptr.FunPtr (IO ())
f22_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a5c1f65da28b559c

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_6ea6382845ca7a26" hs_bindgen_6ea6382845ca7a26_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f23_ptr@
hs_bindgen_6ea6382845ca7a26 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_6ea6382845ca7a26 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_6ea6382845ca7a26_base

{-# NOINLINE f23_ptr #-}

{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility_attributes.h:48:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f23_ptr :: Ptr.FunPtr (IO ())
f23_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6ea6382845ca7a26

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8f9343a5bdbbe418" hs_bindgen_8f9343a5bdbbe418_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f24_ptr@
hs_bindgen_8f9343a5bdbbe418 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_8f9343a5bdbbe418 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8f9343a5bdbbe418_base

{-# NOINLINE f24_ptr #-}

{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility_attributes.h:49:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f24_ptr :: Ptr.FunPtr (IO ())
f24_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8f9343a5bdbbe418

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8185701609035828" hs_bindgen_8185701609035828_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f25_ptr@
hs_bindgen_8185701609035828 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_8185701609035828 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8185701609035828_base

{-# NOINLINE f25_ptr #-}

{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility_attributes.h:52:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f25_ptr :: Ptr.FunPtr (IO ())
f25_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8185701609035828

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_cba990b34ea47f08" hs_bindgen_cba990b34ea47f08_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f26_ptr@
hs_bindgen_cba990b34ea47f08 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_cba990b34ea47f08 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_cba990b34ea47f08_base

{-# NOINLINE f26_ptr #-}

{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility_attributes.h:53:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f26_ptr :: Ptr.FunPtr (IO ())
f26_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cba990b34ea47f08

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d20da035780e1286" hs_bindgen_d20da035780e1286_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f27_ptr@
hs_bindgen_d20da035780e1286 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_d20da035780e1286 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_d20da035780e1286_base

{-# NOINLINE f27_ptr #-}

{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility_attributes.h:54:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f27_ptr :: Ptr.FunPtr (IO ())
f27_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d20da035780e1286

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_53755375b7f63b0d" hs_bindgen_53755375b7f63b0d_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f28_ptr@
hs_bindgen_53755375b7f63b0d ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_53755375b7f63b0d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_53755375b7f63b0d_base

{-# NOINLINE f28_ptr #-}

{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility_attributes.h:55:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f28_ptr :: Ptr.FunPtr (IO ())
f28_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_53755375b7f63b0d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_cba78220552029e8" hs_bindgen_cba78220552029e8_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_attributesvisibility_attribut_Example_get_f29_ptr@
hs_bindgen_cba78220552029e8 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_cba78220552029e8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_cba78220552029e8_base

{-# NOINLINE f29_ptr #-}

{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility_attributes.h:56:56@

    __exported by:__ @attributes\/visibility_attributes.h@
-}
f29_ptr :: Ptr.FunPtr (IO ())
f29_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cba78220552029e8
