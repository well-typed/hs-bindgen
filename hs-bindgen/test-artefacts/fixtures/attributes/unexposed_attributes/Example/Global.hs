{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.i0
    , Example.Global.i1
    , Example.Global.i2
    , Example.Global.i3
    , Example.Global.i4
    , Example.Global.i5
    , Example.Global.i6
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <attributes/unexposed_attributes.h>"
  , "/* test_attributesunexposed_attribute_Example_get_i0 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_3def60283a7974bb (void)"
  , "{"
  , "  return &i0;"
  , "}"
  , "/* test_attributesunexposed_attribute_Example_get_i1 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_59407b1ef1688834 (void)"
  , "{"
  , "  return &i1;"
  , "}"
  , "/* test_attributesunexposed_attribute_Example_get_i2 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_60ac186adaab0fab (void)"
  , "{"
  , "  return &i2;"
  , "}"
  , "/* test_attributesunexposed_attribute_Example_get_i3 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_1ebd18589bbb3b45 (void)"
  , "{"
  , "  return &i3;"
  , "}"
  , "/* test_attributesunexposed_attribute_Example_get_i4 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_a057c2043ab25421 (void)"
  , "{"
  , "  return &i4;"
  , "}"
  , "/* test_attributesunexposed_attribute_Example_get_i5 */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_0ba9aca4d3be16bb (void)"
  , "{"
  , "  return &i5;"
  , "}"
  , "/* test_attributesunexposed_attribute_Example_get_i6 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_94fe2c6b583b5ad1 (void)"
  , "{"
  , "  return &i6;"
  , "}"
  ]))

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i0@
foreign import ccall unsafe "hs_bindgen_3def60283a7974bb" hs_bindgen_3def60283a7974bb_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i0@
hs_bindgen_3def60283a7974bb :: IO (BG.Ptr BG.CInt)
hs_bindgen_3def60283a7974bb =
  fmap BG.fromFFIType hs_bindgen_3def60283a7974bb_base

{-# NOINLINE i0 #-}
{-| __C declaration:__ @i0@

    __defined at:__ @attributes\/unexposed_attributes.h 12:12@

    __exported by:__ @attributes\/unexposed_attributes.h@
-}
i0 :: BG.Ptr BG.CInt
i0 = BG.unsafePerformIO hs_bindgen_3def60283a7974bb

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i1@
foreign import ccall unsafe "hs_bindgen_59407b1ef1688834" hs_bindgen_59407b1ef1688834_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i1@
hs_bindgen_59407b1ef1688834 :: IO (BG.Ptr BG.CInt)
hs_bindgen_59407b1ef1688834 =
  fmap BG.fromFFIType hs_bindgen_59407b1ef1688834_base

{-# NOINLINE i1 #-}
{-| __C declaration:__ @i1@

    __defined at:__ @attributes\/unexposed_attributes.h 13:12@

    __exported by:__ @attributes\/unexposed_attributes.h@
-}
i1 :: BG.Ptr BG.CInt
i1 = BG.unsafePerformIO hs_bindgen_59407b1ef1688834

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i2@
foreign import ccall unsafe "hs_bindgen_60ac186adaab0fab" hs_bindgen_60ac186adaab0fab_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i2@
hs_bindgen_60ac186adaab0fab :: IO (BG.Ptr BG.CInt)
hs_bindgen_60ac186adaab0fab =
  fmap BG.fromFFIType hs_bindgen_60ac186adaab0fab_base

{-# NOINLINE i2 #-}
{-| __C declaration:__ @i2@

    __defined at:__ @attributes\/unexposed_attributes.h 14:12@

    __exported by:__ @attributes\/unexposed_attributes.h@
-}
i2 :: BG.Ptr BG.CInt
i2 = BG.unsafePerformIO hs_bindgen_60ac186adaab0fab

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i3@
foreign import ccall unsafe "hs_bindgen_1ebd18589bbb3b45" hs_bindgen_1ebd18589bbb3b45_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i3@
hs_bindgen_1ebd18589bbb3b45 :: IO (BG.Ptr BG.CInt)
hs_bindgen_1ebd18589bbb3b45 =
  fmap BG.fromFFIType hs_bindgen_1ebd18589bbb3b45_base

{-# NOINLINE i3 #-}
{-| __C declaration:__ @i3@

    __defined at:__ @attributes\/unexposed_attributes.h 30:12@

    __exported by:__ @attributes\/unexposed_attributes.h@
-}
i3 :: BG.Ptr BG.CInt
i3 = BG.unsafePerformIO hs_bindgen_1ebd18589bbb3b45

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i4@
foreign import ccall unsafe "hs_bindgen_a057c2043ab25421" hs_bindgen_a057c2043ab25421_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i4@
hs_bindgen_a057c2043ab25421 :: IO (BG.Ptr BG.CInt)
hs_bindgen_a057c2043ab25421 =
  fmap BG.fromFFIType hs_bindgen_a057c2043ab25421_base

{-# NOINLINE i4 #-}
{-| __C declaration:__ @i4@

    __defined at:__ @attributes\/unexposed_attributes.h 33:70@

    __exported by:__ @attributes\/unexposed_attributes.h@
-}
i4 :: BG.Ptr BG.CInt
i4 = BG.unsafePerformIO hs_bindgen_a057c2043ab25421

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i5@
foreign import ccall unsafe "hs_bindgen_0ba9aca4d3be16bb" hs_bindgen_0ba9aca4d3be16bb_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i5@
hs_bindgen_0ba9aca4d3be16bb :: IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_0ba9aca4d3be16bb =
  fmap BG.fromFFIType hs_bindgen_0ba9aca4d3be16bb_base

{-# NOINLINE hs_bindgen_00f85c051e57a979 #-}
{-| __C declaration:__ @i5@

    __defined at:__ @attributes\/unexposed_attributes.h 36:18@

    __exported by:__ @attributes\/unexposed_attributes.h@

    __unique:__ @test_attributesunexposed_attribute_Example_i5@
-}
hs_bindgen_00f85c051e57a979 :: PtrConst.PtrConst BG.CInt
hs_bindgen_00f85c051e57a979 =
  BG.unsafePerformIO hs_bindgen_0ba9aca4d3be16bb

{-# NOINLINE i5 #-}
i5 :: BG.CInt
i5 =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_00f85c051e57a979)

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i6@
foreign import ccall unsafe "hs_bindgen_94fe2c6b583b5ad1" hs_bindgen_94fe2c6b583b5ad1_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesunexposed_attribute_Example_get_i6@
hs_bindgen_94fe2c6b583b5ad1 :: IO (BG.Ptr BG.CInt)
hs_bindgen_94fe2c6b583b5ad1 =
  fmap BG.fromFFIType hs_bindgen_94fe2c6b583b5ad1_base

{-# NOINLINE i6 #-}
{-| __C declaration:__ @i6@

    __defined at:__ @attributes\/unexposed_attributes.h 42:12@

    __exported by:__ @attributes\/unexposed_attributes.h@
-}
i6 :: BG.Ptr BG.CInt
i6 = BG.unsafePerformIO hs_bindgen_94fe2c6b583b5ad1
