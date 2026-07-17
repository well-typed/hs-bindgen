{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo_function
    , Example.FunPtr.foo_33_1
    , Example.FunPtr.foo_33_2
    , Example.FunPtr.foo_33_3
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <comprehensive/c2hsc.h>"
  , "/* test_comprehensivec2hsc_Example_get_foo_function */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_37f32218c55b498b (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo_function;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_foo_33_1 */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_482f80da2191c5d6 (void)) ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return &foo_33_1;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_foo_33_2 */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_c50591bcbfea3260 (void)) ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return &foo_33_2;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_foo_33_3 */"
  , "__attribute__ ((const))"
  , "signed long long (*hs_bindgen_26b60496f499680a (void)) ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return &foo_33_3;"
  , "}"
  ]))

-- __unique:__ @test_comprehensivec2hsc_Example_get_foo_function@
foreign import ccall unsafe "hs_bindgen_37f32218c55b498b" hs_bindgen_37f32218c55b498b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_foo_function@
hs_bindgen_37f32218c55b498b :: IO (BG.FunPtr (BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO ()))
hs_bindgen_37f32218c55b498b =
  BG.fromFFIType hs_bindgen_37f32218c55b498b_base

{-# NOINLINE foo_function #-}
{-| __C declaration:__ @foo_function@

    __defined at:__ @comprehensive\/c2hsc.h 38:6@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_function :: BG.FunPtr (BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO ())
foo_function =
  BG.unsafePerformIO hs_bindgen_37f32218c55b498b

-- __unique:__ @test_comprehensivec2hsc_Example_get_foo_33_1@
foreign import ccall unsafe "hs_bindgen_482f80da2191c5d6" hs_bindgen_482f80da2191c5d6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_foo_33_1@
hs_bindgen_482f80da2191c5d6 :: IO (BG.FunPtr (BG.CUInt -> IO BG.CUInt))
hs_bindgen_482f80da2191c5d6 =
  BG.fromFFIType hs_bindgen_482f80da2191c5d6_base

{-# NOINLINE foo_33_1 #-}
{-| __C declaration:__ @foo_33_1@

    __defined at:__ @comprehensive\/c2hsc.h 73:10@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_1 :: BG.FunPtr (BG.CUInt -> IO BG.CUInt)
foo_33_1 =
  BG.unsafePerformIO hs_bindgen_482f80da2191c5d6

-- __unique:__ @test_comprehensivec2hsc_Example_get_foo_33_2@
foreign import ccall unsafe "hs_bindgen_c50591bcbfea3260" hs_bindgen_c50591bcbfea3260_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_foo_33_2@
hs_bindgen_c50591bcbfea3260 :: IO (BG.FunPtr (BG.CUInt -> IO BG.CUInt))
hs_bindgen_c50591bcbfea3260 =
  BG.fromFFIType hs_bindgen_c50591bcbfea3260_base

{-# NOINLINE foo_33_2 #-}
{-| __C declaration:__ @foo_33_2@

    __defined at:__ @comprehensive\/c2hsc.h 74:51@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_2 :: BG.FunPtr (BG.CUInt -> IO BG.CUInt)
foo_33_2 =
  BG.unsafePerformIO hs_bindgen_c50591bcbfea3260

-- __unique:__ @test_comprehensivec2hsc_Example_get_foo_33_3@
foreign import ccall unsafe "hs_bindgen_26b60496f499680a" hs_bindgen_26b60496f499680a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_foo_33_3@
hs_bindgen_26b60496f499680a :: IO (BG.FunPtr (BG.CLLong -> IO BG.CLLong))
hs_bindgen_26b60496f499680a =
  BG.fromFFIType hs_bindgen_26b60496f499680a_base

{-# NOINLINE foo_33_3 #-}
{-| __C declaration:__ @foo_33_3@

    __defined at:__ @comprehensive\/c2hsc.h 75:52@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
foo_33_3 :: BG.FunPtr (BG.CLLong -> IO BG.CLLong)
foo_33_3 =
  BG.unsafePerformIO hs_bindgen_26b60496f499680a
