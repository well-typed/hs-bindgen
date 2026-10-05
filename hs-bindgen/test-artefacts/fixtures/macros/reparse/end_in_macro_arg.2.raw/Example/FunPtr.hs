{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.f
    , Example.FunPtr.g
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/end_in_macro_arg.h>"
  , "/* test_macrosreparseend_in_macro_ar_Example_get_f */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_96f030fb3ea3d43d (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  , "/* test_macrosreparseend_in_macro_ar_Example_get_g */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_cd7b12aa8c3ceb75 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &g;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_get_f@
foreign import ccall unsafe "hs_bindgen_96f030fb3ea3d43d" hs_bindgen_96f030fb3ea3d43d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_get_f@
hs_bindgen_96f030fb3ea3d43d :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_96f030fb3ea3d43d =
  fmap BG.fromFFIType hs_bindgen_96f030fb3ea3d43d_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 13:3@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
f :: BG.FunPtr (BG.CInt -> IO BG.CInt)
f = BG.unsafePerformIO hs_bindgen_96f030fb3ea3d43d

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_get_g@
foreign import ccall unsafe "hs_bindgen_cd7b12aa8c3ceb75" hs_bindgen_cd7b12aa8c3ceb75_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_get_g@
hs_bindgen_cd7b12aa8c3ceb75 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_cd7b12aa8c3ceb75 =
  fmap BG.fromFFIType hs_bindgen_cd7b12aa8c3ceb75_base

{-# NOINLINE g #-}
{-| __C declaration:__ @g@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 20:5@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
g :: BG.FunPtr (BG.CInt -> IO BG.CInt)
g = BG.unsafePerformIO hs_bindgen_cd7b12aa8c3ceb75
