{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.f
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/end_in_macro_arg.h>"
  , "/* test_macrosreparseend_in_macro_ar_Example_get_f */"
  , "__attribute__ ((const))"
  , "T (*hs_bindgen_96f030fb3ea3d43d (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_get_f@
foreign import ccall unsafe "hs_bindgen_96f030fb3ea3d43d" hs_bindgen_96f030fb3ea3d43d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_get_f@
hs_bindgen_96f030fb3ea3d43d :: IO (BG.FunPtr (T -> IO T))
hs_bindgen_96f030fb3ea3d43d =
  fmap BG.fromFFIType hs_bindgen_96f030fb3ea3d43d_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 12:3@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
f :: BG.FunPtr (T -> IO T)
f = BG.unsafePerformIO hs_bindgen_96f030fb3ea3d43d
