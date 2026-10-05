{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.arr
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/end_in_macro_arg.h>"
  , "/* test_macrosreparseend_in_macro_ar_Example_get_arr */"
  , "__attribute__ ((const))"
  , "T (*hs_bindgen_fa8758c7fbcb4eb0 (void))[3]"
  , "{"
  , "  return &arr;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_get_arr@
foreign import ccall unsafe "hs_bindgen_fa8758c7fbcb4eb0" hs_bindgen_fa8758c7fbcb4eb0_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_get_arr@
hs_bindgen_fa8758c7fbcb4eb0 :: IO (BG.Ptr (CA.ConstantArray 3 T))
hs_bindgen_fa8758c7fbcb4eb0 =
  fmap BG.fromFFIType hs_bindgen_fa8758c7fbcb4eb0_base

{-# NOINLINE arr #-}
{-| __C declaration:__ @arr@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 23:3@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
arr :: BG.Ptr (CA.ConstantArray 3 T)
arr = BG.unsafePerformIO hs_bindgen_fa8758c7fbcb4eb0
