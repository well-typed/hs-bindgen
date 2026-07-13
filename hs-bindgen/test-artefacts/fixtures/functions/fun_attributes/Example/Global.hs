{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.i
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_attributes.h>"
  , "/* test_functionsfun_attributes_Example_get_i */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_cd32cb4982dd2d1a (void)"
  , "{"
  , "  return &i;"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_attributes_Example_get_i@
foreign import ccall unsafe "hs_bindgen_cd32cb4982dd2d1a" hs_bindgen_cd32cb4982dd2d1a_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_i@
hs_bindgen_cd32cb4982dd2d1a :: IO (BG.Ptr BG.CInt)
hs_bindgen_cd32cb4982dd2d1a =
  BG.fromFFIType hs_bindgen_cd32cb4982dd2d1a_base

{-# NOINLINE i #-}
{-| __C declaration:__ @i@

    __defined at:__ @functions\/fun_attributes.h 132:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
i :: BG.Ptr BG.CInt
i = BG.unsafePerformIO hs_bindgen_cd32cb4982dd2d1a
