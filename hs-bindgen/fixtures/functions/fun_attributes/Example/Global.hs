{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_i@
hs_bindgen_cd32cb4982dd2d1a :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_cd32cb4982dd2d1a =
  RIP.fromFFIType hs_bindgen_cd32cb4982dd2d1a_base

{-# NOINLINE i #-}
{-| __C declaration:__ @i@

    __defined at:__ @functions\/fun_attributes.h 132:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
i :: RIP.Ptr RIP.CInt
i = RIP.unsafePerformIO hs_bindgen_cd32cb4982dd2d1a
