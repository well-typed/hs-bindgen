{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.a
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/object_like_as_function_like.h>"
  , "/* test_macrosobject_like_as_function_Example_get_a */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_06ace787c069879f (void)"
  , "{"
  , "  return &a;"
  , "}"
  ]))

-- __unique:__ @test_macrosobject_like_as_function_Example_get_a@
foreign import ccall unsafe "hs_bindgen_06ace787c069879f" hs_bindgen_06ace787c069879f_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosobject_like_as_function_Example_get_a@
hs_bindgen_06ace787c069879f :: IO (BG.Ptr BG.CInt)
hs_bindgen_06ace787c069879f =
  BG.fromFFIType hs_bindgen_06ace787c069879f_base

{-# NOINLINE a #-}
{-| __C declaration:__ @a@

    __defined at:__ @macros\/object_like_as_function_like.h 13:5@

    __exported by:__ @macros\/object_like_as_function_like.h@
-}
a :: BG.Ptr BG.CInt
a = BG.unsafePerformIO hs_bindgen_06ace787c069879f
