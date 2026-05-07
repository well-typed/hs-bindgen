{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.a
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosobject_like_as_function_Example_get_a@
hs_bindgen_06ace787c069879f :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_06ace787c069879f =
  RIP.fromFFIType hs_bindgen_06ace787c069879f_base

{-# NOINLINE a #-}
{-| __C declaration:__ @a@

    __defined at:__ @macros\/object_like_as_function_like.h 13:5@

    __exported by:__ @macros\/object_like_as_function_like.h@
-}
a :: RIP.Ptr RIP.CInt
a = RIP.unsafePerformIO hs_bindgen_06ace787c069879f
