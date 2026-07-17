{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.h
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/varargs.h>"
  , "/* test_functionsvarargs_Example_get_h */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d7b5ad93f4d7fa04 (void)) (void)"
  , "{"
  , "  return &h;"
  , "}"
  ]))

-- __unique:__ @test_functionsvarargs_Example_get_h@
foreign import ccall unsafe "hs_bindgen_d7b5ad93f4d7fa04" hs_bindgen_d7b5ad93f4d7fa04_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsvarargs_Example_get_h@
hs_bindgen_d7b5ad93f4d7fa04 :: IO (BG.FunPtr (IO ()))
hs_bindgen_d7b5ad93f4d7fa04 =
  BG.fromFFIType hs_bindgen_d7b5ad93f4d7fa04_base

{-# NOINLINE h #-}
{-| __C declaration:__ @h@

    __defined at:__ @functions\/varargs.h 8:6@

    __exported by:__ @functions\/varargs.h@
-}
h :: BG.FunPtr (IO ())
h = BG.unsafePerformIO hs_bindgen_d7b5ad93f4d7fa04
