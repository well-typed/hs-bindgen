{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsvarargs_Example_get_h@
hs_bindgen_d7b5ad93f4d7fa04 :: IO (RIP.FunPtr (IO ()))
hs_bindgen_d7b5ad93f4d7fa04 =
  RIP.fromFFIType hs_bindgen_d7b5ad93f4d7fa04_base

{-# NOINLINE h #-}
{-| __C declaration:__ @h@

    __defined at:__ @functions\/varargs.h 8:6@

    __exported by:__ @functions\/varargs.h@
-}
h :: RIP.FunPtr (IO ())
h = RIP.unsafePerformIO hs_bindgen_d7b5ad93f4d7fa04
